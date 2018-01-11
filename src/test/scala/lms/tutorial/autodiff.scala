package scala.lms.tutorial

import scala.lms.common._
import org.scalatest._
import org.scalatest.Assertions._

import scala.collection.mutable._

class AutoDiffSpec extends FunSuite {

  // computation graph (list of nodes)
  val graph = new ArrayBuffer[Exp]

  def emit(e: Exp): Int = {
    val i = graph.indexOf(e) // hash consing / common subexpression elim
    if (i >= 0) i else try graph.size finally graph += e
  }

  def input(x: Int) = 1*1000*1000 + x // an id not otherwise used (should check in emit)

  // nodes refer to their inputs by index
  type Sym = Int

  sealed abstract class Exp

  case class Const(a: Double) extends Exp
  case class Plus(a: Sym, b: Sym) extends Exp
  case class Times(a: Sym, b: Sym) extends Exp
  case class Div(a: Sym, b: Sym) extends Exp
  case class Log(a: Sym) extends Exp
  case class Sin(a: Sym) extends Exp
  case class Cos(a: Sym) extends Exp

  case class ConstB(a: Boolean) extends Exp 
  case class LessThan(a: Sym, b: Sym) extends Exp
  case class EqualTo(a: Sym, b: Sym) extends Exp
  case class Cond(a: Sym, b: Sym, c: Sym) extends Exp

//  abstract class Bool extends Exp
//  case class LessThan(a: Sym, b: Sym) extends Bool
//  case class Equal(a: Sym, b: Sym) extends Bool
//  case class And(a: Bool, b: Bool) extends Bool
//  case class Not(a: Bool) extends Bool
  
  // smart constructors (LMS-style)
  def const(x: Double) = this emit Const(x)

  def plus(x: Double, y: Double): Double = x + y
  def plus(x: Double, y: Sym): Sym = plus(const(x), y)
  def plus(x: Sym, y: Double): Sym = plus(x, const(y))
  def plus(x: Sym, y: Sym): Sym = this emit Plus(x,y)

  def times(x: Double, y: Double): Double = x * y
  def times(x: Double, y: Sym): Sym = times(const(x), y)
  def times(x: Sym, y: Double): Sym = times(x, const(y))
  def times(x: Sym, y: Sym): Sym = 
    if (x < graph.size && graph(x) == Const(1)) y
    else if (y < graph.size && graph(y) == Const(1)) x
    else this emit Times(x,y)

  def div(x: Double, y: Double): Double = x / y
  def div(x: Double, y: Sym): Sym = div(const(x), y)
  def div(x: Sym, y: Sym): Sym = this emit Div(x,y)

  def log(x: Sym) = emit(Log(x))
  def sin(x: Sym) = emit(Sin(x))
  def cos(x: Sym) = emit(Cos(x))

  def const(x: Boolean) = this emit ConstB(x)

  def lessThan(x: Double, y: Double) = x < y
  def lessThan(x: Double, y: Sym): Sym = lessThan(const(x), y)
  def lessThan(x: Sym, y: Double): Sym = lessThan(x, const(y))
  def lessThan(x: Sym, y: Sym): Sym = this emit LessThan(x, y)
  
  def equalTo(x: Double, y: Double) = x == y
  def equalTo(x: Double, y: Sym): Sym = equalTo(const(x), y)
  def equalTo(x: Sym, y: Double): Sym = equalTo(x, const(y))
  def equalTo(x: Sym, y: Sym): Sym = this emit EqualTo(x, y)
  
  def cond(b: Boolean, l: Sym, r: Sym): Sym = if (b) l else r
  def cond(b: Sym, l: Sym, r: Sym) = this emit Cond(b, l, r)

  def printGraph() = {
    println("IR Graph:")
    for ((e,i) <- graph.zipWithIndex) {
      println(s"x$i = $e")
    }
  }

  // compute derivatives, starting from result node y ("dependent var")
  // return a mapping from node id x to ∂x/∂y
  def backprop(y: Sym): Map[Sym,Sym] = backprop(Map(y -> const(1)))
  def backprop(adj: Map[Sym,Sym]): Map[Sym,Sym] = {
    println("Backprop:")

    def add(i: Sym, s: Sym) = {
      if (adj.contains(i))
        adj(i) = plus(adj(i), s)
      else
        adj(i) = s
    }

    val ins = graph.zipWithIndex.toList.reverse // reverse mode
    for ((e,i) <- ins if adj contains i) {
      def add1(j: Sym, s: Sym) = add(j, times(adj(i),s))
      val dep = e match {
        case Const(x) => 
          Nil
        case ConstB(x) =>
          throw new Exception("boolean typed nodes don't have differentiation")
        case LessThan(x, y) =>
          throw new Exception("boolean typed nodes don't have differentiation")
        case EqualTo(x, y) => 
          throw new Exception("boolean typed nodes don't have differentiation")
        case Cond(b, l, r) =>
          graph(b) match {
            case ConstB(bb) => 
              add1(l, cond(b, const(1), const(0))); add1(r, cond(b, const(0), const(1)))
              List((l, if (bb) s"1" else s"0"), (r, if (bb) s"0" else "1"))
            case LessThan(x, y) => 
              add1(l, cond(b, const(1), const(0))); add1(r, cond(b, const(0), const(1)))
              List((l, s"if $x < $y 1 else 0"), (r, s"if $x < $y 0 else 1"))
            case EqualTo(x, y) =>
              add1(l, cond(b, const(1), const(0))); add1(r, cond(b, const(0), const(1)))
              List((l, s"if $x == $y 1 else 0"),(r, s"if $x == $y 0 else 1"))
            case _ => throw new Exception("condition is not boolean type")
          }
        case Plus(x,y) => 
          add1(x, const(1)); add1(y, const(1))
          List((x,s"1"),(y,s"1"))
        case Times(x,y) => 
          add1(x, y); add1(y, x)
          List((x,s"x$y"),(y,s"x$x"))
        case Div(x,y) => 
          ??? //List(x,y)
        case Log(x) => 
          add1(x,div(const(1),x))
          List((x,s"1/x$x"))
        case Sin(x)     => 
          add1(x,cos(x))
          List((x,s"cos(x$x)"))
        case Cos(x) => 
          add1(x,times(const(-1),sin(x)))
          List((x,s"-1*cos(x$x)"))
      }
      for ((d,e) <- dep) {
        println(s"x$d' += x$i' * ∂x$i/∂x$d = x$i' * $e")
      }
    }

    println("---")
    printGraph()
    println("Adjoints:")
    for ((k,v) <- adj) {
      println(s"x$k' = $v")
    }
    adj
  }

  // evaluate result y (mutable env: memoization)
  def eval(y: Sym)(implicit env: Map[Sym,Double]): Double = {
    env.getOrElse(y, {
        println(graph(y))
      val r = graph(y) match {
        case Const(x) => x
        case ConstB(_) => throw new Exception("Bool has no Double value")
        case LessThan(_, _) => throw new Exception("Bool has no Double value")
        case EqualTo(_, _) =>  throw new Exception("Bool has no Double value")
        case Cond(b, l, r) => graph(b) match {
          case ConstB(bb) => if (bb) eval(l) else eval(r)
          case LessThan(x, y) => if (eval(x) < eval(y)) eval(l) else eval(r)
          case EqualTo(x, y) => if (eval(x) == eval(y)) eval(l) else eval(r)
          case _ => throw new Exception("condition is not boolean type")
        }
        case Plus(x,y) => eval(x) + eval(y)
        case Times(x,y) => eval(x) * eval(y)
        case Div(x,y) => eval(x) / eval(y)
        case Log(x) => math.log(eval(x))
        case Sin(x) => math.sin(eval(x))
        case Cos(x) => math.cos(eval(x))
      }
      env(y) = r
      r
    })
  }


  // ----- test cases -----

  test("1") { // constand boolean in ifthenelse
    def f(x1: Sym, x2: Sym) = cond(const(true), x1, x2)
    graph += Const(0)
    val x1 = input(1)
    val x2 = input(2)
    val y = f(x1, x2)
    val nine = eval(y)(Map(x1 -> 9.0, x2 -> 8.0))
    assert(nine == 9.0)
    val adj = backprop(y)
    val one = eval(adj(x1))(Map(x1 -> 9.0, x2 -> 8.0))
    val zero = eval(adj(x2))(Map(x1 -> 9.0, x2 -> 8.0))
    assert (one == 1.0)
    assert (zero == 0.0)
    graph.clear
  }

  test("2") { // model is abs
    def f(x1: Sym) = cond(lessThan(x1, 0.0), times(-1.0, x1), x1)
    graph += Const(0)
    val x1 = input(1)
    val y = f(x1)
    val nine = eval(y)(Map(x1 -> 9.0))
    assert (nine == 9.0)

    val eight = eval(y)(Map(x1 -> -8.0))
    assert (eight == 8.0)

    val adj = backprop(y)
    val one = eval(adj(x1))(Map(x1 -> 3.0))
    assert(one == 1.0)

    val neg_one = eval(adj(x1))(Map(x1 -> -3.0))
    assert(neg_one == -1.0)
    graph.clear
  }

  test("3") { // model is max
    def f(x1: Sym, x2: Sym) = cond(lessThan(x1, x2), x2, x1)
    graph += Const(0)
    val x1 = input(1)
    val x2 = input(2)
    val y = f(x1, x2)
    val nine = eval(y)(Map(x1 -> 9.0, x2 -> 4.0))
    assert (nine == 9.0)
    val eight = eval(y)(Map(x1 -> 4.0, x2 -> 8.0))
    assert(eight == 8.0)

    val adj = backprop(y)
    val one = eval(adj(x1))(Map(x1 -> 4.0, x2 -> 3.0))
    assert (one == 1.0)
    val zero = eval(adj(x2))(Map(x1 -> 4.0, x2 -> 3.0))
    assert (zero == 0.0)

    val one_again = eval(adj(x2))(Map(x1 -> 4.0, x2 -> 6.0))
    assert(one_again == 1.0)
    val zero_again = eval(adj(x1))(Map(x1 -> 4.0, x2 -> 6.0))
    assert(zero_again == 0.0)
    graph.clear
  }

  test("4") { // while loop expressed as recursive function 
    def f(x1: Sym, x2: Sym): Sym = cond(lessThan(x1, x2), f(times(x1, 2.0), x2), x1)
    graph += Const(0)
    val x1 = input(1)
    val x2 = input(2)
    // val y = f(x1, x2) // FIXME: create infinite number of Exp nodes
    graph.clear
  }

  test("5") { // can we provide a loop(N){operations} function using cond?
    def loop(n: Int): Sym => Sym = { (x1: Sym) =>
      if (n > 0) loop(n-1)(times(2.0, x1)) else x1
    }
    val f = loop(3)
    graph += Const(0)
    val x1 = input(1)
    val y = f(x1)

    val sixteen = eval(y)(Map(x1 -> 2.0))
    assert(sixteen == 16.0)

    val adj = backprop(y)
    val eight = eval(adj(x1))(Map(x1 -> 2.0))
    assert(eight == 8.0)

    graph.clear
  }

  test("test1 - deriv") { // x*x -> 2*x

    def f(x1: Sym, x2: Sym) = times(x1,x2)

    graph += Const(0)

    val x1 = input(1)
    val y = f(x1, x1)

    val nine = eval(y)(Map(x1 -> 3.0))
    assert(nine == 9.0)

    val adj = backprop(y)

    val six = eval(adj(x1))(Map(x1 -> 3.0))
    assert(six == 6.0)


    graph.clear
  }


  test("test2 - 2nd order deriv") { // second order deriv

    def f(x1: Sym, x2: Sym, x3: Sym) = times(x1,times(x2,x3))

    graph += Const(0)

    val x1 = input(1)
    val y = f(x1, x1, x1)

    println(s"y: $y")

    val sixtyFour = eval(y)(Map(x1 -> 4.0))
    assert(sixtyFour == 64.0)

    val adj = backprop(y)
    val y1 = adj(x1)

    val fortyEight = eval(y1)(Map(x1 -> 4.0))
    assert(fortyEight == 48.0)

    val adj2 = backprop(y1)
    val y2 = adj2(x1)

    val twentyFour = eval(y2)(Map(x1 -> 4.0))
    assert(twentyFour == 24.0)


    graph.clear
  }




  test("test3 - 2D grad") {
    
    def f(x1: Sym, x2: Sym) = plus(plus(log(x1), times(x1, x2)), sin(x2)) // - sin in paper

    graph += Const(0)

    val x1 = input(1); val x2 = input(2)
    val y = f(x1, x2)

    printGraph()
    backprop(y)

    // XXX not currently testing anything

    graph.clear
  }


  test("test4 - opt") { // from: http://diffsharp.github.io/DiffSharp/examples-gradientdescent.html    

    // evaluate a function
    def fun(f: (Sym,Sym) => Sym) = {
      val x1 = input(1); val x2 = input(2)
      val y = f(x1, x2)
      (x0: (Double,Double)) =>
        eval(y)(Map(x1 -> x0._1, x2 -> x0._2))
    }

    // evaluate a function's gradient
    def grad(f: (Sym,Sym) => Sym) = {
      val x1 = input(1); val x2 = input(2)
      val y = f(x1, x2)
      val adj = backprop(y)

      println(adj)
      (x0: (Double,Double)) => 
        (eval(adj(x1))(Map(x1 -> x0._1, x2 -> x0._2)), // FIXME: efficiency <- should eval together
         eval(adj(x2))(Map(x1 -> x0._1, x2 -> x0._2)))
    }

    // gradient descent optimization
    def gd(f: (Sym,Sym) => Sym, x0: (Double,Double), eta: Double, epsilon: Double) = {
      def desc(x: (Double,Double)): (Double,Double) = {
        def l2norm(x: (Double,Double)) = math.sqrt(x._1*x._1 + x._2*x._2)
        val g = grad(f)(x)
        val x1 = (x._1 - eta * g._1, x._2 - eta * g._2)
        if (l2norm(g) < epsilon) x else desc(x1)
      }
      desc(x0)
    }

    def f(x1: Sym, x2: Sym) = plus(sin(x1), cos(x2))

    val xmin = gd(f, (1.0,1.0), 0.9, 0.00001)
    assert(xmin == (-1.5707907586270724,3.141591963803541)) // (-π/2, π)

    val fxmin = fun(f)(xmin)
    assert(fxmin == -1.9999999999842597)

    println(xmin)
    println(fxmin)

    graph.clear
  }
}

