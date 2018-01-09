package scala.lms.tutorial

import scala.lms.common._
import org.scalatest._
import org.scalatest.Assertions._

import scala.collection.mutable.{ArrayBuffer, Map}
import scala.collection.{immutable => imm}

trait AutoDiffSpec2 extends Dsl with MathOps {

  // computation graph (list of nodes) 
  val graph = new ArrayBuffer[Exp]

  def emit(e: Exp): Int = {
    val i = graph.indexOf(e) // hash consing / common subexpression elim
    if (i >= 0) i else try graph.size finally graph += e
  }

  def input(x: Int) = 1*1000*1000 + x // an id not otherwise used (should check in emit)

  // nodes refer to their inputs by index
  type Sym = Int

  abstract class Exp

  case class Const1(a: Double) extends Exp
  case class PlaceHolder(a: Rep[Double]) extends Exp 
  case class Plus(a: Sym, b: Sym) extends Exp
  case class Times(a: Sym, b: Sym) extends Exp
  case class Div(a: Sym, b: Sym) extends Exp
  case class Log(a: Sym) extends Exp
  case class Sin(a: Sym) extends Exp
  case class Cos(a: Sym) extends Exp

  // smart constructors (LMS-style)
  def const(x: Double) = this emit Const1(x)
  def placeholder(x: Rep[Double]) = this emit PlaceHolder(x)

  def plus(x: Double, y: Double): Double = x + y
  def plus(x: Double, y: Sym): Sym = plus(const(x), y)
  def plus(x: Sym, y: Double): Sym = plus(x, const(y))
  def plus(x: Sym, y: Sym): Sym = this emit Plus(x,y)

  def times(x: Double, y: Double): Double = x * y
  def times(x: Double, y: Sym): Sym = times(const(x), y)
  def times(x: Sym, y: Double): Sym = times(x, const(y))
  def times(x: Sym, y: Sym): Sym = 
    if (x < graph.size && graph(x) == Const1(1)) y
    else if (y < graph.size && graph(y) == Const1(1)) x
    else this emit Times(x,y)

  def div(x: Double, y: Double): Double = x / y
  def div(x: Double, y: Sym): Sym = div(const(x), y)
  def div(x: Sym, y: Sym): Sym = this emit Div(x,y)

  def log(x: Sym) = emit(Log(x))
  def sin(x: Sym) = emit(Sin(x))
  def cos(x: Sym) = emit(Cos(x))

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
        case Const1(x) => 
          Nil
        case PlaceHolder(x) =>
          Nil
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
  def evalSome(y: Sym*)(implicit env: Map[Sym,Rep[Double]]) = {
    def evalOne(a: Sym): Rep[Double] = env.getOrElse(a, {
      val r: Rep[Double] = graph(a) match {
        case Const1(x) => x
        case PlaceHolder(x) => x
        case Plus(x, y) => evalOne(x) + evalOne(y)
        case Times(x, y) => evalOne(x) * evalOne(y)
        case Div(x, y) => evalOne(x) / evalOne(y)
        case Log(x) => Math.log(evalOne(x))
        case Sin(x) => Math.sin(evalOne(x))
        case Cos(x) => Math.cos(evalOne(x))
      }
      env(a) = r
      r
    })
    y.map(evalOne)
    /*env.getOrElse(y, {
      // println(graph(y))
      val r: Rep[Double] = graph(y) match {
        case Const1(x) => x
        case PlaceHolder(x) => x
        case Plus(x,y) => eval(x) + eval(y)
        case Times(x,y) => eval(x) * eval(y)
        case Div(x,y) => eval(x) / eval(y)
        case Log(x) => Math.log(eval(x))
        case Sin(x) => Math.sin(eval(x))
        case Cos(x) => Math.cos(eval(x))
      }
      env(y) = r
      r
    })*/
  }
}

class AutoDiffSpec2Test extends TutorialFunSuite {
  
  val under = "autodiff" 

  test("1") {
    val snippet = new DslDriver[Double, Double] with AutoDiffSpec2 with MathOpsExp {
      def snippet(x: Rep[Double]) = {
        def f(x1: Sym, x2: Sym) = times(x1, x2)
        graph += Const1(0)
        val x1 = input(1)
        val y = f(x1, x1)
        val adj = backprop(y)
        val res = evalSome(adj(x1))(Map(x1 -> x))(0)
        graph.clear
        res
      }
    }  
    assert(snippet.eval(3.0) === 6.0)
    check("1", snippet.code)
  }

  test("2") { // second order deriv
    val snippet = new DslDriver[Double, Double] with AutoDiffSpec2 with MathOpsExp {
      def snippet(x : Rep[Double]) = {
        def f(x1: Sym, x2: Sym, x3: Sym) = times(x1, times(x2, x3))
        graph += Const1(0)
        val x1 = input(1)
        val y  = f(x1, x1, x1)
        val adj = backprop(y)
        val want = plus(y, adj(x1))
        val res = evalSome(want)(Map(x1 -> x))(0)
        graph.clear
        res
      }
    }
    assert(snippet.eval(4.0) === 112.0)
    check("2", snippet.code)
  } 


  test("3") { // 2D
    val snippet = new DslDriver[(Double, Double), Double] with AutoDiffSpec2 with MathOpsExp with TupleOpsExp { q =>
      override val codegen = new DslGen with ScalaGenStruct with ScalaGenMathOps {
        val IR: q.type = q
      }

      def snippet(a: Rep[(Double, Double)]) = {
        def f(x1: Sym, x2: Sym) = plus(plus(log(x1), times(x1, x2)), sin(x2))
        graph += Const1(0)
        val x1 = input(1); val x2 = input(2)
        val y = f(x1, x2)
        val adj = backprop(y)
        val res = evalSome(plus(y, plus(adj(x1), adj(x2))))(Map(x1 -> a._1, x2 -> a._2))(0)
        graph.clear
        res
      }
    }
    assert(snippet.eval(2.0, 2.0) === 9.686297770838486)
    check("3", snippet.code)
  }

  test("4") {// gradient descent at 1D
    val snippet = new DslDriver[Double, Unit] with AutoDiffSpec2 with MathOpsExp with TupleOpsExp { q =>
      override val codegen = new DslGen with ScalaGenStruct with ScalaGenMathOps with ScalaGenTupleOps {
        val IR: q.type = q
      }

      def snippet(start: Rep[Double]) : Rep[Unit] = {

        // static inputs
        def model(x: Sym) = plus(sin(x), cos(x))
        val eta = 0.9
        val epsilon = 0.00001

        def value (f: Sym => Sym) = {
          val x1 = input(1)
          val y = f(x1)
          (x0: Rep[Double]) => evalSome(y)(Map(x1 -> x0))(0)
        }

        def grad (f: Sym => Sym) = {
          val x1 = input(1)
          val y = f(x1)
          val adj = backprop(y)
          (x0: Rep[Double]) => evalSome(adj(x1))(Map(x1 -> x0))(0)
        }

        // use while loop for it
        var gradient = grad(model)(start)
        var curr = start
        while (Math.abs(gradient) >= epsilon) {
          curr = curr - eta * gradient
          gradient = grad(model)(curr)
        }
        val best_x = readVar(curr)
        val best_y = value(model)(best_x)
        printf("best_x is %f\n", best_x)
        printf("best_y is %f\n", best_y)
    
        /*
        def gd(f: Sym => Sym, x0: Rep[Double]) = {
          def desc: Rep[Double => Double] = fun { (x: Rep[Double]) =>
            def l2norm(x: Rep[Double]) = Math.abs(x)
            val g = grad(f)(x)
            val x1 = x - eta * g
            if (l2norm(g) < epsilon) x else desc(x1)
          }
          desc(x0)
        }

        // results
        val best_x = gd(model, start)
        val best_y = value(model)(best_x)
        return (best_y, best_x)*/
      }
    }
    snippet.eval(0.5)
    check("4", snippet.code)
  }


  test("5") { // from: http://diffsharp.github.io/DiffSharp/examples-gradientdescent.html    
    val snippet = new DslDriver[(Double, Double), Unit] with AutoDiffSpec2 with MathOpsExp with TupleOpsExp { q =>
      override val codegen = new DslGen with ScalaGenStruct with ScalaGenMathOps {
        val IR: q.type = q
      }

      def snippet(start: Rep[(Double,Double)]): Rep[Unit] = {

        // static input:
        def model(x1: Sym, x2: Sym) = plus(sin(x1), cos(x2))
        val eta = 0.9
        val epsilon = 0.00001

        def value(f: (Sym, Sym) => Sym) = {
          val x1 = input(1); val x2 = input(2)
          val y = f(x1, x2)
          (x0: Rep[Double], x00: Rep[Double]) =>
            evalSome(y)(Map(x1 -> x0, x2 -> x00))(0)
        }

        def grad(f: (Sym, Sym) => Sym) = {
          val x1 = input(1); val x2 = input(2)
          val y = f(x1, x2)
          val adj = backprop(y)
          (x0: Rep[Double], x00: Rep[Double]) => {
              val temp = evalSome(adj(x1), adj(x2))(Map(x1 -> x0, x2 -> x00))
              (temp(0), temp(1))
            }
        }

        def l2norm(x0: Rep[Double], x00: Rep[Double]) = Math.sqrt(x0 * x0 + x00 * x00)

        // use while loop for it
        val temp = grad(model)(start._1, start._2)
        var grad1 = temp._1
        var grad2 = temp._2
        var curr1 = start._1
        var curr2 = start._2
        while (l2norm(grad1, grad2) >= epsilon) {
          curr1 = curr1 - eta * grad1
          curr2 = curr2 - eta * grad2
          val temp1 = grad(model)(curr1, curr2)
          grad1 = temp1._1
          grad2 = temp1._2
        }
        val best_x1 = readVar(curr1)
        val best_x2 = readVar(curr2)
        val best_y = value(model)(best_x1, best_x2)
        printf("best_x1 is %f\n", best_x1)
        printf("best_x2 is %f\n", best_x2)
        printf("best_y is %f\n", best_y)

        /*
        def gd(f: (Sym, Sym) => Sym, x0: Rep[(Double, Double)], eta: Rep[Double], epsilon: Rep[Double]) = {
          def desc: Rep[(Double, Double) => (Double, Double)] = fun { (x: Rep[(Double, Double)]) =>
            def l2norm(x: Rep[(Double, Double)]) = Math.sqrt(x._1 * x._1 + x._2 * x._2)
            val g = grad(f)(x)
            val x1 = (x._1 - eta * g._1, x._2 - eta * g._2)
            if (l2norm(g) < epsilon) x else desc(x1)
          }
          desc(x0)
        }
 
        val result = gd(model, (start._1, start._2))
        (value(model)(result), result._1, result._2)
        */
      }    
    }
    snippet.eval((0.0, 1.0))
    check("5", snippet.code)
  }
}

/*
  // ----- test cases -----

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

  test("1") {
    def f(x1: Sym, x2: Sym) = times(x1, x2)
    graph += Const(0)
    val x1 = input(1)
    val y = f(x1, x1)
    val adj = backprop(y)
    def g(xx: Double) = eval(y)(Map(x1 -> xx)) + eval(adj(x1))(Map(x1 -> xx))
        
    val snippet = new DslDriver[Double,Double] {
      def snippet(x: Rep[Double]) = {
	g(x)
      }
    } 
    assert(snippet.eval(3.0) === 15.0)
    check("1", snippet.code)
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
*/