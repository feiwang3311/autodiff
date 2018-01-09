/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((Double)=>(Double)) {
  def apply(x24:Double): Double = {
    val x25 = println("Backprop:")
    val x26 = println("x1000001' += x2' * ∂x2/∂x1000001 = x2' * x1")
    val x27 = println("x1' += x2' * ∂x2/∂x1 = x2' * x1000001")
    val x28 = println("x1000001' += x1' * ∂x1/∂x1000001 = x1' * x1000001")
    val x29 = println("x1000001' += x1' * ∂x1/∂x1000001 = x1' * x1000001")
    val x30 = println("---")
    val x31 = println("IR Graph:")
    val x32 = println("x0 = Const1(0.0)")
    val x33 = println("x1 = Times(1000001,1000001)")
    val x34 = println("x2 = Times(1000001,1)")
    val x35 = println("x3 = Const1(1.0)")
    val x36 = println("x4 = Plus(1,1)")
    val x37 = println("x5 = Plus(4,1)")
    val x38 = println("Adjoints:")
    val x39 = println("x2' = 3")
    val x40 = println("x1' = 1000001")
    val x41 = println("x1000001' = 5")
    val x42 = x24 * x24
    val x43 = x24 * x42
    val x44 = x42 + x42
    val x45 = x44 + x42
    val x46 = x43 + x45
    x46
  }
}
/*****************************************
End of Generated Code
*******************************************/
