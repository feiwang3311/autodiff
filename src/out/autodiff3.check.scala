/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((scala.Tuple2[Double, Double])=>(Double)) {
  def apply(x45:scala.Tuple2[Double, Double]): Double = {
    val x46 = println("Backprop:")
    val x47 = println("x3' += x5' * ∂x5/∂x3 = x5' * 1")
    val x48 = println("x4' += x5' * ∂x5/∂x4 = x5' * 1")
    val x49 = println("x1000002' += x4' * ∂x4/∂x1000002 = x4' * cos(x1000002)")
    val x50 = println("x1' += x3' * ∂x3/∂x1 = x3' * 1")
    val x51 = println("x2' += x3' * ∂x3/∂x2 = x3' * 1")
    val x52 = println("x1000001' += x2' * ∂x2/∂x1000001 = x2' * x1000002")
    val x53 = println("x1000002' += x2' * ∂x2/∂x1000002 = x2' * x1000001")
    val x54 = println("x1000001' += x1' * ∂x1/∂x1000001 = x1' * 1/x1000001")
    val x55 = println("---")
    val x56 = println("IR Graph:")
    val x57 = println("x0 = Const1(0.0)")
    val x58 = println("x1 = Log(1000001)")
    val x59 = println("x2 = Times(1000001,1000002)")
    val x60 = println("x3 = Plus(1,2)")
    val x61 = println("x4 = Sin(1000002)")
    val x62 = println("x5 = Plus(3,4)")
    val x63 = println("x6 = Const1(1.0)")
    val x64 = println("x7 = Cos(1000002)")
    val x65 = println("x8 = Plus(7,1000001)")
    val x66 = println("x9 = Div(6,1000001)")
    val x67 = println("x10 = Plus(1000002,9)")
    val x68 = println("Adjoints:")
    val x69 = println("x2' = 6")
    val x70 = println("x5' = 6")
    val x71 = println("x1000002' = 8")
    val x72 = println("x4' = 6")
    val x73 = println("x1' = 6")
    val x74 = println("x1000001' = 10")
    val x75 = println("x3' = 6")
    val x76 = x45._1
    val x78 = java.lang.Math.log(x76)
    val x77 = x45._2
    val x79 = x76 * x77
    val x80 = x78 + x79
    val x81 = java.lang.Math.sin(x77)
    val x82 = x80 + x81
    val x83 = 1.0 / x76
    val x84 = x77 + x83
    val x85 = java.lang.Math.cos(x77)
    val x86 = x85 + x76
    val x87 = x84 + x86
    val x88 = x82 + x87
    x88
  }
}
/*****************************************
End of Generated Code
*******************************************/
