/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((Double)=>(Unit)) {
  def apply(x68:Double): Unit = {
    val x69 = println("Backprop:")
    val x70 = println("x0' += x2' * ∂x2/∂x0 = x2' * 1")
    val x71 = println("x1' += x2' * ∂x2/∂x1 = x2' * 1")
    val x72 = println("x1000001' += x1' * ∂x1/∂x1000001 = x1' * -1*cos(x1000001)")
    val x73 = println("x1000001' += x0' * ∂x0/∂x1000001 = x0' * cos(x1000001)")
    val x74 = println("---")
    val x75 = println("IR Graph:")
    val x76 = println("x0 = Sin(1000001)")
    val x77 = println("x1 = Cos(1000001)")
    val x78 = println("x2 = Plus(0,1)")
    val x79 = println("x3 = Const1(1.0)")
    val x80 = println("x4 = Const1(-1.0)")
    val x81 = println("x5 = Times(4,0)")
    val x82 = println("x6 = Plus(5,1)")
    val x83 = println("Adjoints:")
    val x84 = println("x2' = 3")
    val x85 = println("x1' = 3")
    val x86 = println("x1000001' = 6")
    val x87 = println("x0' = 3")
    val x88 = java.lang.Math.sin(x68)
    val x89 = -1.0 * x88
    val x90 = java.lang.Math.cos(x68)
    val x91 = x89 + x90
    var x92: Double = x91
    var x93: Double = x68
    val x128 = while ({val x94 = x92
      val x95 = java.lang.Math.abs(x94)
      val x96 = x95 >= 1.0E-5
      x96}) {
      val x98 = x93
      val x99 = x92
      val x100 = 0.9 * x99
      val x101 = x98 - x100
      x93 = x101
      val x103 = println("Backprop:")
      val x104 = println("x0' += x2' * ∂x2/∂x0 = x2' * 1")
      val x105 = println("x1' += x2' * ∂x2/∂x1 = x2' * 1")
      val x106 = println("x1000001' += x1' * ∂x1/∂x1000001 = x1' * -1*cos(x1000001)")
      val x107 = println("x1000001' += x0' * ∂x0/∂x1000001 = x0' * cos(x1000001)")
      val x108 = println("---")
      val x109 = println("IR Graph:")
      val x110 = println("x0 = Sin(1000001)")
      val x111 = println("x1 = Cos(1000001)")
      val x112 = println("x2 = Plus(0,1)")
      val x113 = println("x3 = Const1(1.0)")
      val x114 = println("x4 = Const1(-1.0)")
      val x115 = println("x5 = Times(4,0)")
      val x116 = println("x6 = Plus(5,1)")
      val x117 = println("Adjoints:")
      val x118 = println("x2' = 3")
      val x119 = println("x1' = 3")
      val x120 = println("x1000001' = 6")
      val x121 = println("x0' = 3")
      val x122 = java.lang.Math.sin(x101)
      val x123 = -1.0 * x122
      val x124 = java.lang.Math.cos(x101)
      val x125 = x123 + x124
      x92 = x125
      ()
    }
    val x129 = x93
    val x133 = printf("best_x is %f\n",x129)
    val x130 = java.lang.Math.sin(x129)
    val x131 = java.lang.Math.cos(x129)
    val x132 = x130 + x131
    val x134 = printf("best_y is %f\n",x132)
    x134
  }
}
/*****************************************
End of Generated Code
*******************************************/
