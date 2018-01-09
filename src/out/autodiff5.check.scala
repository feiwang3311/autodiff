/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((scala.Tuple2[Double, Double])=>(Unit)) {
  def apply(x86:scala.Tuple2[Double, Double]): Unit = {
    val x87 = println("Backprop:")
    val x88 = println("x0' += x2' * ∂x2/∂x0 = x2' * 1")
    val x89 = println("x1' += x2' * ∂x2/∂x1 = x2' * 1")
    val x90 = println("x1000002' += x1' * ∂x1/∂x1000002 = x1' * -1*cos(x1000002)")
    val x91 = println("x1000001' += x0' * ∂x0/∂x1000001 = x0' * cos(x1000001)")
    val x92 = println("---")
    val x93 = println("IR Graph:")
    val x94 = println("x0 = Sin(1000001)")
    val x95 = println("x1 = Cos(1000002)")
    val x96 = println("x2 = Plus(0,1)")
    val x97 = println("x3 = Const1(1.0)")
    val x98 = println("x4 = Const1(-1.0)")
    val x99 = println("x5 = Sin(1000002)")
    val x100 = println("x6 = Times(4,5)")
    val x101 = println("x7 = Cos(1000001)")
    val x102 = println("Adjoints:")
    val x103 = println("x2' = 3")
    val x104 = println("x1000002' = 6")
    val x105 = println("x1' = 3")
    val x106 = println("x1000001' = 7")
    val x107 = println("x0' = 3")
    val x108 = x86._1
    val x110 = java.lang.Math.cos(x108)
    var x113: Double = x110
    val x109 = x86._2
    val x111 = java.lang.Math.sin(x109)
    val x112 = -1.0 * x111
    var x114: Double = x112
    var x115: Double = x108
    var x116: Double = x109
    val x162 = while ({val x117 = x113
      val x118 = x114
      val x119 = x117 * x117
      val x120 = x118 * x118
      val x121 = x119 + x120
      val x122 = java.lang.Math.sqrt(x121)
      val x123 = x122 >= 1.0E-5
      x123}) {
      val x125 = x115
      val x126 = x113
      val x127 = 0.9 * x126
      val x128 = x125 - x127
      x115 = x128
      val x130 = x116
      val x131 = x114
      val x132 = 0.9 * x131
      val x133 = x130 - x132
      x116 = x133
      val x135 = println("Backprop:")
      val x136 = println("x0' += x2' * ∂x2/∂x0 = x2' * 1")
      val x137 = println("x1' += x2' * ∂x2/∂x1 = x2' * 1")
      val x138 = println("x1000002' += x1' * ∂x1/∂x1000002 = x1' * -1*cos(x1000002)")
      val x139 = println("x1000001' += x0' * ∂x0/∂x1000001 = x0' * cos(x1000001)")
      val x140 = println("---")
      val x141 = println("IR Graph:")
      val x142 = println("x0 = Sin(1000001)")
      val x143 = println("x1 = Cos(1000002)")
      val x144 = println("x2 = Plus(0,1)")
      val x145 = println("x3 = Const1(1.0)")
      val x146 = println("x4 = Const1(-1.0)")
      val x147 = println("x5 = Sin(1000002)")
      val x148 = println("x6 = Times(4,5)")
      val x149 = println("x7 = Cos(1000001)")
      val x150 = println("Adjoints:")
      val x151 = println("x2' = 3")
      val x152 = println("x1000002' = 6")
      val x153 = println("x1' = 3")
      val x154 = println("x1000001' = 7")
      val x155 = println("x0' = 3")
      val x156 = java.lang.Math.cos(x128)
      x113 = x156
      val x157 = java.lang.Math.sin(x133)
      val x158 = -1.0 * x157
      x114 = x158
      ()
    }
    val x163 = x115
    val x164 = x116
    val x168 = printf("best_x1 is %f\n",x163)
    val x169 = printf("best_x2 is %f\n",x164)
    val x165 = java.lang.Math.sin(x163)
    val x166 = java.lang.Math.cos(x164)
    val x167 = x165 + x166
    val x170 = printf("best_y is %f\n",x167)
    x170
  }
}
/*****************************************
End of Generated Code
*******************************************/
