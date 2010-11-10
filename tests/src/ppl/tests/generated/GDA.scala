package generated

/*****************************************
  Emitting Generated Code
*******************************************/
//class GDA extends ((Array[java.lang.String])=>(Unit)) {
//def apply(x0:Array[java.lang.String]): Unit = {
//val x7 = x0(0)
//val x8 = ppl.dsl.optiml.direct.MLInputReader.read(x7)
//val x19 = x8.numCols
//val x49 = ppl.dsl.optiml.direct.Matrix.zeros(x19,x19)
//val x1 = x0.length
//val x2 = x1 < 2
//val x6 = if (x2) {
//val x3 = println("Usage: GDA <input data file> <output label data file>")
//val x4 = exit(-1)
//x4
//} else {
//()
//}
//val x16 = println(x8)
//val x9 = x0(1)
//val x10 = ppl.dsl.optiml.direct.MLInputReader.readVector(x9)
//val x14 = {x11: (Double) =>
//val x12 = x11 <= 0.0
//val x13 = if (x12) {
//false
//} else {
//true
//}
//x13
//}
//val x15 = x10.toBoolean(x14)
//val x17 = println(x15)
//var x20 = 0.0
//var x21 = 0.0
//val x22 = ppl.dsl.optiml.direct.Vector.zeros(x19)
//var x23 = x22
//var x24 = x22
//val x18 = x15.length
//val x25 = 0 until x18
//val x42 = {x26: (Int) =>
//val x27 = x15(x26)
//val x28 = x27==false
//val x40 = if (x28) {
//val x29 = x21 + 1.0
//x21 = x29
//val x31 = x8(x26)
//val x32 = x23 + x31
//x23 = x32
//()
//} else {
//val x35 = x20 + 1.0
//x20 = x35
//val x31 = x8(x26)
//val x37 = x24 + x31
//x24 = x37
//()
//}
//x40
//}
//val x43 = x25.foreach{
//x42
//}
//var x50 = x49
//val x47 = x23 / x21
//val x48 = x24 / x20
//val x69 = {x51: (Int) =>
//val x52 = x15(x51)
//val x53 = x52==false
//val x67 = if (x53) {
//val x54 = x8(x51)
//val x55 = x54 - x47
//val x56 = x55.trans
//val x57 = x56.outer(x55)
//val x58 = x50 + x57
//x50 = x58
//()
//} else {
//val x54 = x8(x51)
//val x61 = x54 - x48
//val x62 = x61.trans
//val x63 = x62.outer(x61)
//val x64 = x50 + x63
//x50 = x64
//()
//}
//x67
//}
//val x70 = x25.foreach{
//x69
//}
//val x71 = println("GDA parameter calculation finished: ")
//val x44 = x18
//val x45 = 1.0 / x44
//val x46 = x45 * x20
//val x72 = "  phi = "+x46
//val x73 = println(x72)
//val x74 = println("  mu0 = ")
//val x75 = x47.pprint
//val x76 = println("  mu1 = ")
//val x77 = x48.pprint
//val x78 = println("  sigma = ")
//val x79 = x50.pprint
//x79
//}
//}
/*****************************************
  End of Generated Code
*******************************************/
