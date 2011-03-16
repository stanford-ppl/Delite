package generated

/*****************************************
  Emitting Generated Code
*******************************************/
class GDA_merged extends ((Array[java.lang.String])=>(Unit)) {
def apply(x0:Array[java.lang.String]): Unit = {
val x1 = x0.length
val x2 = x1 < 2
val x6 = if (x2) {
val x3 = println("Usage: GDA <input data file> <output label data file>")
val x4 = exit(-1)
x4
} else {
()
}
val x7 = x0(0)
val x54 = {
val x8 = {
new java.io.FileReader(x7)
}
val x9 = {
new java.io.BufferedReader(x8)
}
val x10 = x9.readLine()
var x11 = x10
val x12 = x11
val x13 = x12.trim()
x11 = x13
val x15 = x11
val x16 = x15.split("\\s+")
var x17 = x16
val x18 = x17
val x19 = x18.length
val x20 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](0,x19)
}
while ({val x394 = x11
val x395 = x394 != null
x395}) {
val x21 = x17
val x22 = x21.length
val x23 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x22,true)
}
val x24 = x17
val x25 = x24.length
val x26 = 0 until x25
val x33 = x26.foreach{ x27: Int =>
val x28 = x17
val x29 = x28(x27)
val x30 = java.lang.Double.parseDouble(x29)
val x31 = x23(x27) = x30
x31
}
val x37 = {
val x34 = x20.numRows
val x35 = x20.insertRow(x34,x23)
x35
}
val x38 = x9.readLine()
x11 = x38
val x40 = x11
val x41 = x40 != null
val x49 = if (x41) {
val x42 = x11
val x43 = x42.trim()
x11 = x43
val x45 = x11
val x46 = x45.split("\\s+")
x17 = x46
()
} else {
()
}
x49
}
val x52 = x9.close()
x20
}
val x55 = x0(1)
val x73 = {
val x56 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](0,true)
}
val x57 = {
new java.io.FileReader(x55)
}
val x58 = {
new java.io.BufferedReader(x57)
}
val x59 = x58.readLine()
var x60 = x59
while ({val x397 = x60
val x398 = x397 != null
x398}) {
val x61 = x60
val x62 = x61.trim()
x60 = x62
val x64 = x60
val x65 = java.lang.Double.parseDouble(x64)
val x66 = x56.insert(x56.length,x65)
val x67 = x58.readLine()
x60 = x67
()
}
val x71 = x58.close()
x56
}
var x89 = 0.0
var x90 = 0.0
val x88 = x54.numCols
val x93 = {
val x91 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x88,true)
}
x91
}
var x94 = x93
val x97 = {
val x95 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x88,true)
}
x95
}
var x98 = x97
val x86 = {
val x74 = x73.length
val x75 = x73.isRow
val x76 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Boolean](x74,x75)
}
val x77 = 0 until x74
val x84 = x77.foreach{ x78: Int =>
val x79 = x73(x78)
val x80 = x79 <= 0.0
val x81 = if (x80) {
false
} else {
true
}
val x82 = x76(x78) = x81
x82
}
x76
}
val x87 = x86.length
val x99 = 0 until x87
val x144 = x99.foreach{ x100: Int =>
val x101 = x86(x100)
val x102 = x101 == false
val x142 = if (x102) {
val x103 = x90
val x104 = x103 + 1.0
x90 = x104
val x106 = x94
val x107 = x54.getRow(x100)
val x120 = {
val x108 = x106.length
val x109 = x106.isRow
val x110 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x108,x109)
}
val x111 = 0 until x108
val x118 = x111.foreach{ x112: Int =>
val x113 = x106(x112)
val x114 = x107(x112)
val x115 = x113 + x114
val x116 = x110(x112) = x115
x116
}
x110
}
x94 = x120
()
} else {
val x123 = x89
val x124 = x123 + 1.0
x89 = x124
val x126 = x98
val x107 = x54.getRow(x100)
val x139 = {
val x127 = x126.length
val x128 = x126.isRow
val x129 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x127,x128)
}
val x130 = 0 until x127
val x137 = x130.foreach{ x131: Int =>
val x132 = x126(x131)
val x133 = x107(x131)
val x134 = x132 + x133
val x135 = x129(x131) = x134
x135
}
x129
}
x98 = x139
()
}
x142
}
val x147 = x89
val x149 = x94
val x150 = x90
val x164 = x98
val x165 = x89
val x179 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x88,x88)
}
var x180 = x179
val x163 = {
val x151 = x149.length
val x152 = x149.isRow
val x153 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x151,x152)
}
val x154 = 0 until x151
val x161 = x154.foreach{ x155: Int =>
val x156 = x149(x155)
val x157 = x150
val x158 = x156 / x157
val x159 = x153(x155) = x158
x159
}
x153
}
val x178 = {
val x166 = x164.length
val x167 = x164.isRow
val x168 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x166,x167)
}
val x169 = 0 until x166
val x176 = x169.foreach{ x170: Int =>
val x171 = x164(x170)
val x172 = x165
val x173 = x171 / x172
val x174 = x168(x170) = x173
x174
}
x168
}
val x322 = x99.foreach{ x181: Int =>
val x182 = x86(x181)
val x183 = x182 == false
val x320 = if (x183) {
val x184 = x180
val x185 = x54.getRow(x181)
val x198 = {
val x186 = x185.length
val x187 = x185.isRow
val x188 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x186,x187)
}
val x189 = 0 until x186
val x196 = x189.foreach{ x190: Int =>
val x191 = x185(x190)
val x192 = x163(x190)
val x193 = x191 - x192
val x194 = x188(x190) = x193
x194
}
x188
}
val x210 = {
val x199 = x198.length
val x200 = x198.isRow
val x201 = !x200
val x202 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x199,x201)
}
val x203 = 0 until x199
val x208 = x203.foreach{ x204: Int =>
val x205 = x198(x204)
val x206 = x202(x204) = x205
x206
}
x202
}
val x234 = {
val x220 = x210.length
val x221 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x220,x220)
}
val x222 = 0 until x220
val x232 = x222.foreach{ x223: Int =>
val x199 = x198.length
val x203 = 0 until x199
val x230 = x203.foreach{ x224: Int =>
val x225 = x210(x223)
val x226 = x198(x224)
val x227 = x225 * x226
val x228 = x221(x223, x224) = x227
x228
}
x230
}
x221
}
val x251 = {
val x235 = x184.numRows
val x236 = x184.numCols
val x237 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x235,x236)
}
val x238 = 0 until x235
val x249 = x238.foreach{ x239: Int =>
val x240 = 0 until x236
val x247 = x240.foreach{ x241: Int =>
val x242 = x184(x239, x241)
val x243 = x234(x239, x241)
val x244 = x242 + x243
val x245 = x237(x239, x241) = x244
x245
}
x247
}
x237
}
x180 = x251
()
} else {
val x254 = x180
val x185 = x54.getRow(x181)
val x264 = {
val x186 = x185.length
val x187 = x185.isRow
val x255 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x186,x187)
}
val x189 = 0 until x186
val x262 = x189.foreach{ x256: Int =>
val x257 = x185(x256)
val x258 = x178(x256)
val x259 = x257 - x258
val x260 = x255(x256) = x259
x260
}
x255
}
val x276 = {
val x265 = x264.length
val x266 = x264.isRow
val x267 = !x266
val x268 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x265,x267)
}
val x269 = 0 until x265
val x274 = x269.foreach{ x270: Int =>
val x271 = x264(x270)
val x272 = x268(x270) = x271
x272
}
x268
}
val x300 = {
val x286 = x276.length
val x287 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x286,x286)
}
val x288 = 0 until x286
val x298 = x288.foreach{ x289: Int =>
val x265 = x264.length
val x269 = 0 until x265
val x296 = x269.foreach{ x290: Int =>
val x291 = x276(x289)
val x292 = x264(x290)
val x293 = x291 * x292
val x294 = x287(x289, x290) = x293
x294
}
x296
}
x287
}
val x317 = {
val x301 = x254.numRows
val x302 = x254.numCols
val x303 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x301,x302)
}
val x304 = 0 until x301
val x315 = x304.foreach{ x305: Int =>
val x306 = 0 until x302
val x313 = x306.foreach{ x307: Int =>
val x308 = x254(x305, x307)
val x309 = x300(x305, x307)
val x310 = x308 + x309
val x311 = x303(x305, x307) = x310
x311
}
x313
}
x303
}
x180 = x317
()
}
x320
}
val x323 = print("GDA parameter calculation finished: ")
val x145 = x87
val x146 = 1.0 / x145
val x148 = x146 * x147
val x324 = "  phi = "+x148
val x325 = println(x324)
val x326 = println("  mu0 = ")
val x349 = {
val x327 = x163.isRow
val x347 = if (x327) {
val x328 = print("[ ")
val x329 = x163.length
val x330 = 0 until x329
val x336 = x330.foreach{ x331: Int =>
val x332 = x163(x331)
val x333 = print(x332)
val x334 = print(" ")
x334
}
val x337 = print("]\n")
x337
} else {
val x329 = x163.length
val x330 = 0 until x329
val x345 = x330.foreach{ x339: Int =>
val x340 = print("[")
val x341 = x163(x339)
val x342 = print(x341)
val x343 = print(" ]\n")
x343
}
x345
}
x347
}
val x350 = println("  mu1 = ")
val x373 = {
val x351 = x178.isRow
val x371 = if (x351) {
val x352 = print("[ ")
val x353 = x178.length
val x354 = 0 until x353
val x360 = x354.foreach{ x355: Int =>
val x356 = x178(x355)
val x357 = print(x356)
val x358 = print(" ")
x358
}
val x361 = print("]\n")
x361
} else {
val x353 = x178.length
val x354 = 0 until x353
val x369 = x354.foreach{ x363: Int =>
val x364 = print("[")
val x365 = x178(x363)
val x366 = print(x365)
val x367 = print(" ]\n")
x367
}
x369
}
x371
}
val x374 = println("  sigma = ")
val x375 = x180
val x392 = {
val x376 = x375.numRows
val x377 = 0 until x376
val x390 = x377.foreach{ x378: Int =>
val x379 = print("[ ")
val x380 = x375.numCols
val x381 = 0 until x380
val x387 = x381.foreach{ x382: Int =>
val x383 = x375(x378, x382)
val x384 = print(x383)
val x385 = print(" ")
x385
}
val x388 = print("]\n")
x388
}
x390
}
()
}
}
/*****************************************
  End of Generated Code
*******************************************/
