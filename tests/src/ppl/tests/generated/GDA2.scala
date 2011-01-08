package generated

/*****************************************
  Emitting Generated Code
*******************************************/
class GDA2 extends ((Array[java.lang.String])=>(Unit)) {
def apply(x0:Array[java.lang.String]): Unit = {
val x7 = x0(0)
val x1 = x0.length
val x2 = x1 < 2
val x6 = if (x2) {
val x3 = println("Usage: GDA <input data file> <output label data file>")
val x4 = exit(-1)
x4
} else {
()
}
val x8 = {
val x122 = {
new java.io.FileReader(x7)
}
val x123 = {
new java.io.BufferedReader(x122)
}
val x124 = x123.readLine()
var x125 = x124
val x126 = x125
val x127 = x126.trim()
x125 = x127
val x129 = x125
val x130 = x129.split("\\s+")
var x131 = x130
val x132 = x131
val x133 = x132.length
val x135 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](0,x133)
}
while ({val x427 = x125
val x428 = x427 != null
x428}) {
val x136 = x131
val x137 = x136.length
val x139 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x137,true)
}
val x140 = x131
val x141 = x140.length
val x142 = 0 until x141
val x149 = {x143: (Int) =>
val x144 = x131
val x145 = x144(x143)
val x146 = java.lang.Double.parseDouble(x145)
val x147 = x139(x143) = x146
x147
}
val x150 = x142.foreach{
x149
}
val x152 = {
val x169 = x135.numRows
val x170 = x135.insertRow(x169,x139)
x170
}
val x153 = x123.readLine()
x125 = x153
val x155 = x125
val x156 = x155 != null
val x164 = if (x156) {
val x157 = x125
val x158 = x157.trim()
x125 = x158
val x160 = x125
val x161 = x160.split("\\s+")
x131 = x161
()
} else {
()
}
x164
}
val x167 = x123.close()
x135
}
val x18 = x8.numCols
val x9 = x0(1)
val x10 = {
val x173 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](0,true)
}
val x174 = {
new java.io.FileReader(x9)
}
val x175 = {
new java.io.BufferedReader(x174)
}
val x176 = x175.readLine()
var x177 = x176
while ({val x430 = x177
val x431 = x430 != null
x431}) {
val x178 = x177
val x179 = x178.trim()
x177 = x179
val x181 = x177
val x182 = java.lang.Double.parseDouble(x181)
val x183 = x173.insert(x173.length,x182)
val x184 = x175.readLine()
x177 = x184
()
}
val x188 = x175.close()
x173
}
var x19 = 0.0
var x20 = 0.0
val x21 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x18,true)
}
var x22 = x21
val x23 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x18,true)
}
var x24 = x23
val x14 = {x11: (Double) =>
val x12 = x11 <= 0.0
val x13 = if (x12) {
false
} else {
true
}
x13
}
val x16 = {
val x190 = x10.length
val x191 = x10.isRow
val x193 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Boolean](x190,x191)
}
val x194 = 0 until x190
val x200 = {x195: (Int) =>
val x196 = x10(x195)
val x197 = x14(x196)
val x198 = x193(x195) = x197
x198
}
val x201 = x194.foreach{
x200
}
x193
}
val x17 = x16.length
val x25 = 0 until x17
val x48 = {x26: (Int) =>
val x27 = x16(x26)
val x28 = x27 == false
val x46 = if (x28) {
val x29 = x20
val x30 = x29 + 1.0
x20 = x30
val x32 = x22
val x33 = x8(x26)
val x35 = {
val x203 = x32.length
val x204 = x32.isRow
val x206 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x203,x204)
}
val x207 = 0 until x203
val x214 = {x208: (Int) =>
val x209 = x32(x208)
val x210 = x33(x208)
val x211 = x209 + x210
val x212 = x206(x208) = x211
x212
}
val x215 = x207.foreach{
x214
}
x206
}
x22 = x35
()
} else {
val x38 = x19
val x39 = x38 + 1.0
x19 = x39
val x41 = x24
val x33 = x8(x26)
val x43 = {
val x217 = x41.length
val x218 = x41.isRow
val x220 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x217,x218)
}
val x221 = 0 until x217
val x228 = {x222: (Int) =>
val x223 = x41(x222)
val x224 = x33(x222)
val x225 = x223 + x224
val x226 = x220(x222) = x225
x226
}
val x229 = x221.foreach{
x228
}
x220
}
x24 = x43
()
}
x46
}
val x49 = x25.foreach{
x48
}
val x52 = x19
val x54 = x22
val x55 = x20
val x58 = x24
val x59 = x19
val x63 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x18,x18)
}
var x64 = x63
val x57 = {
val x294 = x54.length
val x295 = x54.isRow
val x297 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x294,x295)
}
val x298 = 0 until x294
val x304 = {x299: (Int) =>
val x300 = x54(x299)
val x301 = x300 / x55
val x302 = x297(x299) = x301
x302
}
val x305 = x298.foreach{
x304
}
x297
}
val x61 = {
val x366 = x58.length
val x367 = x58.isRow
val x369 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x366,x367)
}
val x370 = 0 until x366
val x376 = {x371: (Int) =>
val x372 = x58(x371)
val x373 = x372 / x59
val x374 = x369(x371) = x373
x374
}
val x377 = x370.foreach{
x376
}
x369
}
val x91 = {x65: (Int) =>
val x66 = x16(x65)
val x67 = x66 == false
val x89 = if (x67) {
val x68 = x64
val x69 = x8(x65)
val x71 = {
val x280 = x69.length
val x281 = x69.isRow
val x283 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x280,x281)
}
val x284 = 0 until x280
val x291 = {x285: (Int) =>
val x286 = x69(x285)
val x287 = x57(x285)
val x288 = x286 - x287
val x289 = x283(x285) = x288
x289
}
val x292 = x284.foreach{
x291
}
x283
}
val x72 = {
val x255 = x71.length
val x269 = x71.isRow
val x270 = !x269
val x272 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x255,x270)
}
val x256 = 0 until x255
val x277 = {x273: (Int) =>
val x274 = x71(x273)
val x275 = x272(x273) = x274
x275
}
val x278 = x256.foreach{
x277
}
x272
}
val x74 = {
val x250 = x72.length
val x252 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x250,x250)
}
val x253 = 0 until x250
val x266 = {x254: (Int) =>
val x255 = x71.length
val x256 = 0 until x255
val x263 = {x257: (Int) =>
val x258 = x72(x254)
val x259 = x71(x257)
val x260 = x258 * x259
val x261 = x252(x254, x257) = x260
x261
}
val x264 = x256.foreach{
x263
}
x264
}
val x267 = x253.foreach{
x266
}
x252
}
val x76 = {
val x231 = x68.numRows
val x232 = x68.numCols
val x234 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x231,x232)
}
val x235 = 0 until x231
val x247 = {x236: (Int) =>
val x237 = 0 until x232
val x244 = {x238: (Int) =>
val x239 = x68(x236, x238)
val x240 = x74(x236, x238)
val x241 = x239 + x240
val x242 = x234(x236, x238) = x241
x242
}
val x245 = x237.foreach{
x244
}
x245
}
val x248 = x235.foreach{
x247
}
x234
}
x64 = x76
()
} else {
val x79 = x64
val x69 = x8(x65)
val x81 = {
val x280 = x69.length
val x281 = x69.isRow
val x356 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x280,x281)
}
val x284 = 0 until x280
val x363 = {x357: (Int) =>
val x358 = x69(x357)
val x359 = x61(x357)
val x360 = x358 - x359
val x361 = x356(x357) = x360
x361
}
val x364 = x284.foreach{
x363
}
x356
}
val x82 = {
val x331 = x81.length
val x345 = x81.isRow
val x346 = !x345
val x348 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x331,x346)
}
val x332 = 0 until x331
val x353 = {x349: (Int) =>
val x350 = x81(x349)
val x351 = x348(x349) = x350
x351
}
val x354 = x332.foreach{
x353
}
x348
}
val x84 = {
val x326 = x82.length
val x328 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x326,x326)
}
val x329 = 0 until x326
val x342 = {x330: (Int) =>
val x331 = x81.length
val x332 = 0 until x331
val x339 = {x333: (Int) =>
val x334 = x82(x330)
val x335 = x81(x333)
val x336 = x334 * x335
val x337 = x328(x330, x333) = x336
x337
}
val x340 = x332.foreach{
x339
}
x340
}
val x343 = x329.foreach{
x342
}
x328
}
val x86 = {
val x307 = x79.numRows
val x308 = x79.numCols
val x310 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x307,x308)
}
val x311 = 0 until x307
val x323 = {x312: (Int) =>
val x313 = 0 until x308
val x320 = {x314: (Int) =>
val x315 = x79(x312, x314)
val x316 = x84(x312, x314)
val x317 = x315 + x316
val x318 = x310(x312, x314) = x317
x318
}
val x321 = x313.foreach{
x320
}
x321
}
val x324 = x311.foreach{
x323
}
x310
}
x64 = x86
()
}
x89
}
val x92 = x25.foreach{
x91
}
val x93 = print("GDA parameter calculation finished: ")
val x50 = x17
val x51 = 1.0 / x50
val x53 = x51 * x52
val x94 = "  phi = "+x53
val x95 = println(x94)
val x96 = println("  mu0 = ")
val x97 = {
val x379 = x57.isRow
val x401 = if (x379) {
val x380 = print("[ ")
val x381 = x57.length
val x382 = 0 until x381
val x388 = {x383: (Int) =>
val x384 = x57(x383)
val x385 = print(x384)
val x386 = print(" ")
x386
}
val x389 = x382.foreach{
x388
}
val x390 = print("]\n")
x390
} else {
val x381 = x57.length
val x382 = 0 until x381
val x398 = {x392: (Int) =>
val x393 = print("[")
val x394 = x57(x392)
val x395 = print(x394)
val x396 = print(" ]\n")
x396
}
val x399 = x382.foreach{
x398
}
x399
}
x401
}
val x98 = println("  mu1 = ")
val x99 = {
val x403 = x61.isRow
val x425 = if (x403) {
val x404 = print("[ ")
val x405 = x61.length
val x406 = 0 until x405
val x412 = {x407: (Int) =>
val x408 = x61(x407)
val x409 = print(x408)
val x410 = print(" ")
x410
}
val x413 = x406.foreach{
x412
}
val x414 = print("]\n")
x414
} else {
val x405 = x61.length
val x406 = 0 until x405
val x422 = {x416: (Int) =>
val x417 = print("[")
val x418 = x61(x416)
val x419 = print(x418)
val x420 = print(" ]\n")
x420
}
val x423 = x406.foreach{
x422
}
x423
}
x425
}
val x100 = println("  sigma = ")
val x101 = x64
val x102 = {
val x104 = x101.numRows
val x105 = 0 until x104
val x119 = {x106: (Int) =>
val x107 = print("[ ")
val x108 = x101.numCols
val x109 = 0 until x108
val x115 = {x110: (Int) =>
val x111 = x101(x106, x110)
val x112 = print(x111)
val x113 = print(" ")
x113
}
val x116 = x109.foreach{
x115
}
val x117 = print("]\n")
x117
}
val x120 = x105.foreach{
x119
}
x120
}
x102
}
}
/*****************************************
  End of Generated Code
*******************************************/
