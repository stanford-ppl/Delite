package generated

/*****************************************
  Emitting Generated Code
*******************************************/
class GDA3 extends ((Array[java.lang.String])=>(Unit)) {
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
val x324 = {
new java.io.FileReader(x7)
}
val x325 = {
new java.io.BufferedReader(x324)
}
val x326 = x325.readLine()
var x327 = x326
val x328 = x327
val x329 = x328.trim()
x327 = x329
val x331 = x327
val x332 = x331.split("\\s+")
var x333 = x332
val x334 = x333
val x335 = x334.length
val x337 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](0,x335)
}
while ({val x428 = x327
val x429 = x428 != null
x429}) {
val x338 = x333
val x339 = x338.length
val x340 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x339,true)
}
val x341 = x333
val x342 = x341.length
val x343 = 0 until x342
val x350 = {x344: (Int) =>
val x345 = x333
val x346 = x345(x344)
val x347 = java.lang.Double.parseDouble(x346)
val x348 = x340(x344) = x347
x348
}
val x351 = x343.foreach{
x350
}
val x353 = {
val x370 = x337.numRows
val x371 = x337.insertRow(x370,x340)
x371
}
val x354 = x325.readLine()
x327 = x354
val x356 = x327
val x357 = x356 != null
val x365 = if (x357) {
val x358 = x327
val x359 = x358.trim()
x327 = x359
val x361 = x327
val x362 = x361.split("\\s+")
x333 = x362
()
} else {
()
}
x365
}
val x368 = x325.close()
x337
}
val x25 = x8.numCols
val x9 = x0(1)
val x10 = {
val x373 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](0,true)
}
val x374 = {
new java.io.FileReader(x9)
}
val x375 = {
new java.io.BufferedReader(x374)
}
val x376 = x375.readLine()
var x377 = x376
while ({val x431 = x377
val x432 = x431 != null
x432}) {
val x378 = x377
val x379 = x378.trim()
x377 = x379
val x381 = x377
val x382 = java.lang.Double.parseDouble(x381)
val x383 = x373.insert(x373.length,x382)
val x384 = x375.readLine()
x377 = x384
()
}
val x388 = x375.close()
x373
}
val x11 = x10.length
val x12 = x10.is_row
val x13 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Boolean](x11,x12)
}
val x14 = 0 until x11
val x21 = {x15: (Int) =>
val x16 = x10(x15)
val x17 = x16 <= 0.0
val x18 = if (x17) {
false
} else {
true
}
val x19 = x13(x15) = x18
x19
}
val x22 = x14.foreach{
x21
}
var x26 = 0.0
var x27 = 0.0
val x30 = {
val x28 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x25,true)
}
x28
}
var x31 = x30
val x34 = {
val x32 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x25,true)
}
x32
}
var x35 = x34
val x23 = {
x13
}
val x24 = x23.length
val x36 = 0 until x24
val x83 = {x37: (Int) =>
val x38 = x23(x37)
val x39 = x38 == false
val x81 = if (x39) {
val x40 = x27
val x41 = x40 + 1.0
x27 = x41
val x43 = x31
val x44 = x8(x37)
val x58 = {
val x45 = x43.length
val x46 = x43.is_row
val x47 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x45,x46)
}
val x48 = 0 until x45
val x55 = {x49: (Int) =>
val x50 = x43(x49)
val x51 = x44(x49)
val x52 = x50 + x51
val x53 = x47(x49) = x52
x53
}
val x56 = x48.foreach{
x55
}
x47
}
x31 = x58
()
} else {
val x61 = x26
val x62 = x61 + 1.0
x26 = x62
val x64 = x35
val x44 = x8(x37)
val x78 = {
val x65 = x64.length
val x66 = x64.is_row
val x67 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x65,x66)
}
val x68 = 0 until x65
val x75 = {x69: (Int) =>
val x70 = x64(x69)
val x71 = x44(x69)
val x72 = x70 + x71
val x73 = x67(x69) = x72
x73
}
val x76 = x68.foreach{
x75
}
x67
}
x35 = x78
()
}
x81
}
val x84 = x36.foreach{
x83
}
val x87 = x26
val x89 = x31
val x90 = x27
val x104 = x35
val x105 = x26
val x120 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x25,x25)
}
var x121 = x120
val x103 = {
val x91 = x89.length
val x92 = x89.is_row
val x93 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x91,x92)
}
val x94 = 0 until x91
val x100 = {x95: (Int) =>
val x96 = x89(x95)
val x97 = x96 / x90
val x98 = x93(x95) = x97
x98
}
val x101 = x94.foreach{
x100
}
x93
}
val x118 = {
val x106 = x104.length
val x107 = x104.is_row
val x108 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x106,x107)
}
val x109 = 0 until x106
val x115 = {x110: (Int) =>
val x111 = x104(x110)
val x112 = x111 / x105
val x113 = x108(x110) = x112
x113
}
val x116 = x109.foreach{
x115
}
x108
}
val x245 = {x122: (Int) =>
val x123 = x23(x122)
val x124 = x123 == false
val x243 = if (x124) {
val x125 = x121
val x126 = x8(x122)
val x140 = {
val x127 = x126.length
val x128 = x126.is_row
val x129 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x127,x128)
}
val x130 = 0 until x127
val x137 = {x131: (Int) =>
val x132 = x126(x131)
val x133 = x103(x131)
val x134 = x132 - x133
val x135 = x129(x131) = x134
x135
}
val x138 = x130.foreach{
x137
}
x129
}
val x153 = {
val x141 = x140.length
val x142 = x140.is_row
val x143 = !x142
val x144 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x141,x143)
}
val x145 = 0 until x141
val x150 = {x146: (Int) =>
val x147 = x140(x146)
val x148 = x144(x146) = x147
x148
}
val x151 = x145.foreach{
x150
}
x144
}
val x181 = {
val x164 = x153.length
val x166 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x164,x164)
}
val x167 = 0 until x164
val x178 = {x168: (Int) =>
val x141 = x140.length
val x145 = 0 until x141
val x175 = {x169: (Int) =>
val x170 = x153(x168)
val x171 = x140(x169)
val x172 = x170 * x171
val x173 = x166(x168, x169) = x172
x173
}
val x176 = x145.foreach{
x175
}
x176
}
val x179 = x167.foreach{
x178
}
x166
}
val x183 = {
val x390 = x125.numRows
val x391 = x125.numCols
val x393 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x390,x391)
}
val x394 = 0 until x390
val x406 = {x395: (Int) =>
val x396 = 0 until x391
val x403 = {x397: (Int) =>
val x398 = x125(x395, x397)
val x399 = x181(x395, x397)
val x400 = x398 + x399
val x401 = x393(x395, x397) = x400
x401
}
val x404 = x396.foreach{
x403
}
x404
}
val x407 = x394.foreach{
x406
}
x393
}
x121 = x183
()
} else {
val x186 = x121
val x126 = x8(x122)
val x197 = {
val x127 = x126.length
val x128 = x126.is_row
val x187 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x127,x128)
}
val x130 = 0 until x127
val x194 = {x188: (Int) =>
val x189 = x126(x188)
val x190 = x118(x188)
val x191 = x189 - x190
val x192 = x187(x188) = x191
x192
}
val x195 = x130.foreach{
x194
}
x187
}
val x210 = {
val x198 = x197.length
val x199 = x197.is_row
val x200 = !x199
val x201 = {
new ppl.dsl.optiml.datastruct.scala.VectorImpl[Double](x198,x200)
}
val x202 = 0 until x198
val x207 = {x203: (Int) =>
val x204 = x197(x203)
val x205 = x201(x203) = x204
x205
}
val x208 = x202.foreach{
x207
}
x201
}
val x238 = {
val x221 = x210.length
val x223 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x221,x221)
}
val x224 = 0 until x221
val x235 = {x225: (Int) =>
val x198 = x197.length
val x202 = 0 until x198
val x232 = {x226: (Int) =>
val x227 = x210(x225)
val x228 = x197(x226)
val x229 = x227 * x228
val x230 = x223(x225, x226) = x229
x230
}
val x233 = x202.foreach{
x232
}
x233
}
val x236 = x224.foreach{
x235
}
x223
}
val x240 = {
val x409 = x186.numRows
val x410 = x186.numCols
val x412 = {
new ppl.dsl.optiml.datastruct.scala.MatrixImpl[Double](x409,x410)
}
val x413 = 0 until x409
val x425 = {x414: (Int) =>
val x415 = 0 until x410
val x422 = {x416: (Int) =>
val x417 = x186(x414, x416)
val x418 = x238(x414, x416)
val x419 = x417 + x418
val x420 = x412(x414, x416) = x419
x420
}
val x423 = x415.foreach{
x422
}
x423
}
val x426 = x413.foreach{
x425
}
x412
}
x121 = x240
()
}
x243
}
val x246 = x36.foreach{
x245
}
val x247 = print("GDA parameter calculation finished: ")
val x85 = x24
val x86 = 1.0 / x85
val x88 = x86 * x87
val x248 = "  phi = "+x88
val x249 = println(x248)
val x250 = println("  mu0 = ")
val x275 = {
val x251 = x103.is_row
val x273 = if (x251) {
val x252 = print("[ ")
val x253 = x103.length
val x254 = 0 until x253
val x260 = {x255: (Int) =>
val x256 = x103(x255)
val x257 = print(x256)
val x258 = print(" ")
x258
}
val x261 = x254.foreach{
x260
}
val x262 = print("]\n")
x262
} else {
val x253 = x103.length
val x254 = 0 until x253
val x270 = {x264: (Int) =>
val x265 = print("[")
val x266 = x103(x264)
val x267 = print(x266)
val x268 = print(" ]\n")
x268
}
val x271 = x254.foreach{
x270
}
x271
}
x273
}
val x276 = println("  mu1 = ")
val x301 = {
val x277 = x118.is_row
val x299 = if (x277) {
val x278 = print("[ ")
val x279 = x118.length
val x280 = 0 until x279
val x286 = {x281: (Int) =>
val x282 = x118(x281)
val x283 = print(x282)
val x284 = print(" ")
x284
}
val x287 = x280.foreach{
x286
}
val x288 = print("]\n")
x288
} else {
val x279 = x118.length
val x280 = 0 until x279
val x296 = {x290: (Int) =>
val x291 = print("[")
val x292 = x118(x290)
val x293 = print(x292)
val x294 = print(" ]\n")
x294
}
val x297 = x280.foreach{
x296
}
x297
}
x299
}
val x302 = println("  sigma = ")
val x303 = x121
val x304 = {
val x306 = x303.numRows
val x307 = 0 until x306
val x321 = {x308: (Int) =>
val x309 = print("[ ")
val x310 = x303.numCols
val x311 = 0 until x310
val x317 = {x312: (Int) =>
val x313 = x303(x308, x312)
val x314 = print(x313)
val x315 = print(" ")
x315
}
val x318 = x311.foreach{
x317
}
val x319 = print("]\n")
x319
}
val x322 = x307.foreach{
x321
}
x322
}
x304
}
}
/*****************************************
  End of Generated Code
*******************************************/
