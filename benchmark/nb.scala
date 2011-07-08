/*****************************************
  Emitting Generated Code                  
*******************************************/
class Application extends ((Array[java.lang.String])=>(Unit)) {
def apply(x0:Array[java.lang.String]): Unit = {
val x1 = println("Naive Bayes Example Application")
val x2 = x0.length
val x3 = x2 < 2
val x7 = {
def x7thenb(): Nothing = {
val x4 = println("NaiveBayes <training file> <test file>")
val x5 = exit(-1)
x5
}
if (x3) {
x7thenb()
}
}
val x8 = x0(0)
val x126 = { 
val x10 = new java.io.FileReader(x8)
val x11 = new java.io.BufferedReader(x10)
val x12 = x11.readLine()
var x13: java.lang.String = x12
val x14 = x11.readLine()
var x15: java.lang.String = x14
val x16 = x15
val x17 = x16.trim()
val x18 = x17.split("\\s+")
val x19 = x18(0)
val x20 = java.lang.Integer.parseInt(x19)
val x23 = x20 < 0
val x21 = x18(1)
val x22 = java.lang.Integer.parseInt(x21)
val x24 = x22 < 0
val x25 = x23 || x24
val x28 = {
def x28thenb(): Nothing = {
val x26 = error("Illegal input to readTokenMatrix")
x26
}
if (x25) {
x28thenb()
}
}
val x29 = x11.readLine()
val x30 = new generated.scala.DoubleVectorImpl(0,true)
val x31 = new generated.scala.VectorImpl[generated.scala.Vector[Double]](0,true)
var x33 : Int = 0
val x70 = while (x33 < x20) {
val x34 = x11.readLine()
x15 = x34
val x36 = x15
val x37 = x36.trim()
x15 = x37
val x39 = x15
val x41 = new generated.scala.DoubleVectorImpl(x22,true)
var x42: Int = 0
var x43: Int = 1
val x40 = x39.split("\\s+")
val x45 = x40.length
val x46 = x45 - 1
val x61 = while ({val x44 = x43
val x47 = x44 < x46
x47}) {
val x49 = x43
val x50 = x40(x49)
val x51 = java.lang.Integer.parseInt(x50)
val x52 = x42 += x51
val x53 = x42
val x54 = x43
val x55 = x54 + 1
val x56 = x40(x55)
val x57 = java.lang.Double.parseDouble(x56)
val x58 = x41(x53) = x57
val x59 = x43 += 2
()
}
val x64 = x30.length
val x62 = x40(0)
val x63 = java.lang.Double.parseDouble(x62)
val x65 = x30.insert(x64, x63)
val x66 = x41// unsafe immutable
val x67 = x31.length
val x68 = x31.insert(x67, x66)
x68
x33 = x33 + 1
}
val x72 = x30.length
// a *thin* loop follows: x80
var x71 = 0
val x78 = x30.dcApply(x71)
val x80 = {
val x73 = x30.length
val x74 = x30.isRow
val x75 = !x74
val x76 = new generated.scala.DoubleVectorImpl(x73,x75)
x76
}
x80.dcUpdate(x71, x78)
x71 = 1
while (x71 < x72) {  // begin fat loop x80
val x78 = x30.dcApply(x71)
x80.dcUpdate(x71, x78)
x71 += 1
} // end fat loop x80
val x119 = { 
val x81 = x31.length
val x82 = x81==0
val x117 = {
def x117thenb(): generated.scala.Matrix[Double] = {
val x83 = new generated.scala.DoubleMatrixImpl(0,0)
val x84 = x83// unsafe immutable
x84
}
def x117elseb(): generated.scala.Matrix[Double] = {
val x86 = x31(0)
val x87 = x86.isRow
val x115 = {
def x115thenb(): generated.scala.Matrix[Double] = {
val x88 = x86.length
val x89 = new generated.scala.DoubleMatrixImpl(x81,x88)
var x91 : Int = 0
val x100 = while (x91 < x81) {
var x93 : Int = 0
val x98 = while (x93 < x88) {
val x94 = x31(x91)
val x95 = x94(x93)
val x96 = x89(x91, x93) = x95
x96
x93 = x93 + 1
}
x98
x91 = x91 + 1
}
val x101 = x89// unsafe immutable
x101
}
def x115elseb(): generated.scala.Matrix[Double] = {
val x88 = x86.length
val x103 = new generated.scala.DoubleMatrixImpl(x88,x81)
var x104 : Int = 0
val x112 = while (x104 < x81) {
var x105 : Int = 0
val x110 = while (x105 < x88) {
val x106 = x31(x104)
val x107 = x106(x105)
val x108 = x103(x105, x104) = x107
x108
x105 = x105 + 1
}
x110
x104 = x104 + 1
}
val x113 = x103// unsafe immutable
x113
}
if (x87) {
x115thenb()
} else { 
x115elseb()
}
}
x115
}
if (x82) {
x117thenb()
} else { 
x117elseb()
}
}
x117
}
val x120 = x11.close()
val x121 = x119// unsafe immutable
val x122 = x80// unsafe immutable
val x123 = new generated.scala.DoubleLabelsImpl(x122)
val x124 = new generated.scala.DoubleDoubleTrainingSetImpl(x121,x123)
x124
}
val x127 = x126.numRows
val x128 = "Training model on "+x127
val x129 = x128+" documents."
val x130 = println(x129)
val x131 = Seq()
val x132 = ppl.delite.runtime.profiler.PerformanceTimer.start("app", false)
val x134 = new generated.scala.IndexVectorRangeImpl(0,x127)
val x136 = x134.length
val x137 = x134.isRow
val x133 = x126.numCols
// a *thin* loop follows: x151
var x135 = 0
val x140 = x134.dcApply(x135)
val x145 = x140 * x133
var x142 = 0
val x146 = x145 + x142
val x147 = x126.dcApply(x146)
val x148 = { 
x147
}
var x150: Double = {
if (x133 == 0) {0.0}
else {
val x146 = x145 + x142
val x147 = x126.dcApply(x146)
val x148 = { 
x147
}
x148
}
}
x142 = 1
while (x142 < x133) {  // begin fat loop x150
val x146 = x145 + x142
val x147 = x126.dcApply(x146)
val x148 = { 
x147
}
val x143 = x150
val x144 = x148
val x149 = x143 + x144
x150 = x149
x142 += 1
} // end fat loop x150
val x151 = {
val x138 = new generated.scala.DoubleVectorImpl(x136,x137)
x138
}
x151.dcUpdate(x135, x150)
x135 = 1
while (x135 < x136) {  // begin fat loop x151
val x140 = x134.dcApply(x135)
val x145 = x140 * x133
var x142 = 0
val x146 = x145 + x142
val x147 = x126.dcApply(x146)
val x148 = { 
x147
}
var x150: Double = {
if (x133 == 0) {0.0}
else {
val x146 = x145 + x142
val x147 = x126.dcApply(x146)
val x148 = { 
x147
}
x148
}
}
x142 = 1
while (x142 < x133) {  // begin fat loop x150
val x146 = x145 + x142
val x147 = x126.dcApply(x146)
val x148 = { 
x147
}
val x143 = x150
val x144 = x148
val x149 = x143 + x144
x150 = x149
x142 += 1
} // end fat loop x150
x151.dcUpdate(x135, x150)
x135 += 1
} // end fat loop x151
val x163 = { 
val x160 = new generated.scala.DoubleVectorImpl(x133,true)
x160
}
val x166 = { 
val x160 = new generated.scala.DoubleVectorImpl(x133,true)
x160
}
val x167 = new generated.scala.IndexVectorRangeImpl(0,x133)
val x170 = x167.length
val x152 = x126.labels
val x421 = x126.transposed
val x423 = x421.numCols
val x449 = x133
val x460 = List()
// a *thin* loop follows: x171
var x168 = 0
var x171: Unit = {
var x410: Double = 0.0
var x411: Double = 0.0
var x412: Double = 0.0
var x413: Double = 0.0
var x414: Int = 0
val x409 = x167.dcApply(x168)
val x424 = x409 * x423
val x445 = while ({val x415 = x414
val x416 = x415 < x127
x416}) {
val x418 = x414
val x419 = x152(x418)
val x420 = x419==1
val x442 = {
def x442thenb(): Unit = {
val x422 = x414
val x425 = x424 + x422
val x426 = x421.dcApply(x425)
val x427 = { 
x426
}
val x428 = x410 += x427
val x429 = x414
val x430 = x151(x429)
val x431 = x411 += x430
()
}
def x442elseb(): Unit = {
val x433 = x414
val x434 = x424 + x433
val x435 = x421.dcApply(x434)
val x436 = { 
x435
}
val x437 = x412 += x436
val x438 = x414
val x439 = x151(x438)
val x440 = x413 += x439
()
}
if (x420) {
x442thenb()
} else { 
x442elseb()
}
}
val x443 = x414 += 1
()
}
val x446 = x410
val x448 = x411
val x447 = x446 + 1.0
val x450 = x448 + x449
val x451 = x447 / x450
val x452 = x163(x409) = x451
val x453 = x412
val x455 = x413
val x454 = x453 + 1.0
val x456 = x455 + x449
val x457 = x454 / x456
val x458 = x166(x409) = x457
x458
}
x168 = 1
while (x168 < x170) {  // begin fat loop x171
x171 = {
var x410: Double = 0.0
var x411: Double = 0.0
var x412: Double = 0.0
var x413: Double = 0.0
var x414: Int = 0
val x409 = x167.dcApply(x168)
val x424 = x409 * x423
val x445 = while ({val x415 = x414
val x416 = x415 < x127
x416}) {
val x418 = x414
val x419 = x152(x418)
val x420 = x419==1
val x442 = {
def x442thenb(): Unit = {
val x422 = x414
val x425 = x424 + x422
val x426 = x421.dcApply(x425)
val x427 = { 
x426
}
val x428 = x410 += x427
val x429 = x414
val x430 = x151(x429)
val x431 = x411 += x430
()
}
def x442elseb(): Unit = {
val x433 = x414
val x434 = x424 + x433
val x435 = x421.dcApply(x434)
val x436 = { 
x435
}
val x437 = x412 += x436
val x438 = x414
val x439 = x151(x438)
val x440 = x413 += x439
()
}
if (x420) {
x442thenb()
} else { 
x442elseb()
}
}
val x443 = x414 += 1
()
}
val x446 = x410
val x448 = x411
val x447 = x446 + 1.0
val x450 = x448 + x449
val x451 = x447 / x450
val x452 = x163(x409) = x451
val x453 = x412
val x455 = x413
val x454 = x453 + 1.0
val x456 = x455 + x449
val x457 = x454 / x456
val x458 = x166(x409) = x457
x458
}
x168 += 1
} // end fat loop x171
val x174 = ppl.delite.runtime.profiler.PerformanceTimer.stop("app", false)
val x9 = x0(1)
val x291 = { 
val x175 = new java.io.FileReader(x9)
val x176 = new java.io.BufferedReader(x175)
val x177 = x176.readLine()
var x178: java.lang.String = x177
val x179 = x176.readLine()
var x180: java.lang.String = x179
val x181 = x180
val x182 = x181.trim()
val x183 = x182.split("\\s+")
val x184 = x183(0)
val x185 = java.lang.Integer.parseInt(x184)
val x188 = x185 < 0
val x186 = x183(1)
val x187 = java.lang.Integer.parseInt(x186)
val x189 = x187 < 0
val x190 = x188 || x189
val x193 = {
def x193thenb(): Nothing = {
val x191 = error("Illegal input to readTokenMatrix")
x191
}
if (x190) {
x193thenb()
}
}
val x194 = x176.readLine()
val x195 = new generated.scala.DoubleVectorImpl(0,true)
val x196 = new generated.scala.VectorImpl[generated.scala.Vector[Double]](0,true)
var x198 : Int = 0
val x235 = while (x198 < x185) {
val x199 = x176.readLine()
x180 = x199
val x201 = x180
val x202 = x201.trim()
x180 = x202
val x204 = x180
val x206 = new generated.scala.DoubleVectorImpl(x187,true)
var x207: Int = 0
var x208: Int = 1
val x205 = x204.split("\\s+")
val x210 = x205.length
val x211 = x210 - 1
val x226 = while ({val x209 = x208
val x212 = x209 < x211
x212}) {
val x214 = x208
val x215 = x205(x214)
val x216 = java.lang.Integer.parseInt(x215)
val x217 = x207 += x216
val x218 = x207
val x219 = x208
val x220 = x219 + 1
val x221 = x205(x220)
val x222 = java.lang.Double.parseDouble(x221)
val x223 = x206(x218) = x222
val x224 = x208 += 2
()
}
val x229 = x195.length
val x227 = x205(0)
val x228 = java.lang.Double.parseDouble(x227)
val x230 = x195.insert(x229, x228)
val x231 = x206// unsafe immutable
val x232 = x196.length
val x233 = x196.insert(x232, x231)
x233
x198 = x198 + 1
}
val x237 = x195.length
// a *thin* loop follows: x245
var x236 = 0
val x243 = x195.dcApply(x236)
val x245 = {
val x238 = x195.length
val x239 = x195.isRow
val x240 = !x239
val x241 = new generated.scala.DoubleVectorImpl(x238,x240)
x241
}
x245.dcUpdate(x236, x243)
x236 = 1
while (x236 < x237) {  // begin fat loop x245
val x243 = x195.dcApply(x236)
x245.dcUpdate(x236, x243)
x236 += 1
} // end fat loop x245
val x284 = { 
val x246 = x196.length
val x247 = x246==0
val x282 = {
def x282thenb(): generated.scala.Matrix[Double] = {
val x248 = new generated.scala.DoubleMatrixImpl(0,0)
val x249 = x248// unsafe immutable
x249
}
def x282elseb(): generated.scala.Matrix[Double] = {
val x251 = x196(0)
val x252 = x251.isRow
val x280 = {
def x280thenb(): generated.scala.Matrix[Double] = {
val x253 = x251.length
val x254 = new generated.scala.DoubleMatrixImpl(x246,x253)
var x256 : Int = 0
val x265 = while (x256 < x246) {
var x258 : Int = 0
val x263 = while (x258 < x253) {
val x259 = x196(x256)
val x260 = x259(x258)
val x261 = x254(x256, x258) = x260
x261
x258 = x258 + 1
}
x263
x256 = x256 + 1
}
val x266 = x254// unsafe immutable
x266
}
def x280elseb(): generated.scala.Matrix[Double] = {
val x253 = x251.length
val x268 = new generated.scala.DoubleMatrixImpl(x253,x246)
var x269 : Int = 0
val x277 = while (x269 < x246) {
var x270 : Int = 0
val x275 = while (x270 < x253) {
val x271 = x196(x269)
val x272 = x271(x270)
val x273 = x268(x270, x269) = x272
x273
x270 = x270 + 1
}
x275
x269 = x269 + 1
}
val x278 = x268// unsafe immutable
x278
}
if (x252) {
x280thenb()
} else { 
x280elseb()
}
}
x280
}
if (x247) {
x282thenb()
} else { 
x282elseb()
}
}
x282
}
val x285 = x176.close()
val x286 = x284// unsafe immutable
val x287 = x245// unsafe immutable
val x288 = new generated.scala.DoubleLabelsImpl(x287)
val x289 = new generated.scala.DoubleDoubleTrainingSetImpl(x286,x288)
x289
}
val x292 = println("phi_y1: ")
val x314 = { 
val x293 = x163.isRow
val x312 = {
def x312thenb(): Unit = {
val x294 = print("[ ")
var x296 : Int = 0
val x301 = while (x296 < x133) {
val x297 = x163(x296)
val x298 = print(x297)
val x299 = print(" ")
x299
x296 = x296 + 1
}
val x302 = print("]\n")
x302
}
def x312elseb(): Unit = {
var x304 : Int = 0
val x310 = while (x304 < x133) {
val x305 = print("[")
val x306 = x163(x304)
val x307 = print(x306)
val x308 = print(" ]\n")
x308
x304 = x304 + 1
}
x310
}
if (x293) {
x312thenb()
} else { 
x312elseb()
}
}
x312
}
val x315 = println("phi_y0: ")
val x336 = { 
val x316 = x166.isRow
val x334 = {
def x334thenb(): Unit = {
val x317 = print("[ ")
var x318 : Int = 0
val x323 = while (x318 < x133) {
val x319 = x166(x318)
val x320 = print(x319)
val x321 = print(" ")
x321
x318 = x318 + 1
}
val x324 = print("]\n")
x324
}
def x334elseb(): Unit = {
var x326 : Int = 0
val x332 = while (x326 < x133) {
val x327 = print("[")
val x328 = x166(x326)
val x329 = print(x328)
val x330 = print(" ]\n")
x330
x326 = x326 + 1
}
x332
}
if (x316) {
x334thenb()
} else { 
x334elseb()
}
}
x334
}
val x172 = x127
val x156 = x152.length
var x153 = 0
val x157 = x152.dcApply(x153)
var x159: Double = {
if (x156 == 0) {0.0}
else {
val x157 = x152.dcApply(x153)
x157
}
}
x153 = 1
while (x153 < x156) {  // begin fat loop x159
val x157 = x152.dcApply(x153)
val x154 = x159
val x155 = x157
val x158 = x154 + x155
x159 = x158
x153 += 1
} // end fat loop x159
val x173 = x159 / x172
val x337 = "phi_y: "+x173
val x338 = println(x337)
val x339 = x291.numRows
val x341 = "Testing model on "+x339
val x342 = x341+" documents."
val x343 = println(x342)
val x344 = new generated.scala.IndexVectorRangeImpl(0,x339)
val x346 = x344.length
val x347 = x344.isRow
val x340 = x291.numCols
val x355 = x340 - 0
val x356 = (0.0,0.0)
val x354 = new generated.scala.IndexVectorRangeImpl(0,x340)
val x365 = 1.0 - x173
val x366 = Math.log(x365)
val x371 = Math.log(x173)
// a *thin* loop follows: x390
var x345 = 0
val x350 = x344.dcApply(x345)
val x358 = x350 * x340
var x351 = 0
val x357 = x354.dcApply(x351)
val x359 = x358 + x357
val x360 = x291.dcApply(x359)
val x361 = { 
x360
}
val x362 = x361 > 0.0
val x376 = {
def x376thenb(): scala.Tuple2[Double, Double] = {
val x363 = x166(x357)
val x369 = x163(x357)
val x364 = Math.log(x363)
val x367 = x364 + x366
val x368 = x367 * x361
val x370 = Math.log(x369)
val x372 = x370 + x371
val x373 = x372 * x361
val x374 = (x368,x373)
x374
}
def x376elseb(): scala.Tuple2[Double, Double] = {
x356
}
if (x362) {
x376thenb()
} else { 
x376elseb()
}
}
var x385: scala.Tuple2[Double, Double] = {
if (x355 == 0) {x356}
else {
val x357 = x354.dcApply(x351)
val x359 = x358 + x357
val x360 = x291.dcApply(x359)
val x361 = { 
x360
}
val x362 = x361 > 0.0
val x376 = {
def x376thenb(): scala.Tuple2[Double, Double] = {
val x363 = x166(x357)
val x369 = x163(x357)
val x364 = Math.log(x363)
val x367 = x364 + x366
val x368 = x367 * x361
val x370 = Math.log(x369)
val x372 = x370 + x371
val x373 = x372 * x361
val x374 = (x368,x373)
x374
}
def x376elseb(): scala.Tuple2[Double, Double] = {
x356
}
if (x362) {
x376thenb()
} else { 
x376elseb()
}
}
x376
}
}
x351 = 1
while (x351 < x355) {  // begin fat loop x385
val x357 = x354.dcApply(x351)
val x359 = x358 + x357
val x360 = x291.dcApply(x359)
val x361 = { 
x360
}
val x362 = x361 > 0.0
val x376 = {
def x376thenb(): scala.Tuple2[Double, Double] = {
val x363 = x166(x357)
val x369 = x163(x357)
val x364 = Math.log(x363)
val x367 = x364 + x366
val x368 = x367 * x361
val x370 = Math.log(x369)
val x372 = x370 + x371
val x373 = x372 * x361
val x374 = (x368,x373)
x374
}
def x376elseb(): scala.Tuple2[Double, Double] = {
x356
}
if (x362) {
x376thenb()
} else { 
x376elseb()
}
}
val x352 = x385
val x353 = x376
val x378 = x352._1
val x380 = x353._1
val x382 = x378 + x380
val x379 = x352._2
val x381 = x353._2
val x383 = x379 + x381
val x384 = (x382,x383)
x385 = x384
x351 += 1
} // end fat loop x385
val x386 = x385._1
val x387 = x385._2
val x388 = x387 > x386
val x389 = {
def x389thenb(): Double = {
1.0
}
def x389elseb(): Double = {
0.0
}
if (x388) {
x389thenb()
} else { 
x389elseb()
}
}
val x390 = {
val x348 = new generated.scala.DoubleVectorImpl(x346,x347)
x348
}
x390.dcUpdate(x345, x389)
x345 = 1
while (x345 < x346) {  // begin fat loop x390
val x350 = x344.dcApply(x345)
val x358 = x350 * x340
var x351 = 0
val x357 = x354.dcApply(x351)
val x359 = x358 + x357
val x360 = x291.dcApply(x359)
val x361 = { 
x360
}
val x362 = x361 > 0.0
val x376 = {
def x376thenb(): scala.Tuple2[Double, Double] = {
val x363 = x166(x357)
val x369 = x163(x357)
val x364 = Math.log(x363)
val x367 = x364 + x366
val x368 = x367 * x361
val x370 = Math.log(x369)
val x372 = x370 + x371
val x373 = x372 * x361
val x374 = (x368,x373)
x374
}
def x376elseb(): scala.Tuple2[Double, Double] = {
x356
}
if (x362) {
x376thenb()
} else { 
x376elseb()
}
}
var x385: scala.Tuple2[Double, Double] = {
if (x355 == 0) {x356}
else {
val x357 = x354.dcApply(x351)
val x359 = x358 + x357
val x360 = x291.dcApply(x359)
val x361 = { 
x360
}
val x362 = x361 > 0.0
val x376 = {
def x376thenb(): scala.Tuple2[Double, Double] = {
val x363 = x166(x357)
val x369 = x163(x357)
val x364 = Math.log(x363)
val x367 = x364 + x366
val x368 = x367 * x361
val x370 = Math.log(x369)
val x372 = x370 + x371
val x373 = x372 * x361
val x374 = (x368,x373)
x374
}
def x376elseb(): scala.Tuple2[Double, Double] = {
x356
}
if (x362) {
x376thenb()
} else { 
x376elseb()
}
}
x376
}
}
x351 = 1
while (x351 < x355) {  // begin fat loop x385
val x357 = x354.dcApply(x351)
val x359 = x358 + x357
val x360 = x291.dcApply(x359)
val x361 = { 
x360
}
val x362 = x361 > 0.0
val x376 = {
def x376thenb(): scala.Tuple2[Double, Double] = {
val x363 = x166(x357)
val x369 = x163(x357)
val x364 = Math.log(x363)
val x367 = x364 + x366
val x368 = x367 * x361
val x370 = Math.log(x369)
val x372 = x370 + x371
val x373 = x372 * x361
val x374 = (x368,x373)
x374
}
def x376elseb(): scala.Tuple2[Double, Double] = {
x356
}
if (x362) {
x376thenb()
} else { 
x376elseb()
}
}
val x352 = x385
val x353 = x376
val x378 = x352._1
val x380 = x353._1
val x382 = x378 + x380
val x379 = x352._2
val x381 = x353._2
val x383 = x379 + x381
val x384 = (x382,x383)
x385 = x384
x351 += 1
} // end fat loop x385
val x386 = x385._1
val x387 = x385._2
val x388 = x387 > x386
val x389 = {
def x389thenb(): Double = {
1.0
}
def x389elseb(): Double = {
0.0
}
if (x388) {
x389thenb()
} else { 
x389elseb()
}
}
x390.dcUpdate(x345, x389)
x345 += 1
} // end fat loop x390
val x394 = x339 - 0
val x396 = x291.labels
var x391 = 0
val x395 = x344.dcApply(x391)
val x397 = x396(x395)
val x398 = x390(x395)
val x399 = x397 != x398
val x400 = {
def x400thenb(): Int = {
1
}
def x400elseb(): Int = {
0
}
if (x399) {
x400thenb()
} else { 
x400elseb()
}
}
var x402: Int = {
if (x394 == 0) {0}
else {
val x395 = x344.dcApply(x391)
val x397 = x396(x395)
val x398 = x390(x395)
val x399 = x397 != x398
val x400 = {
def x400thenb(): Int = {
1
}
def x400elseb(): Int = {
0
}
if (x399) {
x400thenb()
} else { 
x400elseb()
}
}
x400
}
}
x391 = 1
while (x391 < x394) {  // begin fat loop x402
val x395 = x344.dcApply(x391)
val x397 = x396(x395)
val x398 = x390(x395)
val x399 = x397 != x398
val x400 = {
def x400thenb(): Int = {
1
}
def x400elseb(): Int = {
0
}
if (x399) {
x400thenb()
} else { 
x400elseb()
}
}
val x392 = x402
val x393 = x400
val x401 = x392 + x393
x402 = x401
x391 += 1
} // end fat loop x402
val x403 = x402.doubleValue()
val x404 = x339.doubleValue()
val x405 = x403 / x404
val x406 = "Test error: "+x405
val x407 = println(x406)
()
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
