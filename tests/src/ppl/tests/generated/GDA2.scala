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
val x125 = {
new java.io.FileReader(x7)
}
val x126 = {
new java.io.BufferedReader(x125)
}
val x127 = x126.readLine()
var x128 = x127
val x129 = x128
val x130 = x129.trim()
x128 = x130
val x132 = x128
val x133 = x132.split("\\s+")
var x134 = x133
val x135 = x134
val x136 = x135.length
val x138 = {
new ppl.dsl.optiml.MatrixImpl[Double](0,x136)
}
while ({val x468 = x128
val x469 = x468 != null
x469}) {
val x139 = x134
val x140 = x139.length
val x142 = {
new ppl.dsl.optiml.VectorImpl[Double](x140,true)
}
val x143 = x134
val x144 = x143.length
val x145 = 0 until x144
val x152 = {x146: (Int) =>
val x147 = x134
val x148 = x147(x146)
val x149 = java.lang.Double.parseDouble(x148)
val x150 = x142(x146) = x149
x150
}
val x153 = x145.foreach{
x152
}
val x155 = {
val x172 = x138.numRows
val x173 = x138.insertRow(x172,x142)
x173
}
val x156 = x126.readLine()
x128 = x156
val x158 = x128
val x159 = x158 != null
val x167 = if (x159) {
val x160 = x128
val x161 = x160.trim()
x128 = x161
val x163 = x128
val x164 = x163.split("\\s+")
x134 = x164
()
} else {
()
}
x167
}
val x170 = x126.close()
x138
}
val x22 = x8.numCols
val x9 = x0(1)
val x10 = {
val x176 = {
new ppl.dsl.optiml.VectorImpl[Double](0,true)
}
val x177 = {
new java.io.FileReader(x9)
}
val x178 = {
new java.io.BufferedReader(x177)
}
val x179 = x178.readLine()
var x180 = x179
while ({val x471 = x180
val x472 = x471 != null
x472}) {
val x181 = x180
val x182 = x181.trim()
x180 = x182
val x184 = java.lang.Double.parseDouble(x180)
val x185 = x176 += x184
val x186 = x178.readLine()
x180 = x186
()
}
val x190 = x178.close()
x176
}
val x17 = println("X: ")
val x18 = {
val x192 = x8.numRows
val x193 = 0 until x192
val x206 = {x194: (Int) =>
val x195 = print("[ ")
val x196 = 0 until x22
val x202 = {x197: (Int) =>
val x198 = x8(x194, x197)
val x199 = print(x198)
val x200 = print(" ")
x200
}
val x203 = x196.foreach{
x202
}
val x204 = print("]\n")
x204
}
val x207 = x193.foreach{
x206
}
x207
}
val x19 = println("Y: ")
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
val x231 = x10.length
val x232 = x10.is_row
val x234 = {
new ppl.dsl.optiml.VectorImpl[Boolean](x231,x232)
}
val x235 = 0 until x231
val x241 = {x236: (Int) =>
val x237 = x10(x236)
val x238 = x14(x237)
val x239 = x234(x236) = x238
x239
}
val x242 = x235.foreach{
x241
}
x234
}
val x21 = x16.length
val x29 = 0 until x21
val x20 = {
val x209 = x16.is_row
val x229 = if (x209) {
val x210 = print("[ ")
val x216 = {x211: (Int) =>
val x212 = x16(x211)
val x213 = print(x212)
val x214 = print(" ")
x214
}
val x217 = x29.foreach{
x216
}
val x218 = print("]\n")
x218
} else {
val x226 = {x220: (Int) =>
val x221 = print("[")
val x222 = x16(x220)
val x223 = print(x222)
val x224 = print(" ]\n")
x224
}
val x227 = x29.foreach{
x226
}
x227
}
x229
}
var x23 = 0.0
var x24 = 0.0
val x25 = {
new ppl.dsl.optiml.VectorImpl[Double](x22,true)
}
var x26 = x25
val x27 = {
new ppl.dsl.optiml.VectorImpl[Double](x22,true)
}
var x28 = x27
val x50 = {x30: (Int) =>
val x31 = x16(x30)
val x32 = x31 == false
val x48 = if (x32) {
val x33 = x24 + 1.0
x24 = x33
val x35 = x26
val x36 = x8(x30)
val x38 = {
val x244 = x35.length
val x245 = x35.is_row
val x247 = {
new ppl.dsl.optiml.VectorImpl[Double](x244,x245)
}
val x248 = 0 until x244
val x255 = {x249: (Int) =>
val x250 = x35(x249)
val x251 = x36(x249)
val x252 = x250 + x251
val x253 = x247(x249) = x252
x253
}
val x256 = x248.foreach{
x255
}
x247
}
x26 = x38
()
} else {
val x41 = x23 + 1.0
x23 = x41
val x43 = x28
val x36 = x8(x30)
val x45 = {
val x258 = x43.length
val x259 = x43.is_row
val x261 = {
new ppl.dsl.optiml.VectorImpl[Double](x258,x259)
}
val x262 = 0 until x258
val x269 = {x263: (Int) =>
val x264 = x43(x263)
val x265 = x36(x263)
val x266 = x264 + x265
val x267 = x261(x263) = x266
x267
}
val x270 = x262.foreach{
x269
}
x261
}
x28 = x45
()
}
x48
}
val x51 = x29.foreach{
x50
}
val x52 = "y_zeros: "+x24
val x53 = println(x52)
val x54 = "y_ones: "+x23
val x55 = println(x54)
val x59 = x26
val x62 = x28
val x66 = {
new ppl.dsl.optiml.MatrixImpl[Double](x22,x22)
}
var x67 = x66
val x61 = {
val x335 = x59.length
val x336 = x59.is_row
val x338 = {
new ppl.dsl.optiml.VectorImpl[Double](x335,x336)
}
val x339 = 0 until x335
val x345 = {x340: (Int) =>
val x341 = x59(x340)
val x342 = x341 / x24
val x343 = x338(x340) = x342
x343
}
val x346 = x339.foreach{
x345
}
x338
}
val x64 = {
val x407 = x62.length
val x408 = x62.is_row
val x410 = {
new ppl.dsl.optiml.VectorImpl[Double](x407,x408)
}
val x411 = 0 until x407
val x417 = {x412: (Int) =>
val x413 = x62(x412)
val x414 = x413 / x23
val x415 = x410(x412) = x414
x415
}
val x418 = x411.foreach{
x417
}
x410
}
val x94 = {x68: (Int) =>
val x69 = x16(x68)
val x70 = x69 == false
val x92 = if (x70) {
val x71 = x67
val x72 = x8(x68)
val x74 = {
val x321 = x72.length
val x322 = x72.is_row
val x324 = {
new ppl.dsl.optiml.VectorImpl[Double](x321,x322)
}
val x325 = 0 until x321
val x332 = {x326: (Int) =>
val x327 = x72(x326)
val x328 = x61(x326)
val x329 = x327 - x328
val x330 = x324(x326) = x329
x330
}
val x333 = x325.foreach{
x332
}
x324
}
val x75 = {
val x296 = x74.length
val x310 = x74.is_row
val x311 = !x310
val x313 = {
new ppl.dsl.optiml.VectorImpl[Double](x296,x311)
}
val x297 = 0 until x296
val x318 = {x314: (Int) =>
val x315 = x74(x314)
val x316 = x313(x314) = x315
x316
}
val x319 = x297.foreach{
x318
}
x313
}
val x77 = {
val x291 = x75.length
val x293 = {
new ppl.dsl.optiml.MatrixImpl[Double](x291,x291)
}
val x294 = 0 until x291
val x307 = {x295: (Int) =>
val x296 = x74.length
val x297 = 0 until x296
val x304 = {x298: (Int) =>
val x299 = x75(x295)
val x300 = x74(x298)
val x301 = x299 * x300
val x302 = x293(x295, x298) = x301
x302
}
val x305 = x297.foreach{
x304
}
x305
}
val x308 = x294.foreach{
x307
}
x293
}
val x79 = {
val x272 = x71.numRows
val x273 = x71.numCols
val x275 = {
new ppl.dsl.optiml.MatrixImpl[Double](x272,x273)
}
val x276 = 0 until x272
val x288 = {x277: (Int) =>
val x278 = 0 until x273
val x285 = {x279: (Int) =>
val x280 = x71(x277, x279)
val x281 = x77(x277, x279)
val x282 = x280 + x281
val x283 = x275(x277, x279) = x282
x283
}
val x286 = x278.foreach{
x285
}
x286
}
val x289 = x276.foreach{
x288
}
x275
}
x67 = x79
()
} else {
val x82 = x67
val x72 = x8(x68)
val x84 = {
val x321 = x72.length
val x322 = x72.is_row
val x397 = {
new ppl.dsl.optiml.VectorImpl[Double](x321,x322)
}
val x325 = 0 until x321
val x404 = {x398: (Int) =>
val x399 = x72(x398)
val x400 = x64(x398)
val x401 = x399 - x400
val x402 = x397(x398) = x401
x402
}
val x405 = x325.foreach{
x404
}
x397
}
val x85 = {
val x372 = x84.length
val x386 = x84.is_row
val x387 = !x386
val x389 = {
new ppl.dsl.optiml.VectorImpl[Double](x372,x387)
}
val x373 = 0 until x372
val x394 = {x390: (Int) =>
val x391 = x84(x390)
val x392 = x389(x390) = x391
x392
}
val x395 = x373.foreach{
x394
}
x389
}
val x87 = {
val x367 = x85.length
val x369 = {
new ppl.dsl.optiml.MatrixImpl[Double](x367,x367)
}
val x370 = 0 until x367
val x383 = {x371: (Int) =>
val x372 = x84.length
val x373 = 0 until x372
val x380 = {x374: (Int) =>
val x375 = x85(x371)
val x376 = x84(x374)
val x377 = x375 * x376
val x378 = x369(x371, x374) = x377
x378
}
val x381 = x373.foreach{
x380
}
x381
}
val x384 = x370.foreach{
x383
}
x369
}
val x89 = {
val x348 = x82.numRows
val x349 = x82.numCols
val x351 = {
new ppl.dsl.optiml.MatrixImpl[Double](x348,x349)
}
val x352 = 0 until x348
val x364 = {x353: (Int) =>
val x354 = 0 until x349
val x361 = {x355: (Int) =>
val x356 = x82(x353, x355)
val x357 = x87(x353, x355)
val x358 = x356 + x357
val x359 = x351(x353, x355) = x358
x359
}
val x362 = x354.foreach{
x361
}
x362
}
val x365 = x352.foreach{
x364
}
x351
}
x67 = x89
()
}
x92
}
val x95 = x29.foreach{
x94
}
val x96 = print("GDA parameter calculation finished: ")
val x56 = x21
val x57 = 1.0 / x56
val x58 = x57 * x23
val x97 = "  phi = "+x58
val x98 = println(x97)
val x99 = println("  mu0 = ")
val x100 = {
val x420 = x61.is_row
val x442 = if (x420) {
val x421 = print("[ ")
val x422 = x61.length
val x423 = 0 until x422
val x429 = {x424: (Int) =>
val x425 = x61(x424)
val x426 = print(x425)
val x427 = print(" ")
x427
}
val x430 = x423.foreach{
x429
}
val x431 = print("]\n")
x431
} else {
val x422 = x61.length
val x423 = 0 until x422
val x439 = {x433: (Int) =>
val x434 = print("[")
val x435 = x61(x433)
val x436 = print(x435)
val x437 = print(" ]\n")
x437
}
val x440 = x423.foreach{
x439
}
x440
}
x442
}
val x101 = println("  mu1 = ")
val x102 = {
val x444 = x64.is_row
val x466 = if (x444) {
val x445 = print("[ ")
val x446 = x64.length
val x447 = 0 until x446
val x453 = {x448: (Int) =>
val x449 = x64(x448)
val x450 = print(x449)
val x451 = print(" ")
x451
}
val x454 = x447.foreach{
x453
}
val x455 = print("]\n")
x455
} else {
val x446 = x64.length
val x447 = 0 until x446
val x463 = {x457: (Int) =>
val x458 = print("[")
val x459 = x64(x457)
val x460 = print(x459)
val x461 = print(" ]\n")
x461
}
val x464 = x447.foreach{
x463
}
x464
}
x466
}
val x103 = println("  sigma = ")
val x104 = x67
val x105 = {
val x107 = x104.numRows
val x108 = 0 until x107
val x122 = {x109: (Int) =>
val x110 = print("[ ")
val x111 = x104.numCols
val x112 = 0 until x111
val x118 = {x113: (Int) =>
val x114 = x104(x109, x113)
val x115 = print(x114)
val x116 = print(" ")
x116
}
val x119 = x112.foreach{
x118
}
val x120 = print("]\n")
x120
}
val x123 = x108.foreach{
x122
}
x123
}
x105
}
}
/*****************************************
  End of Generated Code
*******************************************/
