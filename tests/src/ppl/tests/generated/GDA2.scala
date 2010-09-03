package generated

/*****************************************
  Emitting Generated Code
*******************************************/
class GDA2 extends ((Array[java.lang.String])=>(Unit)) {
def apply(x0:Array[java.lang.String]): Unit = {
val x8 = {
val x96 = {
val x7 = x0(0)
new java.io.FileReader(x7)
}
val x97 = {
new java.io.BufferedReader(x96)
}
val x98 = x97.readLine()
var x99 = x98
val x100 = x99
val x101 = x100.trim()
x99 = x101
val x103 = x99
val x104 = x103.split("\\s+")
var x105 = x104
val x106 = x105
val x109 = {
val x107 = x106.length
new ppl.dsl.optiml.MatrixImpl[Double](0,x107)
}
while ({val x143 = x99
val x144 = x143 != null
x144}) {
val x110 = x105
val x113 = {
val x111 = x110.length
new ppl.dsl.optiml.VectorImpl[Double](x111,true)
}
val x114 = x105
val x115 = x114.length
val x116 = 0 until x115
val x123 = {x117: (Int) =>
val x118 = x105
val x119 = x118(x117)
val x120 = java.lang.Double.parseDouble(x119)
val x121 = x113(x117) = x120
x121
}
val x124 = x116.foreach{
x123
}
val x126 = {
val x145 = x109.numRows
val x146 = x109.insertRow(x145,x113)
x146
}
val x127 = x97.readLine()
x99 = x127
val x129 = x99
val x130 = x129 != null
val x138 = if (x130) {
val x131 = x99
val x132 = x131.trim()
x99 = x132
val x134 = x99
val x135 = x134.split("\\s+")
x105 = x135
()
} else {
()
}
x138
}
val x141 = x97.close()
x109
}
val x1 = x0.length
val x2 = x1 < 2
val x6 = if (x2) {
val x3 = println("Usage: GDA <input data file> <output label data file>")
val x4 = exit(-1)
x4
} else {
()
}
val x17 = println(x8)
val x16 = {
val x10 = {
val x162 = {
new ppl.dsl.optiml.VectorImpl[Double](0,true)
}
val x163 = {
val x9 = x0(1)
new java.io.FileReader(x9)
}
val x164 = {
new java.io.BufferedReader(x163)
}
val x165 = x164.readLine()
var x166 = x165
while ({val x96 = {
val x7 = x0(0)
new java.io.FileReader(x7)
}
val x97 = {
new java.io.BufferedReader(x96)
}
val x98 = x97.readLine()
var x99 = x98
val x143 = x99
val x179 = x166
val x180 = x179 != null
x180}) {
val x167 = x166
val x168 = x167.trim()
x166 = x168
val x170 = x166
val x171 = java.lang.Double.parseDouble(x170)
val x172 = x162 += x171
val x173 = x164.readLine()
x166 = x173
()
}
val x177 = x164.close()
x162
}
val x148 = x10.length
val x151 = {
val x149 = x10.is_row
new ppl.dsl.optiml.VectorImpl[Boolean](x148,x149)
}
val x152 = 0 until x148
val x158 = {x153: (Int) =>
val x14 = {x11: (Double) =>
val x12 = x11 <= 0.0
val x13 = if (x12) {
false
} else {
true
}
x13
}
val x154 = x10(x153)
val x155 = x14(x154)
val x156 = x151(x153) = x155
x156
}
val x159 = x152.foreach{
x158
}
x151
}
val x18 = println(x16)
val x21 = {
val x20 = x8.numCols
new ppl.dsl.optiml.VectorImpl[Double](x20,true)
}
var x22 = x21
val x23 = {
val x20 = x8.numCols
new ppl.dsl.optiml.VectorImpl[Double](x20,true)
}
var x24 = x23
val x19 = x16.length
val x25 = 0 until x19
val x43 = {x26: (Int) =>
val x27 = x16(x26)
val x28 = x27 == false
val x41 = if (x28) {
val x30 = x22
val x33 = {
val x181 = x30.length
val x184 = {
val x182 = x30.is_row
new ppl.dsl.optiml.VectorImpl[Double](x181,x182)
}
val x185 = 0 until x181
val x192 = {x186: (Int) =>
val x187 = x30(x186)
val x31 = x8(x26)
val x188 = x31(x186)
val x189 = x187 + x188
val x190 = x184(x186) = x189
x190
}
val x193 = x185.foreach{
x192
}
x184
}
x22 = x33
()
} else {
val x36 = x24
val x38 = {
val x195 = x36.length
val x198 = {
val x196 = x36.is_row
new ppl.dsl.optiml.VectorImpl[Double](x195,x196)
}
val x199 = 0 until x195
val x206 = {x200: (Int) =>
val x201 = x36(x200)
val x31 = x8(x26)
val x202 = x31(x200)
val x203 = x201 + x202
val x204 = x198(x200) = x203
x204
}
val x207 = x199.foreach{
x206
}
x198
}
x24 = x38
()
}
x41
}
val x44 = x25.foreach{
x43
}
val x48 = x22
val x51 = x24
val x55 = {
val x20 = x8.numCols
new ppl.dsl.optiml.MatrixImpl[Double](x20,x20)
}
var x56 = x55
val x29 = 0.0 + 1.0
val x83 = {x57: (Int) =>
val x58 = x16(x57)
val x59 = x58 == false
val x81 = if (x59) {
val x60 = x56
val x68 = {
val x209 = x60.numRows
val x212 = {
val x66 = {
val x64 = {
val x63 = {
val x61 = x8(x57)
val x259 = x61.length
val x262 = {
val x260 = x61.is_row
new ppl.dsl.optiml.VectorImpl[Double](x259,x260)
}
val x263 = 0 until x259
val x270 = {x264: (Int) =>
val x265 = x61(x264)
val x50 = {
val x273 = x48.length
val x276 = {
val x274 = x48.is_row
new ppl.dsl.optiml.VectorImpl[Double](x273,x274)
}
val x277 = 0 until x273
val x283 = {x278: (Int) =>
val x279 = x48(x278)
val x280 = x279 / x29
val x281 = x276(x278) = x280
x281
}
val x284 = x277.foreach{
x283
}
x276
}
val x266 = x50(x264)
val x267 = x265 - x266
val x268 = x262(x264) = x267
x268
}
val x271 = x263.foreach{
x270
}
x262
}
val x234 = x63.length
val x251 = {
val x248 = x63.is_row
val x249 = !x248
new ppl.dsl.optiml.VectorImpl[Double](x234,x249)
}
val x235 = 0 until x234
val x256 = {x252: (Int) =>
val x253 = x63(x252)
val x254 = x251(x252) = x253
x254
}
val x257 = x235.foreach{
x256
}
x251
}
val x229 = x64.length
val x231 = {
new ppl.dsl.optiml.MatrixImpl[Double](x229,x229)
}
val x232 = 0 until x229
val x245 = {x233: (Int) =>
val x63 = {
val x61 = x8(x57)
val x259 = x61.length
val x262 = {
val x260 = x61.is_row
new ppl.dsl.optiml.VectorImpl[Double](x259,x260)
}
val x263 = 0 until x259
val x270 = {x264: (Int) =>
val x265 = x61(x264)
val x50 = {
val x273 = x48.length
val x276 = {
val x274 = x48.is_row
new ppl.dsl.optiml.VectorImpl[Double](x273,x274)
}
val x277 = 0 until x273
val x283 = {x278: (Int) =>
val x279 = x48(x278)
val x280 = x279 / x29
val x281 = x276(x278) = x280
x281
}
val x284 = x277.foreach{
x283
}
x276
}
val x266 = x50(x264)
val x267 = x265 - x266
val x268 = x262(x264) = x267
x268
}
val x271 = x263.foreach{
x270
}
x262
}
val x234 = x63.length
val x235 = 0 until x234
val x242 = {x236: (Int) =>
val x237 = x64(x233)
val x238 = x63(x233)
val x239 = x237 * x238
val x240 = x231(x233, x236) = x239
x240
}
val x243 = x235.foreach{
x242
}
x243
}
val x246 = x232.foreach{
x245
}
x231
}
val x210 = x66.numCols
new ppl.dsl.optiml.MatrixImpl[Double](x209,x210)
}
val x213 = 0 until x209
val x226 = {x214: (Int) =>
val x215 = x60.numCols
val x216 = 0 until x215
val x223 = {x217: (Int) =>
val x66 = {
val x64 = {
val x63 = {
val x61 = x8(x57)
val x259 = x61.length
val x262 = {
val x260 = x61.is_row
new ppl.dsl.optiml.VectorImpl[Double](x259,x260)
}
val x263 = 0 until x259
val x270 = {x264: (Int) =>
val x265 = x61(x264)
val x50 = {
val x273 = x48.length
val x276 = {
val x274 = x48.is_row
new ppl.dsl.optiml.VectorImpl[Double](x273,x274)
}
val x277 = 0 until x273
val x283 = {x278: (Int) =>
val x279 = x48(x278)
val x280 = x279 / x29
val x281 = x276(x278) = x280
x281
}
val x284 = x277.foreach{
x283
}
x276
}
val x266 = x50(x264)
val x267 = x265 - x266
val x268 = x262(x264) = x267
x268
}
val x271 = x263.foreach{
x270
}
x262
}
val x234 = x63.length
val x251 = {
val x248 = x63.is_row
val x249 = !x248
new ppl.dsl.optiml.VectorImpl[Double](x234,x249)
}
val x235 = 0 until x234
val x256 = {x252: (Int) =>
val x253 = x63(x252)
val x254 = x251(x252) = x253
x254
}
val x257 = x235.foreach{
x256
}
x251
}
val x229 = x64.length
val x231 = {
new ppl.dsl.optiml.MatrixImpl[Double](x229,x229)
}
val x232 = 0 until x229
val x245 = {x233: (Int) =>
val x63 = {
val x61 = x8(x57)
val x259 = x61.length
val x262 = {
val x260 = x61.is_row
new ppl.dsl.optiml.VectorImpl[Double](x259,x260)
}
val x263 = 0 until x259
val x270 = {x264: (Int) =>
val x265 = x61(x264)
val x50 = {
val x273 = x48.length
val x276 = {
val x274 = x48.is_row
new ppl.dsl.optiml.VectorImpl[Double](x273,x274)
}
val x277 = 0 until x273
val x283 = {x278: (Int) =>
val x279 = x48(x278)
val x280 = x279 / x29
val x281 = x276(x278) = x280
x281
}
val x284 = x277.foreach{
x283
}
x276
}
val x266 = x50(x264)
val x267 = x265 - x266
val x268 = x262(x264) = x267
x268
}
val x271 = x263.foreach{
x270
}
x262
}
val x234 = x63.length
val x235 = 0 until x234
val x242 = {x236: (Int) =>
val x237 = x64(x233)
val x238 = x63(x233)
val x239 = x237 * x238
val x240 = x231(x233, x236) = x239
x240
}
val x243 = x235.foreach{
x242
}
x243
}
val x246 = x232.foreach{
x245
}
x231
}
val x218 = x60(x214, x217)
val x219 = x66(x214, x217)
val x220 = x218 + x219
val x221 = x212(x214, x217) = x220
x221
}
val x224 = x216.foreach{
x223
}
x224
}
val x227 = x213.foreach{
x226
}
x212
}
x56 = x68
()
} else {
val x71 = x56
val x78 = {
val x286 = x71.numRows
val x289 = {
val x76 = {
val x74 = {
val x73 = {
val x61 = x8(x57)
val x259 = x61.length
val x336 = {
val x260 = x61.is_row
new ppl.dsl.optiml.VectorImpl[Double](x259,x260)
}
val x263 = 0 until x259
val x343 = {x337: (Int) =>
val x338 = x61(x337)
val x53 = {
val x346 = x51.length
val x349 = {
val x347 = x51.is_row
new ppl.dsl.optiml.VectorImpl[Double](x346,x347)
}
val x350 = 0 until x346
val x356 = {x351: (Int) =>
val x352 = x51(x351)
val x353 = x352 / x29
val x354 = x349(x351) = x353
x354
}
val x357 = x350.foreach{
x356
}
x349
}
val x339 = x53(x337)
val x340 = x338 - x339
val x341 = x336(x337) = x340
x341
}
val x344 = x263.foreach{
x343
}
x336
}
val x311 = x73.length
val x328 = {
val x325 = x73.is_row
val x326 = !x325
new ppl.dsl.optiml.VectorImpl[Double](x311,x326)
}
val x312 = 0 until x311
val x333 = {x329: (Int) =>
val x330 = x73(x329)
val x331 = x328(x329) = x330
x331
}
val x334 = x312.foreach{
x333
}
x328
}
val x306 = x74.length
val x308 = {
new ppl.dsl.optiml.MatrixImpl[Double](x306,x306)
}
val x309 = 0 until x306
val x322 = {x310: (Int) =>
val x73 = {
val x61 = x8(x57)
val x259 = x61.length
val x336 = {
val x260 = x61.is_row
new ppl.dsl.optiml.VectorImpl[Double](x259,x260)
}
val x263 = 0 until x259
val x343 = {x337: (Int) =>
val x338 = x61(x337)
val x53 = {
val x346 = x51.length
val x349 = {
val x347 = x51.is_row
new ppl.dsl.optiml.VectorImpl[Double](x346,x347)
}
val x350 = 0 until x346
val x356 = {x351: (Int) =>
val x352 = x51(x351)
val x353 = x352 / x29
val x354 = x349(x351) = x353
x354
}
val x357 = x350.foreach{
x356
}
x349
}
val x339 = x53(x337)
val x340 = x338 - x339
val x341 = x336(x337) = x340
x341
}
val x344 = x263.foreach{
x343
}
x336
}
val x311 = x73.length
val x312 = 0 until x311
val x319 = {x313: (Int) =>
val x314 = x74(x310)
val x315 = x73(x310)
val x316 = x314 * x315
val x317 = x308(x310, x313) = x316
x317
}
val x320 = x312.foreach{
x319
}
x320
}
val x323 = x309.foreach{
x322
}
x308
}
val x287 = x76.numCols
new ppl.dsl.optiml.MatrixImpl[Double](x286,x287)
}
val x290 = 0 until x286
val x303 = {x291: (Int) =>
val x292 = x71.numCols
val x293 = 0 until x292
val x300 = {x294: (Int) =>
val x76 = {
val x74 = {
val x73 = {
val x61 = x8(x57)
val x259 = x61.length
val x336 = {
val x260 = x61.is_row
new ppl.dsl.optiml.VectorImpl[Double](x259,x260)
}
val x263 = 0 until x259
val x343 = {x337: (Int) =>
val x338 = x61(x337)
val x53 = {
val x346 = x51.length
val x349 = {
val x347 = x51.is_row
new ppl.dsl.optiml.VectorImpl[Double](x346,x347)
}
val x350 = 0 until x346
val x356 = {x351: (Int) =>
val x352 = x51(x351)
val x353 = x352 / x29
val x354 = x349(x351) = x353
x354
}
val x357 = x350.foreach{
x356
}
x349
}
val x339 = x53(x337)
val x340 = x338 - x339
val x341 = x336(x337) = x340
x341
}
val x344 = x263.foreach{
x343
}
x336
}
val x311 = x73.length
val x328 = {
val x325 = x73.is_row
val x326 = !x325
new ppl.dsl.optiml.VectorImpl[Double](x311,x326)
}
val x312 = 0 until x311
val x333 = {x329: (Int) =>
val x330 = x73(x329)
val x331 = x328(x329) = x330
x331
}
val x334 = x312.foreach{
x333
}
x328
}
val x306 = x74.length
val x308 = {
new ppl.dsl.optiml.MatrixImpl[Double](x306,x306)
}
val x309 = 0 until x306
val x322 = {x310: (Int) =>
val x73 = {
val x61 = x8(x57)
val x259 = x61.length
val x336 = {
val x260 = x61.is_row
new ppl.dsl.optiml.VectorImpl[Double](x259,x260)
}
val x263 = 0 until x259
val x343 = {x337: (Int) =>
val x338 = x61(x337)
val x53 = {
val x346 = x51.length
val x349 = {
val x347 = x51.is_row
new ppl.dsl.optiml.VectorImpl[Double](x346,x347)
}
val x350 = 0 until x346
val x356 = {x351: (Int) =>
val x352 = x51(x351)
val x353 = x352 / x29
val x354 = x349(x351) = x353
x354
}
val x357 = x350.foreach{
x356
}
x349
}
val x339 = x53(x337)
val x340 = x338 - x339
val x341 = x336(x337) = x340
x341
}
val x344 = x263.foreach{
x343
}
x336
}
val x311 = x73.length
val x312 = 0 until x311
val x319 = {x313: (Int) =>
val x314 = x74(x310)
val x315 = x73(x310)
val x316 = x314 * x315
val x317 = x308(x310, x313) = x316
x317
}
val x320 = x312.foreach{
x319
}
x320
}
val x323 = x309.foreach{
x322
}
x308
}
val x295 = x71(x291, x294)
val x296 = x76(x291, x294)
val x297 = x295 + x296
val x298 = x289(x291, x294) = x297
x298
}
val x301 = x293.foreach{
x300
}
x301
}
val x304 = x290.foreach{
x303
}
x289
}
x56 = x78
()
}
x81
}
val x84 = x25.foreach{
x83
}
val x85 = print("GDA parameter calculation finished: ")
val x45 = x19
val x46 = 1.0 / x45
val x47 = x46 * x29
val x86 = "  phi = "+x47
val x87 = println(x86)
val x88 = println("  mu0 = ")
val x89 = {
val x50 = {
val x273 = x48.length
val x276 = {
val x274 = x48.is_row
new ppl.dsl.optiml.VectorImpl[Double](x273,x274)
}
val x277 = 0 until x273
val x283 = {x278: (Int) =>
val x279 = x48(x278)
val x280 = x279 / x29
val x281 = x276(x278) = x280
x281
}
val x284 = x277.foreach{
x283
}
x276
}
val x359 = x50.is_row
val x381 = if (x359) {
val x360 = print("[ ")
val x361 = x50.length
val x362 = 0 until x361
val x368 = {x363: (Int) =>
val x364 = x50(x363)
val x365 = print(x364)
val x366 = print(" ")
x366
}
val x369 = x362.foreach{
x368
}
val x370 = print("]\n")
x370
} else {
val x361 = x50.length
val x362 = 0 until x361
val x378 = {x372: (Int) =>
val x373 = print("[")
val x374 = x50(x372)
val x375 = print(x374)
val x376 = print(" ]\n")
x376
}
val x379 = x362.foreach{
x378
}
x379
}
x381
}
val x90 = println("  mu1 = ")
val x91 = {
val x53 = {
val x346 = x51.length
val x349 = {
val x347 = x51.is_row
new ppl.dsl.optiml.VectorImpl[Double](x346,x347)
}
val x350 = 0 until x346
val x356 = {x351: (Int) =>
val x352 = x51(x351)
val x353 = x352 / x29
val x354 = x349(x351) = x353
x354
}
val x357 = x350.foreach{
x356
}
x349
}
val x383 = x53.is_row
val x405 = if (x383) {
val x384 = print("[ ")
val x385 = x53.length
val x386 = 0 until x385
val x392 = {x387: (Int) =>
val x388 = x53(x387)
val x389 = print(x388)
val x390 = print(" ")
x390
}
val x393 = x386.foreach{
x392
}
val x394 = print("]\n")
x394
} else {
val x385 = x53.length
val x386 = 0 until x385
val x402 = {x396: (Int) =>
val x397 = print("[")
val x398 = x53(x396)
val x399 = print(x398)
val x400 = print(" ]\n")
x400
}
val x403 = x386.foreach{
x402
}
x403
}
x405
}
val x92 = println("  sigma = ")
val x93 = x56
val x94 = {
val x407 = x93.numRows
val x408 = 0 until x407
val x422 = {x409: (Int) =>
val x410 = print("[ ")
val x411 = x93.numCols
val x412 = 0 until x411
val x418 = {x413: (Int) =>
val x414 = x93(x409, x413)
val x415 = print(x414)
val x416 = print(" ")
x416
}
val x419 = x412.foreach{
x418
}
val x420 = print("]\n")
x420
}
val x423 = x408.foreach{
x422
}
x423
}
x94
}
}
/*****************************************
  End of Generated Code
*******************************************/

