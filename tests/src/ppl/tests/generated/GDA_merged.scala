package generated

/*****************************************
  Emitting Scala Generated Code
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
val x55 = {
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
new ppl.dsl.optiml.MatrixImpl[Double](0,x19)
}
while ({val x420 = x11
val x421 = x420 != null
x421}) {
val x21 = x17
val x22 = x21.length
val x23 = {
new ppl.dsl.optiml.VectorImpl[Double](x22,true)
}
val x24 = x17
val x25 = x24.length
val x26 = 0 until x25
val x33 = {x27: (Int) =>
val x28 = x17
val x29 = x28(x27)
val x30 = java.lang.Double.parseDouble(x29)
val x31 = x23(x27) = x30
x31
}
val x34 = x26.foreach{
x33
}
val x38 = {
val x35 = x20.numRows
val x36 = x20.insertRow(x35,x23)
x36
}
val x39 = x9.readLine()
x11 = x39
val x41 = x11
val x42 = x41 != null
val x50 = if (x42) {
val x43 = x11
val x44 = x43.trim()
x11 = x44
val x46 = x11
val x47 = x46.split("\\s+")
x17 = x47
()
} else {
()
}
x50
}
val x53 = x9.close()
x20
}
val x56 = x0(1)
val x74 = {
val x57 = {
new ppl.dsl.optiml.VectorImpl[Double](0,true)
}
val x58 = {
new java.io.FileReader(x56)
}
val x59 = {
new java.io.BufferedReader(x58)
}
val x60 = x59.readLine()
var x61 = x60
while ({val x423 = x61
val x424 = x423 != null
x424}) {
val x62 = x61
val x63 = x62.trim()
x61 = x63
val x65 = x61
val x66 = java.lang.Double.parseDouble(x65)
val x67 = x57 += x66
val x68 = x59.readLine()
x61 = x68
()
}
val x72 = x59.close()
x57
}
var x91 = 0.0
var x92 = 0.0
val x90 = x55.numCols
val x95 = {
val x93 = {
new ppl.dsl.optiml.VectorImpl[Double](x90,true)
}
x93
}
var x96 = x95
val x99 = {
val x97 = {
new ppl.dsl.optiml.VectorImpl[Double](x90,true)
}
x97
}
var x100 = x99
val x88 = {
val x75 = x74.length
val x76 = x74.is_row
val x77 = {
new ppl.dsl.optiml.VectorImpl[Boolean](x75,x76)
}
val x78 = 0 until x75
val x85 = {x79: (Int) =>
val x80 = x74(x79)
val x81 = x80 <= 0.0
val x82 = if (x81) {
false
} else {
true
}
val x83 = x77(x79) = x82
x83
}
val x86 = x78.foreach{
x85
}
x77
}
val x89 = x88.length
val x101 = 0 until x89
val x148 = {x102: (Int) =>
val x103 = x88(x102)
val x104 = x103 == false
val x146 = if (x104) {
val x105 = x92
val x106 = x105 + 1.0
x92 = x106
val x108 = x96
val x109 = x55(x102)
val x123 = {
val x110 = x108.length
val x111 = x108.is_row
val x112 = {
new ppl.dsl.optiml.VectorImpl[Double](x110,x111)
}
val x113 = 0 until x110
val x120 = {x114: (Int) =>
val x115 = x108(x114)
val x116 = x109(x114)
val x117 = x115 + x116
val x118 = x112(x114) = x117
x118
}
val x121 = x113.foreach{
x120
}
x112
}
x96 = x123
()
} else {
val x126 = x91
val x127 = x126 + 1.0
x91 = x127
val x129 = x100
val x109 = x55(x102)
val x143 = {
val x130 = x129.length
val x131 = x129.is_row
val x132 = {
new ppl.dsl.optiml.VectorImpl[Double](x130,x131)
}
val x133 = 0 until x130
val x140 = {x134: (Int) =>
val x135 = x129(x134)
val x136 = x109(x134)
val x137 = x135 + x136
val x138 = x132(x134) = x137
x138
}
val x141 = x133.foreach{
x140
}
x132
}
x100 = x143
()
}
x146
}
val x149 = x101.foreach{
x148
}
val x152 = x91
val x154 = x96
val x155 = x92
val x169 = x100
val x170 = x91
val x184 = {
new ppl.dsl.optiml.MatrixImpl[Double](x90,x90)
}
var x185 = x184
val x168 = {
val x156 = x154.length
val x157 = x154.is_row
val x158 = {
new ppl.dsl.optiml.VectorImpl[Double](x156,x157)
}
val x159 = 0 until x156
val x165 = {x160: (Int) =>
val x161 = x154(x160)
val x162 = x161 / x155
val x163 = x158(x160) = x162
x163
}
val x166 = x159.foreach{
x165
}
x158
}
val x183 = {
val x171 = x169.length
val x172 = x169.is_row
val x173 = {
new ppl.dsl.optiml.VectorImpl[Double](x171,x172)
}
val x174 = 0 until x171
val x180 = {x175: (Int) =>
val x176 = x169(x175)
val x177 = x176 / x170
val x178 = x173(x175) = x177
x178
}
val x181 = x174.foreach{
x180
}
x173
}
val x341 = {x186: (Int) =>
val x187 = x88(x186)
val x188 = x187 == false
val x339 = if (x188) {
val x189 = x185
val x190 = x55(x186)
val x204 = {
val x191 = x190.length
val x192 = x190.is_row
val x193 = {
new ppl.dsl.optiml.VectorImpl[Double](x191,x192)
}
val x194 = 0 until x191
val x201 = {x195: (Int) =>
val x196 = x190(x195)
val x197 = x168(x195)
val x198 = x196 - x197
val x199 = x193(x195) = x198
x199
}
val x202 = x194.foreach{
x201
}
x193
}
val x217 = {
val x205 = x204.length
val x206 = x204.is_row
val x207 = !x206
val x208 = {
new ppl.dsl.optiml.VectorImpl[Double](x205,x207)
}
val x209 = 0 until x205
val x214 = {x210: (Int) =>
val x211 = x204(x210)
val x212 = x208(x210) = x211
x212
}
val x215 = x209.foreach{
x214
}
x208
}
val x244 = {
val x228 = x217.length
val x229 = {
new ppl.dsl.optiml.MatrixImpl[Double](x228,x228)
}
val x230 = 0 until x228
val x241 = {x231: (Int) =>
val x205 = x204.length
val x209 = 0 until x205
val x238 = {x232: (Int) =>
val x233 = x217(x231)
val x234 = x204(x232)
val x235 = x233 * x234
val x236 = x229(x231, x232) = x235
x236
}
val x239 = x209.foreach{
x238
}
x239
}
val x242 = x230.foreach{
x241
}
x229
}
val x263 = {
val x245 = x189.numRows
val x246 = x189.numCols
val x247 = {
new ppl.dsl.optiml.MatrixImpl[Double](x245,x246)
}
val x248 = 0 until x245
val x260 = {x249: (Int) =>
val x250 = 0 until x246
val x257 = {x251: (Int) =>
val x252 = x189(x249, x251)
val x253 = x244(x249, x251)
val x254 = x252 + x253
val x255 = x247(x249, x251) = x254
x255
}
val x258 = x250.foreach{
x257
}
x258
}
val x261 = x248.foreach{
x260
}
x247
}
x185 = x263
()
} else {
val x266 = x185
val x190 = x55(x186)
val x277 = {
val x191 = x190.length
val x192 = x190.is_row
val x267 = {
new ppl.dsl.optiml.VectorImpl[Double](x191,x192)
}
val x194 = 0 until x191
val x274 = {x268: (Int) =>
val x269 = x190(x268)
val x270 = x183(x268)
val x271 = x269 - x270
val x272 = x267(x268) = x271
x272
}
val x275 = x194.foreach{
x274
}
x267
}
val x290 = {
val x278 = x277.length
val x279 = x277.is_row
val x280 = !x279
val x281 = {
new ppl.dsl.optiml.VectorImpl[Double](x278,x280)
}
val x282 = 0 until x278
val x287 = {x283: (Int) =>
val x284 = x277(x283)
val x285 = x281(x283) = x284
x285
}
val x288 = x282.foreach{
x287
}
x281
}
val x317 = {
val x301 = x290.length
val x302 = {
new ppl.dsl.optiml.MatrixImpl[Double](x301,x301)
}
val x303 = 0 until x301
val x314 = {x304: (Int) =>
val x278 = x277.length
val x282 = 0 until x278
val x311 = {x305: (Int) =>
val x306 = x290(x304)
val x307 = x277(x305)
val x308 = x306 * x307
val x309 = x302(x304, x305) = x308
x309
}
val x312 = x282.foreach{
x311
}
x312
}
val x315 = x303.foreach{
x314
}
x302
}
val x336 = {
val x318 = x266.numRows
val x319 = x266.numCols
val x320 = {
new ppl.dsl.optiml.MatrixImpl[Double](x318,x319)
}
val x321 = 0 until x318
val x333 = {x322: (Int) =>
val x323 = 0 until x319
val x330 = {x324: (Int) =>
val x325 = x266(x322, x324)
val x326 = x317(x322, x324)
val x327 = x325 + x326
val x328 = x320(x322, x324) = x327
x328
}
val x331 = x323.foreach{
x330
}
x331
}
val x334 = x321.foreach{
x333
}
x320
}
x185 = x336
()
}
x339
}
val x342 = x101.foreach{
x341
}
val x343 = print("GDA parameter calculation finished: ")
val x150 = x89
val x151 = 1.0 / x150
val x153 = x151 * x152
val x344 = "  phi = "+x153
val x345 = println(x344)
val x346 = println("  mu0 = ")
val x371 = {
val x347 = x168.is_row
val x369 = if (x347) {
val x348 = print("[ ")
val x349 = x168.length
val x350 = 0 until x349
val x356 = {x351: (Int) =>
val x352 = x168(x351)
val x353 = print(x352)
val x354 = print(" ")
x354
}
val x357 = x350.foreach{
x356
}
val x358 = print("]\n")
x358
} else {
val x349 = x168.length
val x350 = 0 until x349
val x366 = {x360: (Int) =>
val x361 = print("[")
val x362 = x168(x360)
val x363 = print(x362)
val x364 = print(" ]\n")
x364
}
val x367 = x350.foreach{
x366
}
x367
}
x369
}
val x372 = println("  mu1 = ")
val x397 = {
val x373 = x183.is_row
val x395 = if (x373) {
val x374 = print("[ ")
val x375 = x183.length
val x376 = 0 until x375
val x382 = {x377: (Int) =>
val x378 = x183(x377)
val x379 = print(x378)
val x380 = print(" ")
x380
}
val x383 = x376.foreach{
x382
}
val x384 = print("]\n")
x384
} else {
val x375 = x183.length
val x376 = 0 until x375
val x392 = {x386: (Int) =>
val x387 = print("[")
val x388 = x183(x386)
val x389 = print(x388)
val x390 = print(" ]\n")
x390
}
val x393 = x376.foreach{
x392
}
x393
}
x395
}
val x398 = println("  sigma = ")
val x399 = x185
val x418 = {
val x400 = x399.numRows
val x401 = 0 until x400
val x415 = {x402: (Int) =>
val x403 = print("[ ")
val x404 = x399.numCols
val x405 = 0 until x404
val x411 = {x406: (Int) =>
val x407 = x399(x402, x406)
val x408 = print(x407)
val x409 = print(" ")
x409
}
val x412 = x405.foreach{
x411
}
val x413 = print("]\n")
x413
}
val x416 = x401.foreach{
x415
}
x416
}
()
}
}
/*****************************************
  End of Scala Generated Code
*******************************************/
