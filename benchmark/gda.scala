/*****************************************
  Emitting Generated Code                  
*******************************************/
class Application extends ((Array[java.lang.String])=>(Unit)) {
def apply(x0:Array[java.lang.String]): Unit = {
val x1 = x0.length
val x2 = x1 < 2
val x6 = {
def x6thenb(): Nothing = {
val x3 = println("Usage: GDA <input data file> <output label data file>")
val x4 = exit(-1)
x4
}
if (x2) {
x6thenb()
}
}
val x7 = x0(0)
val x56 = { 
val x8 = new java.io.FileReader(x7)
val x9 = new java.io.BufferedReader(x8)
val x10 = x9.readLine()
var x11: java.lang.String = x10
val x12 = x11
val x13 = x12.trim()
x11 = x13
val x15 = x11
val x16 = x15.split("\\s+")
var x17: Array[java.lang.String] = x16
val x18 = x17
val x19 = x18.length
val x20 = new generated.scala.DoubleMatrixImpl(0,x19)
val x52 = while ({val x21 = x11
val x22 = x21 != null
x22}) {
val x24 = x17
val x25 = x24.length
val x26 = new generated.scala.IndexVectorRangeImpl(0,x25)
val x28 = x26.length
val x29 = x26.isRow
// a *thin* loop follows: x37
var x27 = 0
val x33 = x17
val x32 = x26.dcApply(x27)
val x34 = x33(x32)
val x35 = java.lang.Double.parseDouble(x34)
val x37 = {
val x30 = new generated.scala.DoubleVectorImpl(x28,x29)
x30
}
x37.dcUpdate(x27, x35)
x27 = 1
while (x27 < x28) {  // begin fat loop x37
val x33 = x17
val x32 = x26.dcApply(x27)
val x34 = x33(x32)
val x35 = java.lang.Double.parseDouble(x34)
x37.dcUpdate(x27, x35)
x27 += 1
} // end fat loop x37
val x38 = x20.numRows
val x39 = x20.insertRow(x38,x37)
val x40 = x9.readLine()
x11 = x40
val x42 = x11
val x43 = x42 != null
val x50 = {
def x50thenb(): Unit = {
val x44 = x42.trim()
x11 = x44
val x46 = x11
val x47 = x46.split("\\s+")
x17 = x47
()
}
if (x43) {
x50thenb()
}
}
x50
}
val x53 = x9.close()
val x54 = x20// unsafe immutable
x54
}
val x57 = x0(1)
val x80 = { 
val x58 = new generated.scala.DoubleVectorImpl(0,true)
val x59 = new java.io.FileReader(x57)
val x60 = new java.io.BufferedReader(x59)
val x61 = x60.readLine()
var x62: java.lang.String = x61
val x76 = while ({val x63 = x62
val x64 = x63 != null
x64}) {
val x66 = x62
val x67 = x66.trim()
x62 = x67
val x69 = x62
val x71 = x58.length
val x70 = java.lang.Double.parseDouble(x69)
val x72 = x58.insert(x71, x70)
val x73 = x60.readLine()
x62 = x73
()
}
val x77 = x60.close()
val x78 = x58// unsafe immutable
x78
}
val x82 = x80.length
val x83 = x80.isRow
// a *thin* loop follows: x89
var x81 = 0
val x86 = x80.dcApply(x81)
val x87 = x86 <= 0.0
val x88 = {
def x88thenb(): Boolean = {
false
}
def x88elseb(): Boolean = {
true
}
if (x87) {
x88thenb()
} else { 
x88elseb()
}
}
val x89 = {
val x84 = new generated.scala.BooleanVectorImpl(x82,x83)
x84
}
x89.dcUpdate(x81, x88)
x81 = 1
while (x81 < x82) {  // begin fat loop x89
val x86 = x80.dcApply(x81)
val x87 = x86 <= 0.0
val x88 = {
def x88thenb(): Boolean = {
false
}
def x88elseb(): Boolean = {
true
}
if (x87) {
x88thenb()
} else { 
x88elseb()
}
}
x89.dcUpdate(x81, x88)
x81 += 1
} // end fat loop x89
val x90 = Seq()
val x91 = ppl.delite.runtime.profiler.PerformanceTimer.start("app", false)
val x97 = x82 - 0
val x98 = generated.scala.EmptyVectorDoubleImpl
val x99 = x98.asInstanceOf[generated.scala.Vector[Double]]
val x100 = (0.0,0.0,x99,x99)
val x96 = new generated.scala.IndexVectorRangeImpl(0,x82)
val x92 = x56.numCols
val x105 = new generated.scala.ZeroVectorDoubleImpl(x92, true)
val x106 = x105.asInstanceOf[generated.scala.Vector[Double]]
var x93 = 0
val x101 = x96.dcApply(x93)
val x102 = x89(x101)
val x103 = x102==false
val x109 = {
def x109thenb(): scala.Tuple4[Double, Double, generated.scala.MatrixRow[Double], generated.scala.Vector[Double]] = {
val x104 = x56.getRow(x101)
val x107 = (1.0,0.0,x104,x106)
x107
}
def x109elseb(): scala.Tuple4[Double, Double, generated.scala.Vector[Double], generated.scala.MatrixRow[Double]] = {
val x104 = x56.getRow(x101)
val x108 = (0.0,1.0,x106,x104)
x108
}
if (x103) {
x109thenb()
} else { 
x109elseb()
}
}
var x152: scala.Tuple4[Double, Double, generated.scala.Vector[Double], generated.scala.Vector[Double]] = {
if (x97 == 0) {x100}
else {
val x101 = x96.dcApply(x93)
val x102 = x89(x101)
val x103 = x102==false
val x109 = {
def x109thenb(): scala.Tuple4[Double, Double, generated.scala.MatrixRow[Double], generated.scala.Vector[Double]] = {
val x104 = x56.getRow(x101)
val x107 = (1.0,0.0,x104,x106)
x107
}
def x109elseb(): scala.Tuple4[Double, Double, generated.scala.Vector[Double], generated.scala.MatrixRow[Double]] = {
val x104 = x56.getRow(x101)
val x108 = (0.0,1.0,x106,x104)
x108
}
if (x103) {
x109thenb()
} else { 
x109elseb()
}
}
x109
}
}
x93 = 1
while (x93 < x97) {  // begin fat loop x152
val x101 = x96.dcApply(x93)
val x102 = x89(x101)
val x103 = x102==false
val x109 = {
def x109thenb(): scala.Tuple4[Double, Double, generated.scala.MatrixRow[Double], generated.scala.Vector[Double]] = {
val x104 = x56.getRow(x101)
val x107 = (1.0,0.0,x104,x106)
x107
}
def x109elseb(): scala.Tuple4[Double, Double, generated.scala.Vector[Double], generated.scala.MatrixRow[Double]] = {
val x104 = x56.getRow(x101)
val x108 = (0.0,1.0,x106,x104)
x108
}
if (x103) {
x109thenb()
} else { 
x109elseb()
}
}
val x94 = x152
val x95 = x109
val x112 = x94._3
val x120 = x112.isInstanceOf[generated.scala.ZeroVector[Double]]
val x124 = x112.isRow
val x134 = {
def x134thenb(): generated.scala.Vector[Double] = {
val x116 = x95._3
x116
}
def x134elseb(): generated.scala.Vector[Double] = {
val x116 = x95._3
val x121 = x116.isInstanceOf[generated.scala.ZeroVector[Double]]
val x132 = {
def x132thenb(): generated.scala.Vector[Double] = {
x112
}
def x132elseb(): generated.scala.Vector[Double] = {
val x123 = x112.length
// a *thin* loop follows: x130
var x122 = 0
val x127 = x112.dcApply(x122)
val x128 = x116.dcApply(x122)
val x129 = x127 + x128
val x130 = {
val x125 = new generated.scala.DoubleVectorImpl(x123,x124)
x125
}
x130.dcUpdate(x122, x129)
x122 = 1
while (x122 < x123) {  // begin fat loop x130
val x127 = x112.dcApply(x122)
val x128 = x116.dcApply(x122)
val x129 = x127 + x128
x130.dcUpdate(x122, x129)
x122 += 1
} // end fat loop x130
x130
}
if (x121) {
x132thenb()
} else { 
x132elseb()
}
}
x132
}
if (x120) {
x134thenb()
} else { 
x134elseb()
}
}
val x113 = x94._4
val x135 = x113.isInstanceOf[generated.scala.ZeroVector[Double]]
val x139 = x113.isRow
val x149 = {
def x149thenb(): generated.scala.Vector[Double] = {
val x117 = x95._4
x117
}
def x149elseb(): generated.scala.Vector[Double] = {
val x117 = x95._4
val x136 = x117.isInstanceOf[generated.scala.ZeroVector[Double]]
val x147 = {
def x147thenb(): generated.scala.Vector[Double] = {
x113
}
def x147elseb(): generated.scala.Vector[Double] = {
val x138 = x113.length
// a *thin* loop follows: x145
var x137 = 0
val x142 = x113.dcApply(x137)
val x143 = x117.dcApply(x137)
val x144 = x142 + x143
val x145 = {
val x140 = new generated.scala.DoubleVectorImpl(x138,x139)
x140
}
x145.dcUpdate(x137, x144)
x137 = 1
while (x137 < x138) {  // begin fat loop x145
val x142 = x113.dcApply(x137)
val x143 = x117.dcApply(x137)
val x144 = x142 + x143
x145.dcUpdate(x137, x144)
x137 += 1
} // end fat loop x145
x145
}
if (x136) {
x147thenb()
} else { 
x147elseb()
}
}
x147
}
if (x135) {
x149thenb()
} else { 
x149elseb()
}
}
val x110 = x94._1
val x114 = x95._1
val x118 = x110 + x114
val x111 = x94._2
val x115 = x95._2
val x119 = x111 + x115
val x150 = (x118,x119,x134,x149)
x152 = x150
x93 += 1
} // end fat loop x152
val x155 = x152._3
val x161 = x155.length
val x162 = x155.isRow
val x153 = x152._1
// a *thin* loop follows: x167
var x160 = 0
val x165 = x155.dcApply(x160)
val x166 = x165 / x153
val x167 = {
val x163 = new generated.scala.DoubleVectorImpl(x161,x162)
x163
}
x167.dcUpdate(x160, x166)
x160 = 1
while (x160 < x161) {  // begin fat loop x167
val x165 = x155.dcApply(x160)
val x166 = x165 / x153
x167.dcUpdate(x160, x166)
x160 += 1
} // end fat loop x167
val x156 = x152._4
val x169 = x156.length
val x170 = x156.isRow
val x154 = x152._2
// a *thin* loop follows: x175
var x168 = 0
val x173 = x156.dcApply(x168)
val x174 = x173 / x154
val x175 = {
val x171 = new generated.scala.DoubleVectorImpl(x169,x170)
x171
}
x175.dcUpdate(x168, x174)
x168 = 1
while (x168 < x169) {  // begin fat loop x175
val x173 = x156.dcApply(x168)
val x174 = x173 / x154
x175.dcUpdate(x168, x174)
x168 += 1
} // end fat loop x175
val x179 = new generated.scala.DoubleMatrixImpl(0,0)
// a *thin* loop follows: x277
var x176 = 0
val x180 = x96.dcApply(x176)
val x181 = x89(x180)
val x182 = x181==false
val x187 = x180 * x92
val x266 = {
def x266thenb(): generated.scala.Matrix[Double] = {
// a *thin* loop follows: x193
var x184 = 0
val x188 = x187 + x184
val x189 = x56.dcApply(x188)
val x190 = { 
x189
}
val x191 = x167.dcApply(x184)
val x192 = x190 - x191
val x193 = {
val x185 = new generated.scala.DoubleVectorImpl(x92,true)
x185
}
x193.dcUpdate(x184, x192)
x184 = 1
while (x184 < x92) {  // begin fat loop x193
val x188 = x187 + x184
val x189 = x56.dcApply(x188)
val x190 = { 
x189
}
val x191 = x167.dcApply(x184)
val x192 = x190 - x191
x193.dcUpdate(x184, x192)
x184 += 1
} // end fat loop x193
val x195 = x193.isRow
val x196 = !x195
// a *thin* loop follows: x200
var x194 = 0
val x199 = x193.dcApply(x194)
val x200 = {
val x197 = new generated.scala.DoubleVectorImpl(x92,x196)
x197
}
x200.dcUpdate(x194, x199)
x194 = 1
while (x194 < x92) {  // begin fat loop x200
val x199 = x193.dcApply(x194)
x200.dcUpdate(x194, x199)
x194 += 1
} // end fat loop x200
// a *thin* loop follows: x209
var x201 = 0
val x204 = x187 + x201
val x205 = x56.dcApply(x204)
val x206 = { 
x205
}
val x207 = x167.dcApply(x201)
val x208 = x206 - x207
val x209 = {
val x202 = new generated.scala.DoubleVectorImpl(x92,true)
x202
}
x209.dcUpdate(x201, x208)
x201 = 1
while (x201 < x92) {  // begin fat loop x209
val x204 = x187 + x201
val x205 = x56.dcApply(x204)
val x206 = { 
x205
}
val x207 = x167.dcApply(x201)
val x208 = x206 - x207
x209.dcUpdate(x201, x208)
x201 += 1
} // end fat loop x209
val x224 = { 
val x210 = new generated.scala.DoubleMatrixImpl(x92,x92)
var x212 : Int = 0
val x221 = while (x212 < x92) {
val x214 = x200(x212)
var x213 : Int = 0
val x219 = while (x213 < x92) {
val x215 = x209(x213)
val x216 = x214 * x215
val x217 = x210(x212, x213) = x216
x217
x213 = x213 + 1
}
x219
x212 = x212 + 1
}
val x222 = x210// unsafe immutable
x222
}
x224
}
def x266elseb(): generated.scala.Matrix[Double] = {
// a *thin* loop follows: x234
var x226 = 0
val x229 = x187 + x226
val x230 = x56.dcApply(x229)
val x231 = { 
x230
}
val x232 = x175.dcApply(x226)
val x233 = x231 - x232
val x234 = {
val x227 = new generated.scala.DoubleVectorImpl(x92,true)
x227
}
x234.dcUpdate(x226, x233)
x226 = 1
while (x226 < x92) {  // begin fat loop x234
val x229 = x187 + x226
val x230 = x56.dcApply(x229)
val x231 = { 
x230
}
val x232 = x175.dcApply(x226)
val x233 = x231 - x232
x234.dcUpdate(x226, x233)
x226 += 1
} // end fat loop x234
val x236 = x234.isRow
val x237 = !x236
// a *thin* loop follows: x241
var x235 = 0
val x240 = x234.dcApply(x235)
val x241 = {
val x238 = new generated.scala.DoubleVectorImpl(x92,x237)
x238
}
x241.dcUpdate(x235, x240)
x235 = 1
while (x235 < x92) {  // begin fat loop x241
val x240 = x234.dcApply(x235)
x241.dcUpdate(x235, x240)
x235 += 1
} // end fat loop x241
// a *thin* loop follows: x250
var x242 = 0
val x245 = x187 + x242
val x246 = x56.dcApply(x245)
val x247 = { 
x246
}
val x248 = x175.dcApply(x242)
val x249 = x247 - x248
val x250 = {
val x243 = new generated.scala.DoubleVectorImpl(x92,true)
x243
}
x250.dcUpdate(x242, x249)
x242 = 1
while (x242 < x92) {  // begin fat loop x250
val x245 = x187 + x242
val x246 = x56.dcApply(x245)
val x247 = { 
x246
}
val x248 = x175.dcApply(x242)
val x249 = x247 - x248
x250.dcUpdate(x242, x249)
x242 += 1
} // end fat loop x250
val x264 = { 
val x251 = new generated.scala.DoubleMatrixImpl(x92,x92)
var x252 : Int = 0
val x261 = while (x252 < x92) {
val x254 = x241(x252)
var x253 : Int = 0
val x259 = while (x253 < x92) {
val x255 = x250(x253)
val x256 = x254 * x255
val x257 = x251(x252, x253) = x256
x257
x253 = x253 + 1
}
x259
x252 = x252 + 1
}
val x262 = x251// unsafe immutable
x262
}
x264
}
if (x182) {
x266thenb()
} else { 
x266elseb()
}
}
var x277: generated.scala.Matrix[Double] = {
if (x97 == 0) {x179}
else {
val x180 = x96.dcApply(x176)
val x181 = x89(x180)
val x182 = x181==false
val x187 = x180 * x92
val x266 = {
def x266thenb(): generated.scala.Matrix[Double] = {
// a *thin* loop follows: x193
var x184 = 0
val x188 = x187 + x184
val x189 = x56.dcApply(x188)
val x190 = { 
x189
}
val x191 = x167.dcApply(x184)
val x192 = x190 - x191
val x193 = {
val x185 = new generated.scala.DoubleVectorImpl(x92,true)
x185
}
x193.dcUpdate(x184, x192)
x184 = 1
while (x184 < x92) {  // begin fat loop x193
val x188 = x187 + x184
val x189 = x56.dcApply(x188)
val x190 = { 
x189
}
val x191 = x167.dcApply(x184)
val x192 = x190 - x191
x193.dcUpdate(x184, x192)
x184 += 1
} // end fat loop x193
val x195 = x193.isRow
val x196 = !x195
// a *thin* loop follows: x200
var x194 = 0
val x199 = x193.dcApply(x194)
val x200 = {
val x197 = new generated.scala.DoubleVectorImpl(x92,x196)
x197
}
x200.dcUpdate(x194, x199)
x194 = 1
while (x194 < x92) {  // begin fat loop x200
val x199 = x193.dcApply(x194)
x200.dcUpdate(x194, x199)
x194 += 1
} // end fat loop x200
// a *thin* loop follows: x209
var x201 = 0
val x204 = x187 + x201
val x205 = x56.dcApply(x204)
val x206 = { 
x205
}
val x207 = x167.dcApply(x201)
val x208 = x206 - x207
val x209 = {
val x202 = new generated.scala.DoubleVectorImpl(x92,true)
x202
}
x209.dcUpdate(x201, x208)
x201 = 1
while (x201 < x92) {  // begin fat loop x209
val x204 = x187 + x201
val x205 = x56.dcApply(x204)
val x206 = { 
x205
}
val x207 = x167.dcApply(x201)
val x208 = x206 - x207
x209.dcUpdate(x201, x208)
x201 += 1
} // end fat loop x209
val x224 = { 
val x210 = new generated.scala.DoubleMatrixImpl(x92,x92)
var x212 : Int = 0
val x221 = while (x212 < x92) {
val x214 = x200(x212)
var x213 : Int = 0
val x219 = while (x213 < x92) {
val x215 = x209(x213)
val x216 = x214 * x215
val x217 = x210(x212, x213) = x216
x217
x213 = x213 + 1
}
x219
x212 = x212 + 1
}
val x222 = x210// unsafe immutable
x222
}
x224
}
def x266elseb(): generated.scala.Matrix[Double] = {
// a *thin* loop follows: x234
var x226 = 0
val x229 = x187 + x226
val x230 = x56.dcApply(x229)
val x231 = { 
x230
}
val x232 = x175.dcApply(x226)
val x233 = x231 - x232
val x234 = {
val x227 = new generated.scala.DoubleVectorImpl(x92,true)
x227
}
x234.dcUpdate(x226, x233)
x226 = 1
while (x226 < x92) {  // begin fat loop x234
val x229 = x187 + x226
val x230 = x56.dcApply(x229)
val x231 = { 
x230
}
val x232 = x175.dcApply(x226)
val x233 = x231 - x232
x234.dcUpdate(x226, x233)
x226 += 1
} // end fat loop x234
val x236 = x234.isRow
val x237 = !x236
// a *thin* loop follows: x241
var x235 = 0
val x240 = x234.dcApply(x235)
val x241 = {
val x238 = new generated.scala.DoubleVectorImpl(x92,x237)
x238
}
x241.dcUpdate(x235, x240)
x235 = 1
while (x235 < x92) {  // begin fat loop x241
val x240 = x234.dcApply(x235)
x241.dcUpdate(x235, x240)
x235 += 1
} // end fat loop x241
// a *thin* loop follows: x250
var x242 = 0
val x245 = x187 + x242
val x246 = x56.dcApply(x245)
val x247 = { 
x246
}
val x248 = x175.dcApply(x242)
val x249 = x247 - x248
val x250 = {
val x243 = new generated.scala.DoubleVectorImpl(x92,true)
x243
}
x250.dcUpdate(x242, x249)
x242 = 1
while (x242 < x92) {  // begin fat loop x250
val x245 = x187 + x242
val x246 = x56.dcApply(x245)
val x247 = { 
x246
}
val x248 = x175.dcApply(x242)
val x249 = x247 - x248
x250.dcUpdate(x242, x249)
x242 += 1
} // end fat loop x250
val x264 = { 
val x251 = new generated.scala.DoubleMatrixImpl(x92,x92)
var x252 : Int = 0
val x261 = while (x252 < x92) {
val x254 = x241(x252)
var x253 : Int = 0
val x259 = while (x253 < x92) {
val x255 = x250(x253)
val x256 = x254 * x255
val x257 = x251(x252, x253) = x256
x257
x253 = x253 + 1
}
x259
x252 = x252 + 1
}
val x262 = x251// unsafe immutable
x262
}
x264
}
if (x182) {
x266thenb()
} else { 
x266elseb()
}
}
x266
}
}
x176 = 1
while (x176 < x97) {  // begin fat loop x277
val x180 = x96.dcApply(x176)
val x181 = x89(x180)
val x182 = x181==false
val x187 = x180 * x92
val x266 = {
def x266thenb(): generated.scala.Matrix[Double] = {
// a *thin* loop follows: x193
var x184 = 0
val x188 = x187 + x184
val x189 = x56.dcApply(x188)
val x190 = { 
x189
}
val x191 = x167.dcApply(x184)
val x192 = x190 - x191
val x193 = {
val x185 = new generated.scala.DoubleVectorImpl(x92,true)
x185
}
x193.dcUpdate(x184, x192)
x184 = 1
while (x184 < x92) {  // begin fat loop x193
val x188 = x187 + x184
val x189 = x56.dcApply(x188)
val x190 = { 
x189
}
val x191 = x167.dcApply(x184)
val x192 = x190 - x191
x193.dcUpdate(x184, x192)
x184 += 1
} // end fat loop x193
val x195 = x193.isRow
val x196 = !x195
// a *thin* loop follows: x200
var x194 = 0
val x199 = x193.dcApply(x194)
val x200 = {
val x197 = new generated.scala.DoubleVectorImpl(x92,x196)
x197
}
x200.dcUpdate(x194, x199)
x194 = 1
while (x194 < x92) {  // begin fat loop x200
val x199 = x193.dcApply(x194)
x200.dcUpdate(x194, x199)
x194 += 1
} // end fat loop x200
// a *thin* loop follows: x209
var x201 = 0
val x204 = x187 + x201
val x205 = x56.dcApply(x204)
val x206 = { 
x205
}
val x207 = x167.dcApply(x201)
val x208 = x206 - x207
val x209 = {
val x202 = new generated.scala.DoubleVectorImpl(x92,true)
x202
}
x209.dcUpdate(x201, x208)
x201 = 1
while (x201 < x92) {  // begin fat loop x209
val x204 = x187 + x201
val x205 = x56.dcApply(x204)
val x206 = { 
x205
}
val x207 = x167.dcApply(x201)
val x208 = x206 - x207
x209.dcUpdate(x201, x208)
x201 += 1
} // end fat loop x209
val x224 = { 
val x210 = new generated.scala.DoubleMatrixImpl(x92,x92)
var x212 : Int = 0
val x221 = while (x212 < x92) {
val x214 = x200(x212)
var x213 : Int = 0
val x219 = while (x213 < x92) {
val x215 = x209(x213)
val x216 = x214 * x215
val x217 = x210(x212, x213) = x216
x217
x213 = x213 + 1
}
x219
x212 = x212 + 1
}
val x222 = x210// unsafe immutable
x222
}
x224
}
def x266elseb(): generated.scala.Matrix[Double] = {
// a *thin* loop follows: x234
var x226 = 0
val x229 = x187 + x226
val x230 = x56.dcApply(x229)
val x231 = { 
x230
}
val x232 = x175.dcApply(x226)
val x233 = x231 - x232
val x234 = {
val x227 = new generated.scala.DoubleVectorImpl(x92,true)
x227
}
x234.dcUpdate(x226, x233)
x226 = 1
while (x226 < x92) {  // begin fat loop x234
val x229 = x187 + x226
val x230 = x56.dcApply(x229)
val x231 = { 
x230
}
val x232 = x175.dcApply(x226)
val x233 = x231 - x232
x234.dcUpdate(x226, x233)
x226 += 1
} // end fat loop x234
val x236 = x234.isRow
val x237 = !x236
// a *thin* loop follows: x241
var x235 = 0
val x240 = x234.dcApply(x235)
val x241 = {
val x238 = new generated.scala.DoubleVectorImpl(x92,x237)
x238
}
x241.dcUpdate(x235, x240)
x235 = 1
while (x235 < x92) {  // begin fat loop x241
val x240 = x234.dcApply(x235)
x241.dcUpdate(x235, x240)
x235 += 1
} // end fat loop x241
// a *thin* loop follows: x250
var x242 = 0
val x245 = x187 + x242
val x246 = x56.dcApply(x245)
val x247 = { 
x246
}
val x248 = x175.dcApply(x242)
val x249 = x247 - x248
val x250 = {
val x243 = new generated.scala.DoubleVectorImpl(x92,true)
x243
}
x250.dcUpdate(x242, x249)
x242 = 1
while (x242 < x92) {  // begin fat loop x250
val x245 = x187 + x242
val x246 = x56.dcApply(x245)
val x247 = { 
x246
}
val x248 = x175.dcApply(x242)
val x249 = x247 - x248
x250.dcUpdate(x242, x249)
x242 += 1
} // end fat loop x250
val x264 = { 
val x251 = new generated.scala.DoubleMatrixImpl(x92,x92)
var x252 : Int = 0
val x261 = while (x252 < x92) {
val x254 = x241(x252)
var x253 : Int = 0
val x259 = while (x253 < x92) {
val x255 = x250(x253)
val x256 = x254 * x255
val x257 = x251(x252, x253) = x256
x257
x253 = x253 + 1
}
x259
x252 = x252 + 1
}
val x262 = x251// unsafe immutable
x262
}
x264
}
if (x182) {
x266thenb()
} else { 
x266elseb()
}
}
val x177 = x277
val x178 = x266
val x269 = x177.numRows
val x270 = x177.numCols
val x271 = x269 * x270
// a *thin* loop follows: x275
var x268 = 0
val x272 = x177.dcApply(x268)
val x273 = x178.dcApply(x268)
val x274 = x272 + x273
val x275 = {
x177
}
x275.dcUpdate(x268, x274)
x268 = 1
while (x268 < x271) {  // begin fat loop x275
val x272 = x177.dcApply(x268)
val x273 = x178.dcApply(x268)
val x274 = x272 + x273
x275.dcUpdate(x268, x274)
x268 += 1
} // end fat loop x275
x277 = x275
x176 += 1
} // end fat loop x277
val x278 = Seq(x277)
val x279 = ppl.delite.runtime.profiler.PerformanceTimer.stop("app", false)
val x280 = println(x277)
()
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
