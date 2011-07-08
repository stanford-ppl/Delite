/*****************************************
  Emitting Generated Code                  
*******************************************/
class Application extends ((Array[java.lang.String])=>(Unit)) {
def apply(x0:Array[java.lang.String]): Unit = {
val x1 = x0.length
val x2 = x1 < 1
val x6 = {
def x6thenb(): Nothing = {
val x3 = println("Usage: kmeans <input data file> <initmu data file>")
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
val x57 = new generated.scala.IntVectorImpl(0,false)
val x58 = x57// unsafe immutable
val x59 = new generated.scala.IntLabelsImpl(x58)
val x60 = new generated.scala.DoubleIntTrainingSetImpl(x56,x59)
val x61 = x0(1)
val x110 = { 
val x62 = new java.io.FileReader(x61)
val x63 = new java.io.BufferedReader(x62)
val x64 = x63.readLine()
var x65: java.lang.String = x64
val x66 = x65
val x67 = x66.trim()
x65 = x67
val x69 = x65
val x70 = x69.split("\\s+")
var x71: Array[java.lang.String] = x70
val x72 = x71
val x73 = x72.length
val x74 = new generated.scala.DoubleMatrixImpl(0,x73)
val x106 = while ({val x75 = x65
val x76 = x75 != null
x76}) {
val x78 = x71
val x79 = x78.length
val x80 = new generated.scala.IndexVectorRangeImpl(0,x79)
val x82 = x80.length
val x83 = x80.isRow
// a *thin* loop follows: x91
var x81 = 0
val x87 = x71
val x86 = x80.dcApply(x81)
val x88 = x87(x86)
val x89 = java.lang.Double.parseDouble(x88)
val x91 = {
val x84 = new generated.scala.DoubleVectorImpl(x82,x83)
x84
}
x91.dcUpdate(x81, x89)
x81 = 1
while (x81 < x82) {  // begin fat loop x91
val x87 = x71
val x86 = x80.dcApply(x81)
val x88 = x87(x86)
val x89 = java.lang.Double.parseDouble(x88)
x91.dcUpdate(x81, x89)
x81 += 1
} // end fat loop x91
val x92 = x74.numRows
val x93 = x74.insertRow(x92,x91)
val x94 = x63.readLine()
x65 = x94
val x96 = x65
val x97 = x96 != null
val x104 = {
def x104thenb(): Unit = {
val x98 = x96.trim()
x65 = x98
val x100 = x65
val x101 = x100.split("\\s+")
x71 = x101
()
}
if (x97) {
x104thenb()
}
}
x104
}
val x107 = x63.close()
val x108 = x74// unsafe immutable
x108
}
val x111 = Seq()
val x112 = ppl.delite.runtime.profiler.PerformanceTimer.start("app", false)
var x115: Int = 0
var x116: Double = 1.7976931348623157E308
val x117 = null.asInstanceOf[generated.scala.Matrix[Double]]
var x118: generated.scala.Matrix[Double] = x117
var x119: generated.scala.Matrix[Double] = x110
var x120: Int = 0
val x113 = x60.numRows
val x131 = new generated.scala.IndexVectorRangeImpl(0,x113)
val x133 = x131.length
val x134 = x131.isRow
val x114 = x60.numCols
val x188 = new generated.scala.IndexVectorRangeImpl(0,16)
val x189 = generated.scala.IndexVectorWCImpl
val x190 = new generated.scala.IndexVector2Impl(x188,x189)
val x191 = x190.rowInd
val x192 = x191.isInstanceOf[generated.scala.IndexVector]
val x193 = x190.colInd
val x194 = x193.isInstanceOf[generated.scala.IndexVectorWC]
val x195 = x192 && x194
val x196 = x191.length
val x197 = x196 > 0
val x202 = x113 - 0
val x203 = generated.scala.EmptyVectorDoubleImpl
val x204 = x203.asInstanceOf[generated.scala.Vector[Double]]
val x205 = (x204,0.0)
val x198 = x191(0)
val x211 = new generated.scala.ZeroVectorDoubleImpl(x114, true)
val x212 = x211.asInstanceOf[generated.scala.Vector[Double]]
val x213 = (x212,0.0)
val x259 = x196 - 1
val x260 = x191.isRow
val x271 = { 
val x261 = new generated.scala.IntVectorImpl(x259,x260)
var x263 : Int = 1
val x268 = while (x263 < x196) {
val x264 = x263 - 1
val x265 = x191(x263)
val x266 = x261(x264) = x265
x266
x263 = x263 + 1
}
val x269 = x261// unsafe immutable
x269
}
val x327 = List()
val x375 = while ({val x121 = x116
val x124 = x120
val x122 = Math.abs(x121)
val x123 = x122 > 0.0010
val x125 = x124 < 1000
val x126 = x123 && x125
x126}) {
val x128 = x119
x118 = x128
val x130 = x115 += 1
val x140 = x128.numRows
val x141 = new generated.scala.IndexVectorRangeImpl(0,x140)
val x153 = x128.numCols
val x179 = x141.length
// a *thin* loop follows: x187
var x132 = 0
val x137 = x131.dcApply(x132)
val x149 = x137 * x114
// a *thin* loop follows: x175
var x139 = 0
val x144 = x141.dcApply(x139)
val x154 = x144 * x153
val x174 = { 
// a *thin* loop follows: x159
var x146 = 0
val x150 = x149 + x146
val x151 = x60.dcApply(x150)
val x152 = { 
x151
}
val x155 = x154 + x146
val x156 = x128.dcApply(x155)
val x157 = { 
x156
}
val x158 = x152 - x157
val x159 = {
val x147 = new generated.scala.DoubleVectorImpl(x114,true)
x147
}
x159.dcUpdate(x146, x158)
x146 = 1
while (x146 < x114) {  // begin fat loop x159
val x150 = x149 + x146
val x151 = x60.dcApply(x150)
val x152 = { 
x151
}
val x155 = x154 + x146
val x156 = x128.dcApply(x155)
val x157 = { 
x156
}
val x158 = x152 - x157
x159.dcUpdate(x146, x158)
x146 += 1
} // end fat loop x159
val x161 = x159.isRow
// a *thin* loop follows: x166
var x160 = 0
val x164 = x159.dcApply(x160)
val x165 = x164 * x164
val x166 = {
val x162 = new generated.scala.DoubleVectorImpl(x114,x161)
x162
}
x166.dcUpdate(x160, x165)
x160 = 1
while (x160 < x114) {  // begin fat loop x166
val x164 = x159.dcApply(x160)
val x165 = x164 * x164
x166.dcUpdate(x160, x165)
x160 += 1
} // end fat loop x166
var x167 = 0
val x170 = x166.dcApply(x167)
var x172: Double = {
if (x114 == 0) {0.0}
else {
val x170 = x166.dcApply(x167)
x170
}
}
x167 = 1
while (x167 < x114) {  // begin fat loop x172
val x170 = x166.dcApply(x167)
val x168 = x172
val x169 = x170
val x171 = x168 + x169
x172 = x171
x167 += 1
} // end fat loop x172
x172
}
val x175 = {
val x142 = new generated.scala.DoubleVectorImpl(x140,false)
x142
}
x175.dcUpdate(x139, x174)
x139 = 1
while (x139 < x140) {  // begin fat loop x175
val x144 = x141.dcApply(x139)
val x154 = x144 * x153
val x174 = { 
// a *thin* loop follows: x159
var x146 = 0
val x150 = x149 + x146
val x151 = x60.dcApply(x150)
val x152 = { 
x151
}
val x155 = x154 + x146
val x156 = x128.dcApply(x155)
val x157 = { 
x156
}
val x158 = x152 - x157
val x159 = {
val x147 = new generated.scala.DoubleVectorImpl(x114,true)
x147
}
x159.dcUpdate(x146, x158)
x146 = 1
while (x146 < x114) {  // begin fat loop x159
val x150 = x149 + x146
val x151 = x60.dcApply(x150)
val x152 = { 
x151
}
val x155 = x154 + x146
val x156 = x128.dcApply(x155)
val x157 = { 
x156
}
val x158 = x152 - x157
x159.dcUpdate(x146, x158)
x146 += 1
} // end fat loop x159
val x161 = x159.isRow
// a *thin* loop follows: x166
var x160 = 0
val x164 = x159.dcApply(x160)
val x165 = x164 * x164
val x166 = {
val x162 = new generated.scala.DoubleVectorImpl(x114,x161)
x162
}
x166.dcUpdate(x160, x165)
x160 = 1
while (x160 < x114) {  // begin fat loop x166
val x164 = x159.dcApply(x160)
val x165 = x164 * x164
x166.dcUpdate(x160, x165)
x160 += 1
} // end fat loop x166
var x167 = 0
val x170 = x166.dcApply(x167)
var x172: Double = {
if (x114 == 0) {0.0}
else {
val x170 = x166.dcApply(x167)
x170
}
}
x167 = 1
while (x167 < x114) {  // begin fat loop x172
val x170 = x166.dcApply(x167)
val x168 = x172
val x169 = x170
val x171 = x168 + x169
x172 = x171
x167 += 1
} // end fat loop x172
x172
}
x175.dcUpdate(x139, x174)
x139 += 1
} // end fat loop x175
var x176 = 0
val x180 = x141.dcApply(x176)
var x185: Int = {
if (x179 == 0) {0}
else {
val x180 = x141.dcApply(x176)
x180
}
}
x176 = 1
while (x176 < x179) {  // begin fat loop x185
val x180 = x141.dcApply(x176)
val x177 = x185
val x178 = x180
val x181 = x175(x177)
val x182 = x175(x178)
val x183 = x181 < x182
val x184 = {
def x184thenb(): Int = {
x177
}
def x184elseb(): Int = {
x178
}
if (x183) {
x184thenb()
} else { 
x184elseb()
}
}
x185 = x184
x176 += 1
} // end fat loop x185
val x187 = {
val x135 = new generated.scala.IntVectorImpl(x133,x134)
x135
}
x187.dcUpdate(x132, x185)
x132 = 1
while (x132 < x133) {  // begin fat loop x187
val x137 = x131.dcApply(x132)
val x149 = x137 * x114
// a *thin* loop follows: x175
var x139 = 0
val x144 = x141.dcApply(x139)
val x154 = x144 * x153
val x174 = { 
// a *thin* loop follows: x159
var x146 = 0
val x150 = x149 + x146
val x151 = x60.dcApply(x150)
val x152 = { 
x151
}
val x155 = x154 + x146
val x156 = x128.dcApply(x155)
val x157 = { 
x156
}
val x158 = x152 - x157
val x159 = {
val x147 = new generated.scala.DoubleVectorImpl(x114,true)
x147
}
x159.dcUpdate(x146, x158)
x146 = 1
while (x146 < x114) {  // begin fat loop x159
val x150 = x149 + x146
val x151 = x60.dcApply(x150)
val x152 = { 
x151
}
val x155 = x154 + x146
val x156 = x128.dcApply(x155)
val x157 = { 
x156
}
val x158 = x152 - x157
x159.dcUpdate(x146, x158)
x146 += 1
} // end fat loop x159
val x161 = x159.isRow
// a *thin* loop follows: x166
var x160 = 0
val x164 = x159.dcApply(x160)
val x165 = x164 * x164
val x166 = {
val x162 = new generated.scala.DoubleVectorImpl(x114,x161)
x162
}
x166.dcUpdate(x160, x165)
x160 = 1
while (x160 < x114) {  // begin fat loop x166
val x164 = x159.dcApply(x160)
val x165 = x164 * x164
x166.dcUpdate(x160, x165)
x160 += 1
} // end fat loop x166
var x167 = 0
val x170 = x166.dcApply(x167)
var x172: Double = {
if (x114 == 0) {0.0}
else {
val x170 = x166.dcApply(x167)
x170
}
}
x167 = 1
while (x167 < x114) {  // begin fat loop x172
val x170 = x166.dcApply(x167)
val x168 = x172
val x169 = x170
val x171 = x168 + x169
x172 = x171
x167 += 1
} // end fat loop x172
x172
}
val x175 = {
val x142 = new generated.scala.DoubleVectorImpl(x140,false)
x142
}
x175.dcUpdate(x139, x174)
x139 = 1
while (x139 < x140) {  // begin fat loop x175
val x144 = x141.dcApply(x139)
val x154 = x144 * x153
val x174 = { 
// a *thin* loop follows: x159
var x146 = 0
val x150 = x149 + x146
val x151 = x60.dcApply(x150)
val x152 = { 
x151
}
val x155 = x154 + x146
val x156 = x128.dcApply(x155)
val x157 = { 
x156
}
val x158 = x152 - x157
val x159 = {
val x147 = new generated.scala.DoubleVectorImpl(x114,true)
x147
}
x159.dcUpdate(x146, x158)
x146 = 1
while (x146 < x114) {  // begin fat loop x159
val x150 = x149 + x146
val x151 = x60.dcApply(x150)
val x152 = { 
x151
}
val x155 = x154 + x146
val x156 = x128.dcApply(x155)
val x157 = { 
x156
}
val x158 = x152 - x157
x159.dcUpdate(x146, x158)
x146 += 1
} // end fat loop x159
val x161 = x159.isRow
// a *thin* loop follows: x166
var x160 = 0
val x164 = x159.dcApply(x160)
val x165 = x164 * x164
val x166 = {
val x162 = new generated.scala.DoubleVectorImpl(x114,x161)
x162
}
x166.dcUpdate(x160, x165)
x160 = 1
while (x160 < x114) {  // begin fat loop x166
val x164 = x159.dcApply(x160)
val x165 = x164 * x164
x166.dcUpdate(x160, x165)
x160 += 1
} // end fat loop x166
var x167 = 0
val x170 = x166.dcApply(x167)
var x172: Double = {
if (x114 == 0) {0.0}
else {
val x170 = x166.dcApply(x167)
x170
}
}
x167 = 1
while (x167 < x114) {  // begin fat loop x172
val x170 = x166.dcApply(x167)
val x168 = x172
val x169 = x170
val x171 = x168 + x169
x172 = x171
x167 += 1
} // end fat loop x172
x172
}
x175.dcUpdate(x139, x174)
x139 += 1
} // end fat loop x175
var x176 = 0
val x180 = x141.dcApply(x176)
var x185: Int = {
if (x179 == 0) {0}
else {
val x180 = x141.dcApply(x176)
x180
}
}
x176 = 1
while (x176 < x179) {  // begin fat loop x185
val x180 = x141.dcApply(x176)
val x177 = x185
val x178 = x180
val x181 = x175(x177)
val x182 = x175(x178)
val x183 = x181 < x182
val x184 = {
def x184thenb(): Int = {
x177
}
def x184elseb(): Int = {
x178
}
if (x183) {
x184thenb()
} else { 
x184elseb()
}
}
x185 = x184
x176 += 1
} // end fat loop x185
x187.dcUpdate(x132, x185)
x132 += 1
} // end fat loop x187
var x199 = 0
val x206 = x131.dcApply(x199)
val x207 = x187(x206)
val x208 = x207==x198
val x214 = {
def x214thenb(): scala.Tuple2[generated.scala.MatrixRow[Double], Double] = {
val x209 = x60.getRow(x206)
val x210 = (x209,1.0)
x210
}
def x214elseb(): scala.Tuple2[generated.scala.Vector[Double], Double] = {
x213
}
if (x208) {
x214thenb()
} else { 
x214elseb()
}
}
var x237: scala.Tuple2[generated.scala.Vector[Double], Double] = {
if (x202 == 0) {x205}
else {
val x206 = x131.dcApply(x199)
val x207 = x187(x206)
val x208 = x207==x198
val x214 = {
def x214thenb(): scala.Tuple2[generated.scala.MatrixRow[Double], Double] = {
val x209 = x60.getRow(x206)
val x210 = (x209,1.0)
x210
}
def x214elseb(): scala.Tuple2[generated.scala.Vector[Double], Double] = {
x213
}
if (x208) {
x214thenb()
} else { 
x214elseb()
}
}
x214
}
}
x199 = 1
while (x199 < x202) {  // begin fat loop x237
val x206 = x131.dcApply(x199)
val x207 = x187(x206)
val x208 = x207==x198
val x214 = {
def x214thenb(): scala.Tuple2[generated.scala.MatrixRow[Double], Double] = {
val x209 = x60.getRow(x206)
val x210 = (x209,1.0)
x210
}
def x214elseb(): scala.Tuple2[generated.scala.Vector[Double], Double] = {
x213
}
if (x208) {
x214thenb()
} else { 
x214elseb()
}
}
val x200 = x237
val x201 = x214
val x215 = x200._1
val x219 = x215.isInstanceOf[generated.scala.ZeroVector[Double]]
val x223 = x215.isRow
val x233 = {
def x233thenb(): generated.scala.Vector[Double] = {
val x217 = x201._1
x217
}
def x233elseb(): generated.scala.Vector[Double] = {
val x217 = x201._1
val x220 = x217.isInstanceOf[generated.scala.ZeroVector[Double]]
val x231 = {
def x231thenb(): generated.scala.Vector[Double] = {
x215
}
def x231elseb(): generated.scala.Vector[Double] = {
val x222 = x215.length
// a *thin* loop follows: x229
var x221 = 0
val x226 = x215.dcApply(x221)
val x227 = x217.dcApply(x221)
val x228 = x226 + x227
val x229 = {
val x224 = new generated.scala.DoubleVectorImpl(x222,x223)
x224
}
x229.dcUpdate(x221, x228)
x221 = 1
while (x221 < x222) {  // begin fat loop x229
val x226 = x215.dcApply(x221)
val x227 = x217.dcApply(x221)
val x228 = x226 + x227
x229.dcUpdate(x221, x228)
x221 += 1
} // end fat loop x229
x229
}
if (x220) {
x231thenb()
} else { 
x231elseb()
}
}
x231
}
if (x219) {
x233thenb()
} else { 
x233elseb()
}
}
val x216 = x200._2
val x218 = x201._2
val x234 = x216 + x218
val x235 = (x233,x234)
x237 = x235
x199 += 1
} // end fat loop x237
val x238 = x237._1
val x243 = x238.isRow
val x338 = {
def x338thenb(): generated.scala.Matrix[Double] = {
val x333 = {
def x333thenb(): generated.scala.Matrix[Double] = {
val x239 = x237._2
val x240 = x239==0
val x250 = {
def x250thenb(): generated.scala.Vector[Double] = {
x238
}
def x250elseb(): generated.scala.Vector[Double] = {
val x242 = x238.length
// a *thin* loop follows: x248
var x241 = 0
val x246 = x238.dcApply(x241)
val x247 = x246 / x239
val x248 = {
val x244 = new generated.scala.DoubleVectorImpl(x242,x243)
x244
}
x248.dcUpdate(x241, x247)
x241 = 1
while (x241 < x242) {  // begin fat loop x248
val x246 = x238.dcApply(x241)
val x247 = x246 / x239
x248.dcUpdate(x241, x247)
x241 += 1
} // end fat loop x248
x248
}
if (x240) {
x250thenb()
} else { 
x250elseb()
}
}
val x251 = x250.length
val x252 = new generated.scala.DoubleMatrixImpl(x196,x251)
val x254 = x252.numCols
// a *thin* loop follows: x258
var x253 = 0
var x258: Unit = {
val x255 = x250(x253)
val x256 = x252(0, x253) = x255
x256
}
x253 = 1
while (x253 < x254) {  // begin fat loop x258
x258 = {
val x255 = x250(x253)
val x256 = x252(0, x253) = x255
x256
}
x253 += 1
} // end fat loop x258
// a *thin* loop follows: x328
var x272 = 0
var x328: Unit = {
val x274 = x271.dcApply(x272)
var x275 = 0
val x278 = x131.dcApply(x275)
val x279 = x187(x278)
val x280 = x279==x274
val x283 = {
def x283thenb(): scala.Tuple2[generated.scala.MatrixRow[Double], Double] = {
val x281 = x60.getRow(x278)
val x282 = (x281,1.0)
x282
}
def x283elseb(): scala.Tuple2[generated.scala.Vector[Double], Double] = {
x213
}
if (x280) {
x283thenb()
} else { 
x283elseb()
}
}
var x306: scala.Tuple2[generated.scala.Vector[Double], Double] = {
if (x202 == 0) {x205}
else {
val x278 = x131.dcApply(x275)
val x279 = x187(x278)
val x280 = x279==x274
val x283 = {
def x283thenb(): scala.Tuple2[generated.scala.MatrixRow[Double], Double] = {
val x281 = x60.getRow(x278)
val x282 = (x281,1.0)
x282
}
def x283elseb(): scala.Tuple2[generated.scala.Vector[Double], Double] = {
x213
}
if (x280) {
x283thenb()
} else { 
x283elseb()
}
}
x283
}
}
x275 = 1
while (x275 < x202) {  // begin fat loop x306
val x278 = x131.dcApply(x275)
val x279 = x187(x278)
val x280 = x279==x274
val x283 = {
def x283thenb(): scala.Tuple2[generated.scala.MatrixRow[Double], Double] = {
val x281 = x60.getRow(x278)
val x282 = (x281,1.0)
x282
}
def x283elseb(): scala.Tuple2[generated.scala.Vector[Double], Double] = {
x213
}
if (x280) {
x283thenb()
} else { 
x283elseb()
}
}
val x276 = x306
val x277 = x283
val x284 = x276._1
val x288 = x284.isInstanceOf[generated.scala.ZeroVector[Double]]
val x292 = x284.isRow
val x302 = {
def x302thenb(): generated.scala.Vector[Double] = {
val x286 = x277._1
x286
}
def x302elseb(): generated.scala.Vector[Double] = {
val x286 = x277._1
val x289 = x286.isInstanceOf[generated.scala.ZeroVector[Double]]
val x300 = {
def x300thenb(): generated.scala.Vector[Double] = {
x284
}
def x300elseb(): generated.scala.Vector[Double] = {
val x291 = x284.length
// a *thin* loop follows: x298
var x290 = 0
val x295 = x284.dcApply(x290)
val x296 = x286.dcApply(x290)
val x297 = x295 + x296
val x298 = {
val x293 = new generated.scala.DoubleVectorImpl(x291,x292)
x293
}
x298.dcUpdate(x290, x297)
x290 = 1
while (x290 < x291) {  // begin fat loop x298
val x295 = x284.dcApply(x290)
val x296 = x286.dcApply(x290)
val x297 = x295 + x296
x298.dcUpdate(x290, x297)
x290 += 1
} // end fat loop x298
x298
}
if (x289) {
x300thenb()
} else { 
x300elseb()
}
}
x300
}
if (x288) {
x302thenb()
} else { 
x302elseb()
}
}
val x285 = x276._2
val x287 = x277._2
val x303 = x285 + x287
val x304 = (x302,x303)
x306 = x304
x275 += 1
} // end fat loop x306
val x307 = x306._1
val x308 = x306._2
val x309 = x308==0
val x312 = x307.isRow
val x319 = {
def x319thenb(): generated.scala.Vector[Double] = {
x307
}
def x319elseb(): generated.scala.Vector[Double] = {
val x311 = x307.length
// a *thin* loop follows: x317
var x310 = 0
val x315 = x307.dcApply(x310)
val x316 = x315 / x308
val x317 = {
val x313 = new generated.scala.DoubleVectorImpl(x311,x312)
x313
}
x317.dcUpdate(x310, x316)
x310 = 1
while (x310 < x311) {  // begin fat loop x317
val x315 = x307.dcApply(x310)
val x316 = x315 / x308
x317.dcUpdate(x310, x316)
x310 += 1
} // end fat loop x317
x317
}
if (x309) {
x319thenb()
} else { 
x319elseb()
}
}
val x321 = x252.numCols
// a *thin* loop follows: x325
var x320 = 0
var x325: Unit = {
val x322 = x319(x320)
val x323 = x252(x274, x320) = x322
x323
}
x320 = 1
while (x320 < x321) {  // begin fat loop x325
x325 = {
val x322 = x319(x320)
val x323 = x252(x274, x320) = x322
x323
}
x320 += 1
} // end fat loop x325
x325
}
x272 = 1
while (x272 < x259) {  // begin fat loop x328
x328 = {
val x274 = x271.dcApply(x272)
var x275 = 0
val x278 = x131.dcApply(x275)
val x279 = x187(x278)
val x280 = x279==x274
val x283 = {
def x283thenb(): scala.Tuple2[generated.scala.MatrixRow[Double], Double] = {
val x281 = x60.getRow(x278)
val x282 = (x281,1.0)
x282
}
def x283elseb(): scala.Tuple2[generated.scala.Vector[Double], Double] = {
x213
}
if (x280) {
x283thenb()
} else { 
x283elseb()
}
}
var x306: scala.Tuple2[generated.scala.Vector[Double], Double] = {
if (x202 == 0) {x205}
else {
val x278 = x131.dcApply(x275)
val x279 = x187(x278)
val x280 = x279==x274
val x283 = {
def x283thenb(): scala.Tuple2[generated.scala.MatrixRow[Double], Double] = {
val x281 = x60.getRow(x278)
val x282 = (x281,1.0)
x282
}
def x283elseb(): scala.Tuple2[generated.scala.Vector[Double], Double] = {
x213
}
if (x280) {
x283thenb()
} else { 
x283elseb()
}
}
x283
}
}
x275 = 1
while (x275 < x202) {  // begin fat loop x306
val x278 = x131.dcApply(x275)
val x279 = x187(x278)
val x280 = x279==x274
val x283 = {
def x283thenb(): scala.Tuple2[generated.scala.MatrixRow[Double], Double] = {
val x281 = x60.getRow(x278)
val x282 = (x281,1.0)
x282
}
def x283elseb(): scala.Tuple2[generated.scala.Vector[Double], Double] = {
x213
}
if (x280) {
x283thenb()
} else { 
x283elseb()
}
}
val x276 = x306
val x277 = x283
val x284 = x276._1
val x288 = x284.isInstanceOf[generated.scala.ZeroVector[Double]]
val x292 = x284.isRow
val x302 = {
def x302thenb(): generated.scala.Vector[Double] = {
val x286 = x277._1
x286
}
def x302elseb(): generated.scala.Vector[Double] = {
val x286 = x277._1
val x289 = x286.isInstanceOf[generated.scala.ZeroVector[Double]]
val x300 = {
def x300thenb(): generated.scala.Vector[Double] = {
x284
}
def x300elseb(): generated.scala.Vector[Double] = {
val x291 = x284.length
// a *thin* loop follows: x298
var x290 = 0
val x295 = x284.dcApply(x290)
val x296 = x286.dcApply(x290)
val x297 = x295 + x296
val x298 = {
val x293 = new generated.scala.DoubleVectorImpl(x291,x292)
x293
}
x298.dcUpdate(x290, x297)
x290 = 1
while (x290 < x291) {  // begin fat loop x298
val x295 = x284.dcApply(x290)
val x296 = x286.dcApply(x290)
val x297 = x295 + x296
x298.dcUpdate(x290, x297)
x290 += 1
} // end fat loop x298
x298
}
if (x289) {
x300thenb()
} else { 
x300elseb()
}
}
x300
}
if (x288) {
x302thenb()
} else { 
x302elseb()
}
}
val x285 = x276._2
val x287 = x277._2
val x303 = x285 + x287
val x304 = (x302,x303)
x306 = x304
x275 += 1
} // end fat loop x306
val x307 = x306._1
val x308 = x306._2
val x309 = x308==0
val x312 = x307.isRow
val x319 = {
def x319thenb(): generated.scala.Vector[Double] = {
x307
}
def x319elseb(): generated.scala.Vector[Double] = {
val x311 = x307.length
// a *thin* loop follows: x317
var x310 = 0
val x315 = x307.dcApply(x310)
val x316 = x315 / x308
val x317 = {
val x313 = new generated.scala.DoubleVectorImpl(x311,x312)
x313
}
x317.dcUpdate(x310, x316)
x310 = 1
while (x310 < x311) {  // begin fat loop x317
val x315 = x307.dcApply(x310)
val x316 = x315 / x308
x317.dcUpdate(x310, x316)
x310 += 1
} // end fat loop x317
x317
}
if (x309) {
x319thenb()
} else { 
x319elseb()
}
}
val x321 = x252.numCols
// a *thin* loop follows: x325
var x320 = 0
var x325: Unit = {
val x322 = x319(x320)
val x323 = x252(x274, x320) = x322
x323
}
x320 = 1
while (x320 < x321) {  // begin fat loop x325
x325 = {
val x322 = x319(x320)
val x323 = x252(x274, x320) = x322
x323
}
x320 += 1
} // end fat loop x325
x325
}
x272 += 1
} // end fat loop x328
val x329 = x252// unsafe immutable
x329
}
def x333elseb(): generated.scala.Matrix[Double] = {
val x331 = new generated.scala.DoubleMatrixImpl(0,0)
x331
}
if (x197) {
x333thenb()
} else { 
x333elseb()
}
}
x333
}
def x338elseb(): Nothing = {
val x335 = println("optiml runtime error: illegal matrix constructor")
val x336 = exit(-1)
x336
}
if (x195) {
x338thenb()
} else { 
x338elseb()
}
}
x119 = x338
val x340 = x120 += 1
val x341 = x119
val x342 = x118
val x344 = x341.numRows
val x345 = x341.numCols
val x346 = x344 * x345
val x372 = { 
// a *thin* loop follows: x352
var x343 = 0
val x349 = x341.dcApply(x343)
val x350 = x342.dcApply(x343)
val x351 = x349 - x350
val x352 = {
val x347 = new generated.scala.DoubleMatrixImpl(x344,x345)
x347
}
x352.dcUpdate(x343, x351)
x343 = 1
while (x343 < x346) {  // begin fat loop x352
val x349 = x341.dcApply(x343)
val x350 = x342.dcApply(x343)
val x351 = x349 - x350
x352.dcUpdate(x343, x351)
x343 += 1
} // end fat loop x352
val x354 = x352.numRows
val x355 = x352.numCols
val x356 = x354 * x355
// a *thin* loop follows: x361
var x353 = 0
val x359 = x352.dcApply(x353)
val x360 = java.lang.Math.abs(x359)
val x361 = {
val x357 = new generated.scala.DoubleMatrixImpl(x354,x355)
x357
}
x361.dcUpdate(x353, x360)
x353 = 1
while (x353 < x356) {  // begin fat loop x361
val x359 = x352.dcApply(x353)
val x360 = java.lang.Math.abs(x359)
x361.dcUpdate(x353, x360)
x353 += 1
} // end fat loop x361
val x365 = x361.numRows
val x366 = x361.numCols
val x367 = x365 * x366
var x362 = 0
val x368 = x361.dcApply(x362)
var x370: Double = {
if (x367 == 0) {0.0}
else {
val x368 = x361.dcApply(x362)
x368
}
}
x362 = 1
while (x362 < x367) {  // begin fat loop x370
val x368 = x361.dcApply(x362)
val x363 = x370
val x364 = x368
val x369 = x363 + x364
x370 = x369
x362 += 1
} // end fat loop x370
x370
}
x116 = x372
()
}
val x376 = x120
val x377 = x376==1000
val x381 = {
def x381thenb(): Unit = {
val x378 = println("Maximum iterations exceeded")
val x379 = return ()
x379
}
if (x377) {
x381thenb()
}
}
val x382 = x119
val x383 = Seq(x382)
val x384 = ppl.delite.runtime.profiler.PerformanceTimer.stop("app", false)
val x385 = x115
val x386 = "finished in "+x385
val x387 = x386+" iterations"
val x388 = println(x387)
val x389 = x382.numRows
val x393 = x382.numCols
val x408 = { 
var x391 : Int = 0
val x406 = while (x391 < x389) {
val x392 = print("[ ")
val x396 = x391 * x393
var x395 : Int = 0
val x403 = while (x395 < x393) {
val x397 = x396 + x395
val x398 = x382.dcApply(x397)
val x399 = { 
x398
}
val x400 = print(x399)
val x401 = print(" ")
x401
x395 = x395 + 1
}
val x404 = print("]\n")
x404
x391 = x391 + 1
}
x406
}
()
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
