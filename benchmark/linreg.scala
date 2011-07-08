/*****************************************
  Emitting Generated Code                  
*******************************************/
class Application extends ((Array[java.lang.String])=>(Unit)) {
def apply(x0:Array[java.lang.String]): Unit = {
val x1 = x0.length
val x2 = x1 < 2
val x6 = {
def x6thenb(): Nothing = {
val x3 = println("Usage: LinRegSerial <input vector file> <output vector file>")
val x4 = exit(-1)
x4
}
if (x2) {
x6thenb()
}
}
val x7 = x0(0)
val x30 = { 
val x8 = new generated.scala.DoubleVectorImpl(0,true)
val x9 = new java.io.FileReader(x7)
val x10 = new java.io.BufferedReader(x9)
val x11 = x10.readLine()
var x12: java.lang.String = x11
val x26 = while ({val x13 = x12
val x14 = x13 != null
x14}) {
val x16 = x12
val x17 = x16.trim()
x12 = x17
val x19 = x12
val x21 = x8.length
val x20 = java.lang.Double.parseDouble(x19)
val x22 = x8.insert(x21, x20)
val x23 = x10.readLine()
x12 = x23
()
}
val x27 = x10.close()
val x28 = x8// unsafe immutable
x28
}
val x32 = x30.length
val x33 = x30.isRow
val x34 = !x33
// a *thin* loop follows: x38
var x31 = 0
val x37 = x30.dcApply(x31)
val x38 = {
val x35 = new generated.scala.DoubleVectorImpl(x32,x34)
x35
}
x38.dcUpdate(x31, x37)
x31 = 1
while (x31 < x32) {  // begin fat loop x38
val x37 = x30.dcApply(x31)
x38.dcUpdate(x31, x37)
x31 += 1
} // end fat loop x38
val x39 = x0(1)
val x62 = { 
val x40 = new generated.scala.DoubleVectorImpl(0,true)
val x41 = new java.io.FileReader(x39)
val x42 = new java.io.BufferedReader(x41)
val x43 = x42.readLine()
var x44: java.lang.String = x43
val x58 = while ({val x45 = x44
val x46 = x45 != null
x46}) {
val x48 = x44
val x49 = x48.trim()
x44 = x49
val x51 = x44
val x53 = x40.length
val x52 = java.lang.Double.parseDouble(x51)
val x54 = x40.insert(x53, x52)
val x55 = x42.readLine()
x44 = x55
()
}
val x59 = x42.close()
val x60 = x40// unsafe immutable
x60
}
val x64 = x62.length
val x65 = x62.isRow
val x66 = !x65
// a *thin* loop follows: x70
var x63 = 0
val x69 = x62.dcApply(x63)
val x70 = {
val x67 = new generated.scala.DoubleVectorImpl(x64,x66)
x67
}
x70.dcUpdate(x63, x69)
x63 = 1
while (x63 < x64) {  // begin fat loop x70
val x69 = x62.dcApply(x63)
x70.dcUpdate(x63, x69)
x63 += 1
} // end fat loop x70
val x78 = { 
val x71 = new generated.scala.DoubleVectorImpl(x32,true)
val x73 = x71.length
// a *thin* loop follows: x76
var x72 = 0
val x74 = x71.dcApply(x72)
val x76 = {
x71
}
x76.dcUpdate(x72, 1.0)
x72 = 1
while (x72 < x73) {  // begin fat loop x76
val x74 = x71.dcApply(x72)
x76.dcUpdate(x72, 1.0)
x72 += 1
} // end fat loop x76
x76
}
val x80 = x78.length
val x81 = x78.isRow
val x82 = !x81
// a *thin* loop follows: x85
var x79 = 0
val x85 = {
val x83 = new generated.scala.DoubleVectorImpl(x80,x82)
x83
}
x85.dcUpdate(x79, 1)
x79 = 1
while (x79 < x80) {  // begin fat loop x85
x85.dcUpdate(x79, 1)
x79 += 1
} // end fat loop x85
val x86 = new generated.scala.VectorImpl[generated.scala.Vector[Double]](0,true)
val x87 = x86.length
val x88 = x86.insert(x87, x85)
val x89 = x86.length
val x90 = x86.insert(x89, x38)
val x91 = x86// unsafe immutable
val x404 = println("Unweighted linear regression")
val x405 = println("theta: ")
val x92 = x91.length
val x93 = x92==0
val x130 = { 
val x128 = {
def x128thenb(): generated.scala.Matrix[Double] = {
val x94 = new generated.scala.DoubleMatrixImpl(0,0)
val x95 = x94// unsafe immutable
x95
}
def x128elseb(): generated.scala.Matrix[Double] = {
val x97 = x91(0)
val x98 = x97.isRow
val x126 = {
def x126thenb(): generated.scala.Matrix[Double] = {
val x99 = x97.length
val x100 = new generated.scala.DoubleMatrixImpl(x92,x99)
var x102 : Int = 0
val x111 = while (x102 < x92) {
val x105 = x91(x102)
var x104 : Int = 0
val x109 = while (x104 < x99) {
val x106 = x105(x104)
val x107 = x100(x102, x104) = x106
x107
x104 = x104 + 1
}
x109
x102 = x102 + 1
}
val x112 = x100// unsafe immutable
x112
}
def x126elseb(): generated.scala.Matrix[Double] = {
val x99 = x97.length
val x114 = new generated.scala.DoubleMatrixImpl(x99,x92)
var x115 : Int = 0
val x123 = while (x115 < x92) {
val x117 = x91(x115)
var x116 : Int = 0
val x121 = while (x116 < x99) {
val x118 = x117(x116)
val x119 = x114(x116, x115) = x118
x119
x116 = x116 + 1
}
x121
x115 = x115 + 1
}
val x124 = x114// unsafe immutable
x124
}
if (x98) {
x126thenb()
} else { 
x126elseb()
}
}
x126
}
if (x93) {
x128thenb()
} else { 
x128elseb()
}
}
x128
}
val x131 = x130.numCols
val x132 = x130.numRows
val x151 = { 
val x133 = new generated.scala.DoubleMatrixImpl(x131,x132)
val x134 = x133.numRows
var x136 : Int = 0
val x148 = while (x136 < x134) {
val x137 = x133.numCols
var x139 : Int = 0
val x146 = while (x139 < x137) {
val x140 = x139 * x131
val x141 = x140 + x136
val x142 = x130.dcApply(x141)
val x143 = { 
x142
}
val x144 = x133(x136, x139) = x143
x144
x139 = x139 + 1
}
x146
x136 = x136 + 1
}
val x149 = x133// unsafe immutable
x149
}
val x170 = x151.numRows
val x172 = new generated.scala.IndexVectorRangeImpl(0,x170)
val x175 = x172.length
val x968 = x151.numCols
val x1005 = List()
val x179 = { 
val x171 = new generated.scala.DoubleMatrixImpl(x170,x131)
// a *thin* loop follows: x176
var x173 = 0
var x176: Unit = {
var x962: Int = 0
val x961 = x172.dcApply(x173)
val x969 = x961 * x968
val x970 = x969 + 0
val x971 = x151.dcApply(x970)
val x972 = { 
x971
}
val x1003 = while ({val x963 = x962
val x964 = x171.numCols
val x965 = x963 < x964
x965}) {
var x967: Int = 1
val x973 = x962
val x974 = x973 * x968
val x975 = x974 + 0
val x976 = x151.dcApply(x975)
val x977 = { 
x976
}
val x978 = x972 * x977
var x979: Double = x978
val x997 = while ({val x980 = x967
val x981 = x980 < x968
x981}) {
val x983 = x967
val x987 = x962
val x988 = x967
val x984 = x969 + x983
val x985 = x151.dcApply(x984)
val x986 = { 
x985
}
val x989 = x987 * x968
val x990 = x989 + x988
val x991 = x151.dcApply(x990)
val x992 = { 
x991
}
val x993 = x986 * x992
val x994 = x979 += x993
val x995 = x967 += 1
()
}
val x998 = x962
val x999 = x979
val x1000 = x171(x961, x998) = x999
val x1001 = x962 += 1
()
}
()
}
x173 = 1
while (x173 < x175) {  // begin fat loop x176
x176 = {
var x962: Int = 0
val x961 = x172.dcApply(x173)
val x969 = x961 * x968
val x970 = x969 + 0
val x971 = x151.dcApply(x970)
val x972 = { 
x971
}
val x1003 = while ({val x963 = x962
val x964 = x171.numCols
val x965 = x963 < x964
x965}) {
var x967: Int = 1
val x973 = x962
val x974 = x973 * x968
val x975 = x974 + 0
val x976 = x151.dcApply(x975)
val x977 = { 
x976
}
val x978 = x972 * x977
var x979: Double = x978
val x997 = while ({val x980 = x967
val x981 = x980 < x968
x981}) {
val x983 = x967
val x987 = x962
val x988 = x967
val x984 = x969 + x983
val x985 = x151.dcApply(x984)
val x986 = { 
x985
}
val x989 = x987 * x968
val x990 = x989 + x988
val x991 = x151.dcApply(x990)
val x992 = { 
x991
}
val x993 = x986 * x992
val x994 = x979 += x993
val x995 = x967 += 1
()
}
val x998 = x962
val x999 = x979
val x1000 = x171(x961, x998) = x999
val x1001 = x962 += 1
()
}
()
}
x173 += 1
} // end fat loop x176
val x177 = x171// unsafe immutable
x177
}
val x180 = x179.numCols
val x206 = x179.numRows
val x207 = x206 * x180
val x203 = { 
val x189 = new generated.scala.DoubleMatrixImpl(x180,x180)
var x190: Int = 0
val x200 = while ({val x191 = x190
val x192 = x191 < x180
x192}) {
val x194 = x190
val x195 = x190
val x196 = x190
val x197 = x189(x194, x195) = 1
val x198 = x190 += 1
()
}
val x201 = x189// unsafe immutable
x201
}
val x204 = { 
x203
}
val x368 = { 
// a *thin* loop follows: x211
var x205 = 0
val x210 = x179.dcApply(x205)
val x211 = {
val x208 = new generated.scala.DoubleMatrixImpl(x206,x180)
x208
}
x211.dcUpdate(x205, x210)
x205 = 1
while (x205 < x207) {  // begin fat loop x211
val x210 = x179.dcApply(x205)
x211.dcUpdate(x205, x210)
x205 += 1
} // end fat loop x211
val x212 = x211.cloneL
val x213 = x212.numCols
val x214 = x212.insertAllCols(x213,x204)
var x215: Int = 0
var x216: Boolean = false
var x217: Int = 0
val x364 = while ({val x218 = x216
val x220 = x217
val x221 = x212.numRows
val x219 = !x218
val x222 = x220 < x221
val x223 = x219 && x222
x223}) {
val x225 = x212.numRows
val x226 = x215
val x227 = x225 <= x226
val x230 = {
def x230thenb(): Unit = {
x216 = true
()
}
if (x227) {
x230thenb()
}
}
val x231 = x216
val x232 = !x231
val x361 = {
def x361thenb(): Unit = {
val x233 = x217
var x234: Int = x233
val x264 = while ({val x235 = x216
val x237 = x234
val x238 = x215
val x244 = { 
val x239 = x212.numCols
val x240 = x237 * x239
val x241 = x240 + x238
val x242 = x212.dcApply(x241)
x242
}
val x236 = !x235
val x245 = x244==0.0
val x246 = x236 && x245
x246}) {
val x248 = x234 += 1
val x249 = x212.numCols
val x250 = x234
val x251 = x249==x250
val x262 = {
def x262thenb(): Unit = {
val x252 = x217
x234 = x252
val x254 = x215 += 1
val x255 = x212.numRows
val x256 = x215
val x257 = x255==x256
val x260 = {
def x260thenb(): Unit = {
x216 = true
()
}
if (x257) {
x260thenb()
}
}
x260
}
if (x251) {
x262thenb()
}
}
x262
}
val x265 = x216
val x266 = !x265
val x359 = {
def x359thenb(): Unit = {
val x267 = x234
val x268 = x212.getRow(x267)
val x269 = x217
val x270 = x212.getRow(x269)
val x272 = x212.numCols
// a *thin* loop follows: x276
var x271 = 0
var x276: Unit = {
val x273 = x270(x271)
val x274 = x212(x267, x271) = x273
x274
}
x271 = 1
while (x271 < x272) {  // begin fat loop x276
x276 = {
val x273 = x270(x271)
val x274 = x212(x267, x271) = x273
x274
}
x271 += 1
} // end fat loop x276
val x277 = x217
val x279 = x212.numCols
// a *thin* loop follows: x283
var x278 = 0
var x283: Unit = {
val x280 = x268(x278)
val x281 = x212(x277, x278) = x280
x281
}
x278 = 1
while (x278 < x279) {  // begin fat loop x283
x283 = {
val x280 = x268(x278)
val x281 = x212(x277, x278) = x280
x281
}
x278 += 1
} // end fat loop x283
val x284 = x217
val x285 = x217
val x286 = x212.getRow(x285)
val x287 = x217
val x288 = x215
val x294 = { 
val x289 = x212.numCols
val x290 = x287 * x289
val x291 = x290 + x288
val x292 = x212.dcApply(x291)
x292
}
val x296 = x286.length
// a *thin* loop follows: x304
var x295 = 0
val x301 = x286.dcApply(x295)
val x302 = x301 / x294
val x304 = {
val x297 = x286.length
val x298 = x286.isRow
val x299 = new generated.scala.DoubleVectorImpl(x297,x298)
x299
}
x304.dcUpdate(x295, x302)
x295 = 1
while (x295 < x296) {  // begin fat loop x304
val x301 = x286.dcApply(x295)
val x302 = x301 / x294
x304.dcUpdate(x295, x302)
x295 += 1
} // end fat loop x304
val x306 = x212.numCols
// a *thin* loop follows: x310
var x305 = 0
var x310: Unit = {
val x307 = x304(x305)
val x308 = x212(x284, x305) = x307
x308
}
x305 = 1
while (x305 < x306) {  // begin fat loop x310
x310 = {
val x307 = x304(x305)
val x308 = x212(x284, x305) = x307
x308
}
x305 += 1
} // end fat loop x310
val x311 = x212.numRows
var x313 : Int = 0
val x356 = while (x313 < x311) {
val x314 = x217
val x315 = x313 != x314
val x354 = {
def x354thenb(): Unit = {
val x316 = x212.getRow(x313)
val x317 = x217
val x318 = x212.getRow(x317)
val x319 = x215
val x325 = { 
val x320 = x212.numCols
val x321 = x313 * x320
val x322 = x321 + x319
val x323 = x212.dcApply(x322)
x323
}
val x327 = x318.length
// a *thin* loop follows: x335
var x326 = 0
val x332 = x318.dcApply(x326)
val x333 = x332 * x325
val x335 = {
val x328 = x318.length
val x329 = x318.isRow
val x330 = new generated.scala.DoubleVectorImpl(x328,x329)
x330
}
x335.dcUpdate(x326, x333)
x326 = 1
while (x326 < x327) {  // begin fat loop x335
val x332 = x318.dcApply(x326)
val x333 = x332 * x325
x335.dcUpdate(x326, x333)
x326 += 1
} // end fat loop x335
val x337 = x316.length
// a *thin* loop follows: x346
var x336 = 0
val x342 = x316.dcApply(x336)
val x343 = x335.dcApply(x336)
val x344 = x342 - x343
val x346 = {
val x338 = x316.length
val x339 = x316.isRow
val x340 = new generated.scala.DoubleVectorImpl(x338,x339)
x340
}
x346.dcUpdate(x336, x344)
x336 = 1
while (x336 < x337) {  // begin fat loop x346
val x342 = x316.dcApply(x336)
val x343 = x335.dcApply(x336)
val x344 = x342 - x343
x346.dcUpdate(x336, x344)
x336 += 1
} // end fat loop x346
val x348 = x212.numCols
// a *thin* loop follows: x352
var x347 = 0
var x352: Unit = {
val x349 = x346(x347)
val x350 = x212(x313, x347) = x349
x350
}
x347 = 1
while (x347 < x348) {  // begin fat loop x352
x352 = {
val x349 = x346(x347)
val x350 = x212(x313, x347) = x349
x350
}
x347 += 1
} // end fat loop x352
x352
}
if (x315) {
x354thenb()
}
}
x354
x313 = x313 + 1
}
val x357 = x215 += 1
()
}
if (x266) {
x359thenb()
}
}
x359
}
if (x232) {
x361thenb()
}
}
val x362 = x217 += 1
()
}
val x365 = x212.removeCols(0,x180)
val x366 = x212// unsafe immutable
x366
}
val x393 = { 
val x387 = new generated.scala.DoubleVectorImpl(x170,false)
// a *thin* loop follows: x390
var x388 = 0
var x390: Unit = {
val x1006 = x172.dcApply(x388)
val x1011 = x1006 * x968
// a *thin* loop follows: x1017
var x1008 = 0
val x1012 = x1011 + x1008
val x1013 = x151.dcApply(x1012)
val x1014 = { 
x1013
}
val x1015 = x70.dcApply(x1008)
val x1016 = x1014 * x1015
val x1017 = {
val x1009 = new generated.scala.DoubleVectorImpl(x968,true)
x1009
}
x1017.dcUpdate(x1008, x1016)
x1008 = 1
while (x1008 < x968) {  // begin fat loop x1017
val x1012 = x1011 + x1008
val x1013 = x151.dcApply(x1012)
val x1014 = { 
x1013
}
val x1015 = x70.dcApply(x1008)
val x1016 = x1014 * x1015
x1017.dcUpdate(x1008, x1016)
x1008 += 1
} // end fat loop x1017
var x1018 = 0
val x1021 = x1017.dcApply(x1018)
var x1023: Double = {
if (x968 == 0) {0.0}
else {
val x1021 = x1017.dcApply(x1018)
x1021
}
}
x1018 = 1
while (x1018 < x968) {  // begin fat loop x1023
val x1021 = x1017.dcApply(x1018)
val x1019 = x1023
val x1020 = x1021
val x1022 = x1019 + x1020
x1023 = x1022
x1018 += 1
} // end fat loop x1023
val x1024 = x387(x1006) = x1023
x1024
}
x388 = 1
while (x388 < x175) {  // begin fat loop x390
x390 = {
val x1006 = x172.dcApply(x388)
val x1011 = x1006 * x968
// a *thin* loop follows: x1017
var x1008 = 0
val x1012 = x1011 + x1008
val x1013 = x151.dcApply(x1012)
val x1014 = { 
x1013
}
val x1015 = x70.dcApply(x1008)
val x1016 = x1014 * x1015
val x1017 = {
val x1009 = new generated.scala.DoubleVectorImpl(x968,true)
x1009
}
x1017.dcUpdate(x1008, x1016)
x1008 = 1
while (x1008 < x968) {  // begin fat loop x1017
val x1012 = x1011 + x1008
val x1013 = x151.dcApply(x1012)
val x1014 = { 
x1013
}
val x1015 = x70.dcApply(x1008)
val x1016 = x1014 * x1015
x1017.dcUpdate(x1008, x1016)
x1008 += 1
} // end fat loop x1017
var x1018 = 0
val x1021 = x1017.dcApply(x1018)
var x1023: Double = {
if (x968 == 0) {0.0}
else {
val x1021 = x1017.dcApply(x1018)
x1021
}
}
x1018 = 1
while (x1018 < x968) {  // begin fat loop x1023
val x1021 = x1017.dcApply(x1018)
val x1019 = x1023
val x1020 = x1021
val x1022 = x1019 + x1020
x1023 = x1022
x1018 += 1
} // end fat loop x1023
val x1024 = x387(x1006) = x1023
x1024
}
x388 += 1
} // end fat loop x390
val x391 = x387// unsafe immutable
x391
}
val x394 = x368.numRows
val x396 = new generated.scala.IndexVectorRangeImpl(0,x394)
val x399 = x396.length
val x1029 = x368.numCols
val x403 = { 
val x395 = new generated.scala.DoubleVectorImpl(x394,false)
// a *thin* loop follows: x400
var x397 = 0
var x400: Unit = {
val x1026 = x396.dcApply(x397)
val x1032 = x1026 * x1029
// a *thin* loop follows: x1038
var x1028 = 0
val x1033 = x1032 + x1028
val x1034 = x368.dcApply(x1033)
val x1035 = { 
x1034
}
val x1036 = x393.dcApply(x1028)
val x1037 = x1035 * x1036
val x1038 = {
val x1030 = new generated.scala.DoubleVectorImpl(x1029,true)
x1030
}
x1038.dcUpdate(x1028, x1037)
x1028 = 1
while (x1028 < x1029) {  // begin fat loop x1038
val x1033 = x1032 + x1028
val x1034 = x368.dcApply(x1033)
val x1035 = { 
x1034
}
val x1036 = x393.dcApply(x1028)
val x1037 = x1035 * x1036
x1038.dcUpdate(x1028, x1037)
x1028 += 1
} // end fat loop x1038
var x1039 = 0
val x1042 = x1038.dcApply(x1039)
var x1044: Double = {
if (x1029 == 0) {0.0}
else {
val x1042 = x1038.dcApply(x1039)
x1042
}
}
x1039 = 1
while (x1039 < x1029) {  // begin fat loop x1044
val x1042 = x1038.dcApply(x1039)
val x1040 = x1044
val x1041 = x1042
val x1043 = x1040 + x1041
x1044 = x1043
x1039 += 1
} // end fat loop x1044
val x1045 = x395(x1026) = x1044
x1045
}
x397 = 1
while (x397 < x399) {  // begin fat loop x400
x400 = {
val x1026 = x396.dcApply(x397)
val x1032 = x1026 * x1029
// a *thin* loop follows: x1038
var x1028 = 0
val x1033 = x1032 + x1028
val x1034 = x368.dcApply(x1033)
val x1035 = { 
x1034
}
val x1036 = x393.dcApply(x1028)
val x1037 = x1035 * x1036
val x1038 = {
val x1030 = new generated.scala.DoubleVectorImpl(x1029,true)
x1030
}
x1038.dcUpdate(x1028, x1037)
x1028 = 1
while (x1028 < x1029) {  // begin fat loop x1038
val x1033 = x1032 + x1028
val x1034 = x368.dcApply(x1033)
val x1035 = { 
x1034
}
val x1036 = x393.dcApply(x1028)
val x1037 = x1035 * x1036
x1038.dcUpdate(x1028, x1037)
x1028 += 1
} // end fat loop x1038
var x1039 = 0
val x1042 = x1038.dcApply(x1039)
var x1044: Double = {
if (x1029 == 0) {0.0}
else {
val x1042 = x1038.dcApply(x1039)
x1042
}
}
x1039 = 1
while (x1039 < x1029) {  // begin fat loop x1044
val x1042 = x1038.dcApply(x1039)
val x1040 = x1044
val x1041 = x1042
val x1043 = x1040 + x1041
x1044 = x1043
x1039 += 1
} // end fat loop x1044
val x1045 = x395(x1026) = x1044
x1045
}
x397 += 1
} // end fat loop x400
val x401 = x395// unsafe immutable
x401
}
val x406 = x403.isRow
val x428 = { 
val x426 = {
def x426thenb(): Unit = {
val x407 = print("[ ")
val x408 = x403.length
var x410 : Int = 0
val x415 = while (x410 < x408) {
val x411 = x403(x410)
val x412 = print(x411)
val x413 = print(" ")
x413
x410 = x410 + 1
}
val x416 = print("]\n")
x416
}
def x426elseb(): Unit = {
val x408 = x403.length
var x418 : Int = 0
val x424 = while (x418 < x408) {
val x419 = print("[")
val x420 = x403(x418)
val x421 = print(x420)
val x422 = print(" ]\n")
x422
x418 = x418 + 1
}
x424
}
if (x406) {
x426thenb()
} else { 
x426elseb()
}
}
x426
}
val x429 = print("\n")
val x430 = Seq()
val x431 = ppl.delite.runtime.profiler.PerformanceTimer.start("app", false)
// a *thin* loop follows: x442
var x439 = 0
val x442 = {
val x440 = new generated.scala.DoubleVectorImpl(x80,x82)
x440
}
x442.dcUpdate(x439, 1)
x439 = 1
while (x439 < x80) {  // begin fat loop x442
x442.dcUpdate(x439, 1)
x439 += 1
} // end fat loop x442
val x443 = new generated.scala.VectorImpl[generated.scala.Vector[Double]](0,true)
val x444 = x443.length
val x445 = x443.insert(x444, x442)
val x446 = x443.length
val x447 = x443.insert(x446, x38)
val x448 = x443// unsafe immutable
val x449 = x448.length
val x450 = x449==0
val x487 = { 
val x485 = {
def x485thenb(): generated.scala.Matrix[Double] = {
val x451 = new generated.scala.DoubleMatrixImpl(0,0)
val x452 = x451// unsafe immutable
x452
}
def x485elseb(): generated.scala.Matrix[Double] = {
val x454 = x448(0)
val x455 = x454.isRow
val x483 = {
def x483thenb(): generated.scala.Matrix[Double] = {
val x456 = x454.length
val x457 = new generated.scala.DoubleMatrixImpl(x449,x456)
var x459 : Int = 0
val x468 = while (x459 < x449) {
val x462 = x448(x459)
var x461 : Int = 0
val x466 = while (x461 < x456) {
val x463 = x462(x461)
val x464 = x457(x459, x461) = x463
x464
x461 = x461 + 1
}
x466
x459 = x459 + 1
}
val x469 = x457// unsafe immutable
x469
}
def x483elseb(): generated.scala.Matrix[Double] = {
val x456 = x454.length
val x471 = new generated.scala.DoubleMatrixImpl(x456,x449)
var x472 : Int = 0
val x480 = while (x472 < x449) {
val x474 = x448(x472)
var x473 : Int = 0
val x478 = while (x473 < x456) {
val x475 = x474(x473)
val x476 = x471(x473, x472) = x475
x476
x473 = x473 + 1
}
x478
x472 = x472 + 1
}
val x481 = x471// unsafe immutable
x481
}
if (x455) {
x483thenb()
} else { 
x483elseb()
}
}
x483
}
if (x450) {
x485thenb()
} else { 
x485elseb()
}
}
x485
}
val x488 = x487.numRows
val x489 = x488
val x490 = 25.0 / x489
val x491 = 24.990000000000002 / x490
val x492 = Math.ceil(x491)
val x493 = x492.asInstanceOf[Int]
val x505 = { 
val x494 = new generated.scala.DoubleVectorImpl(x493,true)
var x496 : Int = 0
val x502 = while (x496 < x493) {
val x497 = x496
val x498 = x490 * x497
val x499 = x498 + -10.0
val x500 = x494(x496) = x499
x500
x496 = x496 + 1
}
val x503 = x494// unsafe immutable
x503
}
val x507 = x505.length
val x508 = x505.isRow
val x509 = !x508
// a *thin* loop follows: x513
var x506 = 0
val x512 = x505.dcApply(x506)
val x513 = {
val x510 = new generated.scala.DoubleVectorImpl(x507,x509)
x510
}
x513.dcUpdate(x506, x512)
x506 = 1
while (x506 < x507) {  // begin fat loop x513
val x512 = x505.dcApply(x506)
x513.dcUpdate(x506, x512)
x506 += 1
} // end fat loop x513
val x521 = { 
val x514 = new generated.scala.DoubleVectorImpl(x507,true)
val x516 = x514.length
// a *thin* loop follows: x519
var x515 = 0
val x517 = x514.dcApply(x515)
val x519 = {
x514
}
x519.dcUpdate(x515, 1.0)
x515 = 1
while (x515 < x516) {  // begin fat loop x519
val x517 = x514.dcApply(x515)
x519.dcUpdate(x515, 1.0)
x515 += 1
} // end fat loop x519
x519
}
val x523 = x521.length
val x524 = x521.isRow
val x525 = !x524
// a *thin* loop follows: x528
var x522 = 0
val x528 = {
val x526 = new generated.scala.DoubleVectorImpl(x523,x525)
x526
}
x528.dcUpdate(x522, 1)
x522 = 1
while (x522 < x523) {  // begin fat loop x528
x528.dcUpdate(x522, 1)
x522 += 1
} // end fat loop x528
val x529 = new generated.scala.VectorImpl[generated.scala.Vector[Double]](0,true)
val x530 = x529.length
val x531 = x529.insert(x530, x528)
val x532 = x529.length
val x533 = x529.insert(x532, x513)
val x534 = x529// unsafe immutable
val x535 = x534.length
val x536 = x535==0
val x573 = { 
val x571 = {
def x571thenb(): generated.scala.Matrix[Double] = {
val x537 = new generated.scala.DoubleMatrixImpl(0,0)
val x538 = x537// unsafe immutable
x538
}
def x571elseb(): generated.scala.Matrix[Double] = {
val x540 = x534(0)
val x541 = x540.isRow
val x569 = {
def x569thenb(): generated.scala.Matrix[Double] = {
val x542 = x540.length
val x543 = new generated.scala.DoubleMatrixImpl(x535,x542)
var x545 : Int = 0
val x554 = while (x545 < x535) {
val x548 = x534(x545)
var x547 : Int = 0
val x552 = while (x547 < x542) {
val x549 = x548(x547)
val x550 = x543(x545, x547) = x549
x550
x547 = x547 + 1
}
x552
x545 = x545 + 1
}
val x555 = x543// unsafe immutable
x555
}
def x569elseb(): generated.scala.Matrix[Double] = {
val x542 = x540.length
val x557 = new generated.scala.DoubleMatrixImpl(x542,x535)
var x558 : Int = 0
val x566 = while (x558 < x535) {
val x560 = x534(x558)
var x559 : Int = 0
val x564 = while (x559 < x542) {
val x561 = x560(x559)
val x562 = x557(x559, x558) = x561
x562
x559 = x559 + 1
}
x564
x558 = x558 + 1
}
val x567 = x557// unsafe immutable
x567
}
if (x541) {
x569thenb()
} else { 
x569elseb()
}
}
x569
}
if (x536) {
x571thenb()
} else { 
x571elseb()
}
}
x571
}
val x594 = x573.numRows
val x595 = new generated.scala.IndexVectorRangeImpl(0,x594)
val x597 = x595.length
val x598 = x595.isRow
val x608 = x38.isRow
val x602 = x573.numCols
val x574 = x487.numCols
val x593 = { 
val x575 = new generated.scala.DoubleMatrixImpl(x574,x488)
val x576 = x575.numRows
var x578 : Int = 0
val x590 = while (x578 < x576) {
val x579 = x575.numCols
var x581 : Int = 0
val x588 = while (x581 < x579) {
val x582 = x581 * x574
val x583 = x582 + x578
val x584 = x487.dcApply(x583)
val x585 = { 
x584
}
val x586 = x575(x578, x581) = x585
x586
x581 = x581 + 1
}
x588
x578 = x578 + 1
}
val x591 = x575// unsafe immutable
x591
}
val x656 = x593.numRows
val x658 = new generated.scala.IndexVectorRangeImpl(0,x656)
val x661 = x658.length
val x1054 = x593.numCols
val x910 = !true
// a *thin* loop follows: x932
var x596 = 0
val x601 = x595.dcApply(x596)
val x603 = x601 * x602
val x604 = x603 + 1
val x605 = x573.dcApply(x604)
val x606 = { 
x605
}
// a *thin* loop follows: x618
var x607 = 0
val x611 = x38.dcApply(x607)
val x612 = x606 - x611
val x613 = -0.1 * x612
val x614 = x613 * x612
val x615 = x614 / 200.0
val x616 = Math.exp(x615)
val x617 = x616 / 2.0
val x618 = {
val x609 = new generated.scala.DoubleVectorImpl(x32,x608)
x609
}
x618.dcUpdate(x607, x617)
x607 = 1
while (x607 < x32) {  // begin fat loop x618
val x611 = x38.dcApply(x607)
val x612 = x606 - x611
val x613 = -0.1 * x612
val x614 = x613 * x612
val x615 = x614 / 200.0
val x616 = Math.exp(x615)
val x617 = x616 / 2.0
x618.dcUpdate(x607, x617)
x607 += 1
} // end fat loop x618
val x634 = { 
val x619 = new generated.scala.DoubleMatrixImpl(x32,x32)
var x620: Int = 0
val x631 = while ({val x621 = x620
val x622 = x621 < x32
x622}) {
val x624 = x620
val x625 = x620
val x626 = x620
val x627 = x618(x626)
val x628 = x619(x624, x625) = x627
val x629 = x620 += 1
()
}
val x632 = x619// unsafe immutable
x632
}
val x635 = x634.numCols
val x636 = x634.numRows
val x655 = { 
val x637 = new generated.scala.DoubleMatrixImpl(x635,x636)
val x638 = x637.numRows
var x640 : Int = 0
val x652 = while (x640 < x638) {
val x641 = x637.numCols
var x643 : Int = 0
val x650 = while (x643 < x641) {
val x644 = x643 * x635
val x645 = x644 + x640
val x646 = x634.dcApply(x645)
val x647 = { 
x646
}
val x648 = x637(x640, x643) = x647
x648
x643 = x643 + 1
}
x650
x640 = x640 + 1
}
val x653 = x637// unsafe immutable
x653
}
val x1060 = x655.numCols
val x665 = { 
val x657 = new generated.scala.DoubleMatrixImpl(x656,x635)
// a *thin* loop follows: x662
var x659 = 0
var x662: Unit = {
var x1048: Int = 0
val x1047 = x658.dcApply(x659)
val x1055 = x1047 * x1054
val x1056 = x1055 + 0
val x1057 = x593.dcApply(x1056)
val x1058 = { 
x1057
}
val x1090 = while ({val x1049 = x1048
val x1050 = x657.numCols
val x1051 = x1049 < x1050
x1051}) {
var x1053: Int = 1
val x1059 = x1048
val x1061 = x1059 * x1060
val x1062 = x1061 + 0
val x1063 = x655.dcApply(x1062)
val x1064 = { 
x1063
}
val x1065 = x1058 * x1064
var x1066: Double = x1065
val x1084 = while ({val x1067 = x1053
val x1068 = x1067 < x1060
x1068}) {
val x1070 = x1053
val x1074 = x1048
val x1075 = x1053
val x1071 = x1055 + x1070
val x1072 = x593.dcApply(x1071)
val x1073 = { 
x1072
}
val x1076 = x1074 * x1060
val x1077 = x1076 + x1075
val x1078 = x655.dcApply(x1077)
val x1079 = { 
x1078
}
val x1080 = x1073 * x1079
val x1081 = x1066 += x1080
val x1082 = x1053 += 1
()
}
val x1085 = x1048
val x1086 = x1066
val x1087 = x657(x1047, x1085) = x1086
val x1088 = x1048 += 1
()
}
()
}
x659 = 1
while (x659 < x661) {  // begin fat loop x662
x662 = {
var x1048: Int = 0
val x1047 = x658.dcApply(x659)
val x1055 = x1047 * x1054
val x1056 = x1055 + 0
val x1057 = x593.dcApply(x1056)
val x1058 = { 
x1057
}
val x1090 = while ({val x1049 = x1048
val x1050 = x657.numCols
val x1051 = x1049 < x1050
x1051}) {
var x1053: Int = 1
val x1059 = x1048
val x1061 = x1059 * x1060
val x1062 = x1061 + 0
val x1063 = x655.dcApply(x1062)
val x1064 = { 
x1063
}
val x1065 = x1058 * x1064
var x1066: Double = x1065
val x1084 = while ({val x1067 = x1053
val x1068 = x1067 < x1060
x1068}) {
val x1070 = x1053
val x1074 = x1048
val x1075 = x1053
val x1071 = x1055 + x1070
val x1072 = x593.dcApply(x1071)
val x1073 = { 
x1072
}
val x1076 = x1074 * x1060
val x1077 = x1076 + x1075
val x1078 = x655.dcApply(x1077)
val x1079 = { 
x1078
}
val x1080 = x1073 * x1079
val x1081 = x1066 += x1080
val x1082 = x1053 += 1
()
}
val x1085 = x1048
val x1086 = x1066
val x1087 = x657(x1047, x1085) = x1086
val x1088 = x1048 += 1
()
}
()
}
x659 += 1
} // end fat loop x662
val x663 = x657// unsafe immutable
x663
}
val x684 = x665.numRows
val x686 = new generated.scala.IndexVectorRangeImpl(0,x684)
val x689 = x686.length
val x1099 = x665.numCols
val x693 = { 
val x685 = new generated.scala.DoubleMatrixImpl(x684,x574)
// a *thin* loop follows: x690
var x687 = 0
var x690: Unit = {
var x1093: Int = 0
val x1092 = x686.dcApply(x687)
val x1100 = x1092 * x1099
val x1101 = x1100 + 0
val x1102 = x665.dcApply(x1101)
val x1103 = { 
x1102
}
val x1134 = while ({val x1094 = x1093
val x1095 = x685.numCols
val x1096 = x1094 < x1095
x1096}) {
var x1098: Int = 1
val x1104 = x1093
val x1105 = x1104 * x1054
val x1106 = x1105 + 0
val x1107 = x593.dcApply(x1106)
val x1108 = { 
x1107
}
val x1109 = x1103 * x1108
var x1110: Double = x1109
val x1128 = while ({val x1111 = x1098
val x1112 = x1111 < x1054
x1112}) {
val x1114 = x1098
val x1118 = x1093
val x1119 = x1098
val x1115 = x1100 + x1114
val x1116 = x665.dcApply(x1115)
val x1117 = { 
x1116
}
val x1120 = x1118 * x1054
val x1121 = x1120 + x1119
val x1122 = x593.dcApply(x1121)
val x1123 = { 
x1122
}
val x1124 = x1117 * x1123
val x1125 = x1110 += x1124
val x1126 = x1098 += 1
()
}
val x1129 = x1093
val x1130 = x1110
val x1131 = x685(x1092, x1129) = x1130
val x1132 = x1093 += 1
()
}
()
}
x687 = 1
while (x687 < x689) {  // begin fat loop x690
x690 = {
var x1093: Int = 0
val x1092 = x686.dcApply(x687)
val x1100 = x1092 * x1099
val x1101 = x1100 + 0
val x1102 = x665.dcApply(x1101)
val x1103 = { 
x1102
}
val x1134 = while ({val x1094 = x1093
val x1095 = x685.numCols
val x1096 = x1094 < x1095
x1096}) {
var x1098: Int = 1
val x1104 = x1093
val x1105 = x1104 * x1054
val x1106 = x1105 + 0
val x1107 = x593.dcApply(x1106)
val x1108 = { 
x1107
}
val x1109 = x1103 * x1108
var x1110: Double = x1109
val x1128 = while ({val x1111 = x1098
val x1112 = x1111 < x1054
x1112}) {
val x1114 = x1098
val x1118 = x1093
val x1119 = x1098
val x1115 = x1100 + x1114
val x1116 = x665.dcApply(x1115)
val x1117 = { 
x1116
}
val x1120 = x1118 * x1054
val x1121 = x1120 + x1119
val x1122 = x593.dcApply(x1121)
val x1123 = { 
x1122
}
val x1124 = x1117 * x1123
val x1125 = x1110 += x1124
val x1126 = x1098 += 1
()
}
val x1129 = x1093
val x1130 = x1110
val x1131 = x685(x1092, x1129) = x1130
val x1132 = x1093 += 1
()
}
()
}
x687 += 1
} // end fat loop x690
val x691 = x685// unsafe immutable
x691
}
val x694 = x693.numCols
val x720 = x693.numRows
val x721 = x720 * x694
val x717 = { 
val x703 = new generated.scala.DoubleMatrixImpl(x694,x694)
var x704: Int = 0
val x714 = while ({val x705 = x704
val x706 = x705 < x694
x706}) {
val x708 = x704
val x709 = x704
val x710 = x704
val x711 = x703(x708, x709) = 1
val x712 = x704 += 1
()
}
val x715 = x703// unsafe immutable
x715
}
val x718 = { 
x717
}
val x882 = { 
// a *thin* loop follows: x725
var x719 = 0
val x724 = x693.dcApply(x719)
val x725 = {
val x722 = new generated.scala.DoubleMatrixImpl(x720,x694)
x722
}
x725.dcUpdate(x719, x724)
x719 = 1
while (x719 < x721) {  // begin fat loop x725
val x724 = x693.dcApply(x719)
x725.dcUpdate(x719, x724)
x719 += 1
} // end fat loop x725
val x726 = x725.cloneL
val x727 = x726.numCols
val x728 = x726.insertAllCols(x727,x718)
var x729: Int = 0
var x730: Boolean = false
var x731: Int = 0
val x878 = while ({val x732 = x730
val x734 = x731
val x735 = x726.numRows
val x733 = !x732
val x736 = x734 < x735
val x737 = x733 && x736
x737}) {
val x739 = x726.numRows
val x740 = x729
val x741 = x739 <= x740
val x744 = {
def x744thenb(): Unit = {
x730 = true
()
}
if (x741) {
x744thenb()
}
}
val x745 = x730
val x746 = !x745
val x875 = {
def x875thenb(): Unit = {
val x747 = x731
var x748: Int = x747
val x778 = while ({val x749 = x730
val x751 = x748
val x752 = x729
val x758 = { 
val x753 = x726.numCols
val x754 = x751 * x753
val x755 = x754 + x752
val x756 = x726.dcApply(x755)
x756
}
val x750 = !x749
val x759 = x758==0.0
val x760 = x750 && x759
x760}) {
val x762 = x748 += 1
val x763 = x726.numCols
val x764 = x748
val x765 = x763==x764
val x776 = {
def x776thenb(): Unit = {
val x766 = x731
x748 = x766
val x768 = x729 += 1
val x769 = x726.numRows
val x770 = x729
val x771 = x769==x770
val x774 = {
def x774thenb(): Unit = {
x730 = true
()
}
if (x771) {
x774thenb()
}
}
x774
}
if (x765) {
x776thenb()
}
}
x776
}
val x779 = x730
val x780 = !x779
val x873 = {
def x873thenb(): Unit = {
val x781 = x748
val x782 = x726.getRow(x781)
val x783 = x731
val x784 = x726.getRow(x783)
val x786 = x726.numCols
// a *thin* loop follows: x790
var x785 = 0
var x790: Unit = {
val x787 = x784(x785)
val x788 = x726(x781, x785) = x787
x788
}
x785 = 1
while (x785 < x786) {  // begin fat loop x790
x790 = {
val x787 = x784(x785)
val x788 = x726(x781, x785) = x787
x788
}
x785 += 1
} // end fat loop x790
val x791 = x731
val x793 = x726.numCols
// a *thin* loop follows: x797
var x792 = 0
var x797: Unit = {
val x794 = x782(x792)
val x795 = x726(x791, x792) = x794
x795
}
x792 = 1
while (x792 < x793) {  // begin fat loop x797
x797 = {
val x794 = x782(x792)
val x795 = x726(x791, x792) = x794
x795
}
x792 += 1
} // end fat loop x797
val x798 = x731
val x799 = x731
val x800 = x726.getRow(x799)
val x801 = x731
val x802 = x729
val x808 = { 
val x803 = x726.numCols
val x804 = x801 * x803
val x805 = x804 + x802
val x806 = x726.dcApply(x805)
x806
}
val x810 = x800.length
// a *thin* loop follows: x818
var x809 = 0
val x815 = x800.dcApply(x809)
val x816 = x815 / x808
val x818 = {
val x811 = x800.length
val x812 = x800.isRow
val x813 = new generated.scala.DoubleVectorImpl(x811,x812)
x813
}
x818.dcUpdate(x809, x816)
x809 = 1
while (x809 < x810) {  // begin fat loop x818
val x815 = x800.dcApply(x809)
val x816 = x815 / x808
x818.dcUpdate(x809, x816)
x809 += 1
} // end fat loop x818
val x820 = x726.numCols
// a *thin* loop follows: x824
var x819 = 0
var x824: Unit = {
val x821 = x818(x819)
val x822 = x726(x798, x819) = x821
x822
}
x819 = 1
while (x819 < x820) {  // begin fat loop x824
x824 = {
val x821 = x818(x819)
val x822 = x726(x798, x819) = x821
x822
}
x819 += 1
} // end fat loop x824
val x825 = x726.numRows
var x827 : Int = 0
val x870 = while (x827 < x825) {
val x828 = x731
val x829 = x827 != x828
val x868 = {
def x868thenb(): Unit = {
val x830 = x726.getRow(x827)
val x831 = x731
val x832 = x726.getRow(x831)
val x833 = x729
val x839 = { 
val x834 = x726.numCols
val x835 = x827 * x834
val x836 = x835 + x833
val x837 = x726.dcApply(x836)
x837
}
val x841 = x832.length
// a *thin* loop follows: x849
var x840 = 0
val x846 = x832.dcApply(x840)
val x847 = x846 * x839
val x849 = {
val x842 = x832.length
val x843 = x832.isRow
val x844 = new generated.scala.DoubleVectorImpl(x842,x843)
x844
}
x849.dcUpdate(x840, x847)
x840 = 1
while (x840 < x841) {  // begin fat loop x849
val x846 = x832.dcApply(x840)
val x847 = x846 * x839
x849.dcUpdate(x840, x847)
x840 += 1
} // end fat loop x849
val x851 = x830.length
// a *thin* loop follows: x860
var x850 = 0
val x856 = x830.dcApply(x850)
val x857 = x849.dcApply(x850)
val x858 = x856 - x857
val x860 = {
val x852 = x830.length
val x853 = x830.isRow
val x854 = new generated.scala.DoubleVectorImpl(x852,x853)
x854
}
x860.dcUpdate(x850, x858)
x850 = 1
while (x850 < x851) {  // begin fat loop x860
val x856 = x830.dcApply(x850)
val x857 = x849.dcApply(x850)
val x858 = x856 - x857
x860.dcUpdate(x850, x858)
x850 += 1
} // end fat loop x860
val x862 = x726.numCols
// a *thin* loop follows: x866
var x861 = 0
var x866: Unit = {
val x863 = x860(x861)
val x864 = x726(x827, x861) = x863
x864
}
x861 = 1
while (x861 < x862) {  // begin fat loop x866
x866 = {
val x863 = x860(x861)
val x864 = x726(x827, x861) = x863
x864
}
x861 += 1
} // end fat loop x866
x866
}
if (x829) {
x868thenb()
}
}
x868
x827 = x827 + 1
}
val x871 = x729 += 1
()
}
if (x780) {
x873thenb()
}
}
x873
}
if (x746) {
x875thenb()
}
}
val x876 = x731 += 1
()
}
val x879 = x726.removeCols(0,x694)
val x880 = x726// unsafe immutable
x880
}
val x889 = { 
val x883 = new generated.scala.DoubleVectorImpl(x684,false)
// a *thin* loop follows: x886
var x884 = 0
var x886: Unit = {
val x1136 = x686.dcApply(x884)
val x1141 = x1136 * x1099
// a *thin* loop follows: x1147
var x1138 = 0
val x1142 = x1141 + x1138
val x1143 = x665.dcApply(x1142)
val x1144 = { 
x1143
}
val x1145 = x70.dcApply(x1138)
val x1146 = x1144 * x1145
val x1147 = {
val x1139 = new generated.scala.DoubleVectorImpl(x1099,true)
x1139
}
x1147.dcUpdate(x1138, x1146)
x1138 = 1
while (x1138 < x1099) {  // begin fat loop x1147
val x1142 = x1141 + x1138
val x1143 = x665.dcApply(x1142)
val x1144 = { 
x1143
}
val x1145 = x70.dcApply(x1138)
val x1146 = x1144 * x1145
x1147.dcUpdate(x1138, x1146)
x1138 += 1
} // end fat loop x1147
var x1148 = 0
val x1151 = x1147.dcApply(x1148)
var x1153: Double = {
if (x1099 == 0) {0.0}
else {
val x1151 = x1147.dcApply(x1148)
x1151
}
}
x1148 = 1
while (x1148 < x1099) {  // begin fat loop x1153
val x1151 = x1147.dcApply(x1148)
val x1149 = x1153
val x1150 = x1151
val x1152 = x1149 + x1150
x1153 = x1152
x1148 += 1
} // end fat loop x1153
val x1154 = x883(x1136) = x1153
x1154
}
x884 = 1
while (x884 < x689) {  // begin fat loop x886
x886 = {
val x1136 = x686.dcApply(x884)
val x1141 = x1136 * x1099
// a *thin* loop follows: x1147
var x1138 = 0
val x1142 = x1141 + x1138
val x1143 = x665.dcApply(x1142)
val x1144 = { 
x1143
}
val x1145 = x70.dcApply(x1138)
val x1146 = x1144 * x1145
val x1147 = {
val x1139 = new generated.scala.DoubleVectorImpl(x1099,true)
x1139
}
x1147.dcUpdate(x1138, x1146)
x1138 = 1
while (x1138 < x1099) {  // begin fat loop x1147
val x1142 = x1141 + x1138
val x1143 = x665.dcApply(x1142)
val x1144 = { 
x1143
}
val x1145 = x70.dcApply(x1138)
val x1146 = x1144 * x1145
x1147.dcUpdate(x1138, x1146)
x1138 += 1
} // end fat loop x1147
var x1148 = 0
val x1151 = x1147.dcApply(x1148)
var x1153: Double = {
if (x1099 == 0) {0.0}
else {
val x1151 = x1147.dcApply(x1148)
x1151
}
}
x1148 = 1
while (x1148 < x1099) {  // begin fat loop x1153
val x1151 = x1147.dcApply(x1148)
val x1149 = x1153
val x1150 = x1151
val x1152 = x1149 + x1150
x1153 = x1152
x1148 += 1
} // end fat loop x1153
val x1154 = x883(x1136) = x1153
x1154
}
x884 += 1
} // end fat loop x886
val x887 = x883// unsafe immutable
x887
}
val x890 = x882.numRows
val x892 = new generated.scala.IndexVectorRangeImpl(0,x890)
val x895 = x892.length
val x1159 = x882.numCols
val x899 = { 
val x891 = new generated.scala.DoubleVectorImpl(x890,false)
// a *thin* loop follows: x896
var x893 = 0
var x896: Unit = {
val x1156 = x892.dcApply(x893)
val x1162 = x1156 * x1159
// a *thin* loop follows: x1168
var x1158 = 0
val x1163 = x1162 + x1158
val x1164 = x882.dcApply(x1163)
val x1165 = { 
x1164
}
val x1166 = x889.dcApply(x1158)
val x1167 = x1165 * x1166
val x1168 = {
val x1160 = new generated.scala.DoubleVectorImpl(x1159,true)
x1160
}
x1168.dcUpdate(x1158, x1167)
x1158 = 1
while (x1158 < x1159) {  // begin fat loop x1168
val x1163 = x1162 + x1158
val x1164 = x882.dcApply(x1163)
val x1165 = { 
x1164
}
val x1166 = x889.dcApply(x1158)
val x1167 = x1165 * x1166
x1168.dcUpdate(x1158, x1167)
x1158 += 1
} // end fat loop x1168
var x1169 = 0
val x1172 = x1168.dcApply(x1169)
var x1174: Double = {
if (x1159 == 0) {0.0}
else {
val x1172 = x1168.dcApply(x1169)
x1172
}
}
x1169 = 1
while (x1169 < x1159) {  // begin fat loop x1174
val x1172 = x1168.dcApply(x1169)
val x1170 = x1174
val x1171 = x1172
val x1173 = x1170 + x1171
x1174 = x1173
x1169 += 1
} // end fat loop x1174
val x1175 = x891(x1156) = x1174
x1175
}
x893 = 1
while (x893 < x895) {  // begin fat loop x896
x896 = {
val x1156 = x892.dcApply(x893)
val x1162 = x1156 * x1159
// a *thin* loop follows: x1168
var x1158 = 0
val x1163 = x1162 + x1158
val x1164 = x882.dcApply(x1163)
val x1165 = { 
x1164
}
val x1166 = x889.dcApply(x1158)
val x1167 = x1165 * x1166
val x1168 = {
val x1160 = new generated.scala.DoubleVectorImpl(x1159,true)
x1160
}
x1168.dcUpdate(x1158, x1167)
x1158 = 1
while (x1158 < x1159) {  // begin fat loop x1168
val x1163 = x1162 + x1158
val x1164 = x882.dcApply(x1163)
val x1165 = { 
x1164
}
val x1166 = x889.dcApply(x1158)
val x1167 = x1165 * x1166
x1168.dcUpdate(x1158, x1167)
x1158 += 1
} // end fat loop x1168
var x1169 = 0
val x1172 = x1168.dcApply(x1169)
var x1174: Double = {
if (x1159 == 0) {0.0}
else {
val x1172 = x1168.dcApply(x1169)
x1172
}
}
x1169 = 1
while (x1169 < x1159) {  // begin fat loop x1174
val x1172 = x1168.dcApply(x1169)
val x1170 = x1174
val x1171 = x1172
val x1173 = x1170 + x1171
x1174 = x1173
x1169 += 1
} // end fat loop x1174
val x1175 = x891(x1156) = x1174
x1175
}
x893 += 1
} // end fat loop x896
val x897 = x891// unsafe immutable
x897
}
val x901 = x899.length
val x902 = x899.isRow
val x903 = !x902
// a *thin* loop follows: x907
var x900 = 0
val x906 = x899.dcApply(x900)
val x907 = {
val x904 = new generated.scala.DoubleVectorImpl(x901,x903)
x904
}
x907.dcUpdate(x900, x906)
x900 = 1
while (x900 < x901) {  // begin fat loop x907
val x906 = x899.dcApply(x900)
x907.dcUpdate(x900, x906)
x900 += 1
} // end fat loop x907
// a *thin* loop follows: x916
var x909 = 0
val x913 = x603 + x909
val x914 = x573.dcApply(x913)
val x915 = { 
x914
}
val x916 = {
val x911 = new generated.scala.DoubleVectorImpl(x602,x910)
x911
}
x916.dcUpdate(x909, x915)
x909 = 1
while (x909 < x602) {  // begin fat loop x916
val x913 = x603 + x909
val x914 = x573.dcApply(x913)
val x915 = { 
x914
}
x916.dcUpdate(x909, x915)
x909 += 1
} // end fat loop x916
val x918 = x907.isRow
// a *thin* loop follows: x924
var x917 = 0
val x921 = x907.dcApply(x917)
val x922 = x916.dcApply(x917)
val x923 = x921 * x922
val x924 = {
val x919 = new generated.scala.DoubleVectorImpl(x901,x918)
x919
}
x924.dcUpdate(x917, x923)
x917 = 1
while (x917 < x901) {  // begin fat loop x924
val x921 = x907.dcApply(x917)
val x922 = x916.dcApply(x917)
val x923 = x921 * x922
x924.dcUpdate(x917, x923)
x917 += 1
} // end fat loop x924
var x925 = 0
val x928 = x924.dcApply(x925)
var x930: Double = {
if (x901 == 0) {0.0}
else {
val x928 = x924.dcApply(x925)
x928
}
}
x925 = 1
while (x925 < x901) {  // begin fat loop x930
val x928 = x924.dcApply(x925)
val x926 = x930
val x927 = x928
val x929 = x926 + x927
x930 = x929
x925 += 1
} // end fat loop x930
val x932 = {
val x599 = new generated.scala.DoubleVectorImpl(x597,x598)
x599
}
x932.dcUpdate(x596, x930)
x596 = 1
while (x596 < x597) {  // begin fat loop x932
val x601 = x595.dcApply(x596)
val x603 = x601 * x602
val x604 = x603 + 1
val x605 = x573.dcApply(x604)
val x606 = { 
x605
}
// a *thin* loop follows: x618
var x607 = 0
val x611 = x38.dcApply(x607)
val x612 = x606 - x611
val x613 = -0.1 * x612
val x614 = x613 * x612
val x615 = x614 / 200.0
val x616 = Math.exp(x615)
val x617 = x616 / 2.0
val x618 = {
val x609 = new generated.scala.DoubleVectorImpl(x32,x608)
x609
}
x618.dcUpdate(x607, x617)
x607 = 1
while (x607 < x32) {  // begin fat loop x618
val x611 = x38.dcApply(x607)
val x612 = x606 - x611
val x613 = -0.1 * x612
val x614 = x613 * x612
val x615 = x614 / 200.0
val x616 = Math.exp(x615)
val x617 = x616 / 2.0
x618.dcUpdate(x607, x617)
x607 += 1
} // end fat loop x618
val x634 = { 
val x619 = new generated.scala.DoubleMatrixImpl(x32,x32)
var x620: Int = 0
val x631 = while ({val x621 = x620
val x622 = x621 < x32
x622}) {
val x624 = x620
val x625 = x620
val x626 = x620
val x627 = x618(x626)
val x628 = x619(x624, x625) = x627
val x629 = x620 += 1
()
}
val x632 = x619// unsafe immutable
x632
}
val x635 = x634.numCols
val x636 = x634.numRows
val x655 = { 
val x637 = new generated.scala.DoubleMatrixImpl(x635,x636)
val x638 = x637.numRows
var x640 : Int = 0
val x652 = while (x640 < x638) {
val x641 = x637.numCols
var x643 : Int = 0
val x650 = while (x643 < x641) {
val x644 = x643 * x635
val x645 = x644 + x640
val x646 = x634.dcApply(x645)
val x647 = { 
x646
}
val x648 = x637(x640, x643) = x647
x648
x643 = x643 + 1
}
x650
x640 = x640 + 1
}
val x653 = x637// unsafe immutable
x653
}
val x1060 = x655.numCols
val x665 = { 
val x657 = new generated.scala.DoubleMatrixImpl(x656,x635)
// a *thin* loop follows: x662
var x659 = 0
var x662: Unit = {
var x1048: Int = 0
val x1047 = x658.dcApply(x659)
val x1055 = x1047 * x1054
val x1056 = x1055 + 0
val x1057 = x593.dcApply(x1056)
val x1058 = { 
x1057
}
val x1090 = while ({val x1049 = x1048
val x1050 = x657.numCols
val x1051 = x1049 < x1050
x1051}) {
var x1053: Int = 1
val x1059 = x1048
val x1061 = x1059 * x1060
val x1062 = x1061 + 0
val x1063 = x655.dcApply(x1062)
val x1064 = { 
x1063
}
val x1065 = x1058 * x1064
var x1066: Double = x1065
val x1084 = while ({val x1067 = x1053
val x1068 = x1067 < x1060
x1068}) {
val x1070 = x1053
val x1074 = x1048
val x1075 = x1053
val x1071 = x1055 + x1070
val x1072 = x593.dcApply(x1071)
val x1073 = { 
x1072
}
val x1076 = x1074 * x1060
val x1077 = x1076 + x1075
val x1078 = x655.dcApply(x1077)
val x1079 = { 
x1078
}
val x1080 = x1073 * x1079
val x1081 = x1066 += x1080
val x1082 = x1053 += 1
()
}
val x1085 = x1048
val x1086 = x1066
val x1087 = x657(x1047, x1085) = x1086
val x1088 = x1048 += 1
()
}
()
}
x659 = 1
while (x659 < x661) {  // begin fat loop x662
x662 = {
var x1048: Int = 0
val x1047 = x658.dcApply(x659)
val x1055 = x1047 * x1054
val x1056 = x1055 + 0
val x1057 = x593.dcApply(x1056)
val x1058 = { 
x1057
}
val x1090 = while ({val x1049 = x1048
val x1050 = x657.numCols
val x1051 = x1049 < x1050
x1051}) {
var x1053: Int = 1
val x1059 = x1048
val x1061 = x1059 * x1060
val x1062 = x1061 + 0
val x1063 = x655.dcApply(x1062)
val x1064 = { 
x1063
}
val x1065 = x1058 * x1064
var x1066: Double = x1065
val x1084 = while ({val x1067 = x1053
val x1068 = x1067 < x1060
x1068}) {
val x1070 = x1053
val x1074 = x1048
val x1075 = x1053
val x1071 = x1055 + x1070
val x1072 = x593.dcApply(x1071)
val x1073 = { 
x1072
}
val x1076 = x1074 * x1060
val x1077 = x1076 + x1075
val x1078 = x655.dcApply(x1077)
val x1079 = { 
x1078
}
val x1080 = x1073 * x1079
val x1081 = x1066 += x1080
val x1082 = x1053 += 1
()
}
val x1085 = x1048
val x1086 = x1066
val x1087 = x657(x1047, x1085) = x1086
val x1088 = x1048 += 1
()
}
()
}
x659 += 1
} // end fat loop x662
val x663 = x657// unsafe immutable
x663
}
val x684 = x665.numRows
val x686 = new generated.scala.IndexVectorRangeImpl(0,x684)
val x689 = x686.length
val x1099 = x665.numCols
val x693 = { 
val x685 = new generated.scala.DoubleMatrixImpl(x684,x574)
// a *thin* loop follows: x690
var x687 = 0
var x690: Unit = {
var x1093: Int = 0
val x1092 = x686.dcApply(x687)
val x1100 = x1092 * x1099
val x1101 = x1100 + 0
val x1102 = x665.dcApply(x1101)
val x1103 = { 
x1102
}
val x1134 = while ({val x1094 = x1093
val x1095 = x685.numCols
val x1096 = x1094 < x1095
x1096}) {
var x1098: Int = 1
val x1104 = x1093
val x1105 = x1104 * x1054
val x1106 = x1105 + 0
val x1107 = x593.dcApply(x1106)
val x1108 = { 
x1107
}
val x1109 = x1103 * x1108
var x1110: Double = x1109
val x1128 = while ({val x1111 = x1098
val x1112 = x1111 < x1054
x1112}) {
val x1114 = x1098
val x1118 = x1093
val x1119 = x1098
val x1115 = x1100 + x1114
val x1116 = x665.dcApply(x1115)
val x1117 = { 
x1116
}
val x1120 = x1118 * x1054
val x1121 = x1120 + x1119
val x1122 = x593.dcApply(x1121)
val x1123 = { 
x1122
}
val x1124 = x1117 * x1123
val x1125 = x1110 += x1124
val x1126 = x1098 += 1
()
}
val x1129 = x1093
val x1130 = x1110
val x1131 = x685(x1092, x1129) = x1130
val x1132 = x1093 += 1
()
}
()
}
x687 = 1
while (x687 < x689) {  // begin fat loop x690
x690 = {
var x1093: Int = 0
val x1092 = x686.dcApply(x687)
val x1100 = x1092 * x1099
val x1101 = x1100 + 0
val x1102 = x665.dcApply(x1101)
val x1103 = { 
x1102
}
val x1134 = while ({val x1094 = x1093
val x1095 = x685.numCols
val x1096 = x1094 < x1095
x1096}) {
var x1098: Int = 1
val x1104 = x1093
val x1105 = x1104 * x1054
val x1106 = x1105 + 0
val x1107 = x593.dcApply(x1106)
val x1108 = { 
x1107
}
val x1109 = x1103 * x1108
var x1110: Double = x1109
val x1128 = while ({val x1111 = x1098
val x1112 = x1111 < x1054
x1112}) {
val x1114 = x1098
val x1118 = x1093
val x1119 = x1098
val x1115 = x1100 + x1114
val x1116 = x665.dcApply(x1115)
val x1117 = { 
x1116
}
val x1120 = x1118 * x1054
val x1121 = x1120 + x1119
val x1122 = x593.dcApply(x1121)
val x1123 = { 
x1122
}
val x1124 = x1117 * x1123
val x1125 = x1110 += x1124
val x1126 = x1098 += 1
()
}
val x1129 = x1093
val x1130 = x1110
val x1131 = x685(x1092, x1129) = x1130
val x1132 = x1093 += 1
()
}
()
}
x687 += 1
} // end fat loop x690
val x691 = x685// unsafe immutable
x691
}
val x694 = x693.numCols
val x720 = x693.numRows
val x721 = x720 * x694
val x717 = { 
val x703 = new generated.scala.DoubleMatrixImpl(x694,x694)
var x704: Int = 0
val x714 = while ({val x705 = x704
val x706 = x705 < x694
x706}) {
val x708 = x704
val x709 = x704
val x710 = x704
val x711 = x703(x708, x709) = 1
val x712 = x704 += 1
()
}
val x715 = x703// unsafe immutable
x715
}
val x718 = { 
x717
}
val x882 = { 
// a *thin* loop follows: x725
var x719 = 0
val x724 = x693.dcApply(x719)
val x725 = {
val x722 = new generated.scala.DoubleMatrixImpl(x720,x694)
x722
}
x725.dcUpdate(x719, x724)
x719 = 1
while (x719 < x721) {  // begin fat loop x725
val x724 = x693.dcApply(x719)
x725.dcUpdate(x719, x724)
x719 += 1
} // end fat loop x725
val x726 = x725.cloneL
val x727 = x726.numCols
val x728 = x726.insertAllCols(x727,x718)
var x729: Int = 0
var x730: Boolean = false
var x731: Int = 0
val x878 = while ({val x732 = x730
val x734 = x731
val x735 = x726.numRows
val x733 = !x732
val x736 = x734 < x735
val x737 = x733 && x736
x737}) {
val x739 = x726.numRows
val x740 = x729
val x741 = x739 <= x740
val x744 = {
def x744thenb(): Unit = {
x730 = true
()
}
if (x741) {
x744thenb()
}
}
val x745 = x730
val x746 = !x745
val x875 = {
def x875thenb(): Unit = {
val x747 = x731
var x748: Int = x747
val x778 = while ({val x749 = x730
val x751 = x748
val x752 = x729
val x758 = { 
val x753 = x726.numCols
val x754 = x751 * x753
val x755 = x754 + x752
val x756 = x726.dcApply(x755)
x756
}
val x750 = !x749
val x759 = x758==0.0
val x760 = x750 && x759
x760}) {
val x762 = x748 += 1
val x763 = x726.numCols
val x764 = x748
val x765 = x763==x764
val x776 = {
def x776thenb(): Unit = {
val x766 = x731
x748 = x766
val x768 = x729 += 1
val x769 = x726.numRows
val x770 = x729
val x771 = x769==x770
val x774 = {
def x774thenb(): Unit = {
x730 = true
()
}
if (x771) {
x774thenb()
}
}
x774
}
if (x765) {
x776thenb()
}
}
x776
}
val x779 = x730
val x780 = !x779
val x873 = {
def x873thenb(): Unit = {
val x781 = x748
val x782 = x726.getRow(x781)
val x783 = x731
val x784 = x726.getRow(x783)
val x786 = x726.numCols
// a *thin* loop follows: x790
var x785 = 0
var x790: Unit = {
val x787 = x784(x785)
val x788 = x726(x781, x785) = x787
x788
}
x785 = 1
while (x785 < x786) {  // begin fat loop x790
x790 = {
val x787 = x784(x785)
val x788 = x726(x781, x785) = x787
x788
}
x785 += 1
} // end fat loop x790
val x791 = x731
val x793 = x726.numCols
// a *thin* loop follows: x797
var x792 = 0
var x797: Unit = {
val x794 = x782(x792)
val x795 = x726(x791, x792) = x794
x795
}
x792 = 1
while (x792 < x793) {  // begin fat loop x797
x797 = {
val x794 = x782(x792)
val x795 = x726(x791, x792) = x794
x795
}
x792 += 1
} // end fat loop x797
val x798 = x731
val x799 = x731
val x800 = x726.getRow(x799)
val x801 = x731
val x802 = x729
val x808 = { 
val x803 = x726.numCols
val x804 = x801 * x803
val x805 = x804 + x802
val x806 = x726.dcApply(x805)
x806
}
val x810 = x800.length
// a *thin* loop follows: x818
var x809 = 0
val x815 = x800.dcApply(x809)
val x816 = x815 / x808
val x818 = {
val x811 = x800.length
val x812 = x800.isRow
val x813 = new generated.scala.DoubleVectorImpl(x811,x812)
x813
}
x818.dcUpdate(x809, x816)
x809 = 1
while (x809 < x810) {  // begin fat loop x818
val x815 = x800.dcApply(x809)
val x816 = x815 / x808
x818.dcUpdate(x809, x816)
x809 += 1
} // end fat loop x818
val x820 = x726.numCols
// a *thin* loop follows: x824
var x819 = 0
var x824: Unit = {
val x821 = x818(x819)
val x822 = x726(x798, x819) = x821
x822
}
x819 = 1
while (x819 < x820) {  // begin fat loop x824
x824 = {
val x821 = x818(x819)
val x822 = x726(x798, x819) = x821
x822
}
x819 += 1
} // end fat loop x824
val x825 = x726.numRows
var x827 : Int = 0
val x870 = while (x827 < x825) {
val x828 = x731
val x829 = x827 != x828
val x868 = {
def x868thenb(): Unit = {
val x830 = x726.getRow(x827)
val x831 = x731
val x832 = x726.getRow(x831)
val x833 = x729
val x839 = { 
val x834 = x726.numCols
val x835 = x827 * x834
val x836 = x835 + x833
val x837 = x726.dcApply(x836)
x837
}
val x841 = x832.length
// a *thin* loop follows: x849
var x840 = 0
val x846 = x832.dcApply(x840)
val x847 = x846 * x839
val x849 = {
val x842 = x832.length
val x843 = x832.isRow
val x844 = new generated.scala.DoubleVectorImpl(x842,x843)
x844
}
x849.dcUpdate(x840, x847)
x840 = 1
while (x840 < x841) {  // begin fat loop x849
val x846 = x832.dcApply(x840)
val x847 = x846 * x839
x849.dcUpdate(x840, x847)
x840 += 1
} // end fat loop x849
val x851 = x830.length
// a *thin* loop follows: x860
var x850 = 0
val x856 = x830.dcApply(x850)
val x857 = x849.dcApply(x850)
val x858 = x856 - x857
val x860 = {
val x852 = x830.length
val x853 = x830.isRow
val x854 = new generated.scala.DoubleVectorImpl(x852,x853)
x854
}
x860.dcUpdate(x850, x858)
x850 = 1
while (x850 < x851) {  // begin fat loop x860
val x856 = x830.dcApply(x850)
val x857 = x849.dcApply(x850)
val x858 = x856 - x857
x860.dcUpdate(x850, x858)
x850 += 1
} // end fat loop x860
val x862 = x726.numCols
// a *thin* loop follows: x866
var x861 = 0
var x866: Unit = {
val x863 = x860(x861)
val x864 = x726(x827, x861) = x863
x864
}
x861 = 1
while (x861 < x862) {  // begin fat loop x866
x866 = {
val x863 = x860(x861)
val x864 = x726(x827, x861) = x863
x864
}
x861 += 1
} // end fat loop x866
x866
}
if (x829) {
x868thenb()
}
}
x868
x827 = x827 + 1
}
val x871 = x729 += 1
()
}
if (x780) {
x873thenb()
}
}
x873
}
if (x746) {
x875thenb()
}
}
val x876 = x731 += 1
()
}
val x879 = x726.removeCols(0,x694)
val x880 = x726// unsafe immutable
x880
}
val x889 = { 
val x883 = new generated.scala.DoubleVectorImpl(x684,false)
// a *thin* loop follows: x886
var x884 = 0
var x886: Unit = {
val x1136 = x686.dcApply(x884)
val x1141 = x1136 * x1099
// a *thin* loop follows: x1147
var x1138 = 0
val x1142 = x1141 + x1138
val x1143 = x665.dcApply(x1142)
val x1144 = { 
x1143
}
val x1145 = x70.dcApply(x1138)
val x1146 = x1144 * x1145
val x1147 = {
val x1139 = new generated.scala.DoubleVectorImpl(x1099,true)
x1139
}
x1147.dcUpdate(x1138, x1146)
x1138 = 1
while (x1138 < x1099) {  // begin fat loop x1147
val x1142 = x1141 + x1138
val x1143 = x665.dcApply(x1142)
val x1144 = { 
x1143
}
val x1145 = x70.dcApply(x1138)
val x1146 = x1144 * x1145
x1147.dcUpdate(x1138, x1146)
x1138 += 1
} // end fat loop x1147
var x1148 = 0
val x1151 = x1147.dcApply(x1148)
var x1153: Double = {
if (x1099 == 0) {0.0}
else {
val x1151 = x1147.dcApply(x1148)
x1151
}
}
x1148 = 1
while (x1148 < x1099) {  // begin fat loop x1153
val x1151 = x1147.dcApply(x1148)
val x1149 = x1153
val x1150 = x1151
val x1152 = x1149 + x1150
x1153 = x1152
x1148 += 1
} // end fat loop x1153
val x1154 = x883(x1136) = x1153
x1154
}
x884 = 1
while (x884 < x689) {  // begin fat loop x886
x886 = {
val x1136 = x686.dcApply(x884)
val x1141 = x1136 * x1099
// a *thin* loop follows: x1147
var x1138 = 0
val x1142 = x1141 + x1138
val x1143 = x665.dcApply(x1142)
val x1144 = { 
x1143
}
val x1145 = x70.dcApply(x1138)
val x1146 = x1144 * x1145
val x1147 = {
val x1139 = new generated.scala.DoubleVectorImpl(x1099,true)
x1139
}
x1147.dcUpdate(x1138, x1146)
x1138 = 1
while (x1138 < x1099) {  // begin fat loop x1147
val x1142 = x1141 + x1138
val x1143 = x665.dcApply(x1142)
val x1144 = { 
x1143
}
val x1145 = x70.dcApply(x1138)
val x1146 = x1144 * x1145
x1147.dcUpdate(x1138, x1146)
x1138 += 1
} // end fat loop x1147
var x1148 = 0
val x1151 = x1147.dcApply(x1148)
var x1153: Double = {
if (x1099 == 0) {0.0}
else {
val x1151 = x1147.dcApply(x1148)
x1151
}
}
x1148 = 1
while (x1148 < x1099) {  // begin fat loop x1153
val x1151 = x1147.dcApply(x1148)
val x1149 = x1153
val x1150 = x1151
val x1152 = x1149 + x1150
x1153 = x1152
x1148 += 1
} // end fat loop x1153
val x1154 = x883(x1136) = x1153
x1154
}
x884 += 1
} // end fat loop x886
val x887 = x883// unsafe immutable
x887
}
val x890 = x882.numRows
val x892 = new generated.scala.IndexVectorRangeImpl(0,x890)
val x895 = x892.length
val x1159 = x882.numCols
val x899 = { 
val x891 = new generated.scala.DoubleVectorImpl(x890,false)
// a *thin* loop follows: x896
var x893 = 0
var x896: Unit = {
val x1156 = x892.dcApply(x893)
val x1162 = x1156 * x1159
// a *thin* loop follows: x1168
var x1158 = 0
val x1163 = x1162 + x1158
val x1164 = x882.dcApply(x1163)
val x1165 = { 
x1164
}
val x1166 = x889.dcApply(x1158)
val x1167 = x1165 * x1166
val x1168 = {
val x1160 = new generated.scala.DoubleVectorImpl(x1159,true)
x1160
}
x1168.dcUpdate(x1158, x1167)
x1158 = 1
while (x1158 < x1159) {  // begin fat loop x1168
val x1163 = x1162 + x1158
val x1164 = x882.dcApply(x1163)
val x1165 = { 
x1164
}
val x1166 = x889.dcApply(x1158)
val x1167 = x1165 * x1166
x1168.dcUpdate(x1158, x1167)
x1158 += 1
} // end fat loop x1168
var x1169 = 0
val x1172 = x1168.dcApply(x1169)
var x1174: Double = {
if (x1159 == 0) {0.0}
else {
val x1172 = x1168.dcApply(x1169)
x1172
}
}
x1169 = 1
while (x1169 < x1159) {  // begin fat loop x1174
val x1172 = x1168.dcApply(x1169)
val x1170 = x1174
val x1171 = x1172
val x1173 = x1170 + x1171
x1174 = x1173
x1169 += 1
} // end fat loop x1174
val x1175 = x891(x1156) = x1174
x1175
}
x893 = 1
while (x893 < x895) {  // begin fat loop x896
x896 = {
val x1156 = x892.dcApply(x893)
val x1162 = x1156 * x1159
// a *thin* loop follows: x1168
var x1158 = 0
val x1163 = x1162 + x1158
val x1164 = x882.dcApply(x1163)
val x1165 = { 
x1164
}
val x1166 = x889.dcApply(x1158)
val x1167 = x1165 * x1166
val x1168 = {
val x1160 = new generated.scala.DoubleVectorImpl(x1159,true)
x1160
}
x1168.dcUpdate(x1158, x1167)
x1158 = 1
while (x1158 < x1159) {  // begin fat loop x1168
val x1163 = x1162 + x1158
val x1164 = x882.dcApply(x1163)
val x1165 = { 
x1164
}
val x1166 = x889.dcApply(x1158)
val x1167 = x1165 * x1166
x1168.dcUpdate(x1158, x1167)
x1158 += 1
} // end fat loop x1168
var x1169 = 0
val x1172 = x1168.dcApply(x1169)
var x1174: Double = {
if (x1159 == 0) {0.0}
else {
val x1172 = x1168.dcApply(x1169)
x1172
}
}
x1169 = 1
while (x1169 < x1159) {  // begin fat loop x1174
val x1172 = x1168.dcApply(x1169)
val x1170 = x1174
val x1171 = x1172
val x1173 = x1170 + x1171
x1174 = x1173
x1169 += 1
} // end fat loop x1174
val x1175 = x891(x1156) = x1174
x1175
}
x893 += 1
} // end fat loop x896
val x897 = x891// unsafe immutable
x897
}
val x901 = x899.length
val x902 = x899.isRow
val x903 = !x902
// a *thin* loop follows: x907
var x900 = 0
val x906 = x899.dcApply(x900)
val x907 = {
val x904 = new generated.scala.DoubleVectorImpl(x901,x903)
x904
}
x907.dcUpdate(x900, x906)
x900 = 1
while (x900 < x901) {  // begin fat loop x907
val x906 = x899.dcApply(x900)
x907.dcUpdate(x900, x906)
x900 += 1
} // end fat loop x907
// a *thin* loop follows: x916
var x909 = 0
val x913 = x603 + x909
val x914 = x573.dcApply(x913)
val x915 = { 
x914
}
val x916 = {
val x911 = new generated.scala.DoubleVectorImpl(x602,x910)
x911
}
x916.dcUpdate(x909, x915)
x909 = 1
while (x909 < x602) {  // begin fat loop x916
val x913 = x603 + x909
val x914 = x573.dcApply(x913)
val x915 = { 
x914
}
x916.dcUpdate(x909, x915)
x909 += 1
} // end fat loop x916
val x918 = x907.isRow
// a *thin* loop follows: x924
var x917 = 0
val x921 = x907.dcApply(x917)
val x922 = x916.dcApply(x917)
val x923 = x921 * x922
val x924 = {
val x919 = new generated.scala.DoubleVectorImpl(x901,x918)
x919
}
x924.dcUpdate(x917, x923)
x917 = 1
while (x917 < x901) {  // begin fat loop x924
val x921 = x907.dcApply(x917)
val x922 = x916.dcApply(x917)
val x923 = x921 * x922
x924.dcUpdate(x917, x923)
x917 += 1
} // end fat loop x924
var x925 = 0
val x928 = x924.dcApply(x925)
var x930: Double = {
if (x901 == 0) {0.0}
else {
val x928 = x924.dcApply(x925)
x928
}
}
x925 = 1
while (x925 < x901) {  // begin fat loop x930
val x928 = x924.dcApply(x925)
val x926 = x930
val x927 = x928
val x929 = x926 + x927
x930 = x929
x925 += 1
} // end fat loop x930
x932.dcUpdate(x596, x930)
x596 += 1
} // end fat loop x932
val x933 = Seq(x932)
val x934 = ppl.delite.runtime.profiler.PerformanceTimer.stop("app", false)
val x935 = println("Locally weighted linear regression")
val x936 = println("guess: ")
val x937 = x932.isRow
val x958 = { 
val x956 = {
def x956thenb(): Unit = {
val x938 = print("[ ")
var x940 : Int = 0
val x945 = while (x940 < x597) {
val x941 = x932(x940)
val x942 = print(x941)
val x943 = print(" ")
x943
x940 = x940 + 1
}
val x946 = print("]\n")
x946
}
def x956elseb(): Unit = {
var x948 : Int = 0
val x954 = while (x948 < x597) {
val x949 = print("[")
val x950 = x932(x948)
val x951 = print(x950)
val x952 = print(" ]\n")
x952
x948 = x948 + 1
}
x954
}
if (x937) {
x956thenb()
} else { 
x956elseb()
}
}
x956
}
val x959 = print("\n")
()
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
