/*****************************************
  Emitting Generated Code                  
*******************************************/
class Application extends ((Array[java.lang.String])=>(Unit)) {
def apply(x0:Array[java.lang.String]): Unit = {
val x1 = { generated.scala.Global.randRef.setSeed(generated.scala.Global.INITIAL_SEED);   generated.scala.Global.intRandRef.setSeed(generated.scala.Global.INITIAL_SEED); }
val x2 = x0.length
val x3 = x2 < 3
val x7 = {
def x7thenb(): Nothing = {
val x4 = println("Usage: RBM <MNIST data file> <numHiddenUnits> <numcases>")
val x5 = exit(-1)
x5
}
if (x3) {
x7thenb()
}
}
val x8 = x0(1)
val x9 = java.lang.Integer.parseInt(x8)
val x10 = "Using "+x9
val x11 = x10+" hidden units."
val x12 = println(x11)
val x13 = println("Reading MNIST dataset")
val x16 = x0(0)
val x65 = { 
val x17 = new java.io.FileReader(x16)
val x18 = new java.io.BufferedReader(x17)
val x19 = x18.readLine()
var x20: java.lang.String = x19
val x21 = x20
val x22 = x21.trim()
x20 = x22
val x24 = x20
val x25 = x24.split("\\s+")
var x26: Array[java.lang.String] = x25
val x27 = x26
val x28 = x27.length
val x29 = new generated.scala.DoubleMatrixImpl(0,x28)
val x61 = while ({val x30 = x20
val x31 = x30 != null
x31}) {
val x33 = x26
val x34 = x33.length
val x35 = new generated.scala.IndexVectorRangeImpl(0,x34)
val x37 = x35.length
val x38 = x35.isRow
// a *thin* loop follows: x46
var x36 = 0
val x42 = x26
val x41 = x35.dcApply(x36)
val x43 = x42(x41)
val x44 = java.lang.Double.parseDouble(x43)
val x46 = {
val x39 = new generated.scala.DoubleVectorImpl(x37,x38)
x39
}
x46.dcUpdate(x36, x44)
x36 = 1
while (x36 < x37) {  // begin fat loop x46
val x42 = x26
val x41 = x35.dcApply(x36)
val x43 = x42(x41)
val x44 = java.lang.Double.parseDouble(x43)
x46.dcUpdate(x36, x44)
x36 += 1
} // end fat loop x46
val x47 = x29.numRows
val x48 = x29.insertRow(x47,x46)
val x49 = x18.readLine()
x20 = x49
val x51 = x20
val x52 = x51 != null
val x59 = {
def x59thenb(): Unit = {
val x53 = x51.trim()
x20 = x53
val x55 = x20
val x56 = x55.split("\\s+")
x26 = x56
()
}
if (x52) {
x59thenb()
}
}
x59
}
val x62 = x18.close()
val x63 = x29// unsafe immutable
x63
}
val x67 = x65.numRows
val x68 = x65.numCols
val x69 = x67 * x68
// a *thin* loop follows: x74
var x66 = 0
val x72 = x65.dcApply(x66)
val x73 = x72.floatValue()
val x74 = {
val x70 = new generated.scala.FloatMatrixImpl(x67,x68)
x70
}
x74.dcUpdate(x66, x73)
x66 = 1
while (x66 < x69) {  // begin fat loop x74
val x72 = x65.dcApply(x66)
val x73 = x72.floatValue()
x74.dcUpdate(x66, x73)
x66 += 1
} // end fat loop x74
val x75 = x74.numCols
val x89 = { 
val x78 = new generated.scala.FloatMatrixImpl(x75,x9)
val x80 = x78.numRows
val x81 = x78.numCols
val x82 = x80 * x81
// a *thin* loop follows: x87
var x79 = 0
val x83 = x78.dcApply(x79)
val x84 = generated.scala.Global.randRef.nextGaussian()
val x85 = x84.floatValue()
val x87 = {
x78
}
x87.dcUpdate(x79, x85)
x79 = 1
while (x79 < x82) {  // begin fat loop x87
val x83 = x78.dcApply(x79)
val x84 = generated.scala.Global.randRef.nextGaussian()
val x85 = x84.floatValue()
x87.dcUpdate(x79, x85)
x79 += 1
} // end fat loop x87
x87
}
val x91 = x89.numRows
val x92 = x89.numCols
val x93 = x91 * x92
// a *thin* loop follows: x98
var x90 = 0
val x96 = x89.dcApply(x90)
val x97 = x96 * 0.1f
val x98 = {
val x94 = new generated.scala.FloatMatrixImpl(x91,x92)
x94
}
x98.dcUpdate(x90, x97)
x90 = 1
while (x90 < x93) {  // begin fat loop x98
val x96 = x89.dcApply(x90)
val x97 = x96 * 0.1f
x98.dcUpdate(x90, x97)
x90 += 1
} // end fat loop x98
val x99 = x98.cloneL
val x103 = { 
val x100 = new generated.scala.FloatVectorImpl(x9,true)
x100
}
val x107 = { 
val x104 = new generated.scala.FloatVectorImpl(x75,true)
x104
}
val x110 = { 
val x108 = new generated.scala.FloatMatrixImpl(x75,x9)
x108
}
var x111: generated.scala.Matrix[Float] = x110
val x102 = { 
val x100 = new generated.scala.FloatVectorImpl(x9,true)
x100
}
var x114: generated.scala.Vector[Float] = x102
val x106 = { 
val x104 = new generated.scala.FloatVectorImpl(x75,true)
x104
}
var x117: generated.scala.Vector[Float] = x106
val x118 = Seq()
val x119 = ppl.delite.runtime.profiler.PerformanceTimer.start("app", false)
var x120: Int = 0
val x14 = x0(2)
val x15 = java.lang.Integer.parseInt(x14)
val x76 = x74.numRows
val x77 = x76 / x15
val x884 = List()
val x336 = { 
val x325 = new generated.scala.FloatMatrixImpl(x15,x9)
val x327 = x325.numRows
val x328 = x325.numCols
val x329 = x327 * x328
// a *thin* loop follows: x334
var x326 = 0
val x330 = x325.dcApply(x326)
val x331 = generated.scala.Global.randRef.nextFloat()
val x332 = x331.asInstanceOf[Float]
val x334 = {
x325
}
x334.dcUpdate(x326, x332)
x326 = 1
while (x326 < x329) {  // begin fat loop x334
val x330 = x325.dcApply(x326)
val x331 = generated.scala.Global.randRef.nextFloat()
val x332 = x331.asInstanceOf[Float]
x334.dcUpdate(x326, x332)
x326 += 1
} // end fat loop x334
x334
}
val x675 = x15
val x745 = 0.1f / x675
val x833 = while ({val x121 = x120
val x122 = x121 < 10
x122}) {
var x124: Float = 0.0f
var x125: Int = 0
val x824 = while ({val x126 = x125
val x127 = x126 < x77
x127}) {
val x129 = x125
val x131 = x125
val x130 = x129 * x15
val x132 = x131 + 1
val x133 = x132 * x15
val x134 = x133 - x130
val x162 = { 
val x135 = new generated.scala.FloatMatrixImpl(x134,x75)
var x136: Int = x130
val x159 = while ({val x137 = x136
val x138 = x137 < x133
x138}) {
var x140: Int = 0
val x156 = while ({val x141 = x140
val x142 = x141 < x75
x142}) {
val x144 = x136
val x146 = x140
val x147 = x136
val x148 = x140
val x145 = x144 - x130
val x149 = x147 * x75
val x150 = x149 + x148
val x151 = x74.dcApply(x150)
val x152 = { 
x151
}
val x153 = x135(x145, x146) = x152
val x154 = x140 += 1
()
}
val x157 = x136 += 1
()
}
val x160 = x135// unsafe immutable
x160
}
val x186 = x162.numRows
val x189 = new generated.scala.IndexVectorRangeImpl(0,x186)
val x192 = x189.length
val x256 = x162.numCols
val x196 = { 
val x185 = { 
val x163 = x99.numCols
val x164 = x99.numRows
val x165 = new generated.scala.FloatMatrixImpl(x163,x164)
val x166 = x165.numRows
var x168 : Int = 0
val x182 = while (x168 < x166) {
val x169 = x165.numCols
var x171 : Int = 0
val x180 = while (x171 < x169) {
val x177 = { 
val x172 = x99.numCols
val x173 = x171 * x172
val x174 = x173 + x168
val x175 = x99.dcApply(x174)
x175
}
val x178 = x165(x168, x171) = x177
x178
x171 = x171 + 1
}
x180
x168 = x168 + 1
}
val x183 = x165// unsafe immutable
x183
}
val x187 = x99.numCols
val x188 = new generated.scala.FloatMatrixImpl(x186,x187)
// a *thin* loop follows: x193
var x190 = 0
var x193: Unit = {
var x837: Int = 0
val x836 = x189.dcApply(x190)
val x843 = x836 * x256
val x844 = x843 + 0
val x845 = x162.dcApply(x844)
val x846 = { 
x845
}
val x882 = while ({val x838 = x837
val x839 = x188.numCols
val x840 = x838 < x839
x840}) {
var x842: Int = 1
val x847 = x837
val x853 = { 
val x848 = x185.numCols
val x849 = x847 * x848
val x850 = x849 + 0
val x851 = x185.dcApply(x850)
x851
}
val x854 = x846 * x853
var x855: Float = x854
val x876 = while ({val x856 = x842
val x857 = x185.numCols
val x858 = x856 < x857
x858}) {
val x860 = x842
val x864 = x837
val x865 = x842
val x871 = { 
val x866 = x185.numCols
val x867 = x864 * x866
val x868 = x867 + x865
val x869 = x185.dcApply(x868)
x869
}
val x861 = x843 + x860
val x862 = x162.dcApply(x861)
val x863 = { 
x862
}
val x872 = x863 * x871
val x873 = x855 += x872
val x874 = x842 += 1
()
}
val x877 = x837
val x878 = x855
val x879 = x188(x836, x877) = x878
val x880 = x837 += 1
()
}
()
}
x190 = 1
while (x190 < x192) {  // begin fat loop x193
x193 = {
var x837: Int = 0
val x836 = x189.dcApply(x190)
val x843 = x836 * x256
val x844 = x843 + 0
val x845 = x162.dcApply(x844)
val x846 = { 
x845
}
val x882 = while ({val x838 = x837
val x839 = x188.numCols
val x840 = x838 < x839
x840}) {
var x842: Int = 1
val x847 = x837
val x853 = { 
val x848 = x185.numCols
val x849 = x847 * x848
val x850 = x849 + 0
val x851 = x185.dcApply(x850)
x851
}
val x854 = x846 * x853
var x855: Float = x854
val x876 = while ({val x856 = x842
val x857 = x185.numCols
val x858 = x856 < x857
x858}) {
val x860 = x842
val x864 = x837
val x865 = x842
val x871 = { 
val x866 = x185.numCols
val x867 = x864 * x866
val x868 = x867 + x865
val x869 = x185.dcApply(x868)
x869
}
val x861 = x843 + x860
val x862 = x162.dcApply(x861)
val x863 = { 
x862
}
val x872 = x863 * x871
val x873 = x855 += x872
val x874 = x842 += 1
()
}
val x877 = x837
val x878 = x855
val x879 = x188(x836, x877) = x878
val x880 = x837 += 1
()
}
()
}
x190 += 1
} // end fat loop x193
val x194 = x188// unsafe immutable
x194
}
val x222 = { 
val x197 = x103.isRow
val x220 = {
def x220thenb(): generated.scala.Matrix[Float] = {
val x198 = x103.length
val x199 = 1 * x198
val x200 = new generated.scala.FloatMatrixImpl(x15,x199)
val x201 = new generated.scala.IndexVectorRangeImpl(0,x199)
val x204 = x201.length
// a *thin* loop follows: x205
var x202 = 0
var x205: Unit = {
val x886 = x103.length
var x888: Int = 0
val x885 = x201.dcApply(x202)
val x887 = x885 % x886
val x897 = while ({val x889 = x888
val x890 = x889 < x15
x890}) {
val x892 = x888
val x893 = x103(x887)
val x894 = x200(x892, x885) = x893
val x895 = x888 += 1
()
}
()
}
x202 = 1
while (x202 < x204) {  // begin fat loop x205
x205 = {
val x886 = x103.length
var x888: Int = 0
val x885 = x201.dcApply(x202)
val x887 = x885 % x886
val x897 = while ({val x889 = x888
val x890 = x889 < x15
x890}) {
val x892 = x888
val x893 = x103(x887)
val x894 = x200(x892, x885) = x893
val x895 = x888 += 1
()
}
()
}
x202 += 1
} // end fat loop x205
val x206 = x200// unsafe immutable
x206
}
def x220elseb(): generated.scala.Matrix[Float] = {
val x208 = x103.length
val x209 = x15 * x208
val x210 = new generated.scala.FloatMatrixImpl(x209,1)
val x211 = x103.length
val x212 = x15 * x211
val x213 = new generated.scala.IndexVectorRangeImpl(0,x212)
val x216 = x213.length
// a *thin* loop follows: x217
var x214 = 0
var x217: Unit = {
val x900 = x103.length
var x902: Int = 0
val x899 = x213.dcApply(x214)
val x901 = x899 % x900
val x911 = while ({val x903 = x902
val x904 = x903 < 1
x904}) {
val x906 = x902
val x907 = x103(x901)
val x908 = x210(x899, x906) = x907
val x909 = x902 += 1
()
}
()
}
x214 = 1
while (x214 < x216) {  // begin fat loop x217
x217 = {
val x900 = x103.length
var x902: Int = 0
val x899 = x213.dcApply(x214)
val x901 = x899 % x900
val x911 = while ({val x903 = x902
val x904 = x903 < 1
x904}) {
val x906 = x902
val x907 = x103(x901)
val x908 = x210(x899, x906) = x907
val x909 = x902 += 1
()
}
()
}
x214 += 1
} // end fat loop x217
val x218 = x210// unsafe immutable
x218
}
if (x197) {
x220thenb()
} else { 
x220elseb()
}
}
x220
}
val x224 = x196.numRows
val x225 = x196.numCols
val x226 = x224 * x225
// a *thin* loop follows: x232
var x223 = 0
val x229 = x196.dcApply(x223)
val x230 = x222.dcApply(x223)
val x231 = x229 + x230
val x232 = {
val x227 = new generated.scala.FloatMatrixImpl(x224,x225)
x227
}
x232.dcUpdate(x223, x231)
x223 = 1
while (x223 < x226) {  // begin fat loop x232
val x229 = x196.dcApply(x223)
val x230 = x222.dcApply(x223)
val x231 = x229 + x230
x232.dcUpdate(x223, x231)
x223 += 1
} // end fat loop x232
val x234 = x232.numRows
val x235 = x232.numCols
val x236 = x234 * x235
val x255 = { 
// a *thin* loop follows: x246
var x233 = 0
val x239 = x232.dcApply(x233)
val x240 = x239
val x241 = x240 * -1.0
val x242 = Math.exp(x241)
val x243 = 1.0 + x242
val x244 = 1.0 / x243
val x245 = x244.asInstanceOf[Float]
val x246 = {
val x237 = new generated.scala.FloatMatrixImpl(x234,x235)
x237
}
x246.dcUpdate(x233, x245)
x233 = 1
while (x233 < x236) {  // begin fat loop x246
val x239 = x232.dcApply(x233)
val x240 = x239
val x241 = x240 * -1.0
val x242 = Math.exp(x241)
val x243 = 1.0 + x242
val x244 = 1.0 / x243
val x245 = x244.asInstanceOf[Float]
x246.dcUpdate(x233, x245)
x233 += 1
} // end fat loop x246
x246
}
val x276 = x255.numCols
val x277 = x255.numRows
val x338 = x277 * x276
// a *thin* loop follows: x345
var x337 = 0
val x341 = x255.dcApply(x337)
val x342 = x336.dcApply(x337)
val x343 = x341 > x342
val x344 = {
def x344thenb(): Float = {
1.0f
}
def x344elseb(): Float = {
0.0f
}
if (x343) {
x344thenb()
} else { 
x344elseb()
}
}
val x345 = {
val x339 = new generated.scala.FloatMatrixImpl(x277,x276)
x339
}
x345.dcUpdate(x337, x344)
x337 = 1
while (x337 < x338) {  // begin fat loop x345
val x341 = x255.dcApply(x337)
val x342 = x336.dcApply(x337)
val x343 = x341 > x342
val x344 = {
def x344thenb(): Float = {
1.0f
}
def x344elseb(): Float = {
0.0f
}
if (x343) {
x344thenb()
} else { 
x344elseb()
}
}
x345.dcUpdate(x337, x344)
x337 += 1
} // end fat loop x345
val x368 = { 
val x346 = x99.numCols
val x347 = x99.numRows
val x348 = new generated.scala.FloatMatrixImpl(x346,x347)
val x349 = x348.numRows
var x351 : Int = 0
val x365 = while (x351 < x349) {
val x352 = x348.numCols
var x354 : Int = 0
val x363 = while (x354 < x352) {
val x360 = { 
val x355 = x99.numCols
val x356 = x354 * x355
val x357 = x356 + x351
val x358 = x99.dcApply(x357)
x358
}
val x361 = x348(x351, x354) = x360
x361
x354 = x354 + 1
}
x363
x351 = x351 + 1
}
val x366 = x348// unsafe immutable
x366
}
val x392 = x345.numRows
val x395 = new generated.scala.IndexVectorRangeImpl(0,x392)
val x398 = x395.length
val x920 = x345.numCols
val x402 = { 
val x391 = { 
val x369 = x368.numCols
val x370 = x368.numRows
val x371 = new generated.scala.FloatMatrixImpl(x369,x370)
val x372 = x371.numRows
var x374 : Int = 0
val x388 = while (x374 < x372) {
val x375 = x371.numCols
var x377 : Int = 0
val x386 = while (x377 < x375) {
val x383 = { 
val x378 = x368.numCols
val x379 = x377 * x378
val x380 = x379 + x374
val x381 = x368.dcApply(x380)
x381
}
val x384 = x371(x374, x377) = x383
x384
x377 = x377 + 1
}
x386
x374 = x374 + 1
}
val x389 = x371// unsafe immutable
x389
}
val x393 = x368.numCols
val x394 = new generated.scala.FloatMatrixImpl(x392,x393)
// a *thin* loop follows: x399
var x396 = 0
var x399: Unit = {
var x914: Int = 0
val x913 = x395.dcApply(x396)
val x921 = x913 * x920
val x922 = x921 + 0
val x923 = x345.dcApply(x922)
val x924 = { 
x923
}
val x960 = while ({val x915 = x914
val x916 = x394.numCols
val x917 = x915 < x916
x917}) {
var x919: Int = 1
val x925 = x914
val x931 = { 
val x926 = x391.numCols
val x927 = x925 * x926
val x928 = x927 + 0
val x929 = x391.dcApply(x928)
x929
}
val x932 = x924 * x931
var x933: Float = x932
val x954 = while ({val x934 = x919
val x935 = x391.numCols
val x936 = x934 < x935
x936}) {
val x938 = x919
val x942 = x914
val x943 = x919
val x949 = { 
val x944 = x391.numCols
val x945 = x942 * x944
val x946 = x945 + x943
val x947 = x391.dcApply(x946)
x947
}
val x939 = x921 + x938
val x940 = x345.dcApply(x939)
val x941 = { 
x940
}
val x950 = x941 * x949
val x951 = x933 += x950
val x952 = x919 += 1
()
}
val x955 = x914
val x956 = x933
val x957 = x394(x913, x955) = x956
val x958 = x914 += 1
()
}
()
}
x396 = 1
while (x396 < x398) {  // begin fat loop x399
x399 = {
var x914: Int = 0
val x913 = x395.dcApply(x396)
val x921 = x913 * x920
val x922 = x921 + 0
val x923 = x345.dcApply(x922)
val x924 = { 
x923
}
val x960 = while ({val x915 = x914
val x916 = x394.numCols
val x917 = x915 < x916
x917}) {
var x919: Int = 1
val x925 = x914
val x931 = { 
val x926 = x391.numCols
val x927 = x925 * x926
val x928 = x927 + 0
val x929 = x391.dcApply(x928)
x929
}
val x932 = x924 * x931
var x933: Float = x932
val x954 = while ({val x934 = x919
val x935 = x391.numCols
val x936 = x934 < x935
x936}) {
val x938 = x919
val x942 = x914
val x943 = x919
val x949 = { 
val x944 = x391.numCols
val x945 = x942 * x944
val x946 = x945 + x943
val x947 = x391.dcApply(x946)
x947
}
val x939 = x921 + x938
val x940 = x345.dcApply(x939)
val x941 = { 
x940
}
val x950 = x941 * x949
val x951 = x933 += x950
val x952 = x919 += 1
()
}
val x955 = x914
val x956 = x933
val x957 = x394(x913, x955) = x956
val x958 = x914 += 1
()
}
()
}
x396 += 1
} // end fat loop x399
val x400 = x394// unsafe immutable
x400
}
val x428 = { 
val x403 = x107.isRow
val x426 = {
def x426thenb(): generated.scala.Matrix[Float] = {
val x404 = x107.length
val x405 = 1 * x404
val x406 = new generated.scala.FloatMatrixImpl(x15,x405)
val x407 = new generated.scala.IndexVectorRangeImpl(0,x405)
val x410 = x407.length
// a *thin* loop follows: x411
var x408 = 0
var x411: Unit = {
val x963 = x107.length
var x965: Int = 0
val x962 = x407.dcApply(x408)
val x964 = x962 % x963
val x974 = while ({val x966 = x965
val x967 = x966 < x15
x967}) {
val x969 = x965
val x970 = x107(x964)
val x971 = x406(x969, x962) = x970
val x972 = x965 += 1
()
}
()
}
x408 = 1
while (x408 < x410) {  // begin fat loop x411
x411 = {
val x963 = x107.length
var x965: Int = 0
val x962 = x407.dcApply(x408)
val x964 = x962 % x963
val x974 = while ({val x966 = x965
val x967 = x966 < x15
x967}) {
val x969 = x965
val x970 = x107(x964)
val x971 = x406(x969, x962) = x970
val x972 = x965 += 1
()
}
()
}
x408 += 1
} // end fat loop x411
val x412 = x406// unsafe immutable
x412
}
def x426elseb(): generated.scala.Matrix[Float] = {
val x414 = x107.length
val x415 = x15 * x414
val x416 = new generated.scala.FloatMatrixImpl(x415,1)
val x417 = x107.length
val x418 = x15 * x417
val x419 = new generated.scala.IndexVectorRangeImpl(0,x418)
val x422 = x419.length
// a *thin* loop follows: x423
var x420 = 0
var x423: Unit = {
val x977 = x107.length
var x979: Int = 0
val x976 = x419.dcApply(x420)
val x978 = x976 % x977
val x988 = while ({val x980 = x979
val x981 = x980 < 1
x981}) {
val x983 = x979
val x984 = x107(x978)
val x985 = x416(x976, x983) = x984
val x986 = x979 += 1
()
}
()
}
x420 = 1
while (x420 < x422) {  // begin fat loop x423
x423 = {
val x977 = x107.length
var x979: Int = 0
val x976 = x419.dcApply(x420)
val x978 = x976 % x977
val x988 = while ({val x980 = x979
val x981 = x980 < 1
x981}) {
val x983 = x979
val x984 = x107(x978)
val x985 = x416(x976, x983) = x984
val x986 = x979 += 1
()
}
()
}
x420 += 1
} // end fat loop x423
val x424 = x416// unsafe immutable
x424
}
if (x403) {
x426thenb()
} else { 
x426elseb()
}
}
x426
}
val x430 = x402.numRows
val x431 = x402.numCols
val x432 = x430 * x431
// a *thin* loop follows: x438
var x429 = 0
val x435 = x402.dcApply(x429)
val x436 = x428.dcApply(x429)
val x437 = x435 + x436
val x438 = {
val x433 = new generated.scala.FloatMatrixImpl(x430,x431)
x433
}
x438.dcUpdate(x429, x437)
x429 = 1
while (x429 < x432) {  // begin fat loop x438
val x435 = x402.dcApply(x429)
val x436 = x428.dcApply(x429)
val x437 = x435 + x436
x438.dcUpdate(x429, x437)
x429 += 1
} // end fat loop x438
val x440 = x438.numRows
val x441 = x438.numCols
val x442 = x440 * x441
val x461 = { 
// a *thin* loop follows: x452
var x439 = 0
val x445 = x438.dcApply(x439)
val x446 = x445
val x447 = x446 * -1.0
val x448 = Math.exp(x447)
val x449 = 1.0 + x448
val x450 = 1.0 / x449
val x451 = x450.asInstanceOf[Float]
val x452 = {
val x443 = new generated.scala.FloatMatrixImpl(x440,x441)
x443
}
x452.dcUpdate(x439, x451)
x439 = 1
while (x439 < x442) {  // begin fat loop x452
val x445 = x438.dcApply(x439)
val x446 = x445
val x447 = x446 * -1.0
val x448 = Math.exp(x447)
val x449 = 1.0 + x448
val x450 = 1.0 / x449
val x451 = x450.asInstanceOf[Float]
x452.dcUpdate(x439, x451)
x439 += 1
} // end fat loop x452
x452
}
val x485 = x461.numRows
val x488 = new generated.scala.IndexVectorRangeImpl(0,x485)
val x491 = x488.length
val x556 = x461.numCols
val x495 = { 
val x484 = { 
val x462 = x99.numCols
val x463 = x99.numRows
val x464 = new generated.scala.FloatMatrixImpl(x462,x463)
val x465 = x464.numRows
var x467 : Int = 0
val x481 = while (x467 < x465) {
val x468 = x464.numCols
var x470 : Int = 0
val x479 = while (x470 < x468) {
val x476 = { 
val x471 = x99.numCols
val x472 = x470 * x471
val x473 = x472 + x467
val x474 = x99.dcApply(x473)
x474
}
val x477 = x464(x467, x470) = x476
x477
x470 = x470 + 1
}
x479
x467 = x467 + 1
}
val x482 = x464// unsafe immutable
x482
}
val x486 = x99.numCols
val x487 = new generated.scala.FloatMatrixImpl(x485,x486)
// a *thin* loop follows: x492
var x489 = 0
var x492: Unit = {
var x991: Int = 0
val x990 = x488.dcApply(x489)
val x997 = x990 * x556
val x998 = x997 + 0
val x999 = x461.dcApply(x998)
val x1000 = { 
x999
}
val x1036 = while ({val x992 = x991
val x993 = x487.numCols
val x994 = x992 < x993
x994}) {
var x996: Int = 1
val x1001 = x991
val x1007 = { 
val x1002 = x484.numCols
val x1003 = x1001 * x1002
val x1004 = x1003 + 0
val x1005 = x484.dcApply(x1004)
x1005
}
val x1008 = x1000 * x1007
var x1009: Float = x1008
val x1030 = while ({val x1010 = x996
val x1011 = x484.numCols
val x1012 = x1010 < x1011
x1012}) {
val x1014 = x996
val x1018 = x991
val x1019 = x996
val x1025 = { 
val x1020 = x484.numCols
val x1021 = x1018 * x1020
val x1022 = x1021 + x1019
val x1023 = x484.dcApply(x1022)
x1023
}
val x1015 = x997 + x1014
val x1016 = x461.dcApply(x1015)
val x1017 = { 
x1016
}
val x1026 = x1017 * x1025
val x1027 = x1009 += x1026
val x1028 = x996 += 1
()
}
val x1031 = x991
val x1032 = x1009
val x1033 = x487(x990, x1031) = x1032
val x1034 = x991 += 1
()
}
()
}
x489 = 1
while (x489 < x491) {  // begin fat loop x492
x492 = {
var x991: Int = 0
val x990 = x488.dcApply(x489)
val x997 = x990 * x556
val x998 = x997 + 0
val x999 = x461.dcApply(x998)
val x1000 = { 
x999
}
val x1036 = while ({val x992 = x991
val x993 = x487.numCols
val x994 = x992 < x993
x994}) {
var x996: Int = 1
val x1001 = x991
val x1007 = { 
val x1002 = x484.numCols
val x1003 = x1001 * x1002
val x1004 = x1003 + 0
val x1005 = x484.dcApply(x1004)
x1005
}
val x1008 = x1000 * x1007
var x1009: Float = x1008
val x1030 = while ({val x1010 = x996
val x1011 = x484.numCols
val x1012 = x1010 < x1011
x1012}) {
val x1014 = x996
val x1018 = x991
val x1019 = x996
val x1025 = { 
val x1020 = x484.numCols
val x1021 = x1018 * x1020
val x1022 = x1021 + x1019
val x1023 = x484.dcApply(x1022)
x1023
}
val x1015 = x997 + x1014
val x1016 = x461.dcApply(x1015)
val x1017 = { 
x1016
}
val x1026 = x1017 * x1025
val x1027 = x1009 += x1026
val x1028 = x996 += 1
()
}
val x1031 = x991
val x1032 = x1009
val x1033 = x487(x990, x1031) = x1032
val x1034 = x991 += 1
()
}
()
}
x489 += 1
} // end fat loop x492
val x493 = x487// unsafe immutable
x493
}
val x524 = x495.numRows
val x525 = x495.numCols
val x526 = x524 * x525
// a *thin* loop follows: x532
var x523 = 0
val x529 = x495.dcApply(x523)
val x530 = x222.dcApply(x523)
val x531 = x529 + x530
val x532 = {
val x527 = new generated.scala.FloatMatrixImpl(x524,x525)
x527
}
x532.dcUpdate(x523, x531)
x523 = 1
while (x523 < x526) {  // begin fat loop x532
val x529 = x495.dcApply(x523)
val x530 = x222.dcApply(x523)
val x531 = x529 + x530
x532.dcUpdate(x523, x531)
x523 += 1
} // end fat loop x532
val x626 = x186 * x256
// a *thin* loop follows: x632
var x625 = 0
val x629 = x162.dcApply(x625)
val x630 = x461.dcApply(x625)
val x631 = x629 - x630
val x632 = {
val x627 = new generated.scala.FloatMatrixImpl(x186,x256)
x627
}
x632.dcUpdate(x625, x631)
x625 = 1
while (x625 < x626) {  // begin fat loop x632
val x629 = x162.dcApply(x625)
val x630 = x461.dcApply(x625)
val x631 = x629 - x630
x632.dcUpdate(x625, x631)
x625 += 1
} // end fat loop x632
val x634 = x632.numRows
val x635 = x632.numCols
val x636 = x634 * x635
// a *thin* loop follows: x641
var x633 = 0
val x639 = x632.dcApply(x633)
val x640 = x639 * x639
val x641 = {
val x637 = new generated.scala.FloatMatrixImpl(x634,x635)
x637
}
x641.dcUpdate(x633, x640)
x633 = 1
while (x633 < x636) {  // begin fat loop x641
val x639 = x632.dcApply(x633)
val x640 = x639 * x639
x641.dcUpdate(x633, x640)
x633 += 1
} // end fat loop x641
val x645 = x641.numRows
val x646 = x641.numCols
val x647 = x645 * x646
var x642 = 0
val x648 = x641.dcApply(x642)
var x650: Float = {
if (x647 == 0) {0.0f}
else {
val x648 = x641.dcApply(x642)
x648
}
}
x642 = 1
while (x642 < x647) {  // begin fat loop x650
val x648 = x641.dcApply(x642)
val x643 = x650
val x644 = x648
val x649 = x643 + x644
x650 = x649
x642 += 1
} // end fat loop x650
val x651 = x124 += x650
val x652 = x120
val x655 = x111
val x657 = x655.numRows
val x658 = x655.numCols
val x659 = x657 * x658
val x653 = x652 > 5
val x654 = {
def x654thenb(): Float = {
0.9f
}
def x654elseb(): Float = {
0.5f
}
if (x653) {
x654thenb()
} else { 
x654elseb()
}
}
// a *thin* loop follows: x664
var x656 = 0
val x662 = x655.dcApply(x656)
val x663 = x662 * x654
val x664 = {
val x660 = new generated.scala.FloatMatrixImpl(x657,x658)
x660
}
x664.dcUpdate(x656, x663)
x656 = 1
while (x656 < x659) {  // begin fat loop x664
val x662 = x655.dcApply(x656)
val x663 = x662 * x654
x664.dcUpdate(x656, x663)
x656 += 1
} // end fat loop x664
val x275 = { 
val x257 = new generated.scala.FloatMatrixImpl(x256,x186)
val x258 = x257.numRows
var x260 : Int = 0
val x272 = while (x260 < x258) {
val x261 = x257.numCols
var x263 : Int = 0
val x270 = while (x263 < x261) {
val x264 = x263 * x256
val x265 = x264 + x260
val x266 = x162.dcApply(x265)
val x267 = { 
x266
}
val x268 = x257(x260, x263) = x267
x268
x263 = x263 + 1
}
x270
x260 = x260 + 1
}
val x273 = x257// unsafe immutable
x273
}
val x297 = x275.numRows
val x299 = new generated.scala.IndexVectorRangeImpl(0,x297)
val x302 = x299.length
val x1045 = x275.numCols
val x296 = { 
val x278 = new generated.scala.FloatMatrixImpl(x276,x277)
val x279 = x278.numRows
var x281 : Int = 0
val x293 = while (x281 < x279) {
val x282 = x278.numCols
var x284 : Int = 0
val x291 = while (x284 < x282) {
val x285 = x284 * x276
val x286 = x285 + x281
val x287 = x255.dcApply(x286)
val x288 = { 
x287
}
val x289 = x278(x281, x284) = x288
x289
x284 = x284 + 1
}
x291
x281 = x281 + 1
}
val x294 = x278// unsafe immutable
x294
}
val x1051 = x296.numCols
val x306 = { 
val x298 = new generated.scala.FloatMatrixImpl(x297,x276)
// a *thin* loop follows: x303
var x300 = 0
var x303: Unit = {
var x1039: Int = 0
val x1038 = x299.dcApply(x300)
val x1046 = x1038 * x1045
val x1047 = x1046 + 0
val x1048 = x275.dcApply(x1047)
val x1049 = { 
x1048
}
val x1081 = while ({val x1040 = x1039
val x1041 = x298.numCols
val x1042 = x1040 < x1041
x1042}) {
var x1044: Int = 1
val x1050 = x1039
val x1052 = x1050 * x1051
val x1053 = x1052 + 0
val x1054 = x296.dcApply(x1053)
val x1055 = { 
x1054
}
val x1056 = x1049 * x1055
var x1057: Float = x1056
val x1075 = while ({val x1058 = x1044
val x1059 = x1058 < x1051
x1059}) {
val x1061 = x1044
val x1065 = x1039
val x1066 = x1044
val x1062 = x1046 + x1061
val x1063 = x275.dcApply(x1062)
val x1064 = { 
x1063
}
val x1067 = x1065 * x1051
val x1068 = x1067 + x1066
val x1069 = x296.dcApply(x1068)
val x1070 = { 
x1069
}
val x1071 = x1064 * x1070
val x1072 = x1057 += x1071
val x1073 = x1044 += 1
()
}
val x1076 = x1039
val x1077 = x1057
val x1078 = x298(x1038, x1076) = x1077
val x1079 = x1039 += 1
()
}
()
}
x300 = 1
while (x300 < x302) {  // begin fat loop x303
x303 = {
var x1039: Int = 0
val x1038 = x299.dcApply(x300)
val x1046 = x1038 * x1045
val x1047 = x1046 + 0
val x1048 = x275.dcApply(x1047)
val x1049 = { 
x1048
}
val x1081 = while ({val x1040 = x1039
val x1041 = x298.numCols
val x1042 = x1040 < x1041
x1042}) {
var x1044: Int = 1
val x1050 = x1039
val x1052 = x1050 * x1051
val x1053 = x1052 + 0
val x1054 = x296.dcApply(x1053)
val x1055 = { 
x1054
}
val x1056 = x1049 * x1055
var x1057: Float = x1056
val x1075 = while ({val x1058 = x1044
val x1059 = x1058 < x1051
x1059}) {
val x1061 = x1044
val x1065 = x1039
val x1066 = x1044
val x1062 = x1046 + x1061
val x1063 = x275.dcApply(x1062)
val x1064 = { 
x1063
}
val x1067 = x1065 * x1051
val x1068 = x1067 + x1066
val x1069 = x296.dcApply(x1068)
val x1070 = { 
x1069
}
val x1071 = x1064 * x1070
val x1072 = x1057 += x1071
val x1073 = x1044 += 1
()
}
val x1076 = x1039
val x1077 = x1057
val x1078 = x298(x1038, x1076) = x1077
val x1079 = x1039 += 1
()
}
()
}
x300 += 1
} // end fat loop x303
val x304 = x298// unsafe immutable
x304
}
val x666 = x306.numRows
val x667 = x306.numCols
val x668 = x666 * x667
val x534 = x532.numRows
val x535 = x532.numCols
val x536 = x534 * x535
val x555 = { 
// a *thin* loop follows: x546
var x533 = 0
val x539 = x532.dcApply(x533)
val x540 = x539
val x541 = x540 * -1.0
val x542 = Math.exp(x541)
val x543 = 1.0 + x542
val x544 = 1.0 / x543
val x545 = x544.asInstanceOf[Float]
val x546 = {
val x537 = new generated.scala.FloatMatrixImpl(x534,x535)
x537
}
x546.dcUpdate(x533, x545)
x533 = 1
while (x533 < x536) {  // begin fat loop x546
val x539 = x532.dcApply(x533)
val x540 = x539
val x541 = x540 * -1.0
val x542 = Math.exp(x541)
val x543 = 1.0 + x542
val x544 = 1.0 / x543
val x545 = x544.asInstanceOf[Float]
x546.dcUpdate(x533, x545)
x533 += 1
} // end fat loop x546
x546
}
val x575 = { 
val x557 = new generated.scala.FloatMatrixImpl(x556,x485)
val x558 = x557.numRows
var x560 : Int = 0
val x572 = while (x560 < x558) {
val x561 = x557.numCols
var x563 : Int = 0
val x570 = while (x563 < x561) {
val x564 = x563 * x556
val x565 = x564 + x560
val x566 = x461.dcApply(x565)
val x567 = { 
x566
}
val x568 = x557(x560, x563) = x567
x568
x563 = x563 + 1
}
x570
x560 = x560 + 1
}
val x573 = x557// unsafe immutable
x573
}
val x576 = x555.numCols
val x597 = x575.numRows
val x599 = new generated.scala.IndexVectorRangeImpl(0,x597)
val x602 = x599.length
val x1090 = x575.numCols
val x577 = x555.numRows
val x596 = { 
val x578 = new generated.scala.FloatMatrixImpl(x576,x577)
val x579 = x578.numRows
var x581 : Int = 0
val x593 = while (x581 < x579) {
val x582 = x578.numCols
var x584 : Int = 0
val x591 = while (x584 < x582) {
val x585 = x584 * x576
val x586 = x585 + x581
val x587 = x555.dcApply(x586)
val x588 = { 
x587
}
val x589 = x578(x581, x584) = x588
x589
x584 = x584 + 1
}
x591
x581 = x581 + 1
}
val x594 = x578// unsafe immutable
x594
}
val x1096 = x596.numCols
val x606 = { 
val x598 = new generated.scala.FloatMatrixImpl(x597,x576)
// a *thin* loop follows: x603
var x600 = 0
var x603: Unit = {
var x1084: Int = 0
val x1083 = x599.dcApply(x600)
val x1091 = x1083 * x1090
val x1092 = x1091 + 0
val x1093 = x575.dcApply(x1092)
val x1094 = { 
x1093
}
val x1126 = while ({val x1085 = x1084
val x1086 = x598.numCols
val x1087 = x1085 < x1086
x1087}) {
var x1089: Int = 1
val x1095 = x1084
val x1097 = x1095 * x1096
val x1098 = x1097 + 0
val x1099 = x596.dcApply(x1098)
val x1100 = { 
x1099
}
val x1101 = x1094 * x1100
var x1102: Float = x1101
val x1120 = while ({val x1103 = x1089
val x1104 = x1103 < x1096
x1104}) {
val x1106 = x1089
val x1110 = x1084
val x1111 = x1089
val x1107 = x1091 + x1106
val x1108 = x575.dcApply(x1107)
val x1109 = { 
x1108
}
val x1112 = x1110 * x1096
val x1113 = x1112 + x1111
val x1114 = x596.dcApply(x1113)
val x1115 = { 
x1114
}
val x1116 = x1109 * x1115
val x1117 = x1102 += x1116
val x1118 = x1089 += 1
()
}
val x1121 = x1084
val x1122 = x1102
val x1123 = x598(x1083, x1121) = x1122
val x1124 = x1084 += 1
()
}
()
}
x600 = 1
while (x600 < x602) {  // begin fat loop x603
x603 = {
var x1084: Int = 0
val x1083 = x599.dcApply(x600)
val x1091 = x1083 * x1090
val x1092 = x1091 + 0
val x1093 = x575.dcApply(x1092)
val x1094 = { 
x1093
}
val x1126 = while ({val x1085 = x1084
val x1086 = x598.numCols
val x1087 = x1085 < x1086
x1087}) {
var x1089: Int = 1
val x1095 = x1084
val x1097 = x1095 * x1096
val x1098 = x1097 + 0
val x1099 = x596.dcApply(x1098)
val x1100 = { 
x1099
}
val x1101 = x1094 * x1100
var x1102: Float = x1101
val x1120 = while ({val x1103 = x1089
val x1104 = x1103 < x1096
x1104}) {
val x1106 = x1089
val x1110 = x1084
val x1111 = x1089
val x1107 = x1091 + x1106
val x1108 = x575.dcApply(x1107)
val x1109 = { 
x1108
}
val x1112 = x1110 * x1096
val x1113 = x1112 + x1111
val x1114 = x596.dcApply(x1113)
val x1115 = { 
x1114
}
val x1116 = x1109 * x1115
val x1117 = x1102 += x1116
val x1118 = x1089 += 1
()
}
val x1121 = x1084
val x1122 = x1102
val x1123 = x598(x1083, x1121) = x1122
val x1124 = x1084 += 1
()
}
()
}
x600 += 1
} // end fat loop x603
val x604 = x598// unsafe immutable
x604
}
// a *thin* loop follows: x674
var x665 = 0
val x671 = x306.dcApply(x665)
val x672 = x606.dcApply(x665)
val x673 = x671 - x672
val x674 = {
val x669 = new generated.scala.FloatMatrixImpl(x666,x667)
x669
}
x674.dcUpdate(x665, x673)
x665 = 1
while (x665 < x668) {  // begin fat loop x674
val x671 = x306.dcApply(x665)
val x672 = x606.dcApply(x665)
val x673 = x671 - x672
x674.dcUpdate(x665, x673)
x665 += 1
} // end fat loop x674
val x677 = x674.numRows
val x678 = x674.numCols
val x679 = x677 * x678
// a *thin* loop follows: x684
var x676 = 0
val x682 = x674.dcApply(x676)
val x683 = x682 / x675
val x684 = {
val x680 = new generated.scala.FloatMatrixImpl(x677,x678)
x680
}
x684.dcUpdate(x676, x683)
x676 = 1
while (x676 < x679) {  // begin fat loop x684
val x682 = x674.dcApply(x676)
val x683 = x682 / x675
x684.dcUpdate(x676, x683)
x676 += 1
} // end fat loop x684
val x686 = x99.numRows
val x687 = x99.numCols
val x688 = x686 * x687
// a *thin* loop follows: x696
var x685 = 0
val x693 = x99.dcApply(x685)
val x694 = x693 * 2.0E-4f
val x696 = {
val x689 = x99.numRows
val x690 = x99.numCols
val x691 = new generated.scala.FloatMatrixImpl(x689,x690)
x691
}
x696.dcUpdate(x685, x694)
x685 = 1
while (x685 < x688) {  // begin fat loop x696
val x693 = x99.dcApply(x685)
val x694 = x693 * 2.0E-4f
x696.dcUpdate(x685, x694)
x685 += 1
} // end fat loop x696
val x698 = x684.numRows
val x699 = x684.numCols
val x700 = x698 * x699
// a *thin* loop follows: x706
var x697 = 0
val x703 = x684.dcApply(x697)
val x704 = x696.dcApply(x697)
val x705 = x703 - x704
val x706 = {
val x701 = new generated.scala.FloatMatrixImpl(x698,x699)
x701
}
x706.dcUpdate(x697, x705)
x697 = 1
while (x697 < x700) {  // begin fat loop x706
val x703 = x684.dcApply(x697)
val x704 = x696.dcApply(x697)
val x705 = x703 - x704
x706.dcUpdate(x697, x705)
x697 += 1
} // end fat loop x706
val x708 = x706.numRows
val x709 = x706.numCols
val x710 = x708 * x709
// a *thin* loop follows: x715
var x707 = 0
val x713 = x706.dcApply(x707)
val x714 = x713 * 0.1f
val x715 = {
val x711 = new generated.scala.FloatMatrixImpl(x708,x709)
x711
}
x715.dcUpdate(x707, x714)
x707 = 1
while (x707 < x710) {  // begin fat loop x715
val x713 = x706.dcApply(x707)
val x714 = x713 * 0.1f
x715.dcUpdate(x707, x714)
x707 += 1
} // end fat loop x715
val x717 = x664.numRows
val x718 = x664.numCols
val x719 = x717 * x718
// a *thin* loop follows: x725
var x716 = 0
val x722 = x664.dcApply(x716)
val x723 = x715.dcApply(x716)
val x724 = x722 + x723
val x725 = {
val x720 = new generated.scala.FloatMatrixImpl(x717,x718)
x720
}
x725.dcUpdate(x716, x724)
x716 = 1
while (x716 < x719) {  // begin fat loop x725
val x722 = x664.dcApply(x716)
val x723 = x715.dcApply(x716)
val x724 = x722 + x723
x725.dcUpdate(x716, x724)
x716 += 1
} // end fat loop x725
x111 = x725
val x727 = x117
val x729 = x727.length
val x730 = x727.isRow
// a *thin* loop follows: x735
var x728 = 0
val x733 = x727.dcApply(x728)
val x734 = x733 * x654
val x735 = {
val x731 = new generated.scala.FloatVectorImpl(x729,x730)
x731
}
x735.dcUpdate(x728, x734)
x728 = 1
while (x728 < x729) {  // begin fat loop x735
val x733 = x727.dcApply(x728)
val x734 = x733 * x654
x735.dcUpdate(x728, x734)
x728 += 1
} // end fat loop x735
val x317 = new generated.scala.IndexVectorRangeImpl(0,x256)
val x320 = x317.length
val x324 = { 
val x316 = new generated.scala.FloatVectorImpl(x256,true)
// a *thin* loop follows: x321
var x318 = 0
var x321: Unit = {
val x1128 = x317.dcApply(x318)
val x1129 = x162.getCol(x1128)
val x1133 = x1129.length
var x1130 = 0
val x1134 = x1129.dcApply(x1130)
var x1136: Float = {
if (x1133 == 0) {0.0f}
else {
val x1134 = x1129.dcApply(x1130)
x1134
}
}
x1130 = 1
while (x1130 < x1133) {  // begin fat loop x1136
val x1134 = x1129.dcApply(x1130)
val x1131 = x1136
val x1132 = x1134
val x1135 = x1131 + x1132
x1136 = x1135
x1130 += 1
} // end fat loop x1136
val x1137 = x316(x1128) = x1136
x1137
}
x318 = 1
while (x318 < x320) {  // begin fat loop x321
x321 = {
val x1128 = x317.dcApply(x318)
val x1129 = x162.getCol(x1128)
val x1133 = x1129.length
var x1130 = 0
val x1134 = x1129.dcApply(x1130)
var x1136: Float = {
if (x1133 == 0) {0.0f}
else {
val x1134 = x1129.dcApply(x1130)
x1134
}
}
x1130 = 1
while (x1130 < x1133) {  // begin fat loop x1136
val x1134 = x1129.dcApply(x1130)
val x1131 = x1136
val x1132 = x1134
val x1135 = x1131 + x1132
x1136 = x1135
x1130 += 1
} // end fat loop x1136
val x1137 = x316(x1128) = x1136
x1137
}
x318 += 1
} // end fat loop x321
val x322 = x316// unsafe immutable
x322
}
val x737 = x324.length
val x738 = x324.isRow
val x617 = new generated.scala.IndexVectorRangeImpl(0,x556)
val x620 = x617.length
val x624 = { 
val x616 = new generated.scala.FloatVectorImpl(x556,true)
// a *thin* loop follows: x621
var x618 = 0
var x621: Unit = {
val x1139 = x617.dcApply(x618)
val x1140 = x461.getCol(x1139)
val x1144 = x1140.length
var x1141 = 0
val x1145 = x1140.dcApply(x1141)
var x1147: Float = {
if (x1144 == 0) {0.0f}
else {
val x1145 = x1140.dcApply(x1141)
x1145
}
}
x1141 = 1
while (x1141 < x1144) {  // begin fat loop x1147
val x1145 = x1140.dcApply(x1141)
val x1142 = x1147
val x1143 = x1145
val x1146 = x1142 + x1143
x1147 = x1146
x1141 += 1
} // end fat loop x1147
val x1148 = x616(x1139) = x1147
x1148
}
x618 = 1
while (x618 < x620) {  // begin fat loop x621
x621 = {
val x1139 = x617.dcApply(x618)
val x1140 = x461.getCol(x1139)
val x1144 = x1140.length
var x1141 = 0
val x1145 = x1140.dcApply(x1141)
var x1147: Float = {
if (x1144 == 0) {0.0f}
else {
val x1145 = x1140.dcApply(x1141)
x1145
}
}
x1141 = 1
while (x1141 < x1144) {  // begin fat loop x1147
val x1145 = x1140.dcApply(x1141)
val x1142 = x1147
val x1143 = x1145
val x1146 = x1142 + x1143
x1147 = x1146
x1141 += 1
} // end fat loop x1147
val x1148 = x616(x1139) = x1147
x1148
}
x618 += 1
} // end fat loop x621
val x622 = x616// unsafe immutable
x622
}
// a *thin* loop follows: x744
var x736 = 0
val x741 = x324.dcApply(x736)
val x742 = x624.dcApply(x736)
val x743 = x741 - x742
val x744 = {
val x739 = new generated.scala.FloatVectorImpl(x737,x738)
x739
}
x744.dcUpdate(x736, x743)
x736 = 1
while (x736 < x737) {  // begin fat loop x744
val x741 = x324.dcApply(x736)
val x742 = x624.dcApply(x736)
val x743 = x741 - x742
x744.dcUpdate(x736, x743)
x736 += 1
} // end fat loop x744
val x747 = x744.isRow
// a *thin* loop follows: x752
var x746 = 0
val x750 = x744.dcApply(x746)
val x751 = x750 * x745
val x752 = {
val x748 = new generated.scala.FloatVectorImpl(x737,x747)
x748
}
x752.dcUpdate(x746, x751)
x746 = 1
while (x746 < x737) {  // begin fat loop x752
val x750 = x744.dcApply(x746)
val x751 = x750 * x745
x752.dcUpdate(x746, x751)
x746 += 1
} // end fat loop x752
val x754 = x735.isRow
// a *thin* loop follows: x760
var x753 = 0
val x757 = x735.dcApply(x753)
val x758 = x752.dcApply(x753)
val x759 = x757 + x758
val x760 = {
val x755 = new generated.scala.FloatVectorImpl(x729,x754)
x755
}
x760.dcUpdate(x753, x759)
x753 = 1
while (x753 < x729) {  // begin fat loop x760
val x757 = x735.dcApply(x753)
val x758 = x752.dcApply(x753)
val x759 = x757 + x758
x760.dcUpdate(x753, x759)
x753 += 1
} // end fat loop x760
x117 = x760
val x762 = x114
val x764 = x762.length
val x765 = x762.isRow
// a *thin* loop follows: x770
var x763 = 0
val x768 = x762.dcApply(x763)
val x769 = x768 * x654
val x770 = {
val x766 = new generated.scala.FloatVectorImpl(x764,x765)
x766
}
x770.dcUpdate(x763, x769)
x763 = 1
while (x763 < x764) {  // begin fat loop x770
val x768 = x762.dcApply(x763)
val x769 = x768 * x654
x770.dcUpdate(x763, x769)
x763 += 1
} // end fat loop x770
val x308 = new generated.scala.IndexVectorRangeImpl(0,x276)
val x311 = x308.length
val x315 = { 
val x307 = new generated.scala.FloatVectorImpl(x276,true)
// a *thin* loop follows: x312
var x309 = 0
var x312: Unit = {
val x1150 = x308.dcApply(x309)
val x1151 = x255.getCol(x1150)
val x1155 = x1151.length
var x1152 = 0
val x1156 = x1151.dcApply(x1152)
var x1158: Float = {
if (x1155 == 0) {0.0f}
else {
val x1156 = x1151.dcApply(x1152)
x1156
}
}
x1152 = 1
while (x1152 < x1155) {  // begin fat loop x1158
val x1156 = x1151.dcApply(x1152)
val x1153 = x1158
val x1154 = x1156
val x1157 = x1153 + x1154
x1158 = x1157
x1152 += 1
} // end fat loop x1158
val x1159 = x307(x1150) = x1158
x1159
}
x309 = 1
while (x309 < x311) {  // begin fat loop x312
x312 = {
val x1150 = x308.dcApply(x309)
val x1151 = x255.getCol(x1150)
val x1155 = x1151.length
var x1152 = 0
val x1156 = x1151.dcApply(x1152)
var x1158: Float = {
if (x1155 == 0) {0.0f}
else {
val x1156 = x1151.dcApply(x1152)
x1156
}
}
x1152 = 1
while (x1152 < x1155) {  // begin fat loop x1158
val x1156 = x1151.dcApply(x1152)
val x1153 = x1158
val x1154 = x1156
val x1157 = x1153 + x1154
x1158 = x1157
x1152 += 1
} // end fat loop x1158
val x1159 = x307(x1150) = x1158
x1159
}
x309 += 1
} // end fat loop x312
val x313 = x307// unsafe immutable
x313
}
val x772 = x315.length
val x773 = x315.isRow
val x608 = new generated.scala.IndexVectorRangeImpl(0,x576)
val x611 = x608.length
val x615 = { 
val x607 = new generated.scala.FloatVectorImpl(x576,true)
// a *thin* loop follows: x612
var x609 = 0
var x612: Unit = {
val x1161 = x608.dcApply(x609)
val x1162 = x555.getCol(x1161)
val x1166 = x1162.length
var x1163 = 0
val x1167 = x1162.dcApply(x1163)
var x1169: Float = {
if (x1166 == 0) {0.0f}
else {
val x1167 = x1162.dcApply(x1163)
x1167
}
}
x1163 = 1
while (x1163 < x1166) {  // begin fat loop x1169
val x1167 = x1162.dcApply(x1163)
val x1164 = x1169
val x1165 = x1167
val x1168 = x1164 + x1165
x1169 = x1168
x1163 += 1
} // end fat loop x1169
val x1170 = x607(x1161) = x1169
x1170
}
x609 = 1
while (x609 < x611) {  // begin fat loop x612
x612 = {
val x1161 = x608.dcApply(x609)
val x1162 = x555.getCol(x1161)
val x1166 = x1162.length
var x1163 = 0
val x1167 = x1162.dcApply(x1163)
var x1169: Float = {
if (x1166 == 0) {0.0f}
else {
val x1167 = x1162.dcApply(x1163)
x1167
}
}
x1163 = 1
while (x1163 < x1166) {  // begin fat loop x1169
val x1167 = x1162.dcApply(x1163)
val x1164 = x1169
val x1165 = x1167
val x1168 = x1164 + x1165
x1169 = x1168
x1163 += 1
} // end fat loop x1169
val x1170 = x607(x1161) = x1169
x1170
}
x609 += 1
} // end fat loop x612
val x613 = x607// unsafe immutable
x613
}
// a *thin* loop follows: x779
var x771 = 0
val x776 = x315.dcApply(x771)
val x777 = x615.dcApply(x771)
val x778 = x776 - x777
val x779 = {
val x774 = new generated.scala.FloatVectorImpl(x772,x773)
x774
}
x779.dcUpdate(x771, x778)
x771 = 1
while (x771 < x772) {  // begin fat loop x779
val x776 = x315.dcApply(x771)
val x777 = x615.dcApply(x771)
val x778 = x776 - x777
x779.dcUpdate(x771, x778)
x771 += 1
} // end fat loop x779
val x781 = x779.isRow
// a *thin* loop follows: x786
var x780 = 0
val x784 = x779.dcApply(x780)
val x785 = x784 * x745
val x786 = {
val x782 = new generated.scala.FloatVectorImpl(x772,x781)
x782
}
x786.dcUpdate(x780, x785)
x780 = 1
while (x780 < x772) {  // begin fat loop x786
val x784 = x779.dcApply(x780)
val x785 = x784 * x745
x786.dcUpdate(x780, x785)
x780 += 1
} // end fat loop x786
val x788 = x770.isRow
// a *thin* loop follows: x794
var x787 = 0
val x791 = x770.dcApply(x787)
val x792 = x786.dcApply(x787)
val x793 = x791 + x792
val x794 = {
val x789 = new generated.scala.FloatVectorImpl(x764,x788)
x789
}
x794.dcUpdate(x787, x793)
x787 = 1
while (x787 < x764) {  // begin fat loop x794
val x791 = x770.dcApply(x787)
val x792 = x786.dcApply(x787)
val x793 = x791 + x792
x794.dcUpdate(x787, x793)
x787 += 1
} // end fat loop x794
x114 = x794
val x796 = x111
val x798 = x99.numRows
val x799 = x99.numCols
val x800 = x798 * x799
// a *thin* loop follows: x805
var x797 = 0
val x801 = x99.dcApply(x797)
val x802 = x796.dcApply(x797)
val x803 = x801 + x802
val x805 = {
x99
}
x805.dcUpdate(x797, x803)
x797 = 1
while (x797 < x800) {  // begin fat loop x805
val x801 = x99.dcApply(x797)
val x802 = x796.dcApply(x797)
val x803 = x801 + x802
x805.dcUpdate(x797, x803)
x797 += 1
} // end fat loop x805
val x806 = x117
val x808 = x107.length
// a *thin* loop follows: x813
var x807 = 0
val x809 = x107.dcApply(x807)
val x810 = x806.dcApply(x807)
val x811 = x809 + x810
val x813 = {
x107
}
x813.dcUpdate(x807, x811)
x807 = 1
while (x807 < x808) {  // begin fat loop x813
val x809 = x107.dcApply(x807)
val x810 = x806.dcApply(x807)
val x811 = x809 + x810
x813.dcUpdate(x807, x811)
x807 += 1
} // end fat loop x813
val x814 = x114
val x816 = x103.length
// a *thin* loop follows: x821
var x815 = 0
val x817 = x103.dcApply(x815)
val x818 = x814.dcApply(x815)
val x819 = x817 + x818
val x821 = {
x103
}
x821.dcUpdate(x815, x819)
x815 = 1
while (x815 < x816) {  // begin fat loop x821
val x817 = x103.dcApply(x815)
val x818 = x814.dcApply(x815)
val x819 = x817 + x818
x821.dcUpdate(x815, x819)
x815 += 1
} // end fat loop x821
val x822 = x125 += 1
()
}
val x825 = x120
val x826 = "--> Epoch "+x825
val x827 = println(x826)
val x828 = x124
val x829 = " error = "+x828
val x830 = println(x829)
val x831 = x120 += 1
()
}
val x834 = ppl.delite.runtime.profiler.PerformanceTimer.stop("app", false)
()
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
