package generated

class GDA_N extends ((Array[java.lang.String])=>(Unit)) {
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
      while ({val x422 = x11
        val x423 = x422 != null
        x423}) {
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
      while ({val x425 = x61
        val x426 = x425 != null
        x426}) {
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
    val x170 = x100
    val x171 = x91
    val x186 = {
      new ppl.dsl.optiml.MatrixImpl[Double](x90,x90)
    }
    var x187 = x186
    val x169 = {
      val x156 = x154.length
      val x157 = x154.is_row
      val x158 = {
        new ppl.dsl.optiml.VectorImpl[Double](x156,x157)
      }
      val x159 = 0 until x156
      val x166 = {x160: (Int) =>
        val x161 = x154(x160)
        val x162 = x155
        val x163 = x161 / x162
        val x164 = x158(x160) = x163
        x164
      }
      val x167 = x159.foreach{
        x166
      }
      x158
    }
    val x185 = {
      val x172 = x170.length
      val x173 = x170.is_row
      val x174 = {
        new ppl.dsl.optiml.VectorImpl[Double](x172,x173)
      }
      val x175 = 0 until x172
      val x182 = {x176: (Int) =>
        val x177 = x170(x176)
        val x178 = x171
        val x179 = x177 / x178
        val x180 = x174(x176) = x179
        x180
      }
      val x183 = x175.foreach{
        x182
      }
      x174
    }
    val x343 = {x188: (Int) =>
      val x189 = x88(x188)
      val x190 = x189 == false
      val x341 = if (x190) {
        val x191 = x187
        val x192 = x55(x188)
        val x206 = {
          val x193 = x192.length
          val x194 = x192.is_row
          val x195 = {
            new ppl.dsl.optiml.VectorImpl[Double](x193,x194)
          }
          val x196 = 0 until x193
          val x203 = {x197: (Int) =>
            val x198 = x192(x197)
            val x199 = x169(x197)
            val x200 = x198 - x199
            val x201 = x195(x197) = x200
            x201
          }
          val x204 = x196.foreach{
            x203
          }
          x195
        }
        val x219 = {
          val x207 = x206.length
          val x208 = x206.is_row
          val x209 = !x208
          val x210 = {
            new ppl.dsl.optiml.VectorImpl[Double](x207,x209)
          }
          val x211 = 0 until x207
          val x216 = {x212: (Int) =>
            val x213 = x206(x212)
            val x214 = x210(x212) = x213
            x214
          }
          val x217 = x211.foreach{
            x216
          }
          x210
        }
        val x246 = {
          val x230 = x219.length
          val x231 = {
            new ppl.dsl.optiml.MatrixImpl[Double](x230,x230)
          }
          val x232 = 0 until x230
          val x243 = {x233: (Int) =>
            val x207 = x206.length
            val x211 = 0 until x207
            val x240 = {x234: (Int) =>
              val x235 = x219(x233)
              val x236 = x206(x234)
              val x237 = x235 * x236
              val x238 = x231(x233, x234) = x237
              x238
            }
            val x241 = x211.foreach{
              x240
            }
            x241
          }
          val x244 = x232.foreach{
            x243
          }
          x231
        }
        val x265 = {
          val x247 = x191.numRows
          val x248 = x191.numCols
          val x249 = {
            new ppl.dsl.optiml.MatrixImpl[Double](x247,x248)
          }
          val x250 = 0 until x247
          val x262 = {x251: (Int) =>
            val x252 = 0 until x248
            val x259 = {x253: (Int) =>
              val x254 = x191(x251, x253)
              val x255 = x246(x251, x253)
              val x256 = x254 + x255
              val x257 = x249(x251, x253) = x256
              x257
            }
            val x260 = x252.foreach{
              x259
            }
            x260
          }
          val x263 = x250.foreach{
            x262
          }
          x249
        }
        x187 = x265
        ()
      } else {
        val x268 = x187
        val x192 = x55(x188)
        val x279 = {
          val x193 = x192.length
          val x194 = x192.is_row
          val x269 = {
            new ppl.dsl.optiml.VectorImpl[Double](x193,x194)
          }
          val x196 = 0 until x193
          val x276 = {x270: (Int) =>
            val x271 = x192(x270)
            val x272 = x185(x270)
            val x273 = x271 - x272
            val x274 = x269(x270) = x273
            x274
          }
          val x277 = x196.foreach{
            x276
          }
          x269
        }
        val x292 = {
          val x280 = x279.length
          val x281 = x279.is_row
          val x282 = !x281
          val x283 = {
            new ppl.dsl.optiml.VectorImpl[Double](x280,x282)
          }
          val x284 = 0 until x280
          val x289 = {x285: (Int) =>
            val x286 = x279(x285)
            val x287 = x283(x285) = x286
            x287
          }
          val x290 = x284.foreach{
            x289
          }
          x283
        }
        val x319 = {
          val x303 = x292.length
          val x304 = {
            new ppl.dsl.optiml.MatrixImpl[Double](x303,x303)
          }
          val x305 = 0 until x303
          val x316 = {x306: (Int) =>
            val x280 = x279.length
            val x284 = 0 until x280
            val x313 = {x307: (Int) =>
              val x308 = x292(x306)
              val x309 = x279(x307)
              val x310 = x308 * x309
              val x311 = x304(x306, x307) = x310
              x311
            }
            val x314 = x284.foreach{
              x313
            }
            x314
          }
          val x317 = x305.foreach{
            x316
          }
          x304
        }
        val x338 = {
          val x320 = x268.numRows
          val x321 = x268.numCols
          val x322 = {
            new ppl.dsl.optiml.MatrixImpl[Double](x320,x321)
          }
          val x323 = 0 until x320
          val x335 = {x324: (Int) =>
            val x325 = 0 until x321
            val x332 = {x326: (Int) =>
              val x327 = x268(x324, x326)
              val x328 = x319(x324, x326)
              val x329 = x327 + x328
              val x330 = x322(x324, x326) = x329
              x330
            }
            val x333 = x325.foreach{
              x332
            }
            x333
          }
          val x336 = x323.foreach{
            x335
          }
          x322
        }
        x187 = x338
        ()
      }
      x341
    }
    val x344 = x101.foreach{
      x343
    }
    val x345 = print("GDA parameter calculation finished: ")
    val x150 = x89
    val x151 = 1.0 / x150
    val x153 = x151 * x152
    val x346 = "  phi = "+x153
    val x347 = println(x346)
    val x348 = println("  mu0 = ")
    val x373 = {
      val x349 = x169.is_row
      val x371 = if (x349) {
        val x350 = print("[ ")
        val x351 = x169.length
        val x352 = 0 until x351
        val x358 = {x353: (Int) =>
          val x354 = x169(x353)
          val x355 = print(x354)
          val x356 = print(" ")
          x356
        }
        val x359 = x352.foreach{
          x358
        }
        val x360 = print("]\n")
        x360
      } else {
        val x351 = x169.length
        val x352 = 0 until x351
        val x368 = {x362: (Int) =>
          val x363 = print("[")
          val x364 = x169(x362)
          val x365 = print(x364)
          val x366 = print(" ]\n")
          x366
        }
        val x369 = x352.foreach{
          x368
        }
        x369
      }
      x371
    }
    val x374 = println("  mu1 = ")
    val x399 = {
      val x375 = x185.is_row
      val x397 = if (x375) {
        val x376 = print("[ ")
        val x377 = x185.length
        val x378 = 0 until x377
        val x384 = {x379: (Int) =>
          val x380 = x185(x379)
          val x381 = print(x380)
          val x382 = print(" ")
          x382
        }
        val x385 = x378.foreach{
          x384
        }
        val x386 = print("]\n")
        x386
      } else {
        val x377 = x185.length
        val x378 = 0 until x377
        val x394 = {x388: (Int) =>
          val x389 = print("[")
          val x390 = x185(x388)
          val x391 = print(x390)
          val x392 = print(" ]\n")
          x392
        }
        val x395 = x378.foreach{
          x394
        }
        x395
      }
      x397
    }
    val x400 = println("  sigma = ")
    val x401 = x187
    val x420 = {
      val x402 = x401.numRows
      val x403 = 0 until x402
      val x417 = {x404: (Int) =>
        val x405 = print("[ ")
        val x406 = x401.numCols
        val x407 = 0 until x406
        val x413 = {x408: (Int) =>
          val x409 = x401(x404, x408)
          val x410 = print(x409)
          val x411 = print(" ")
          x411
        }
        val x414 = x407.foreach{
          x413
        }
        val x415 = print("]\n")
        x415
      }
      val x418 = x403.foreach{
        x417
      }
      x418
    }
    ()
  }
}