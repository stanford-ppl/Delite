package templates

import chisel3._
import chisel3.util


class flatW(val w: Int) extends Bundle {
  val addr = UInt(w.W)
  val data = UInt(w.W)
  val en = Bool()

  override def cloneType = (new flatW(w)).asInstanceOf[this.type] // See chisel3 bug 358
}
class flatR(val w: Int) extends Bundle {
  val addr = UInt(w.W)
  val en = Bool()

  override def cloneType = (new flatR(w)).asInstanceOf[this.type] // See chisel3 bug 358
}
class multidimW(val N: Int, val w: Int) extends Bundle {
  val addr = Vec(N, UInt(w.W))
  val data = UInt(w.W)
  val en = Bool()

  override def cloneType = (new multidimW(N, w)).asInstanceOf[this.type] // See chisel3 bug 358
}
class multidimR(val N: Int, val w: Int) extends Bundle {
  val addr = Vec(N, UInt(w.W))
  val en = Bool()
  
  override def cloneType = (new multidimR(N, w)).asInstanceOf[this.type] // See chisel3 bug 358
}


class Mem1D(val size: Int) extends Module { // Unbanked, inner 1D mem
  val io = IO( new Bundle {
    val w = new flatW(32).asInput
    val r = new flatR(32).asInput
    val output = new Bundle {
      val data  = UInt(32.W).asOutput
    }
    val debug = new Bundle {
      val invalidRAddr = Bool().asOutput
      val invalidWAddr = Bool().asOutput
      val rwOn = Bool().asOutput
      val error = Bool().asOutput
    }
  })

  // We can do better than MaxJ by forcing mems to be single-ported since
  //   we know how to properly schedule reads and writes
  val m = Mem(UInt(width = 32), size /*, seqRead = true deprecated? */)
  val wInBound = io.w.addr < UInt(size)
  val rInBound = io.r.addr < UInt(size)

  val reg_rAddr = Reg(UInt())
  when (io.w.en & wInBound) {m(io.w.addr) := io.w.data}
  .elsewhen (io.r.en & rInBound) {reg_rAddr := io.r.addr}

  io.output.data := m(reg_rAddr)
  io.debug.invalidRAddr := ~rInBound
  io.debug.invalidWAddr := ~wInBound
  io.debug.rwOn := io.w.en & io.r.en
  io.debug.error := ~rInBound | ~wInBound | (io.w.en & io.r.en)

}


// Last dimension is the leading-dim
class MemND(val dims: List[Int]) extends Module { 
  val depth = dims.reduce{_*_} // Size of memory
  val N = dims.length // Number of dimensions

  val io = IO( new Bundle {
    val w = new multidimW(N, 32).asInput
    val wMask = Bool().asInput // Mask passed by SRAM for when a multidimW comes through without "really" being selected
    val r = new multidimR(N, 32).asInput
    val rMask = Bool().asInput // Mask passed by SRAM for when a multidimR comes through without "really" being selected
    val output = new Bundle {
      val data  = UInt(32.W).asOutput
    }
    val debug = new Bundle {
      val invalidRAddr = Bool().asOutput
      val invalidWAddr = Bool().asOutput
      val rwOn = Bool().asOutput
      val error = Bool().asOutput
    }
  })

  // Init masks
  val wMask = Wire(true.B)
  wMask := io.wMask
  val rMask = Wire(true.B)
  rMask := io.rMask

  // Instantiate 1D mem
  val m = Module(new Mem1D(depth))

  // Address flattening
  m.io.w.addr := io.w.addr.zipWithIndex.map{ case (addr, i) =>
    addr * UInt(dims.drop(i).reduce{_*_}/dims(i))
  }.reduce{_+_}
  m.io.r.addr := io.r.addr.zipWithIndex.map{ case (addr, i) =>
    addr * UInt(dims.drop(i).reduce{_*_}/dims(i))
  }.reduce{_+_}

  // Check if read/write is in bounds
  val rInBound = io.r.addr.zip(dims).map { case (addr, bound) => addr < UInt(bound) }.reduce{_&_}
  val wInBound = io.w.addr.zip(dims).map { case (addr, bound) => addr < UInt(bound) }.reduce{_&_}

  // Connect the other ports
  m.io.w.data := io.w.data
  m.io.w.en := io.w.en & wMask
  m.io.r.en := io.r.en & rMask
  io.output.data := m.io.output.data
  io.debug.invalidWAddr := ~wInBound
  io.debug.invalidRAddr := ~rInBound
  io.debug.rwOn := io.w.en & io.r.en
  io.debug.error := ~wInBound | ~rInBound | (io.w.en & io.r.en)
}


/*
                            
                                                           __________             ___SRAM__
         _        _           _______                     |          |--bundleND-|   MemND |               
        | |------| |---------|       |                    |          |           |_________|                        
   IO(Vec(bundleSRAM))-------| Mux1H |-----bundleSRAM-----|   VAT    |--bundleND-|   MemND |    
        |_|------|_|---------|_______|                    |          |           |_________|                        
                               | | |                      |__________|--bundleND-|   MemND |               
                             stageEnables                                        |_________|
                                                                        
                                                                    
*/
class SRAM(val logicalDims: List[Int], val w: Int, 
           val banks: List[Int], val strides: List[Int], val numWriters: Int, val numReaders: Int,
           val wPar: Int, val rPar: Int, val bankingMode: String) extends Module { 

  // Overloaded construters
  // Tuple unpacker
  def this(tuple: (List[Int], Int, List[Int], List[Int], Int, Int,
           Int, Int, String)) = this(tuple._1,tuple._2,tuple._3,tuple._4,tuple._5,tuple._6,tuple._7,tuple._8,tuple._9)
  // Bankmode-less
  def this(logicalDims: List[Int], w: Int, 
           banks: List[Int], strides: List[Int], numWriters: Int, numReaders: Int,
           wPar: Int, rPar: Int) = this(logicalDims, w, banks, strides, numWriters, numReaders, wPar, rPar, "strided")
  // If 1D, spatial will make banks and strides scalars instead of lists
  def this(logicalDims: List[Int], w: Int, 
           banks: Int, strides: Int, numWriters: Int, numReaders: Int,
           wPar: Int, rPar: Int) = this(logicalDims, w, List(banks), List(strides), numWriters, numReaders, wPar, rPar, "strided")

  val depth = logicalDims.reduce{_*_} // Size of memory
  val N = logicalDims.length // Number of dimensions

  val io = IO( new Bundle {
    // TODO: w bundle gets forcefully generated as output in verilog
    //       so the only way to make it an input seems to flatten the
    //       Vec(numWriters, Vec(wPar, _)) to a 1D vector and then reconstruct it
    val w = Vec(numWriters*wPar, new multidimW(N, 32).asInput)
    val globalWEn = Vec(numWriters, Bool().asInput)
    val wSel = Vec(numWriters, Bool().asInput) // Selects between multiple write bundles
    val r = Vec(numReaders*rPar,new multidimR(N, 32).asInput) // TODO: Spatial allows only one reader per mem
    val rSel = Vec(numReaders, Bool().asInput)
    val output = new Bundle {
      val data  = Vec(rPar, UInt(32.W).asOutput)
    }
    val debug = new Bundle {
      val invalidRAddr = Bool().asOutput
      val invalidWAddr = Bool().asOutput
      val rwOn = Bool().asOutput
      val readCollision = Bool().asOutput
      val writeCollision = Bool().asOutput
      val error = Bool().asOutput
    }
  })

  // Get info on physical dims
  // TODO: Upcast dims to evenly bank
  val physicalDims = logicalDims.zip(banks).map { case (dim, b) => dim/b}
  val numMems = banks.reduce{_*_}

  // Create physical mems
  val m = (0 until numMems).map{ i => Module(new MemND(physicalDims))}

  // Reconstruct io.w as 2d vector
  val reconstructedW = (0 until numWriters).map{ i => 
    Vec((0 until wPar).map { j => io.w(i*wPar + j) })
  }
  val reconstructedR = (0 until numReaders).map{ i => 
    Vec((0 until rPar).map { j => io.r(i*rPar + j) })
  }

  // Mux wrideBundles and connect to internal
  val selectedWVec = chisel3.util.Mux1H(io.wSel, reconstructedW)
  val selectedRVec = chisel3.util.Mux1H(io.rSel, reconstructedR)
  val selectedGlobalWen = chisel3.util.Mux1H(io.wSel, io.globalWEn)

  // TODO: Should connect multidimW's directly to their banks rather than all-to-all connections
  // Convert selectedWVec to translated physical addresses
  val wConversions = selectedWVec.map{ wbundle => 
    // Writer conversion
    val convertedW = Wire(new multidimW(N,w))
    val physicalAddrsW = wbundle.addr.zip(banks).map{ case (logical, b) => logical / UInt(b) }
    physicalAddrsW.zipWithIndex.foreach { case (calculatedAddr, i) => convertedW.addr(i) := calculatedAddr}
    convertedW.data := wbundle.data
    convertedW.en := wbundle.en & selectedGlobalWen
    val bankCoordsW = wbundle.addr.zip(banks).map{ case (logical, b) => logical % UInt(b) }
    val bankCoordW = bankCoordsW.zipWithIndex.map{ case (c, i) => c*UInt(banks.drop(i).reduce{_*_}/banks(i)) }.reduce{_+_}

    (convertedW, bankCoordW)
  }
  val convertedWVec = wConversions.map{_._1}
  val bankIdW = wConversions.map{_._2}

  val rConversions = selectedRVec.map{ rbundle => 
    // Reader conversion
    val convertedR = Wire(new multidimR(N,w))
    val physicalAddrs = rbundle.addr.zip(banks).map{ case (logical, b) => logical / UInt(b) }
    physicalAddrs.zipWithIndex.foreach { case (calculatedAddr, i) => convertedR.addr(i) := calculatedAddr}
    convertedR.en := rbundle.en
    val bankCoordsR = rbundle.addr.zip(banks).map{ case (logical, b) => logical % UInt(b) }
    val bankCoordR = bankCoordsR.zipWithIndex.map{ case (c, i) => c*UInt(banks.drop(i).reduce{_*_}/banks(i)) }.reduce{_+_}

    (convertedR, bankCoordR)
  }
  val convertedRVec = rConversions.map{_._1}
  val bankIdR = rConversions.map{_._2}

  // TODO: Doing inefficient thing here of all-to-all connection between bundlesNDs and MemNDs
  // Convert bankCoords for each bundle to a bit vector
  // TODO: Probably need to have a dummy multidimW port to default to for unused banks so we don't overwrite anything
  m.zipWithIndex.foreach{ case (mem, i) => 
    val bundleSelect = bankIdW.map{ b => b === i.U }
    mem.io.wMask := bundleSelect.reduce{_|_}
    mem.io.w := chisel3.util.PriorityMux(bundleSelect, convertedWVec)
  }

  // TODO: Doing inefficient thing here of all-to-all connection between bundlesNDs and MemNDs
  // Convert bankCoords for each bundle to a bit vector
  m.zipWithIndex.foreach{ case (mem, i) => 
    val bundleSelect = bankIdR.map{ b => (b === i.U) }
    mem.io.rMask := bundleSelect.reduce{_|_}
    mem.io.r := chisel3.util.PriorityMux(bundleSelect, convertedRVec)
  }

  // Connect read data to output
  io.output.data.zip(bankIdR).foreach { case (wire, id) => 
    val sel = (0 until numMems).map{ i => (id === i.U)}
    val datas = m.map{ _.io.output.data }
    val d = chisel3.util.PriorityMux(sel, datas)
    wire := d
  }

  var wId = 0
  def connectWPort(wBundle: Vec[multidimW], en: Bool) {
    (0 until wPar).foreach{ i => 
      io.w(i + wId*wPar) := wBundle(i) 
    }
    io.globalWEn(wId) := en
    wId = wId + 1
  }
  def connectWPort(addr: List[UInt], data: List[UInt], en: List[Bool]) {
    (0 until wPar).foreach{ i => 
      io.w(i + wId*wPar).data := data(i)
      io.w(i + wId*wPar).en := en(i) 
      (0 until N).foreach { j =>
        io.w(i*N + wId*wPar + j) := addr(j + i*N)
      }
    }
    wId = wId + 1
  }
  var rId = 0
  def connectRPort(rBundle: Vec[multidimR]) {
    (0 until rPar).foreach{ i => 
      io.r(i + rId*rPar) := rBundle(i) 
    }
    rId = rId + 1
  }
  def connectRPort(addr: List[UInt]) {
    (0 until rPar).foreach{ i => 
      (0 until N).foreach { j =>
        io.r(i*N + rId*rPar + j) := addr(j + i*N)
      }
    }
    rId = rId + 1
  }


  // Connect debug signals
  val wInBound = selectedWVec.map{ v => v.addr.zip(logicalDims).map { case (addr, bound) => addr < UInt(bound) }.reduce{_&_}}.reduce{_&_}
  val rInBound = selectedRVec.map{ v => v.addr.zip(logicalDims).map { case (addr, bound) => addr < UInt(bound) }.reduce{_&_}}.reduce{_&_}
  val writeOn = selectedWVec.map{ v => v.en }
  val readOn = selectedRVec.map{ v => v.en }
  val rwOn = writeOn.zip(readOn).map{ case(a,b) => a&b}.reduce{_|_}
  val rCollide = bankIdR.zip( readOn).map{ case(id1,en1) => bankIdR.zip( readOn).map{ case(id2,en2) => Mux((id1 === id2) & en1 & en2, 1.U, 0.U)}.reduce{_+_} }.reduce{_+_} !=  readOn.map{Mux(_, 1.U, 0.U)}.reduce{_+_}
  val wCollide = bankIdW.zip(writeOn).map{ case(id1,en1) => bankIdW.zip(writeOn).map{ case(id2,en2) => Mux((id1 === id2) & en1 & en2, 1.U, 0.U)}.reduce{_+_} }.reduce{_+_} != writeOn.map{Mux(_, 1.U, 0.U)}.reduce{_+_}
  io.debug.invalidWAddr := ~wInBound
  io.debug.invalidRAddr := ~rInBound
  io.debug.rwOn := rwOn
  io.debug.readCollision := rCollide
  io.debug.writeCollision := wCollide
  io.debug.error := ~wInBound | ~rInBound | rwOn | rCollide | wCollide

}


class NBufSRAM(val logicalDims: List[Int], val numBufs: Int, val w: Int, 
           val banks: List[Int], val strides: List[Int], val numWriters: Int, val numReaders: Int,
           val wPar: Int, val rPar: Int, val bankingMode: String) extends Module { 

  // Overloaded construters
  // Tuple unpacker
  def this(tuple: (List[Int], Int, Int, List[Int], List[Int], Int, Int,
           Int, Int, String)) = this(tuple._1,tuple._2,tuple._3,tuple._4,tuple._5,tuple._6,tuple._7,tuple._8,tuple._9,tuple._10)
  // Bankmode-less
  def this(logicalDims: List[Int], numBufs: Int, w: Int, 
           banks: List[Int], strides: List[Int], numWriters: Int, numReaders: Int,
           wPar: Int, rPar: Int) = this(logicalDims, numBufs, w, banks, strides, numWriters, numReaders, wPar, rPar, "strided")
  // If 1D, spatial will make banks and strides scalars instead of lists
  def this(logicalDims: List[Int], numBufs: Int, w: Int, 
           banks: Int, strides: Int, numWriters: Int, numReaders: Int,
           wPar: Int, rPar: Int) = this(logicalDims, numBufs, w, List(banks), List(strides), numWriters, numReaders, wPar, rPar, "strided")

  val depth = logicalDims.reduce{_*_} // Size of memory
  val N = logicalDims.length // Number of dimensions

  val io = IO( new Bundle {
    // TODO: w bundle gets forcefully generated as output in verilog
    //       so the only way to make it an input seems to flatten the
    //       Vec(numWriters, Vec(wPar, _)) to a 1D vector and then reconstruct it
    val w = Vec(numWriters*wPar, new multidimW(N, 32).asInput)
    val globalWEn = Vec(numWriters, Bool().asInput)
    val wSel = Vec(numWriters, Bool().asInput) // Selects between multiple write bundles
    val r = Vec(numReaders*rPar,new multidimR(N, 32).asInput) // TODO: Spatial allows only one reader per mem
    val rSel = Vec(numReaders, Bool().asInput)
    val output = new Bundle {
      val data  = Vec(rPar, UInt(32.W).asOutput)
    }
    val debug = new Bundle {
      val invalidRAddr = Bool().asOutput
      val invalidWAddr = Bool().asOutput
      val rwOn = Bool().asOutput
      val readCollision = Bool().asOutput
      val writeCollision = Bool().asOutput
      val error = Bool().asOutput
    }
  })

  // Get info on physical dims
  // TODO: Upcast dims to evenly bank
  val physicalDims = logicalDims.zip(banks).map { case (dim, b) => dim/b}
  val numMems = banks.reduce{_*_}

  // Create physical mems
  val m = (0 until numMems).map{ i => Module(new MemND(physicalDims))}

  // Reconstruct io.w as 2d vector
  val reconstructedW = (0 until numWriters).map{ i => 
    Vec((0 until wPar).map { j => io.w(i*wPar + j) })
  }
  val reconstructedR = (0 until numReaders).map{ i => 
    Vec((0 until rPar).map { j => io.r(i*rPar + j) })
  }

  // Mux wrideBundles and connect to internal
  val selectedWVec = chisel3.util.Mux1H(io.wSel, reconstructedW)
  val selectedRVec = chisel3.util.Mux1H(io.rSel, reconstructedR)
  val selectedGlobalWen = chisel3.util.Mux1H(io.wSel, io.globalWEn)

  // TODO: Should connect multidimW's directly to their banks rather than all-to-all connections
  // Convert selectedWVec to translated physical addresses
  val wConversions = selectedWVec.map{ wbundle => 
    // Writer conversion
    val convertedW = Wire(new multidimW(N,w))
    val physicalAddrsW = wbundle.addr.zip(banks).map{ case (logical, b) => logical / UInt(b) }
    physicalAddrsW.zipWithIndex.foreach { case (calculatedAddr, i) => convertedW.addr(i) := calculatedAddr}
    convertedW.data := wbundle.data
    convertedW.en := wbundle.en & selectedGlobalWen
    val bankCoordsW = wbundle.addr.zip(banks).map{ case (logical, b) => logical % UInt(b) }
    val bankCoordW = bankCoordsW.zipWithIndex.map{ case (c, i) => c*UInt(banks.drop(i).reduce{_*_}/banks(i)) }.reduce{_+_}

    (convertedW, bankCoordW)
  }
  val convertedWVec = wConversions.map{_._1}
  val bankIdW = wConversions.map{_._2}

  val rConversions = selectedRVec.map{ rbundle => 
    // Reader conversion
    val convertedR = Wire(new multidimR(N,w))
    val physicalAddrs = rbundle.addr.zip(banks).map{ case (logical, b) => logical / UInt(b) }
    physicalAddrs.zipWithIndex.foreach { case (calculatedAddr, i) => convertedR.addr(i) := calculatedAddr}
    convertedR.en := rbundle.en
    val bankCoordsR = rbundle.addr.zip(banks).map{ case (logical, b) => logical % UInt(b) }
    val bankCoordR = bankCoordsR.zipWithIndex.map{ case (c, i) => c*UInt(banks.drop(i).reduce{_*_}/banks(i)) }.reduce{_+_}

    (convertedR, bankCoordR)
  }
  val convertedRVec = rConversions.map{_._1}
  val bankIdR = rConversions.map{_._2}

  // TODO: Doing inefficient thing here of all-to-all connection between bundlesNDs and MemNDs
  // Convert bankCoords for each bundle to a bit vector
  // TODO: Probably need to have a dummy multidimW port to default to for unused banks so we don't overwrite anything
  m.zipWithIndex.foreach{ case (mem, i) => 
    val bundleSelect = bankIdW.map{ b => b === i.U }
    mem.io.wMask := bundleSelect.reduce{_|_}
    mem.io.w := chisel3.util.PriorityMux(bundleSelect, convertedWVec)
  }

  // TODO: Doing inefficient thing here of all-to-all connection between bundlesNDs and MemNDs
  // Convert bankCoords for each bundle to a bit vector
  m.zipWithIndex.foreach{ case (mem, i) => 
    val bundleSelect = bankIdR.map{ b => (b === i.U) }
    mem.io.rMask := bundleSelect.reduce{_|_}
    mem.io.r := chisel3.util.PriorityMux(bundleSelect, convertedRVec)
  }

  // Connect read data to output
  io.output.data.zip(bankIdR).foreach { case (wire, id) => 
    val sel = (0 until numMems).map{ i => (id === i.U)}
    val datas = m.map{ _.io.output.data }
    val d = chisel3.util.PriorityMux(sel, datas)
    wire := d
  }

  var wId = 0
  def connectWPort(wBundle: Vec[multidimW], en: Bool) {
    (0 until wPar).foreach{ i => 
      io.w(i + wId*wPar) := wBundle(i) 
    }
    io.globalWEn(wId) := en
    wId = wId + 1
  }
  def connectWPort(addr: List[UInt], data: List[UInt], en: List[Bool]) {
    (0 until wPar).foreach{ i => 
      io.w(i + wId*wPar).data := data(i)
      io.w(i + wId*wPar).en := en(i) 
      (0 until N).foreach { j =>
        io.w(i*N + wId*wPar + j) := addr(j + i*N)
      }
    }
    wId = wId + 1
  }
  var rId = 0
  def connectRPort(rBundle: Vec[multidimR]) {
    (0 until rPar).foreach{ i => 
      io.r(i + rId*rPar) := rBundle(i) 
    }
    rId = rId + 1
  }
  def connectRPort(addr: List[UInt]) {
    (0 until rPar).foreach{ i => 
      (0 until N).foreach { j =>
        io.r(i*N + rId*rPar + j) := addr(j + i*N)
      }
    }
    rId = rId + 1
  }


  // Connect debug signals
  val wInBound = selectedWVec.map{ v => v.addr.zip(logicalDims).map { case (addr, bound) => addr < UInt(bound) }.reduce{_&_}}.reduce{_&_}
  val rInBound = selectedRVec.map{ v => v.addr.zip(logicalDims).map { case (addr, bound) => addr < UInt(bound) }.reduce{_&_}}.reduce{_&_}
  val writeOn = selectedWVec.map{ v => v.en }
  val readOn = selectedRVec.map{ v => v.en }
  val rwOn = writeOn.zip(readOn).map{ case(a,b) => a&b}.reduce{_|_}
  val rCollide = bankIdR.zip( readOn).map{ case(id1,en1) => bankIdR.zip( readOn).map{ case(id2,en2) => Mux((id1 === id2) & en1 & en2, 1.U, 0.U)}.reduce{_+_} }.reduce{_+_} !=  readOn.map{Mux(_, 1.U, 0.U)}.reduce{_+_}
  val wCollide = bankIdW.zip(writeOn).map{ case(id1,en1) => bankIdW.zip(writeOn).map{ case(id2,en2) => Mux((id1 === id2) & en1 & en2, 1.U, 0.U)}.reduce{_+_} }.reduce{_+_} != writeOn.map{Mux(_, 1.U, 0.U)}.reduce{_+_}
  io.debug.invalidWAddr := ~wInBound
  io.debug.invalidRAddr := ~rInBound
  io.debug.rwOn := rwOn
  io.debug.readCollision := rCollide
  io.debug.writeCollision := wCollide
  io.debug.error := ~wInBound | ~rInBound | rwOn | rCollide | wCollide

}
