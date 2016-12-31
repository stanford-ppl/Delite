package templates

import chisel3._



class Mem1D(val size: Int) extends Module { // Unbanked, inner 1D mem
  val io = IO( new Bundle {
    val input = new Bundle {
      val wAddr = UInt(32.W).asInput
      val wData = UInt(32.W).asInput
      val rAddr = UInt(32.W).asInput
      val wEn = Bool().asInput
      val rEn = Bool().asInput
    }
    val output = new Bundle {
      val rData  = UInt(32.W).asOutput
    }
  })

  // We can do better than MaxJ by forcing mems to be single-ported since
  //   we know how to properly schedule reads and writes
  val m = Mem(UInt(width = 32), size /*, seqRead = true deprecated? */)

  val reg_rAddr = Reg(UInt())
  when (io.input.wEn) {m(io.input.wAddr) := io.input.wData}
  .elsewhen (io.input.rEn) {reg_rAddr := io.input.rAddr}

  io.output.rData := m(reg_rAddr)

}

// Last dimension is the leading-dim
class MemND(val dims: List[Int]) extends Module { 
  val depth = dims.reduce{_*_} // Size of memory
  val N = dims.length // Number of dimensions

  val io = IO( new Bundle {
    val input = new Bundle {
      val wAddr = Vec(N, UInt(32.W).asInput)
      val rAddr = Vec(N, UInt(32.W).asInput)
      val wData = UInt(32.W).asInput
      val wEn = Bool().asInput
      val rEn = Bool().asInput
    }
    val output = new Bundle {
      val rData  = UInt(32.W).asOutput
      val invalidRAddr = Bool().asOutput
      val invalidWAddr = Bool().asOutput
    }
  })

  // Instantiate 1D mem
  val m = Module(new Mem1D(depth))

  // Address flattening
  m.io.input.wAddr := io.input.wAddr.zipWithIndex.map{ case (addr, i) =>
    addr * UInt(dims.drop(i).reduce{_*_}/dims(i))
  }.reduce{_+_}
  m.io.input.rAddr := io.input.rAddr.zipWithIndex.map{ case (addr, i) =>
    addr * UInt(dims.drop(i).reduce{_*_}/dims(i))
  }.reduce{_+_}

  // Check if read/write is in bounds
  val rInBound = io.input.rAddr.zip(dims).map { case (addr, bound) => addr < UInt(bound) }.reduce{_&_}
  val wInBound = io.input.wAddr.zip(dims).map { case (addr, bound) => addr < UInt(bound) }.reduce{_&_}

  // Connect the other ports
  m.io.input.wData := io.input.wData
  m.io.input.wEn := io.input.wEn & wInBound
  m.io.input.rEn := io.input.rEn & rInBound
  io.output.rData := m.io.output.rData
  io.output.invalidWAddr := ~wInBound
  io.output.invalidRAddr := ~rInBound
}

class SRAM(val virtDims: List[Int], val numBufs: Int, val w: Int, 
           val banks: List[Int], val strides: List[Int], val numWriters: Int) extends Module { 
  val depth = virtDims.reduce{_*_} // Size of memory
  val N = virtDims.length // Number of dimensions

  val io = IO( new Bundle {
    val input = new Bundle {
      val wAddr = Vec(N, UInt(32.W).asInput)
      val rAddr = Vec(N, UInt(32.W).asInput)
      val wData = UInt(32.W).asInput
      val wEn = Bool().asInput
      val rEn = Bool().asInput
    }
    val output = new Bundle {
      val rData  = UInt(32.W).asOutput
      val invalidRAddr = Bool().asOutput
      val invalidWAddr = Bool().asOutput
    }
  })


  // // TODO: Upcast dims to evenly bank
  // val physDims = virtDims.zip(banks).map { case (dim, b) => dim/b}
  // val numMems = banks.reduce{_*_}

  // // Instantiate 1D mem
  // val m = Module(new Mem1D(depth))

  // // Address flattening
  // m.io.input.wAddr := io.input.wAddr.zipWithIndex.map{ case (addr, i) =>
  //   addr * UInt(dims.drop(i).reduce{_*_}/dims(i))
  // }.reduce{_+_}
  // m.io.input.rAddr := io.input.rAddr.zipWithIndex.map{ case (addr, i) =>
  //   addr * UInt(dims.drop(i).reduce{_*_}/dims(i))
  // }.reduce{_+_}

  // // Check if read/write is in bounds
  // val rInBound = io.input.rAddr.zip(dims).map { case (addr, bound) => addr < UInt(bound) }.reduce{_&_}
  // val wInBound = io.input.wAddr.zip(dims).map { case (addr, bound) => addr < UInt(bound) }.reduce{_&_}

  // // Connect the other ports
  // m.io.input.wData := io.input.wData
  // m.io.input.wEn := io.input.wEn & wInBound
  // m.io.input.rEn := io.input.rEn & rInBound
  // io.output.rData := m.io.output.rData
  // io.output.invalidWAddr := ~wInBound
  // io.output.invalidRAddr := ~rInBound
}
