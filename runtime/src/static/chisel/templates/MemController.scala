package templates

import chisel3._

class FromAccel(val p: Int) extends Bundle {
  // Command signals
  val base   = UInt(32.W)
  val offset   = UInt(32.W)
  val size = UInt(32.W)
  val en = Bool()

  // Data signals  
  val data = Vec(p, UInt(32.W))
  val pop = Bool()

  override def cloneType = (new FromAccel(p)).asInstanceOf[this.type] // See chisel3 bug 358
}
class ToAccel(val p: Int) extends Bundle {
  val data   = Vec(p, UInt(32.W))
  val valid = Bool()
  val done = Bool()

  override def cloneType = (new ToAccel(p)).asInstanceOf[this.type] // See chisel3 bug 358
}
class FromDRAM(val p: Int) extends Bundle {
  val data   = Vec(p, UInt(32.W))
  val tag = UInt(32.W)
  val valid = Bool()

  override def cloneType = (new FromDRAM(p)).asInstanceOf[this.type] // See chisel3 bug 358
}
class ToDRAM(val p: Int) extends Bundle {
  val addr   = UInt(32.W)
  val size  = UInt(32.W)
  val data = Vec(p, UInt(32.W))
  val base = UInt(32.W)
  val tag = UInt(32.W)
  val valid = Bool()

  override def cloneType = (new ToDRAM(p)).asInstanceOf[this.type] // See chisel3 bug 358
}


class MemController(val p: Int) extends Module {
  val io = IO(new Bundle{
    val AccelToCtrl = new FromAccel(p).asInput
    val CtrlToAccel = new ToAccel(p).asOutput
    val DRAMToCtrl = new FromDRAM(p).asInput
    val CtrlToDRAM = new ToDRAM(p).asOutput
  })

  // TODO: Implement full memory controller that interfaces with DRAM or DRAMSim

  // Temporarily pass through signals from hw to test harness
  io.CtrlToDRAM.base := io.AccelToCtrl.base // Only used for the janky mem controller
  io.CtrlToDRAM.addr := io.AccelToCtrl.offset + io.AccelToCtrl.base
  io.CtrlToDRAM.data.zip(io.AccelToCtrl.data).foreach{ case (data, port) => data := port }
  io.CtrlToDRAM.size := io.AccelToCtrl.size
  io.CtrlToDRAM.valid := io.AccelToCtrl.en

  io.CtrlToAccel.data.zip(io.DRAMToCtrl.data).foreach{ case (data, port) => data := port }
  io.CtrlToAccel.valid := io.DRAMToCtrl.valid

  // Create FIFO to hold data from DRAM
  val burstSize = 64
  val fifo = Module(new FIFO(p, p, burstSize))
  fifo.io.in := io.DRAMToCtrl.data
  fifo.io.push := io.DRAMToCtrl.valid
  fifo.io.pop := io.AccelToCtrl.pop
  io.CtrlToAccel.data := fifo.io.out
  io.CtrlToAccel.valid := !fifo.io.empty | (io.AccelToCtrl.en & io.CtrlToDRAM.size === 0.U)

}

