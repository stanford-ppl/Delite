package templates

import chisel3._

class FromAccel(val p: Int) extends Bundle {
  val addr   = UInt(32.W)
  val size = UInt(32.W)
  val data = Vec(p, UInt(32.W))
  val valid = Bool()
  val read = Bool() // For acknowledging data going ToAccel

  override def cloneType = (new FromAccel(p)).asInstanceOf[this.type] // See chisel3 bug 358
}
class ToAccel(val p: Int) extends Bundle {
  val data   = Vec(p, UInt(32.W))
  val valid = Bool()

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
  val tag = UInt(32.W)
  val valid = Bool()

  override def cloneType = (new ToDRAM(p)).asInstanceOf[this.type] // See chisel3 bug 358
}


class MemController(val p: Int) extends Module {
  val io = IO(new Bundle{
    val AccelIn = new FromAccel(p).asInput
    val AccelOut = new ToAccel(p).asOutput
    val DRAMIn = new FromDRAM(p).asInput
    val DRAMOut = new ToDRAM(p).asOutput
  })

  // TODO: Implement full memory controller that interfaces with DRAM or DRAMSim

  // Temporarily pass through signals from hw to test harness
  io.DRAMOut.addr := io.AccelIn.addr
  io.DRAMOut.data.zip(io.AccelIn.data).foreach{ case (data, port) => data := port }
  io.DRAMOut.valid := io.AccelIn.valid
  io.DRAMOut.size := io.AccelIn.size

  io.AccelOut.data.zip(io.DRAMIn.data).foreach{ case (data, port) => data := port }
  io.AccelOut.valid := io.DRAMIn.valid

}

