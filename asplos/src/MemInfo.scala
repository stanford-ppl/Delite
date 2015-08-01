package asplos
import asplos._

import scala.collection.mutable.ListBuffer

trait MemInfoTrait {
  val IR: PPLOpsExp
  import IR._

  object MemoryTemplate extends Enumeration {
    type MemoryTemplate = Value
    val BLK_RAM, FIFO, CAM, REGFILE, PQ = Value
  }

  object Location extends Enumeration {
    type Location = Value
    val ON_CHIP, OFF_CHIP, CACHED = Value
  }


  import MemoryTemplate._
  import Location._
  class MemInfo {
      val template : MemoryTemplate = BLK_RAM
      val typeName : String = "Int"
      val widthBits: Int = 32
      val depth:    Int = 0
      val readers: ListBuffer[Sym[Any]] = ListBuffer[Sym[Any]]()
      val writers: ListBuffer[Sym[Any]] = ListBuffer[Sym[Any]]()
      val location: Location = ON_CHIP
      val vectorWidth: Int = 1
      val isDblBuf: Boolean = false
  }
}
