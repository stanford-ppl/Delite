// See LICENSE.txt for license details.
package templates

import chisel3._

object Utils {
  def delay(sig: Bool, length: Int) = {
    if (length == 0) {
      sig
    } else {
      val regs = (0 until length).map { i => Reg(init = 0.U) }
      regs(0) := sig
      (length-1 until 0 by -1).map { i => 
        regs(i) := regs(i-1)
      }
      regs(length-1)
    }
  }

  def min[T <: chisel3.core.Data](a: T, b: T): T = {
    (a,b) match {
      case (aa:UInt,bb:UInt) => Mux(aa < bb, a, b)
      case (_,_) => a // TODO: implement for other types
    }
  }

  def max[T <: chisel3.core.Data](a: T, b: T): T = {
    (a,b) match {
      case (aa:UInt,bb:UInt) => Mux(aa > bb, a, b)
      case (_,_) => a // TODO: implement for other types
    }
  }
}
