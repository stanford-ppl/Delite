// See LICENSE.txt for license details.
package templates

import chisel3._

object Utils {
  def delay[T <: chisel3.core.Data](sig: T, length: Int):T = {
    if (length == 0) {
      sig
    } else {
      val regs = (0 until length).map { i => Reg(init = 0.U) } // TODO: Make this type T
      sig match {
        case s:Bool => 
          regs(0) := Mux(s, 1.U, 0.U)
          (length-1 until 0 by -1).map { i => 
            regs(i) := regs(i-1)
          }
          (regs(length-1) === 1.U).asInstanceOf[T]
        case s:UInt => 
          regs(0) := s
          (length-1 until 0 by -1).map { i => 
            regs(i) := regs(i-1)
          }
          (regs(length-1)).asInstanceOf[T]
      }
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
