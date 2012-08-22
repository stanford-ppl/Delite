package ppl.dsl.deliszt.extern

import scala.virtualization.lms.internal._
import java.io.PrintWriter
import ppl.delite.framework.extern.codegen.cuda.CudaGenExternalBase
import ppl.dsl.deliszt.datastruct.CudaGenDataStruct

import ppl.dsl.deliszt.DeLisztExp

trait DeLisztCudaGenExternal extends CudaGenExternalBase with CudaGenDataStruct {
  val IR: DeLisztExp
  import IR._

  override def emitExternalNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitExternalNode(sym, rhs)
  }
    
  override def emitExternalLib(rhs: Def[Any]): Unit = rhs match {
    case _ => super.emitExternalLib(rhs)
  }
}
