package ppl.dsl.deliszt.intm

import ppl.dsl.delizst.datastruct.CudaGenDataStruct
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.common.{CudaGenBase, ScalaGenBase, CGenBase}
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.delizst.{DeLisztExp, DeLiszt}
import ppl.dsl.delizst.datastruct.scala._
import ppl.dsl.delizst.datastruct.scala.MetaInteger._

trait IntMOps extends DSLType with Variables {
  this: DeLiszt =>

  val _0 : IntM
  val _1 : IntM
  val _2 : IntM
  val _3 : IntM
  val _4 : IntM
  val _5 : IntM
  val _6 : IntM
  val _7 : IntM
  val _8 : IntM
  val _9 : IntM
  val _10 : IntM
  val _11 : IntM
  val _12 : IntM
  val _13 : IntM
  val _14 : IntM
  val _15 : IntM
  val _16 : IntM
  val _17 : IntM
  val _18 : IntM
  val _19 : IntM
  val _20 : IntM
  val _21 : IntM
  val _22 : IntM

  def intm_obj[IntM]
}


trait IntMOpsExp extends IntMOps with VariablesExp {
  this: DeLisztExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure
  case class IntMObject(value : Int) extends Def[IntM] {
  }

  /////////////////////////////////////
  // implemented via kernel embedding

  ////////////////////
  // object interface

  ///////////////////
  // class interface
}

trait ScalaGenIntMOps extends ScalaGenBase {
  val IR: IntMOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case m@IntMObjectNew(numRows, numCols) => emitValDef(sym, "new " + remap(m.mM) + "(" + quote(numRows) + "," + quote(numCols) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenIntMOps extends CudaGenBase with CudaGenDataStruct {
  val IR: IntMOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenIntMOps extends CGenBase {
  val IR: IntMOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
