package ppl.dsl.deliszt.intm

import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication}
import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.common.{CudaGenBase, ScalaGenBase, CGenBase}
import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

trait IntMOps extends Variables {
  this: DeLiszt =>

  /* val _0 = intm_obj[_0]
  val _1 = intm_obj[_1]
  val _2 = intm_obj[_2]
  val _3 = intm_obj[_3]
  val _4 = intm_obj[_4]
  val _5 = intm_obj[_5]
  val _6 = intm_obj[_6]
  val _7 = intm_obj[_7]
  val _8 = intm_obj[_8]
  val _9 = intm_obj[_9]
  val _10 = intm_obj[_10]
  val _11 = intm_obj[_11]
  val _12 = intm_obj[_12]
  val _13 = intm_obj[_13]
  val _14 = intm_obj[_14]
  val _15 = intm_obj[_15]
  val _16 = intm_obj[_16]
  val _17 = intm_obj[_17]
  val _18 = intm_obj[_18]
  val _19 = intm_obj[_19]
  val _20 = intm_obj[_20]
  val _21 = intm_obj[_21]
  val _22 = intm_obj[_22]

  def intm_obj[N<:IntM:MVal] : Rep[IntM] */
}

trait IntMOpsExp extends IntMOps with VariablesExp {
  this: DeLisztExp  =>

 /* //////////////////////////////////////////////////
  // implemented via method on real data structure
  case class IntMObject[N<:IntM:MVal]() extends Def[IntM] {
    val depth = MIntDepth[N]
  }

  /////////////////////////////////////
  // implemented via kernel embedding

  ////////////////////
  // object interface
  def intm_obj[N<:IntM:MVal] = IntMObject[N]

  ///////////////////
  // class interface */
}

trait ScalaGenIntMOps extends ScalaGenBase {
  val IR: IntMOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // these are the ops that call through to the underlying real data structure
    //case m@IntMObject() => emitValDef(sym, "generated.scala.MetaInteger._" + m.depth)
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenIntMOps extends CudaGenBase {
  val IR: IntMOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenIntMOps extends CGenBase {
  val IR: IntMOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
