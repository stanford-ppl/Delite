package ppl.dsl.optisdr.primitive

import java.io.{PrintWriter}

import scala.reflect.Manifest
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import ppl.dsl.optisdr._

trait SDRIntOps extends Variables {
  this: OptiSDR =>
  
  implicit def repToSDRIntOps(x: Rep[Int]) = new SDRIntOpsCls(x)
  implicit def varToSDRIntOps(x: Var[Int]) = new SDRIntOpsCls(readVar(x))
  
  // Objects methods
  class SDRIntOpsCls(x: Rep[Int]) {
    def <<(y: Rep[Int])(implicit ctx: SourceContext) = sdrint_lshift(x, y)
    def >>(y: Rep[Int])(implicit ctx: SourceContext) = sdrint_rshift(x, y)
    def >>>(y: Rep[Int])(implicit ctx: SourceContext) = sdrint_rashift(x, y)
  }

  def sdrint_lshift(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) : Rep[Int]
  def sdrint_rshift(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) : Rep[Int]
  def sdrint_rashift(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) : Rep[Int]
}

trait SDRIntOpsExp extends SDRIntOps with VariablesExp with BaseFatExp {
  this: OptiSDRExp =>

  case class SDRIntLShift(a: Exp[Int], b: Rep[Int]) extends Def[Int]
  case class SDRIntRShift(a: Exp[Int], b: Rep[Int]) extends Def[Int]
  case class SDRIntRAShift(a: Exp[Int], b: Rep[Int]) extends Def[Int]
  
  def sdrint_lshift(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = reflectPure(SDRIntLShift(a,b))
  def sdrint_rshift(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = reflectPure(SDRIntRShift(a,b))
  def sdrint_rashift(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = reflectPure(SDRIntRAShift(a,b))
}

trait SDRIntOpsExpOpt extends SDRIntOpsExp {
  this: OptiSDRExp =>
}

trait BaseGenSDRIntOps extends GenericFatCodegen {
  val IR: SDRIntOpsExp
  import IR._
  
  override def unapplySimpleIndex(e: Def[Any]) = e match {
    // What is this for???
    case _ => super.unapplySimpleIndex(e)
  }  
}

trait ScalaGenSDRIntOps extends BaseGenSDRIntOps with ScalaGenFat {
  val IR: SDRIntOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case SDRIntLShift(lhs,rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
    case SDRIntRShift(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case SDRIntRAShift(lhs,rhs) => emitValDef(sym, quote(lhs) + " <<< " + quote(rhs))
    
    case _ => super.emitNode(sym, rhs)
  }
}