package ppl.dsl.optisdr.primitive

import java.io.{PrintWriter}

import scala.reflect.Manifest
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import ppl.dsl.optisdr._

trait UIntOps extends Variables {
  this: OptiSDR =>
    
  // Static methods
  object UInt {
    def apply(x: Int)(implicit ctx: SourceContext) = uint_new(unit(x))
    def apply(x: Rep[Int])(implicit ctx: SourceContext) = uint_new(x)
  }
  
  // Implicit numeric conversions
  //implicit def repIntToRepUInt(x: Rep[Int]): Rep[UInt] = UInt(x)
  
  implicit def repToUIntOps(x: Rep[UInt]) = new UIntOpsCls(x)
  implicit def varToUIntOps(x: Var[UInt]) = new UIntOpsCls(readVar(x))
  
  // Objects methods
  class UIntOpsCls(x: Rep[UInt]) {
    def +(y: Rep[UInt])(implicit ctx: SourceContext) = uint_plus(x,y)
    def -(y: Rep[UInt])(implicit ctx: SourceContext) = uint_minus(x,y)
    def *(y: Rep[UInt])(implicit ctx: SourceContext) = uint_times(x,y)
    def /(y: Rep[UInt])(implicit ctx: SourceContext) = uint_divide(x,y)

    def unary_~()(implicit ctx: SourceContext) = uint_binarynot(x)
    def &(y: Rep[UInt])(implicit ctx: SourceContext) = uint_binaryand(x,y)
    def |(y: Rep[UInt])(implicit ctx: SourceContext) = uint_binaryor(x,y)
    def ^(y: Rep[UInt])(implicit ctx: SourceContext) = uint_binaryxor(x,y)
    def abs(implicit ctx: SourceContext) = uint_abs(x)
    def exp(implicit ctx: SourceContext) = uint_exp(x)
    
    def <<(y: Rep[Int])(implicit ctx: SourceContext) = uint_lshift(x, y)
    def >>(y: Rep[Int])(implicit ctx: SourceContext) = uint_rshift(x, y)
    def >>>(y: Rep[Int])(implicit ctx: SourceContext) = uint_rashift(x, y)
    
    def intValue = uint_int_value(x)
  }
  
  // Arith implicit
  implicit def uintArith : Arith[UInt] = new Arith[UInt] {
    def +=(a: Rep[UInt], b: Rep[UInt])(implicit ctx: SourceContext) = uint_plus(a,b)
    def +(a: Rep[UInt], b: Rep[UInt])(implicit ctx: SourceContext) = uint_plus(a,b)
    def -(a: Rep[UInt], b: Rep[UInt])(implicit ctx: SourceContext) = uint_minus(a,b)
    def *(a: Rep[UInt], b: Rep[UInt])(implicit ctx: SourceContext) = uint_times(a,b)
    def /(a: Rep[UInt], b: Rep[UInt])(implicit ctx: SourceContext) = uint_divide(a,b)
    def abs(a: Rep[UInt])(implicit ctx: SourceContext) = uint_abs(a)
    def exp(a: Rep[UInt])(implicit ctx: SourceContext) = uint_exp(a)
    
    def empty(implicit ctx: SourceContext) = UInt(unit(0))
    def zero(a: Rep[UInt])(implicit ctx: SourceContext) = empty
  }
  
  // Constructor
  def uint_new(x: Rep[Int])(implicit ctx: SourceContext) : Rep[UInt]
  
  // Math ops
  def uint_plus(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_minus(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_times(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_divide(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  
  // Binary ops
  def uint_binarynot(x: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_binaryand(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_binaryor(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_binaryxor(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  
  def uint_lshift(a: Rep[UInt], b: Rep[Int])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_rshift(a: Rep[UInt], b: Rep[Int])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_rashift(a: Rep[UInt], b: Rep[Int])(implicit ctx: SourceContext) : Rep[UInt]
  
  def uint_abs(x: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_exp(x: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  
  // Conversions
  def uint_int_value(x: Rep[UInt]) : Rep[Int]
}

trait UIntOpsExp extends UIntOps with VariablesExp with BaseFatExp {
  this: OptiSDRExp =>
  
  // Object creation
  case class UIntNew(x: Exp[Int]) extends Def[UInt]
  
  def uint_new(x: Exp[Int])(implicit ctx: SourceContext) = reflectPure(UIntNew(x))
  
  // Operations
  case class UIntPlus(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  case class UIntMinus(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  case class UIntTimes(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  case class UIntDivide(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  
  case class UIntBinaryNot(x: Exp[UInt]) extends Def[UInt]
  case class UIntBinaryAnd(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  case class UIntBinaryOr(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  case class UIntBinaryXor(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  
  case class UIntLShift(a: Exp[UInt], b: Rep[Int]) extends Def[UInt]
  case class UIntRShift(a: Exp[UInt], b: Rep[Int]) extends Def[UInt]
  case class UIntRAShift(a: Exp[UInt], b: Rep[Int]) extends Def[UInt]
  
  case class UIntAbs(x: Exp[UInt]) extends Def[UInt]
  case class UIntExp(x: Exp[UInt]) extends Def[UInt]
  
  def uint_plus(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntPlus(x, y))
  def uint_minus(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntMinus(x, y))
  def uint_times(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntTimes(x, y))
  def uint_divide(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntDivide(x, y))
  
  def uint_binarynot(x: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntBinaryNot(x))
  def uint_binaryand(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntBinaryAnd(x, y))
  def uint_binaryor(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntBinaryOr(x, y))
  def uint_binaryxor(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntBinaryXor(x, y))
  
  def uint_lshift(a: Rep[UInt], b: Rep[Int])(implicit ctx: SourceContext) = reflectPure(UIntLShift(a,b))
  def uint_rshift(a: Rep[UInt], b: Rep[Int])(implicit ctx: SourceContext) = reflectPure(UIntRShift(a,b))
  def uint_rashift(a: Rep[UInt], b: Rep[Int])(implicit ctx: SourceContext) = reflectPure(UIntRAShift(a,b))
  
  def uint_abs(x: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntAbs(x))
  def uint_exp(x: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntExp(x))
  
  // Conversions
  case class UIntIntValue(x: Exp[UInt]) extends Def[Int]
  
  def uint_int_value(x: Exp[UInt]) = reflectPure(UIntIntValue(x))
}


trait UIntOpsExpOpt extends UIntOpsExp {
  this: OptiSDRExp =>
  
  object UIntConst {
    def unapply(x: Exp[UInt]) : Option[Int] = {
      x match {
        case Def(UIntNew(Const(e))) => Some(e)
      }
    }
  }

  override def uint_plus(lhs: Exp[UInt], rhs: Exp[UInt])(implicit ctx: SourceContext) : Exp[UInt] = (lhs,rhs) match {
    case (UIntConst(x), UIntConst(y)) => UInt(x+y)
    case (UIntConst(0), y) => y
    case (y, UIntConst(0)) => y
    case _ => super.uint_plus(lhs, rhs)
  }
  
  override def uint_minus(lhs: Exp[UInt], rhs: Exp[UInt])(implicit ctx: SourceContext) : Exp[UInt] = (lhs,rhs) match {
    case (UIntConst(x), UIntConst(y)) => UInt(x-y)
    case (y, UIntConst(0)) => y
    case _ => super.uint_minus(lhs, rhs)
  }
  
  override def uint_times(lhs: Exp[UInt], rhs: Exp[UInt])(implicit ctx: SourceContext) : Exp[UInt] = (lhs,rhs) match {
    case (UIntConst(x), UIntConst(y)) => UInt(x*y)
    case (_, UIntConst(0)) => rhs
    case (UIntConst(0), _) => lhs
    case _ => super.uint_times(lhs, rhs)
  }
}

trait BaseGenUIntOps extends GenericFatCodegen {
  val IR: UIntOpsExp
  import IR._
  
  override def unapplySimpleIndex(e: Def[Any]) = e match {
    // What is this for???
    case _ => super.unapplySimpleIndex(e)
  }  
}

trait ScalaGenUIntOps extends BaseGenUIntOps with ScalaGenFat {
  val IR: UIntOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure

    case UIntPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case UIntMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case UIntTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case UIntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    
    case UIntAbs(e) => emitValDef(sym, quote(e))
    case UIntExp(e) => emitValDef(sym, "exp(" + quote(e) + ")")
    
    case UIntBinaryNot(lhs) => emitValDef(sym, "~" + quote(lhs))
    case UIntBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case UIntBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case UIntBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    
    case UIntLShift(lhs,rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
    case UIntRShift(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case UIntRAShift(lhs,rhs) => emitValDef(sym, quote(lhs) + " <<< " + quote(rhs))
    
    case _ => super.emitNode(sym, rhs)
  }
}