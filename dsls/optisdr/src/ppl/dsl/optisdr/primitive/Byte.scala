package ppl.dsl.optisdr.primitive

import java.io.{PrintWriter}

import scala.reflect.Manifest
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import ppl.dsl.optila.capabilities.ArithOps

import ppl.dsl.optisdr._

trait ByteOps extends Variables {
  this: OptiSDR with ArithOps =>
  
  implicit def byteToRepByte(x: Byte) = unit(x)
  
  implicit def repToByteOps(x: Rep[Byte]) = new ByteOpsCls(x)
  implicit def varToByteOps(x: Var[Byte]) = new ByteOpsCls(readVar(x))
  
  // Objects methods
  class ByteOpsCls(x: Rep[Byte]) {
    def unary_~()(implicit ctx: SourceContext) = byte_binarynot(x)
    def &(y: Rep[Byte])(implicit ctx: SourceContext) = byte_binaryand(x,y)
    def |(y: Rep[Byte])(implicit ctx: SourceContext) = byte_binaryor(x,y)
    def ^(y: Rep[Byte])(implicit ctx: SourceContext) = byte_binaryxor(x,y)
    
    def <<(y: Rep[Int])(implicit ctx: SourceContext) = byte_lshift(x, y)
    def >>(y: Rep[Int])(implicit ctx: SourceContext) = byte_rshift(x, y)
    def >>>(y: Rep[Int])(implicit ctx: SourceContext) = byte_rashift(x, y)
  }
  
  // Arith implicit
  implicit def byteArith : Arith[Byte] = new Arith[Byte] {
    def +=(a: Rep[Byte], b: Rep[Byte])(implicit ctx: SourceContext) = arith_plus(a,b)
    def +(a: Rep[Byte], b: Rep[Byte])(implicit ctx: SourceContext) = arith_plus(a,b)
    def -(a: Rep[Byte], b: Rep[Byte])(implicit ctx: SourceContext) = arith_minus(a,b)
    def *(a: Rep[Byte], b: Rep[Byte])(implicit ctx: SourceContext) = arith_times(a,b)
    def /(a: Rep[Byte], b: Rep[Byte])(implicit ctx: SourceContext) = byte_divide(a,b)
    def abs(a: Rep[Byte])(implicit ctx: SourceContext) = arith_abs(a)
    def exp(a: Rep[Byte])(implicit ctx: SourceContext) = arith_exp(a).asInstanceOf[Byte]
    
    def empty(implicit ctx: SourceContext) = unit(0.toByte)
    def zero(a: Rep[Byte])(implicit ctx: SourceContext) = empty
  }
  
  def byte_divide(x: Rep[Byte], y: Rep[Byte])(implicit ctx: SourceContext) : Rep[Byte]
  
  // Binary ops
  def byte_binarynot(x: Rep[Byte])(implicit ctx: SourceContext) : Rep[Byte]
  def byte_binaryand(x: Rep[Byte], y: Rep[Byte])(implicit ctx: SourceContext) : Rep[Byte]
  def byte_binaryor(x: Rep[Byte], y: Rep[Byte])(implicit ctx: SourceContext) : Rep[Byte]
  def byte_binaryxor(x: Rep[Byte], y: Rep[Byte])(implicit ctx: SourceContext) : Rep[Byte]
  
  def byte_lshift(a: Rep[Byte], b: Rep[Int])(implicit ctx: SourceContext) : Rep[Byte]
  def byte_rshift(a: Rep[Byte], b: Rep[Int])(implicit ctx: SourceContext) : Rep[Byte]
  def byte_rashift(a: Rep[Byte], b: Rep[Int])(implicit ctx: SourceContext) : Rep[Byte]
}

trait ByteOpsExp extends ByteOps with VariablesExp with BaseFatExp {
  this: OptiSDRExp =>
  
  // Operations
  case class ByteDivide(x: Exp[Byte], y: Exp[Byte]) extends Def[Byte]
  
  case class ByteBinaryNot(x: Exp[Byte]) extends Def[Byte]
  case class ByteBinaryAnd(x: Exp[Byte], y: Exp[Byte]) extends Def[Byte]
  case class ByteBinaryOr(x: Exp[Byte], y: Exp[Byte]) extends Def[Byte]
  case class ByteBinaryXor(x: Exp[Byte], y: Exp[Byte]) extends Def[Byte]
  
  case class ByteLShift(a: Exp[Byte], b: Rep[Int]) extends Def[Byte]
  case class ByteRShift(a: Exp[Byte], b: Rep[Int]) extends Def[Byte]
  case class ByteRAShift(a: Exp[Byte], b: Rep[Int]) extends Def[Byte]
  
  def byte_divide(x: Exp[Byte], y: Exp[Byte])(implicit ctx: SourceContext) = reflectPure(ByteDivide(x, y))
  
  def byte_binarynot(x: Exp[Byte])(implicit ctx: SourceContext) = reflectPure(ByteBinaryNot(x))
  def byte_binaryand(x: Exp[Byte], y: Exp[Byte])(implicit ctx: SourceContext) = reflectPure(ByteBinaryAnd(x, y))
  def byte_binaryor(x: Exp[Byte], y: Exp[Byte])(implicit ctx: SourceContext) = reflectPure(ByteBinaryOr(x, y))
  def byte_binaryxor(x: Exp[Byte], y: Exp[Byte])(implicit ctx: SourceContext) = reflectPure(ByteBinaryXor(x, y))
  
  def byte_lshift(a: Rep[Byte], b: Rep[Int])(implicit ctx: SourceContext) = reflectPure(ByteLShift(a,b))
  def byte_rshift(a: Rep[Byte], b: Rep[Int])(implicit ctx: SourceContext) = reflectPure(ByteRShift(a,b))
  def byte_rashift(a: Rep[Byte], b: Rep[Int])(implicit ctx: SourceContext) = reflectPure(ByteRAShift(a,b))
}


trait ByteOpsExpOpt extends ByteOpsExp {
  this: OptiSDRExp =>
}

trait BaseGenByteOps extends GenericFatCodegen {
  val IR: ByteOpsExp
  import IR._
  
  override def unapplySimpleIndex(e: Def[Any]) = e match {
    // What is this for???
    case _ => super.unapplySimpleIndex(e)
  }  
}

trait ScalaGenByteOps extends BaseGenByteOps with ScalaGenFat {
  val IR: ByteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure

    case ByteDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    
    case ByteBinaryNot(lhs) => emitValDef(sym, "~" + quote(lhs))
    case ByteBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case ByteBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case ByteBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    
    case ByteLShift(lhs,rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
    case ByteRShift(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case ByteRAShift(lhs,rhs) => emitValDef(sym, quote(lhs) + " <<< " + quote(rhs))
    
    case _ => super.emitNode(sym, rhs)
  }
}