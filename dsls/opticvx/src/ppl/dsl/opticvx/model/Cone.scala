package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq


trait Cone extends HasArity[Cone] {
  val size: IRPoly
  def conj: Cone

  def simplify: Cone

  def project_eval(params: Seq[Int], v: Seq[Double]): Seq[Double]
}

case class ConeZero(val arity: Int) extends Cone {
  val size: IRPoly = IRPoly.const(0, arity)
  def conj: Cone = ConeZero(arity)

  def arityOp(op: ArityOp): Cone = ConeZero(op.arity)

  def project_eval(params: Seq[Int], v: Seq[Double]): Seq[Double] = {
    if(v.size != 0) throw new IRValidationException()
    v
  }

  def simplify: Cone = this
}

//The trivial scalar cone (the only proper cone over R)
case class ConeNonNegative(val arity: Int) extends Cone {
  val size: IRPoly = IRPoly.const(1,arity)
  def conj: Cone = ConeNonNegative(arity)
  
  def arityOp(op: ArityOp): Cone = ConeNonNegative(op.arity)

  def project_eval(params: Seq[Int], v: Seq[Double]): Seq[Double] = {
    if(v.size != 1) throw new IRValidationException()
    if (v(0) >= 0.0) Seq(v(0))
    else Seq(0.0)
  }

  def simplify: Cone = this
}

//Second order cone
case class ConeSecondOrder(val dim: IRPoly) extends Cone {
  val arity: Int = dim.arity
  val size: IRPoly = dim + IRPoly.const(1, arity)
  def conj: Cone = ConeSecondOrder(dim)
    
  def arityOp(op: ArityOp): Cone = ConeSecondOrder(dim.arityOp(op))
  
  if (size.arity != arity) throw new IRValidationException()

  def project_eval(params: Seq[Int], v: Seq[Double]): Seq[Double] = {
    //println("in: " + v.toString)
    if(v.size != size.eval(params)(IntLikeInt)) throw new IRValidationException()
    val vn2 = v.drop(1).foldLeft(0.0)((b,a) => b+a*a)
    val v0 = v(0)
    if((v0 <= 0)&&(v0*v0 >= vn2)) {
      //println("out: " + ((0 until v.size) map (a => 0.0)).toString)
      return (0 until v.size) map (a => 0.0)
    }
    else if((v0 >= 0)&&(v0*v0 >= vn2)) {
      //println("out: " + v.toString)
      return v
    }
    else {
      val scl = (1 + v0/scala.math.sqrt(vn2))/2
      //println("out: " + (Seq(scl * scala.math.sqrt(vn2)) ++ v.drop(1).map(a => scl*a)).toString)
      return Seq(scl * scala.math.sqrt(vn2)) ++ v.drop(1).map(a => scl*a)
    }
  }

  def simplify: Cone = this
}

//Cartesian-product of cones
case class ConeProduct(val arg1: Cone, val arg2: Cone) extends Cone {
  val arity: Int = arg1.arity
  val size: IRPoly = arg1.size + arg2.size
  def conj: Cone = ConeProduct(arg1.conj, arg2.conj)

  if (arg1.arity != arg2.arity) throw new IRValidationException()
  
  def arityOp(op: ArityOp): Cone = ConeProduct(arg1.arityOp(op), arg2.arityOp(op))

  def project_eval(params: Seq[Int], v: Seq[Double]): Seq[Double] = {
    if(v.size != size.eval(params)(IntLikeInt)) throw new IRValidationException()
    val sz1 = arg1.size.eval(params)(IntLikeInt)
    arg1.project_eval(params, v.take(sz1)) ++ arg2.project_eval(params, v.drop(sz1))
  }

  def simplify: Cone = {
    val sa1 = arg1.simplify
    val sa2 = arg2.simplify
    if(sa1.isInstanceOf[ConeZero]) {
      sa2
    }
    else if(sa2.isInstanceOf[ConeZero]) {
      sa1
    }
    else {
      ConeProduct(sa1, sa2)
    }
  }
}

//For-loop product of cones
case class ConeFor(val len: IRPoly, val body: Cone) extends Cone {
  val arity: Int = len.arity
  val size: IRPoly = body.size.sum(arity).substituteAt(arity, len)
  def conj: Cone = ConeFor(len, body.conj)

  if(body.arity != (len.arity + 1)) throw new IRValidationException()
  if(size.arity != arity) throw new IRValidationException()
  
  def arityOp(op: ArityOp): Cone = ConeFor(len.arityOp(op), body.arityOp(op.promote))

  def project_eval(params: Seq[Int], v: Seq[Double]): Seq[Double] = {
    if(v.size != size.eval(params)(IntLikeInt)) throw new IRValidationException()
    val ll = len.eval(params)(IntLikeInt)
    var rv: Seq[Double] = Seq()
    var vv: Seq[Double] = v
    for(i <- 0 until ll) {
      val sz = body.size.eval(params :+ i)(IntLikeInt)
      rv = rv ++ body.project_eval(params :+ i, vv.take(sz))
      vv = vv.drop(sz)
    }
    rv
  }

  def simplify: Cone = {
    val sb = body.simplify
    if(sb.isInstanceOf[ConeZero]) {
      ConeZero(arity)
    }
    else if(len == IRPoly.const(0, arity)) {
      ConeZero(arity)
    }
    else if(len == IRPoly.const(1, arity)) {
      sb.substituteAt(arity, IRPoly.const(0, arity)).simplify
    }
    else {
      ConeFor(len, sb)
    }
  }
}
