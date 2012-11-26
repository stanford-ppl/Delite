// Almap = Abstract Linear MAP
// This represents a linear map as a composition of a set of linear mapping primitives

package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.solver._
import scala.collection.immutable.Seq


sealed trait Almap extends HasArity[Almap] {
  //The domain and codomain sizes of this map
  val domain: IRPoly
  val codomain: IRPoly

  //Constraints that all the shape properties must share the map's arity
  def arityVerify() {
    if (domain.arity != arity) throw new IRValidationException()
    if (codomain.arity != arity) throw new IRValidationException()
  }
  
  //The transpose
  def T: Almap

  //Code generation for this matrix
  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V

  def +(a: Almap) = {
    if((domain != a.domain)||(codomain != a.codomain)) throw new IRValidationException()
    AlmapSum(this, a)
  }

  def -(a: Almap) = {
    if((domain != a.domain)||(codomain != a.codomain)) throw new IRValidationException()
    AlmapSum(this, AlmapNeg(a))
  }

  def unary_-() = {
    AlmapNeg(this)
  }

  def *(a: Almap) = {
    if(domain != a.codomain) throw new IRValidationException()
    AlmapProd(this, a)
  }
}

//The identity map
case class AlmapIdentity(val domain: IRPoly) extends Almap {
  val arity: Int = domain.arity
  val codomain: IRPoly = domain
  
  def arityOp(op: ArityOp): Almap = AlmapIdentity(domain.arityOp(op))

  def T: Almap = this

  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    x
  }
}


//The zero map
case class AlmapZero(val domain: IRPoly, val codomain: IRPoly) extends Almap {
  val arity: Int = domain.arity
  
  def arityOp(op: ArityOp): Almap = AlmapZero(domain.arityOp(op), codomain.arityOp(op))

  def T: Almap = AlmapZero(codomain, domain)

  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    zero(codomain)
  }
}

//The sum of two linear maps
case class AlmapSum(val arg1: Almap, val arg2: Almap) extends Almap {
  val arity: Int = arg1.arity
  val domain: IRPoly = arg1.domain
  val codomain: IRPoly = arg1.codomain

  if (arg2.arity != arity) throw new IRValidationException()
  if (arg2.domain != domain) throw new IRValidationException()
  if (arg2.codomain != codomain) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapSum(arg1.arityOp(op), arg2.arityOp(op))

  def T: Almap = AlmapSum(arg1.T, arg2.T)
  
  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    arg1.mmpy(x) + arg2.mmpy(x)
  }
}

//Negation of a linear map
case class AlmapNeg(val arg: Almap) extends Almap {
  val arity: Int = arg.arity
  val domain: IRPoly = arg.domain
  val codomain: IRPoly = arg.codomain
  
  def arityOp(op: ArityOp): Almap = AlmapNeg(arg.arityOp(op))

  def T: Almap = AlmapNeg(arg.T)
  
  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    -x
  }
}

//Scale of a linear map by some indexing of the input space
case class AlmapScaleInput(val arg: Almap, val scale: IRPoly) extends Almap {
  val arity: Int = arg.arity
  val domain: IRPoly = arg.domain
  val codomain: IRPoly = arg.codomain

  if (arity != scale.arity) throw new IRValidationException()
  
  def arityOp(op: ArityOp): Almap = AlmapScaleInput(arg.arityOp(op), scale.arityOp(op))
  
  def T: Almap = AlmapScaleInput(arg.T, scale)
  
  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    scaleinput(x, scale)
  }
}

//Scale of a linear map by a constant
case class AlmapScaleConstant(val arg: Almap, val scale: Double) extends Almap {
  val arity: Int = arg.arity
  val domain: IRPoly = arg.domain
  val codomain: IRPoly = arg.codomain
  
  def arityOp(op: ArityOp): Almap = AlmapScaleConstant(arg.arityOp(op), scale)
  
  def T: Almap = AlmapScaleConstant(arg.T, scale)

  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    scaleconstant(x, scale)
  }
}


//The vertical concatenation of two linear maps
case class AlmapVCat(val arg1: Almap, val arg2: Almap) extends Almap {
  val arity: Int = arg1.arity
  val domain: IRPoly = arg1.domain
  val codomain: IRPoly = arg1.codomain + arg2.codomain

  if (arg1.arity != arg2.arity) throw new IRValidationException()
  if (arg1.domain != arg2.domain) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapVCat(arg1.arityOp(op), arg2.arityOp(op))

  def T: Almap = AlmapHCat(arg1.T, arg2.T)
  
  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    arg1.mmpy(x) ++ arg2.mmpy(x)
  }
}


//The vertical concatenation of a number of linear maps, depending on problem size
case class AlmapVCatFor(val len: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val domain: IRPoly = body.domain.demote
  val codomain: IRPoly = body.codomain.sum(arity).substituteAt(arity, len)

  if (body.arity != (len.arity + 1)) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapVCatFor(len.arityOp(op), body.arityOp(op))

  def T: Almap = AlmapHCatFor(len, body.T)
  
  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    catfor(len, body.mmpy(x.promote))
  }
}

// "Puts" the given almap at the target index, all other entries are 0
case class AlmapVPut(val len: IRPoly, val at: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val domain: IRPoly = body.domain.demote
  val codomain: IRPoly = body.codomain.sum(arity).substituteAt(arity, len)

  if (body.arity != (len.arity + 1)) throw new IRValidationException()
  if (len.arity != at.arity) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapVPut(len.arityOp(op), at.arityOp(op), body.arityOp(op))

  def T: Almap = AlmapHPut(len, at, body.T)

  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    zero(body.codomain.sum(arity).substituteAt(arity, at)) ++
      body.substituteAt(arity, at).mmpy(x) ++
        zero(body.codomain.sum(arity).substituteAt(arity, at + IRPoly.const(1, arity)))
  }
}

//The horizontal concatenation of two linear maps
case class AlmapHCat(val arg1: Almap, val arg2: Almap) extends Almap {
  val arity: Int = arg1.arity
  val domain: IRPoly = arg1.domain + arg2.domain
  val codomain: IRPoly = arg1.codomain

  if (arg1.arity != arg2.arity) throw new IRValidationException()
  if (arg1.codomain != arg2.codomain) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapHCat(arg1.arityOp(op), arg2.arityOp(op))

  def T: Almap = AlmapVCat(arg1.T, arg2.T)
  
  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    arg1.mmpy(x(IRPoly.const(0, arity), arg1.domain)) +  
      arg2.mmpy(x(arg1.domain, arg1.domain + arg2.domain))
  }
}


//The horzontal concatenation of a number of linear maps, depending on problem size
case class AlmapHCatFor(val len: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val domain: IRPoly = body.domain.sum(arity).substituteAt(arity, len)
  val codomain: IRPoly = body.codomain.demote

  if (body.arity != (len.arity + 1)) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapHCatFor(len.arityOp(op), body.arityOp(op))

  def T: Almap = AlmapVCatFor(len, body.T)

  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    addfor(
      len,
      body.mmpy(
        slice(
          x.promote,
          body.domain.sum(arity),
          body.domain)))
  }
}

// "Puts" the given almap at the target index, all other entries are 0
case class AlmapHPut(val len: IRPoly, val at: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val domain: IRPoly = body.domain.sum(arity).substituteAt(arity, len)
  val codomain: IRPoly = body.codomain.demote

  if (body.arity != (len.arity + 1)) throw new IRValidationException()
  if (len.arity != at.arity) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapHPut(len.arityOp(op), at.arityOp(op), body.arityOp(op))

  def T: Almap = AlmapVPut(len, at, body.T)

  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    body.substituteAt(arity, at).mmpy(
      slice(
        x, 
        body.domain.sum(arity).substituteAt(arity, at),
        body.domain.substituteAt(arity, at)))
  }
}

//The sum of a problem-size-dependent number of linear ops
case class AlmapSumFor(val len: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val domain: IRPoly = body.domain.demote
  val codomain: IRPoly = body.codomain.demote

  if (body.arity != (len.arity + 1)) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapSumFor(len.arityOp(op), body.arityOp(op))

  def T: Almap = AlmapSumFor(len, body.T)

  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    addfor(len, body.mmpy(x))
  }
}

//Matrix multiply
case class AlmapProd(val argl: Almap, val argr: Almap) extends Almap {
  if (argl.arity != argr.arity) throw new IRValidationException()
  if (argl.domain != argr.codomain) throw new IRValidationException()

  val arity: Int = argl.arity
  val domain: IRPoly = argr.domain
  val codomain: IRPoly = argl.codomain

  def arityOp(op: ArityOp): Almap = AlmapProd(argl.arityOp(op), argr.arityOp(op))

  def T: Almap = AlmapProd(argr.T, argl.T)

  arityVerify()

  def mmpy[V <: HasArity[V]](x: V)(implicit e: AVectorLike[V]): V = {
    import e._
    if(size(x) != domain) throw new IRValidationException()
    argl.mmpy(argr.mmpy(x))
  }
}


