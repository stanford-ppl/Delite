// Almap = Abstract Linear MAP
// This represents a linear map as a composition of a set of linear mapping primitives

package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq


sealed trait Almap extends HasInput[Almap] {
  //The domain and codomain sizes of this map
  val domain: IRPoly
  val codomain: IRPoly

  //Constraints that all the shape properties must share the map's arity
  def arityVerify() {
    if (input.arity != arity) throw new IRValidationException()
    if (domain.arity != arity) throw new IRValidationException()
    if (codomain.arity != arity) throw new IRValidationException()
  }
  
  //The transpose
  def T: Almap

  //Code generation for this matrix
  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V

  def mmpycheck[V <: HasInput[V]](x: V)(tv: =>V)(implicit e: AVectorLike[V]): V = {
    if(e.size(x) != domain) {
      print(e.size(x).toString + "\n")
      print(domain.toString + "\n")
      print(codomain.toString + "\n")
      throw new IRValidationException()
    }
    if(x.input != input) throw new IRValidationException()
    if(e.input != input) throw new IRValidationException()
    val v: V = tv
    if(v.input != input) throw new IRValidationException()
    if(e.size(v) != codomain) throw new IRValidationException()
    v
  }

  //Is this matrix zero?
  def is0: Boolean

  //Does this matrix use no inputs
  def isPure: Boolean

  //try to simplify this Almap
  def simplify: Almap

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

  def *[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = {
    mmpy(x)
  }
}

//The identity map
case class AlmapIdentity(val input: InputDesc, val domain: IRPoly) extends Almap {
  val arity: Int = input.arity
  val codomain: IRPoly = domain
  if(input.arity != domain.arity) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapIdentity(input.arityOp(op), domain.arityOp(op))
  def inputOp(op: InputOp): Almap = AlmapIdentity(op.input, domain)

  def T: Almap = this

  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    x
  }

  def is0: Boolean = (domain == IRPoly.const(0, arity))

  def isPure: Boolean = true

  def simplify: Almap = this

  override def toString: String = "eye(" + domain.toString + ")"
}


//The zero map
case class AlmapZero(val input: InputDesc, val domain: IRPoly, val codomain: IRPoly) extends Almap {
  val arity: Int = input.arity
  if(domain.arity != arity) throw new IRValidationException()
  if(codomain.arity != arity) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapZero(input.arityOp(op), domain.arityOp(op), codomain.arityOp(op))
  def inputOp(op: InputOp): Almap = AlmapZero(op.input, domain, codomain)

  def T: Almap = AlmapZero(input, codomain, domain)

  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    zero(codomain)
  }

  def is0: Boolean = true

  def isPure: Boolean = true

  def simplify: Almap = this

  override def toString: String = "zeros(" + domain.toString + ", " + codomain.toString + ")"
}

//The sum of two linear maps
case class AlmapSum(val arg1: Almap, val arg2: Almap) extends Almap {
  val arity: Int = arg1.arity
  val input: InputDesc = arg1.input
  val domain: IRPoly = arg1.domain
  val codomain: IRPoly = arg1.codomain

  if (arg2.arity != arity) throw new IRValidationException()
  if (arg2.input != input) throw new IRValidationException()
  if (arg2.domain != domain) throw new IRValidationException()
  if (arg2.codomain != codomain) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapSum(arg1.arityOp(op), arg2.arityOp(op))
  def inputOp(op: InputOp): Almap = AlmapSum(arg1.inputOp(op), arg2.inputOp(op))

  def T: Almap = AlmapSum(arg1.T, arg2.T)
  
  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    arg1.mmpy(x) + arg2.mmpy(x)
  }

  def is0: Boolean = arg1.is0 && arg2.is0

  def isPure: Boolean = arg1.isPure && arg2.isPure
  
  def simplify: Almap = {
    val sa1 = arg1.simplify
    val sa2 = arg2.simplify
    if(sa1.is0) {
      sa2
    }
    else if(sa2.is0) {
      sa1
    }
    else {
      AlmapSum(sa1, sa2)
    }
  }

  override def toString: String = "sum(" + arg1.toString + ", " + arg2.toString + ")"
}

//Negation of a linear map
case class AlmapNeg(val arg: Almap) extends Almap {
  val arity: Int = arg.arity
  val input: InputDesc = arg.input
  val domain: IRPoly = arg.domain
  val codomain: IRPoly = arg.codomain
  
  def arityOp(op: ArityOp): Almap = AlmapNeg(arg.arityOp(op))
  def inputOp(op: InputOp): Almap = AlmapNeg(arg.inputOp(op))

  def T: Almap = AlmapNeg(arg.T)
  
  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    -arg.mmpy(x)
  }

  def is0: Boolean = arg.is0

  def isPure: Boolean = arg.isPure

  def simplify: Almap = {
    val sa = arg.simplify
    if(sa.is0) {
      sa
    }
    else {
      AlmapNeg(sa)
    }
  }

  override def toString: String = "neg(" + arg.toString + ")"
}

/*
//Scale of a linear map by some indexing of the input space
case class AlmapScaleInput(val arg: Almap, val scale: IRPoly) extends Almap {
  val arity: Int = arg.arity
  val domain: IRPoly = arg.domain
  val codomain: IRPoly = arg.codomain

  if (arity != scale.arity) throw new IRValidationException()
  
  def arityOp(op: ArityOp): Almap = AlmapScaleInput(arg.arityOp(op), scale.arityOp(op))
  
  def T: Almap = AlmapScaleInput(arg.T, scale)
  
  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    if(e.arity != this.arity) throw new IRValidationException()
    val arity: Int = this.arity
    if(size(x) != domain) throw new IRValidationException()
    scaleinput(arg.mmpy(x), scale)
  }

  def is0: Boolean = arg.is0

  def isPure: Boolean = false
}
*/

//Scale of a linear map by a constant
case class AlmapScaleConstant(val arg: Almap, val scale: Double) extends Almap {
  val arity: Int = arg.arity
  val input: InputDesc = arg.input
  val domain: IRPoly = arg.domain
  val codomain: IRPoly = arg.codomain
  
  def arityOp(op: ArityOp): Almap = AlmapScaleConstant(arg.arityOp(op), scale)
  def inputOp(op: InputOp): Almap = AlmapScaleConstant(arg.inputOp(op), scale)
  
  def T: Almap = AlmapScaleConstant(arg.T, scale)

  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    scaleconstant(arg.mmpy(x), scale)
  }

  def is0: Boolean = arg.is0 || (scale == 0)

  def isPure: Boolean = arg.isPure

  def simplify: Almap = {
    val sa = arg.simplify
    if(sa.is0) {
      sa
    }
    else {
      AlmapScaleConstant(sa, scale)
    }
  }

  override def toString: String = "scale(" + arg.toString + ", " + scale.toString + ")"
}


//The vertical concatenation of two linear maps
case class AlmapVCat(val arg1: Almap, val arg2: Almap) extends Almap {
  val arity: Int = arg1.arity
  val input: InputDesc = arg1.input
  val domain: IRPoly = arg1.domain
  val codomain: IRPoly = arg1.codomain + arg2.codomain

  if (arg1.arity != arg2.arity) throw new IRValidationException()
  if (arg1.input != arg2.input) throw new IRValidationException()
  if (arg1.domain != arg2.domain) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapVCat(arg1.arityOp(op), arg2.arityOp(op))
  def inputOp(op: InputOp): Almap = AlmapVCat(arg1.inputOp(op), arg2.inputOp(op))

  def T: Almap = AlmapHCat(arg1.T, arg2.T)
  
  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    arg1.mmpy(x) ++ arg2.mmpy(x)
  }

  def is0: Boolean = arg1.is0 && arg2.is0

  def isPure: Boolean = arg1.isPure && arg2.isPure

  def simplify: Almap = {
    val sa1 = arg1.simplify
    val sa2 = arg2.simplify
    if((sa1.is0)&&(sa2.is0)) {
      AlmapZero(input, domain, codomain)
    }
    else if(sa1.codomain == IRPoly.const(0, arity)) {
      sa2
    }
    else if(sa2.codomain == IRPoly.const(0, arity)) {
      sa1
    }
    else {
      AlmapVCat(sa1, sa2)
    }
  }

  override def toString: String = "vcat(" + arg1.toString + ", " + arg2.toString + ")"
}


//The vertical concatenation of a number of linear maps, depending on problem size
case class AlmapVCatFor(val len: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val input: InputDesc = body.input.demote
  val domain: IRPoly = body.domain.demote
  val codomain: IRPoly = body.codomain.sum(arity).substituteAt(arity, len)

  if (body.arity != (len.arity + 1)) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapVCatFor(len.arityOp(op), body.arityOp(op.promote))
  def inputOp(op: InputOp): Almap = AlmapVCatFor(len, body.inputOp(op.promote))

  def T: Almap = AlmapHCatFor(len, body.T)
  
  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    catfor(len, body.mmpy(x.promote)(e.promote))
  }

  def is0: Boolean = body.is0

  def isPure: Boolean = body.isPure

  def simplify: Almap = {
    val sb = body.simplify
    if(sb.is0) {
      AlmapZero(input, domain, codomain)
    }
    else if(sb.codomain == IRPoly.const(0, arity)) {
      AlmapZero(input, domain, codomain)
    }
    else {
      AlmapVCatFor(len, sb)
    }
  }

  override def toString: String = "vcatfor(" + len.toString + ": " + body.toString + ")"
}

/*
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

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    if(e.arity != this.arity) throw new IRValidationException()
    val arity: Int = this.arity
    if(size(x) != domain) throw new IRValidationException()
    zero(body.codomain.sum(arity).substituteAt(arity, at)) ++
      body.substituteAt(arity, at).mmpy(x) ++
        zero(body.codomain.sum(arity).substituteAt(arity, at + IRPoly.const(1, arity)))
  }

  def is0: Boolean = body.is0

  def isPure: Boolean = body.isPure
}
*/

//The horizontal concatenation of two linear maps
case class AlmapHCat(val arg1: Almap, val arg2: Almap) extends Almap {
  val arity: Int = arg1.arity
  val input: InputDesc = arg1.input
  val domain: IRPoly = arg1.domain + arg2.domain
  val codomain: IRPoly = arg1.codomain

  if (arg1.arity != arg2.arity) throw new IRValidationException()
  if (arg1.input != arg2.input) throw new IRValidationException()
  if (arg1.codomain != arg2.codomain) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapHCat(arg1.arityOp(op), arg2.arityOp(op))
  def inputOp(op: InputOp): Almap = AlmapHCat(arg1.inputOp(op), arg2.inputOp(op))

  def T: Almap = AlmapVCat(arg1.T, arg2.T)
  
  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    arg1.mmpy(x(IRPoly.const(0, this.arity), arg1.domain)) +  
      arg2.mmpy(x(arg1.domain, arg2.domain))
  }

  def is0: Boolean = arg1.is0 && arg2.is0

  def isPure: Boolean = arg1.isPure && arg2.isPure

  def simplify: Almap = {
    val sa1 = arg1.simplify
    val sa2 = arg2.simplify
    if((sa1.is0)&&(sa2.is0)) {
      AlmapZero(input, domain, codomain)
    }
    else if(sa1.domain == IRPoly.const(0, arity)) {
      sa2
    }
    else if(sa2.domain == IRPoly.const(0, arity)) {
      sa1
    }
    else {
      AlmapHCat(sa1, sa2)
    }
  }

  override def toString: String = "hcat(" + arg1.toString + ", " + arg2.toString + ")"
}


//The horzontal concatenation of a number of linear maps, depending on problem size
case class AlmapHCatFor(val len: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val input: InputDesc = body.input.demote
  val domain: IRPoly = body.domain.sum(arity).substituteAt(arity, len)
  val codomain: IRPoly = body.codomain.demote

  if (body.arity != (len.arity + 1)) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapHCatFor(len.arityOp(op), body.arityOp(op.promote))
  def inputOp(op: InputOp): Almap = AlmapHCatFor(len, body.inputOp(op.promote))

  def T: Almap = AlmapVCatFor(len, body.T)

  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    val arity: Int = this.arity
    sumfor(
      len,
      body.mmpy(
        slice(
          x.promote,
          body.domain.sum(arity),
          body.domain))(e.promote))
  }

  def is0: Boolean = body.is0

  def isPure: Boolean = body.isPure

  def simplify: Almap = {
    val sb = body.simplify
    if(sb.is0) {
      AlmapZero(input, domain, codomain)
    }
    else if(sb.domain == IRPoly.const(0, arity)) {
      AlmapZero(input, domain, codomain)
    }
    else {
      AlmapHCatFor(len, sb)
    }
  }

  override def toString: String = "hcatfor(" + len.toString + ": " + body.toString + ")"
}

/*
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

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    if(e.arity != this.arity) throw new IRValidationException()
    val arity: Int = this.arity
    if(size(x) != domain) throw new IRValidationException()
    body.substituteAt(arity, at).mmpy(
      slice(
        x, 
        body.domain.sum(arity).substituteAt(arity, at),
        body.domain.substituteAt(arity, at)))
  }

  def is0: Boolean = body.is0

  def isPure: Boolean = body.isPure
}
*/

//The horizontal concatenation of two linear maps
case class AlmapDiagCat(val arg1: Almap, val arg2: Almap) extends Almap {
  val arity: Int = arg1.arity
  val input: InputDesc = arg1.input
  val domain: IRPoly = arg1.domain + arg2.domain
  val codomain: IRPoly = arg1.codomain + arg2.codomain

  if (arg1.arity != arg2.arity) throw new IRValidationException()
  if (arg1.input != arg2.input) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapDiagCat(arg1.arityOp(op), arg2.arityOp(op))
  def inputOp(op: InputOp): Almap = AlmapDiagCat(arg1.inputOp(op), arg2.inputOp(op))

  def T: Almap = AlmapDiagCat(arg1.T, arg2.T)
  
  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    val arity: Int = this.arity
    cat(arg1.mmpy(x(IRPoly.const(0, arity), arg1.domain)),
      arg2.mmpy(x(arg1.domain, arg2.domain)))
  }

  def is0: Boolean = arg1.is0 && arg2.is0

  def isPure: Boolean = arg1.isPure && arg2.isPure

  def simplify: Almap = {
    val sa1 = arg1.simplify
    val sa2 = arg2.simplify
    if((sa1.is0)&&(sa2.is0)) {
      AlmapZero(input, domain, codomain)
    }
    else if((sa1.domain == IRPoly.const(0, arity))||(sa1.codomain == IRPoly.const(0, arity))||
      (sa2.domain == IRPoly.const(0, arity))||(sa2.codomain == IRPoly.const(0, arity))) {
      AlmapVCat(
        AlmapHCat(sa1, AlmapZero(input, sa2.domain, sa1.codomain)),
        AlmapHCat(AlmapZero(input, sa1.domain, sa2.codomain), sa2)).simplify
    }
    else {
      AlmapDiagCat(sa1, sa2)
    }
  }

  override def toString: String = "diagcat(" + arg1.toString + ", " + arg2.toString + ")"
}


//The horzontal concatenation of a number of linear maps, depending on problem size
case class AlmapDiagCatFor(val len: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val input: InputDesc = body.input.demote
  val domain: IRPoly = body.domain.sum(arity).substituteAt(arity, len)
  val codomain: IRPoly = body.codomain.sum(arity).substituteAt(arity, len)

  if (body.arity != (len.arity + 1)) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapDiagCatFor(len.arityOp(op), body.arityOp(op.promote))
  def inputOp(op: InputOp): Almap = AlmapDiagCatFor(len, body.inputOp(op.promote))

  def T: Almap = AlmapDiagCatFor(len, body.T)

  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    val arity: Int = this.arity
    catfor(
      len,
      body.mmpy(
        slice(
          x.promote,
          body.domain.sum(arity),
          body.domain))(e.promote))
  }

  def is0: Boolean = body.is0

  def isPure: Boolean = body.isPure

  def simplify: Almap = {
    val sb = body.simplify
    if(sb.is0) {
      AlmapZero(input, domain, codomain)
    }
    else if(sb.domain == IRPoly.const(0, arity)) {
      AlmapZero(input, domain, codomain)
    }
    else if(sb.codomain == IRPoly.const(0, arity)) {
      AlmapZero(input, domain, codomain)
    }
    else {
      AlmapDiagCatFor(len, sb)
    }
  }

  override def toString: String = "diagcatfor(" + len.toString + ": " + body.toString + ")"
}

//The sum of a problem-size-dependent number of linear ops
case class AlmapSumFor(val len: IRPoly, val body: Almap) extends Almap {
  val arity: Int = len.arity
  val input: InputDesc = body.input.demote
  val domain: IRPoly = body.domain.demote
  val codomain: IRPoly = body.codomain.demote

  if (body.arity != (len.arity + 1)) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapSumFor(len.arityOp(op), body.arityOp(op.promote))
  def inputOp(op: InputOp): Almap = AlmapSumFor(len, body.inputOp(op.promote))

  def T: Almap = AlmapSumFor(len, body.T)

  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    sumfor(len, body.mmpy(x.promote)(e.promote))
  }

  def is0: Boolean = body.is0

  def isPure: Boolean = body.isPure

  def simplify: Almap = {
    val sb = body.simplify
    if(sb.is0) {
      AlmapZero(input, domain, codomain)
    }
    else if(sb.domain == IRPoly.const(0, arity)) {
      AlmapZero(input, domain, codomain)
    }
    else if(sb.codomain == IRPoly.const(0, arity)) {
      AlmapZero(input, domain, codomain)
    }
    else {
      AlmapSumFor(len, sb)
    }
  }

  override def toString: String = "sumfor(" + len.toString + ": " + body.toString + ")"
}

//Matrix multiply
case class AlmapProd(val argl: Almap, val argr: Almap) extends Almap {
  val arity: Int = argl.arity
  val input: InputDesc = argl.input
  val domain: IRPoly = argr.domain
  val codomain: IRPoly = argl.codomain

  if (argl.arity != argr.arity) throw new IRValidationException()
  if (argl.input != argr.input) throw new IRValidationException()
  if (argl.domain != argr.codomain) throw new IRValidationException()

  def arityOp(op: ArityOp): Almap = AlmapProd(argl.arityOp(op), argr.arityOp(op))
  def inputOp(op: InputOp): Almap = AlmapProd(argl.inputOp(op), argr.inputOp(op))

  def T: Almap = AlmapProd(argr.T, argl.T)

  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    argl.mmpy(argr.mmpy(x))
  }

  def is0: Boolean = argl.is0 || argr.is0

  def isPure: Boolean = argl.isPure && argr.isPure

  def simplify: Almap = {
    val sal = argl.simplify
    val sar = argr.simplify
    if((sal.is0)||(sar.is0)) {
      AlmapZero(input, domain, codomain)
    }
    else if(sal.isInstanceOf[AlmapIdentity]) {
      sar
    }
    else if(sar.isInstanceOf[AlmapIdentity]) {
      sal
    }
    else {
      AlmapProd(sal, sar)
    }
  }

  override def toString: String = "mmpy(" + argl.toString + " * " + argr.toString + ")"
}

//Input matrix
case class AlmapInput(val input: InputDesc, val iidx: Int, val sidx: Seq[IRPoly]) extends Almap {
  val arity: Int = input.arity

  if((iidx < 0)||(iidx >= input.args.length)) throw new IRValidationException()
  for(s <- sidx) {
    if(s.arity != arity) throw new IRValidationException()
  }

  val domain: IRPoly = input.args(iidx).domain.substituteSeq(sidx)
  val codomain: IRPoly = input.args(iidx).codomain.substituteSeq(sidx)

  def arityOp(op: ArityOp): Almap = AlmapInput(input.arityOp(op), iidx, sidx map (s => s.arityOp(op)))
  def inputOp(op: InputOp): Almap = {
    if(op.xs.length != input.args.length) throw new IRValidationException()
    for(i <- 0 until input.args.length) {
      if(op.xs(i).arity != input.args(i).domain.arity) throw new IRValidationException()
    }
    op.xs(iidx).substituteSeq(sidx)
  }

  def T: Almap = AlmapInputT(input, iidx, sidx)

  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    mmpyinput(x, iidx, sidx)
  }

  def is0: Boolean = false
  def isPure: Boolean = false

  def simplify: Almap = this

  override def toString: String = "input(" + iidx.toString + ", " + sidx.toString + ")"
}

case class AlmapInputT(val input: InputDesc, val iidx: Int, val sidx: Seq[IRPoly]) extends Almap {
  val arity: Int = input.arity

  if((iidx < 0)||(iidx >= input.args.length)) throw new IRValidationException()
  for(s <- sidx) {
    if(s.arity != arity) throw new IRValidationException()
  }

  val domain: IRPoly = input.args(iidx).codomain.substituteSeq(sidx)
  val codomain: IRPoly = input.args(iidx).domain.substituteSeq(sidx)

  def arityOp(op: ArityOp): Almap = AlmapInput(input.arityOp(op), iidx, sidx map (s => s.arityOp(op)))
  def inputOp(op: InputOp): Almap = {
    if(op.xs.length != input.args.length) throw new IRValidationException()
    for(i <- 0 until input.args.length) {
      if(op.xs(i).arity != input.args(i).codomain.arity) throw new IRValidationException()
    }
    op.xs(iidx).substituteSeq(sidx)
  }

  def T: Almap = AlmapInput(input, iidx, sidx)

  arityVerify()

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    import e._
    mmpyinputtranspose(x, iidx, sidx)
  }

  def is0: Boolean = false
  def isPure: Boolean = false

  def simplify: Almap = this

  override def toString: String = "inputT(" + iidx.toString + ", " + sidx.toString + ")"
}

case class AlmapVector(val arg: AVector) extends Almap {
  val arity: Int = arg.arity
  val input: InputDesc = arg.input
  val domain: IRPoly = IRPoly.const(1, arity)
  val codomain: IRPoly = arg.size

  arityVerify()

  def arityOp(op: ArityOp): Almap = AlmapVector(arg.arityOp(op))
  def inputOp(op: InputOp): Almap = AlmapVector(arg.inputOp(op))

  def T: Almap = AlmapVectorT(arg)

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    if(e.size(x) != IRPoly.const(1, arity)) throw new IRValidationException()
    e.mpy(arg.translate, x)
  }

  def is0: Boolean = arg.is0
  def isPure: Boolean = arg.isPure

  def simplify: Almap = {
    val sa: AVector = arg.simplify
    if(sa.is0) {
      AlmapZero(input, domain, codomain)
    }
    else {
      AlmapVector(sa)
    }
  }

  override def toString: String = "vector(" + arg.toString + ")"
}

case class AlmapVectorT(val arg: AVector) extends Almap {
  val arity: Int = arg.arity
  val input: InputDesc = arg.input
  val domain: IRPoly = arg.size
  val codomain: IRPoly = IRPoly.const(1, arity)

  arityVerify()

  def arityOp(op: ArityOp): Almap = AlmapVectorT(arg.arityOp(op))
  def inputOp(op: InputOp): Almap = AlmapVectorT(arg.inputOp(op))

  def T: Almap = AlmapVectorT(arg)

  def mmpy[V <: HasInput[V]](x: V)(implicit e: AVectorLike[V]): V = mmpycheck(x) {
    e.dot(arg.translate, x)
  }

  def is0: Boolean = arg.is0
  def isPure: Boolean = arg.isPure

  def simplify: Almap = {
    val sa: AVector = arg.simplify
    if(sa.is0) {
      AlmapZero(input, domain, codomain)
    }
    else {
      AlmapVectorT(sa)
    }
  }

  override def toString: String = "vectorT(" + arg.toString + ")"
}


case class AVectorLikeAlmap(val input: InputDesc, val domain: IRPoly) extends AVectorLike[Almap] {
  val arity: Int = domain.arity
  if(input.arity != domain.arity) throw new IRValidationException()
  def size(arg: Almap): IRPoly = arg.codomain
  def zero(size: IRPoly): Almap = AlmapZero(input, domain, size)
  def one: Almap = {
    if(domain != IRPoly.const(1, domain.arity)) throw new IRValidationException()
    AlmapIdentity(input, domain)
  }
  def sum(arg1: Almap, arg2: Almap): Almap = arg1 + arg2
  def sumfor(len: IRPoly, arg: Almap): Almap = AlmapSumFor(len, arg)
  def neg(arg: Almap): Almap = -arg
  //def scaleinput(arg: Almap, scale: IRPoly): Almap = AlmapScaleInput(arg, scale)
  def scaleconstant(arg: Almap, scale: Double): Almap = AlmapScaleConstant(arg, scale)
  def cat(arg1: Almap, arg2: Almap): Almap = AlmapVCat(arg1, arg2)
  def catfor(len: IRPoly, arg: Almap): Almap = AlmapVCatFor(len, arg)
  def slice(arg: Almap, at: IRPoly, size: IRPoly): Almap = {
    val almappfx = AlmapHCat(
      AlmapHCat(
        AlmapZero(input, at, size), 
        AlmapIdentity(input, size)), 
      AlmapZero(input, arg.codomain - (at + size), size))
    AlmapProd(almappfx, arg)
  }
  def mmpyinput(arg: Almap, iidx: Int, sidx: Seq[IRPoly]): Almap = {
    AlmapProd(AlmapInput(input, iidx, sidx), arg)
  }
  def mmpyinputtranspose(arg: Almap, iidx: Int, sidx: Seq[IRPoly]): Almap = {
    AlmapProd(AlmapInputT(input, iidx, sidx), arg)
  }
  def read(iidx: Int, sidx: Seq[IRPoly]): Almap = {
    AlmapVector(AVectorRead(input, iidx, sidx))
  }
  def dot(arg1: Almap, arg2: Almap): Almap = {
    AlmapProd(arg1.T, arg2)
  }
  def mpy(arg: Almap, scale: Almap): Almap = {
    AlmapProd(arg, scale)
  }

  def arityOp(op: ArityOp): AVectorLike[Almap] = AVectorLikeAlmap(input.arityOp(op), domain.arityOp(op))
  def inputOp(op: InputOp): AVectorLike[Almap] = AVectorLikeAlmap(op.input, domain)
}
