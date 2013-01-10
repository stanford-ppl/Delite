package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq

case class Function(
  // sizes of the input arguments to this function
  val argSize: Seq[IRPoly],
  // polynomial to determine the vexity of the output of this function
  val vexity: SignumPoly,
  // polynomial to determine the sign of the output of this function
  val sign: SignumPoly,
  // size of the inner variable of this function
  var varSize: IRPoly,
  // objective and constraints for inner problem (objective is return value)
  val objective: AVector,
  val affineAlmap: Almap,
  val affineOffset: AVector,
  val conicAlmap: Almap,
  val conicOffset: AVector,
  val conicCone: Cone) extends HasArity[Function]
{
  val arity: Int = varSize.arity
  // first, make sure that the signum polynomials have the correct number of inputs
  // two inputs for each argument, one for sign and one for vexity
  if(vexity.arity != 2*argSize.length) throw new IRValidationException()
  if(sign.arity != 2*argSize.length) throw new IRValidationException()
  // next, verify that all the constraints have the appropriate size
  // this also implicitly verifies that all the arguments have the same arity
  val totalSize: IRPoly = argSize.foldLeft(varSize)((x,y) => x + y)
  if(objective.size != totalSize) throw new IRValidationException()
  if(affineAlmap.domain != totalSize) throw new IRValidationException()
  if(conicAlmap.domain != totalSize) throw new IRValidationException()
  // verify that all codomains agree
  if(affineAlmap.codomain != affineOffset.size) throw new IRValidationException()
  if(conicAlmap.codomain != conicOffset.size) throw new IRValidationException()
  if(conicAlmap.codomain != conicCone.size) throw new IRValidationException()

  def arityOp(op: ArityOp): Function = Function(
    argSize map (x => x.arityOp(op)),
    vexity,
    sign,
    varSize.arityOp(op),
    objective.arityOp(op),
    affineAlmap.arityOp(op),
    affineOffset.arityOp(op),
    conicAlmap.arityOp(op),
    conicOffset.arityOp(op),
    conicCone.arityOp(op))
}

  /*
  case class FunDesc(val va: Signum, val sg: Signum, val name: String)
  case class ArgDesc(val t: Signum, val nt: Signum, val sh: Shape, val name: String)
  
  case class FunBuilder0(fd: FunDesc) {
    def arg(
      tonicity: Signum = Tonicity.none, 
      shape: Shape = ShapeScalar(), 
      niltonicity: Signum = Tonicity.none, 
      name: String = ""): FunBuilder1
    = FunBuilder1(fd, ArgDesc(tonicity, niltonicity, shape, name))
  }
  
  case class FunBuilder1(fd: FunDesc, ad0: ArgDesc) {
    def arg(
      tonicity: Signum = Tonicity.none, 
      shape: Shape = ShapeScalar(), 
      niltonicity: Signum = Tonicity.none, 
      name: String = ""): FunBuilder2
    = FunBuilder2(fd, ad0, ArgDesc(tonicity, niltonicity, shape, name))
    
    def body(fx: (Expr) => Expr) = DcpFun1(fd, ad0, fx)
  }
  
  case class FunBuilder2(fd: FunDesc, ad0: ArgDesc, ad1: ArgDesc) {
    def arg(
      tonicity: Signum = Tonicity.none, 
      shape: Shape = ShapeScalar(), 
      niltonicity: Signum = Tonicity.none,
      name: String = ""): FunBuilder3
    = FunBuilder3(fd, ad0, ad1, ArgDesc(tonicity, niltonicity, shape, name))
    
    def body(fx: (Expr,Expr) => Expr) = DcpFun2(fd, ad0, ad1, fx)
  }
  
  case class FunBuilder3(fd: FunDesc, ad0: ArgDesc, ad1: ArgDesc, ad2: ArgDesc) {
    def body(fx: (Expr,Expr,Expr) => Expr) = DcpFun3(fd, ad0, ad1, ad2, fx)
  }
  
  case class DcpFun1(fd: FunDesc, ad0: ArgDesc, fx: (Expr) => Expr) {
    def apply(a0: Expr): Expr = dcpfun1apply(fd, ad0, fx, a0)
  }
  
  case class DcpFun2(fd: FunDesc, ad0: ArgDesc, ad1: ArgDesc, fx: (Expr,Expr) => Expr) {
    def apply(a0: Expr, a1: Expr): Expr = dcpfun2apply(fd, ad0, ad1, fx, a0, a1)
  }
  
  case class DcpFun3(fd: FunDesc, ad0: ArgDesc, ad1: ArgDesc, ad2: ArgDesc, fx: (Expr,Expr,Expr) => Expr) {
    def apply(a0: Expr, a1: Expr, a2: Expr): Expr = dcpfun3apply(fd, ad0, ad1, ad2, fx, a0, a1, a2)
  }
  
  def dcpfun1apply(fd: FunDesc, ad0: ArgDesc, fx: (Expr) => Expr, a0: Expr): Expr
  def dcpfun2apply(fd: FunDesc, ad0: ArgDesc, ad1: ArgDesc, fx: (Expr,Expr) => Expr, a0: Expr, a1: Expr): Expr
  def dcpfun3apply(fd: FunDesc, ad0: ArgDesc, ad1: ArgDesc, ad2: ArgDesc, fx: (Expr,Expr,Expr) => Expr, a0: Expr, a1: Expr, a2: Expr): Expr
  */
