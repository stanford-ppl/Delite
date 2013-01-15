package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq

case class Function(
  // sizes of the input arguments to this function
  val argSize: Seq[IRPoly],
  // polynomial to determine the sign of this function
  val sign: SignumPoly,
  // polynomial to determine the tonicity of this function
  val tonicity: Seq[SignumPoly],
  // polynomial to determine the vexity of this function
  val vexity: SignumPoly,
  // size of the inner variable of this function
  var varSize: IRPoly,
  // objective and constraints for inner problem (objective is return value)
  val objectiveArgAlmap: Seq[Almap],
  val objectiveVarAlmap: Almap,
  val objectiveOffset: AVector,
  val affineArgAlmap: Seq[Almap],
  val affineVarAlmap: Almap,
  val affineOffset: AVector,
  val conicArgAlmap: Seq[Almap],
  val conicVarAlmap: Almap,
  val conicOffset: AVector,
  val conicCone: Cone) extends HasArity[Function]
{
  val arity: Int = varSize.arity
  // first, make sure that the signum polynomials have the correct number of inputs
  // two inputs for each argument, one for sign and one for vexity
  if(vexity.arity != argSize.length) throw new IRValidationException()
  if(sign.arity != argSize.length) throw new IRValidationException()
  if(tonicity.length != argSize.length) throw new IRValidationException()
  for(t <- tonicity) {
    if(t.arity != argSize.length) throw new IRValidationException()
  }
  // next, verify that all the constraints have the appropriate size
  // this also implicitly verifies that all the arguments have the same arity
  if(objectiveArgAlmap.length != argSize.length) throw new IRValidationException()
  if(affineArgAlmap.length != argSize.length) throw new IRValidationException()
  if(conicArgAlmap.length != argSize.length) throw new IRValidationException()
  for(i <- 0 until argSize.length) {
    if(objectiveArgAlmap(i).domain != argSize(i)) throw new IRValidationException()
    if(affineArgAlmap(i).domain != argSize(i)) throw new IRValidationException()
    if(conicArgAlmap(i).domain != argSize(i)) throw new IRValidationException() 
  }
  if(objectiveVarAlmap.domain != varSize) throw new IRValidationException()
  if(affineVarAlmap.domain != varSize) throw new IRValidationException()
  if(conicVarAlmap.domain != varSize) throw new IRValidationException()
  // verify that all codomains agree
  if(objectiveVarAlmap.codomain != objectiveOffset.size) throw new IRValidationException()
  if(affineVarAlmap.codomain != affineOffset.size) throw new IRValidationException()
  if(conicVarAlmap.codomain != conicOffset.size) throw new IRValidationException()
  if(conicVarAlmap.codomain != conicCone.size) throw new IRValidationException()
  for(i <- 0 until argSize.length) {
    if(objectiveArgAlmap(i).codomain != objectiveOffset.size)
    if(affineArgAlmap(i).codomain != affineOffset.size) throw new IRValidationException()
    if(conicArgAlmap(i).codomain != conicOffset.size) throw new IRValidationException()
  }

  def arityOp(op: ArityOp): Function = Function(
    argSize map (x => x.arityOp(op)),
    sign,
    tonicity,
    vexity,
    varSize.arityOp(op),
    objectiveArgAlmap map (x => x.arityOp(op)),
    objectiveVarAlmap.arityOp(op),
    objectiveOffset.arityOp(op),
    affineArgAlmap map (x => x.arityOp(op)),
    affineVarAlmap.arityOp(op),
    affineOffset.arityOp(op),
    conicArgAlmap map (x => x.arityOp(op)),
    conicVarAlmap.arityOp(op),
    conicOffset.arityOp(op),
    conicCone.arityOp(op))


  def +(y: Function): Function = {
    // the two functions to be added must take the same arguments
    if(argSize != y.argSize) throw new IRValidationException()
    // form the output function
    Function(
      argSize,
      sign + y.sign,
      for(i <- 0 until argSize.length) yield tonicity(i) + y.tonicity(i),
      vexity + y.vexity,
      varSize + y.varSize,
      for(i <- 0 until argSize.length) yield objectiveArgAlmap(i) + y.objectiveArgAlmap(i),
      AlmapHCat(objectiveVarAlmap, y.objectiveVarAlmap),
      objectiveOffset + y.objectiveOffset,
      for(i <- 0 until argSize.length) yield AlmapVCat(affineArgAlmap(i), y.affineArgAlmap(i)),
      Almap.diagCat(affineVarAlmap, y.affineVarAlmap),
      affineOffset ++ y.affineOffset,
      for(i <- 0 until argSize.length) yield AlmapVCat(conicArgAlmap(i), y.conicArgAlmap(i)),
      Almap.diagCat(conicVarAlmap, y.conicVarAlmap),
      conicOffset ++ y.conicOffset,
      ConeProduct(conicCone, y.conicCone))
  }

  def unary_-(): Function = Function(
    argSize,
    -sign,
    tonicity map (x => -x),
    -vexity,
    varSize,
    objectiveArgAlmap map (x => -x),
    -objectiveVarAlmap,
    -objectiveOffset,
    affineArgAlmap,
    affineVarAlmap,
    affineOffset,
    conicArgAlmap,
    conicVarAlmap,
    conicOffset,
    conicCone)

  def -(y: Function): Function = this + (-y)

  def compose(ys: Seq[Function]): Function = {
    // verify that the same number of arguments are given for both functions
    if(ys.length != argSize.length) throw new IRValidationException()
    for(i <- 0 until ys.length) {
      if(ys(i).argSize.length != ys(0).argSize.length) throw new IRValidationException()
    }
    // form the output function
    Function(
      ys(0).argSize.length,
      sign.polyeval(ys map (x => x.sign)),
      for(i <- 0 until ys(0).argSize.length) yield {
        var tacc: SignumPoly = SignumPoly.const(Signum.Zero, ys(0).argSize.length)
        for(j <- 0 until argSize.length) {
          tacc = tacc + tonicity(j).polyeval(ys map (x => x.sign)) * ys(j).tonicity(i)
        }
        tacc
      },
      {
        var vacc: SignumPoly = vexity.polyeval(ys map (x => x.sign))
        for(j <- 0 until argSize.length) {
          tacc = tacc + tonicity(j).polyeval(ys map (x => x.sign)) * ys(j).vexity
        }
        tacc
      },
      ys.foldLeft(varSize)((b,a) => b + a),
      
    )
  }
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
