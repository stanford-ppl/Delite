package ppl.dsl.opticvx.problem

/*

trait Expr {
  //The number of integer parameters in this expression's scope
  val nIntParams: Int
  //The shape/input desc of this expression
  val shape: IShape
  //The shapes/input desc of bindings in the local scope
  //By default, bindings[0] is the variable, and bindings[1] is the input
  val bindings: Seq[IShape]

  if (shape.nIntParams != nIntParams) throw new ProblemIRValidationException()
  for (b <- bindings) {
    if (b.nIntParams != nIntParams) throw new ProblemIRValidationException()
  }
}

//A compound expression representing a for loop
case class ExprFor(val size: Size, val body: Expr) extends Expr {
  val nIntParams: Int = size.nIntParams
  val shape: IShape = IShapeFor(nIntParams, size, body.shape)
  val bindings: Seq[IShape] = body.bindings

  if (size.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (body.nIntParams != (nIntParams + 1)) throw new ProblemIRValidationException()
}

//A compound expression of different subexpressions
case class ExprStruct(val body: Seq[Expr]) extends Expr {
  val nIntParams: Int = body(0).nIntParams
  val bindings: Seq[IShape] = body(0).bindings
  val shape: IShape = IShapeStruct(nIntParams, body map ((x) => x.shape))

  for (b <- body) {
    if (b.nIntParams != nIntParams) throw new ProblemIRValidationException()
    if (b.bindings != bindings) throw new ProblemIRValidationException()
  }
}

case class ExprIndex(val at: Size, val arg: Expr) extends Expr {
  val nIntParams: Int = arg.nIntParams
  
  if (at.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (arg.nIntParams != nIntParams) throw new ProblemIRValidationException()
  
  val shape: IShape = arg.shape match {
    case IShapeFor(nip, sz, b) => b
    case _ => throw new ProblemIRValidationException()
  }
  
  val bindings: Seq[IShape] = arg.bindings
}

case class ExprAccess(val at: Int, val arg: Expr) extends Expr {
  val nIntParams: Int = arg.nIntParams
  
  val shape: IShape = arg.shape match {
    case IShapeStruct(nip, bs) => bs(at)
    case _ => throw new ProblemIRValidationException()
  }
  
  val bindings: Seq[IShape] = arg.bindings
}

//A reference to a bound expression
case class ExprReference(val nIntParams: Int, val bindings: Seq[IShape], val index: Int) extends Expr {
  val shape: IShape = bindings(index)
}

//A compound expression under which a variable is bound
case class ExprLet(val bound: Expr, val body: Expr) extends Expr {
  val nIntParams = bound.nIntParams
  val shape: IShape = body.shape
  val bindings: Seq[IShape] = body.bindings :+ bound.shape

  if (body.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (bound.nIntParams != nIntParams) throw new ProblemIRValidationException()
}

case class ExprSum(val arg1: Expr, val arg2: Expr) extends Expr {
  val nIntParams: Int = arg1.nIntParams

  if (arg1.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (arg2.nIntParams != nIntParams) throw new ProblemIRValidationException()

  val arg1ii: Boolean = arg1.shape match {
    case IShapeScalar(nip, ii) => ii
    case _ => throw new ProblemIRValidationException()
  }
  val arg2ii: Boolean = arg2.shape match {
    case IShapeScalar(nip, ii) => ii
    case _ => throw new ProblemIRValidationException()
  }

  val shape: IShape = IShapeScalar(nIntParams, arg1ii && arg2ii)
  val bindings: Seq[IShape] = arg1.bindings

  if (arg1.bindings != bindings) throw new ProblemIRValidationException()
  if (arg2.bindings != bindings) throw new ProblemIRValidationException()
}

case class ExprReduce(val arg: Expr) extends Expr {
  val nIntParams: Int = arg.nIntParams

  val shape: IShape = arg.shape match {
    case IShapeFor(_, _, IShapeScalar(nip, ii)) => IShapeScalar(nip, ii)
    case _ => throw new ProblemIRValidationException()
  }

  val bindings: Seq[IShape] = arg.bindings
}

case class ExprProd(val arg1: Expr, val arg2: Expr) {
  val nIntParams: Int = arg1.nIntParams

  if (arg1.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (arg2.nIntParams != nIntParams) throw new ProblemIRValidationException()
  
  val arg1ii: Boolean = arg1.shape match {
    case IShapeScalar(nip, ii) => ii
    case _ => throw new ProblemIRValidationException()
  }
  val arg2ii: Boolean = arg2.shape match {
    case IShapeScalar(nip, ii) => ii
    case _ => throw new ProblemIRValidationException()
  }
  
  //Can't multiply two noninput expressions, since the result would be nonaffine
  if((!arg1ii)&&(!arg2ii)) throw new ProblemIRValidationException()

  val shape: IShape = IShapeScalar(nIntParams, arg1ii && arg2ii)
  val bindings: Seq[IShape] = arg1.bindings

  if (arg1.bindings != bindings) throw new ProblemIRValidationException()
  if (arg2.bindings != bindings) throw new ProblemIRValidationException()
}

case class ExprNeg(val arg: Expr) extends Expr {
  val nIntParams: Int = arg.nIntParams
  
  val argii: Boolean = arg.shape match {
    case IShapeScalar(nip, ii) => ii
    case _ => throw new ProblemIRValidationException()
  }
  
  val shape: IShape = IShapeScalar(nIntParams, argii)
  val bindings: Seq[IShape] = arg.bindings
}

*/


