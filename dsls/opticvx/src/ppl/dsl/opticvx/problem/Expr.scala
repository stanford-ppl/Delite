package ppl.dsl.opticvx.problem

abstract class Expr {
  def shape: Shape
  def isInput: Boolean
  def verify(): Unit
}

//A compound expression representing a for loop
class ExprFor(val size: Size, val body: Expr) extends Expr {
  def shape: Shape = ShapeFor(size, body.shape)
  def isInput: Boolean = body.isInput
}
//A compound expression of different subexpressions
class ExprStruct(val body: Seq[Expr]) extends Expr {
  def shape: Shape = ShapeStruct(body map ((x) => x.shape))
  def isInput: Boolean = ((body map ((x) => x.isInput)).foldLeft(true)((i,s) => i && s))
}

//A compound expression representing a reference to the problem variable
class ExprVariable(val shape: Shape) extends Expr {
  def shape: Shape = shape
  def isInput: Boolean = false
}

class ExprInput(val shape: Shape) extends Expr {
  def shape: Shape = shape
  def isInput: Boolean = true
}

class ExprSum(val arg1: Expr, val arg2: Expr) extends Expr {
  def shape: Shape = ShapeScalar()
  def isInput: Boolean = arg1.isInput && arg2.isInput
}

class ExprReduce(val body: Expr) extends Expr {
  def shape: Shape = ShapeScalar()
  def isInput: Boolean = body.isInput
}

class ExprProd(val scale: Expr, val arg: Expr) {
  def shape: Shape = ShapeScalar()
  def isInput: Boolean = scale.isInput && arg.isInput
}

class ExprNeg(val arg: Expr) extends Expr {
  def shape: Shape = ShapeScalar()
  def isInput: Boolean = arg.isInput
}

class ExprIndex(val at: Size, val arg: Expr) extends Expr {
  def shape: Shape = arg.shape match {
    case ShapeFor(size, body) => body
    case _ => throw new Exception("Can't index a non-for-shaped expression.")
  }
  def isInput: Boolean = arg.isInput
}

class ExprAccess(val at: Int, val arg: Expr) extends Expr {
  def shape: Shape = arg.shape match {
    case ShapeStruct(body) => body(at)
    case _ => throw new Exception("Can't access a non-struct-shaped expression.")
  }
  def isInput: Boolean = arg.isInput
}
