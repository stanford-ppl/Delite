package ppl.dsl.CVX

  // traits for convexity analysis
  trait Convex
  trait Concave
  trait Affine extends Convex with Concave
  
  
  sealed class Expr
  class ConvexExpr extends Expr with Convex
  class ConcaveExpr extends Expr with Concave
  // affine expressions are both convex and concave
  // this allows us to define behavior on convex and concave functions
  // and have affine functions properly handled without a separate case
  class AffineExpr extends Expr with Affine
  
  // these are singleton objects for DCP verification
  sealed abstract class ArgAttribute
  trait Nondecreasing extends ArgAttribute
  trait Nonincreasing extends ArgAttribute
  object Nondecreasing extends Nondecreasing
  object Nonincreasing extends Nonincreasing
  
  sealed abstract class FuncAttribute
  // trait ConvexFunc extends FuncAttribute
  //   trait ConcaveFunc extends FuncAttribute
  //   trait AffineFunc extends FuncAttribute with ConvexFunc with ConcaveFunc
  object ConvexFunc extends FuncAttribute with Convex
  object ConcaveFunc extends FuncAttribute with Concave
  object AffineFunc extends FuncAttribute with Affine
  object Nonconvex extends FuncAttribute

  final class OptVar extends AffineExpr
  final class ConstExpr extends AffineExpr
  
  // anything that is none of the above has no attached traits

  // apparently, type BLARGH = Int with Double with String
  // makes a "set" of types for the <: and :> operators...
  
  /*
   * class for numeric container--assuming one does not already exist
   */
  //class Constants extends Int with Float with Double //with Matrix[Float] with Matrix[Double]