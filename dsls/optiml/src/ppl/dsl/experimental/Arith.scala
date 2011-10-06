package ppl.dsl.experimental

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import java.io.PrintWriter
import scala.virtualization.lms.internal.{CLikeCodegen}

trait ArithInternal[Rep[X],T] {
  def +(a: Rep[T], b: Rep[T]): Rep[T]
}

trait ArithOps extends Variables with OverloadHack {
  this: Sandbox =>
  
  type Arith[X] = ArithInternal[Rep,X]

  implicit def arithToArithOps[T:Arith:Manifest](n: T) = new ArithOpsCls(unit(n))
  implicit def repArithToArithOps[T:Arith:Manifest](n: Rep[T]) = new ArithOpsCls(n)
  implicit def varArithToArithOps[T:Arith:Manifest](n: Var[T]) = new ArithOpsCls(readVar(n))

  // to do Rep[Int] * Float, it should get converted to Rep[Float] * Float
  // TODO: this only works when invoked explicitly (won't kick in itself)
  implicit def chainRepArithToArithOps[A,B](a: Rep[A])
    (implicit mA: Manifest[A], aA: Arith[A], mB: Manifest[B], aB: Arith[B], c: Rep[A] => Rep[B]) = new ArithOpsCls(c(a))

  class ArithOpsCls[T](lhs: Rep[T])(implicit mT: Manifest[T], arith: Arith[T]){
    // TODO: if B == Rep[T] below, the ops implicit does not work unless it is called explicitly (no unambiguous resolution?)
    def +(rhs: Rep[T]): Rep[T] = arith.+(lhs,rhs)
  }


  /**
   * Vector
   */
  // TESTING
  // trait CanPlusEquals[A,B,R] {
  //   def apply(lhs: Rep[A], rhs: Rep[B]): Rep[R]    
  // }
  // implicit def canPlusEqualsDD[T:Manifest] = new CanPlusEquals[DenseVector[T],DenseVector[T],DenseVector[T]] {
  //   def apply(lhs: Rep[DenseVector[T]], rhs: Rep[DenseVector[T]]) = lhs += rhs 
  // }
  
  implicit def denseVectorArith[T:Arith:Manifest]: Arith[DenseVector[T]] = new Arith[DenseVector[T]] {
    def +(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]]): Rep[DenseVector[T]] = a+b
  }
  
  implicit def sparseVectorArith[T:Arith:Manifest]: Arith[SparseVector[T]] = new Arith[SparseVector[T]] {
    def +(a: Rep[SparseVector[T]], b: Rep[SparseVector[T]]) = a+b
  }
  
  implicit val intArith: Arith[Int] = new Arith[Int] {
    def +(a: Rep[Int], b: Rep[Int]) = arith_plus(a,b)
  }
  
  def arith_plus[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T]): Rep[T]  
}

trait ArithOpsExp extends ArithOps with VariablesExp {
  this: SandboxExp =>

  case class ArithPlus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[T]

  def arith_plus[T:Manifest:Numeric](lhs: Exp[T], rhs: Exp[T]): Exp[T] = reflectPure(ArithPlus(lhs, rhs))
}

trait ScalaGenArithOps extends ScalaGenBase {
  val IR: ArithOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ArithPlus(a,b) => emitValDef(sym, quote(a) + " + " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}


 
