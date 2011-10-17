package ppl.dsl.optila.capabilities

import scala.virtualization.lms.common.{Variables, Base}
import ppl.dsl.optila.{DenseVector,Matrix}
import ppl.dsl.optila.{OptiLAExp, OptiLA}

trait CloneableInternal[Rep[X],T] {
  def cloneL(lhs: Rep[T]): Rep[T]
  def mutable(lhs: Rep[T]): Rep[T]
}

trait CloneableOps extends Variables {
  this: OptiLA =>

  type Cloneable[X] = CloneableInternal[Rep,X]
  
  implicit def cloneableToCloneableOps[T:Cloneable:Manifest](n: T) = new CloneableOpsCls(unit(n))
  implicit def repCloneableToCloneableOps[T:Cloneable:Manifest](n: Rep[T]) = new CloneableOpsCls(n)
  implicit def varCloneableToCloneableOps[T:Cloneable:Manifest](n: Var[T]) = new CloneableOpsCls(readVar(n))

  class CloneableOpsCls[T](lhs: Rep[T])(implicit mT: Manifest[T], cloneable: Cloneable[T]){
    def cloneL() = cloneable.cloneL(lhs)
    def mutable() = cloneable.mutable(lhs)
  }
  
//  implicit def vectorCloneable[A,VA <% Interface[Vector[A]]] = new Cloneable[VA] {
  implicit def vectorCloneable[A,VA](implicit toOps: Rep[VA] => VecOpsCls[A]) = new Cloneable[VA] {
    def cloneL(lhs: Rep[VA]) = lhs.cloneL().asInstanceOf[Rep[VA]] //toIntf(lhs).cloneL().ops.elem.asInstanceOf[Rep[VA]]
    def mutable(lhs: Rep[VA]) = lhs.mutable().asInstanceOf[Rep[VA]] //toIntf(lhs).mutable().ops.elem.asInstanceOf[Rep[VA]]
  }
  
  // implicit def denseVectorCloneable[T:Manifest]: Cloneable[DenseVector[T]] = new Cloneable[DenseVector[T]] {
  //   def cloneL(lhs: Rep[DenseVector[T]]) = lhs.cloneL()
  //   def mutable(lhs: Rep[DenseVector[T]]) = lhs.mutable()
  // }
  // 
  // implicit def rangeVectorCloneable: Cloneable[RangeVector] = new Cloneable[RangeVector] {
  //   def cloneL(lhs: Rep[RangeVector]) = lhs.cloneL()
  //   def mutable(lhs: Rep[RangeVector]) = lhs.mutable()
  // }
  
  implicit def matrixCloneable[T:Manifest]: Cloneable[Matrix[T]] = new Cloneable[Matrix[T]] {
    def cloneL(lhs: Rep[Matrix[T]]) = lhs.cloneL()
    def mutable(lhs: Rep[Matrix[T]]) = lhs.mutable()
  }
  
  implicit def tuple2Cloneable[A:Manifest:Cloneable,B:Manifest:Cloneable]: Cloneable[Tuple2[A,B]] =
    new Cloneable[Tuple2[A,B]] {
      def cloneL(lhs: Rep[Tuple2[A,B]]) = Tuple2(lhs._1.cloneL, lhs._2.cloneL)
      def mutable(lhs: Rep[Tuple2[A,B]]) = Tuple2(lhs._1.mutable, lhs._2.mutable)
  }
  
  implicit def tuple3Cloneable[A:Manifest:Cloneable,B:Manifest:Cloneable,C:Manifest:Cloneable]: Cloneable[Tuple3[A,B,C]] =
    new Cloneable[Tuple3[A,B,C]] {
      def cloneL(lhs: Rep[Tuple3[A,B,C]]) = Tuple3(lhs._1.cloneL, lhs._2.cloneL, lhs._3.cloneL)
      def mutable(lhs: Rep[Tuple3[A,B,C]]) = Tuple3(lhs._1.mutable, lhs._2.mutable, lhs._3.mutable)
  }
  
  implicit def tuple4Cloneable[A:Manifest:Cloneable,B:Manifest:Cloneable,C:Manifest:Cloneable,D:Manifest:Cloneable]: Cloneable[Tuple4[A,B,C,D]] =
    new Cloneable[Tuple4[A,B,C,D]] {
      def cloneL(lhs: Rep[Tuple4[A,B,C,D]]) = Tuple4(lhs._1.cloneL, lhs._2.cloneL, lhs._3.cloneL, lhs._4.cloneL)
      def mutable(lhs: Rep[Tuple4[A,B,C,D]]) = Tuple4(lhs._1.mutable, lhs._2.mutable, lhs._3.mutable, lhs._4.mutable)
  }
  
  implicit def numericCloneable[T:Numeric]: Cloneable[T] = new Cloneable[T] {
    def cloneL(lhs: Rep[T]) = lhs
    def mutable(lhs: Rep[T]) = lhs // TODO: 5.mutable?
  }  
}

