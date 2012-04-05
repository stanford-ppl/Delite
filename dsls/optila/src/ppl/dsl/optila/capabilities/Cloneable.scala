package ppl.dsl.optila.capabilities

import scala.virtualization.lms.common.{Variables, Base}
import scala.reflect.SourceContext
import ppl.dsl.optila.{DenseVector,Matrix}
import ppl.dsl.optila.{OptiLAExp, OptiLA}

trait CloneableInternal[Rep[X],T] {
  def Clone(lhs: Rep[T])(implicit ctx: SourceContext): Rep[T]
  def mutable(lhs: Rep[T])(implicit ctx: SourceContext): Rep[T]
}

trait CloneableOps extends Variables {
  this: OptiLA =>

  type Cloneable[X] = CloneableInternal[Rep,X]
  
  implicit def cloneableToCloneableOps[T:Cloneable:Manifest](n: T) = new CloneableOpsCls(unit(n))
  implicit def repCloneableToCloneableOps[T:Cloneable:Manifest](n: Rep[T]) = new CloneableOpsCls(n)
  implicit def varCloneableToCloneableOps[T:Cloneable:Manifest](n: Var[T]) = new CloneableOpsCls(readVar(n))

  class CloneableOpsCls[T](lhs: Rep[T])(implicit mT: Manifest[T], cloneable: Cloneable[T]){
    def Clone()(implicit ctx: SourceContext) = cloneable.Clone(lhs)
    def mutable()(implicit ctx: SourceContext) = cloneable.mutable(lhs)
  }
  
//  implicit def vectorCloneable[A,VA <% Interface[Vector[A]]] = new Cloneable[VA] {
  implicit def vectorCloneable[A,VA](implicit toOps: Rep[VA] => VecOpsCls[A]) = new Cloneable[VA] {
    def Clone(lhs: Rep[VA])(implicit ctx: SourceContext) = lhs.Clone().asInstanceOf[Rep[VA]] //toIntf(lhs).Clone().ops.elem.asInstanceOf[Rep[VA]]
    def mutable(lhs: Rep[VA])(implicit ctx: SourceContext) = lhs.mutable().asInstanceOf[Rep[VA]] //toIntf(lhs).mutable().ops.elem.asInstanceOf[Rep[VA]]
  }
  
  // implicit def denseVectorCloneable[T:Manifest]: Cloneable[DenseVector[T]] = new Cloneable[DenseVector[T]] {
  //   def Clone(lhs: Rep[DenseVector[T]]) = lhs.Clone()
  //   def mutable(lhs: Rep[DenseVector[T]]) = lhs.mutable()
  // }
  // 
  // implicit def rangeVectorCloneable: Cloneable[RangeVector] = new Cloneable[RangeVector] {
  //   def Clone(lhs: Rep[RangeVector]) = lhs.Clone()
  //   def mutable(lhs: Rep[RangeVector]) = lhs.mutable()
  // }
  
  implicit def matrixCloneable[A,MA](implicit tOps: Rep[MA] => MatOpsCls[A]) = new Cloneable[MA] {
    def Clone(lhs: Rep[MA])(implicit ctx: SourceContext) = lhs.Clone().asInstanceOf[Rep[MA]]
    def mutable(lhs: Rep[MA])(implicit ctx: SourceContext) = lhs.mutable().asInstanceOf[Rep[MA]]
  }
  
  implicit def tuple2Cloneable[A:Manifest:Cloneable,B:Manifest:Cloneable]: Cloneable[Tuple2[A,B]] =
    new Cloneable[Tuple2[A,B]] {
      def Clone(lhs: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) = Tuple2(lhs._1.Clone, lhs._2.Clone)
      def mutable(lhs: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) = Tuple2(lhs._1.mutable, lhs._2.mutable)
  }
  
  implicit def tuple3Cloneable[A:Manifest:Cloneable,B:Manifest:Cloneable,C:Manifest:Cloneable]: Cloneable[Tuple3[A,B,C]] =
    new Cloneable[Tuple3[A,B,C]] {
      def Clone(lhs: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) = Tuple3(lhs._1.Clone, lhs._2.Clone, lhs._3.Clone)
      def mutable(lhs: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) = Tuple3(lhs._1.mutable, lhs._2.mutable, lhs._3.mutable)
  }
  
  implicit def tuple4Cloneable[A:Manifest:Cloneable,B:Manifest:Cloneable,C:Manifest:Cloneable,D:Manifest:Cloneable]: Cloneable[Tuple4[A,B,C,D]] =
    new Cloneable[Tuple4[A,B,C,D]] {
      def Clone(lhs: Rep[Tuple4[A,B,C,D]])(implicit ctx: SourceContext) = Tuple4(lhs._1.Clone, lhs._2.Clone, lhs._3.Clone, lhs._4.Clone)
      def mutable(lhs: Rep[Tuple4[A,B,C,D]])(implicit ctx: SourceContext) = Tuple4(lhs._1.mutable, lhs._2.mutable, lhs._3.mutable, lhs._4.mutable)
  }
  
  implicit def numericCloneable[T:Numeric]: Cloneable[T] = new Cloneable[T] {
    def Clone(lhs: Rep[T])(implicit ctx: SourceContext) = lhs
    def mutable(lhs: Rep[T])(implicit ctx: SourceContext) = lhs // TODO: 5.mutable?
  }  
}

