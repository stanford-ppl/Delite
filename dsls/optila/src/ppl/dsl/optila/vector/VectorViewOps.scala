package ppl.dsl.optila.vector

import ppl.dsl.optila.{Vector, DenseVector, VectorView}
import ppl.dsl.optila.{OptiLAExp, OptiLA}
import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase}
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter

trait VectorViewOps extends DSLType with Base with OverloadHack { this: OptiLA =>

  implicit def repToVectorViewVecOps[A:Manifest](x: Rep[VectorView[A]]) = new VectorViewVecOpsCls(x)
  implicit def varToVectorViewVecOps[A:Manifest](x: Var[VectorView[A]]) = new VectorViewVecOpsCls(readVar(x))
  implicit def vectorViewToInterface[A:Manifest](lhs: Rep[VectorView[A]]) = new VInterface(new VectorViewVecOpsCls(lhs))
    
  class VectorViewVecOpsCls[A:Manifest](val elem: Rep[VectorView[A]]) extends VecOpsCls[A] {
    // type VA = VectorView
    // def toOps(x: Rep[VectorView[A]]) = repToVectorViewVecOps(x)
    // def toIntf(x: Rep[VectorView[A]]) = vectorViewToInterface(x)
    // def builder: VectorBuilder[Int,VectorView] = vectorViewVectorBuilder
    def mA = manifest[A]
    //def mVA = manifest[VectorView]
    
    type V[X] = DenseVector[X]       
    type Self = VectorView[A]
    def wrap(x: Rep[VectorView[A]]) = vectorViewToInterface(x)
    def toOps[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)
    def toIntf[B:Manifest](x: Rep[DenseVector[B]]): Interface[Vector[B]] = denseToInterface(x)
    def builder[B:Manifest]: VectorBuilder[B,V[B]] = denseVectorBuilder[B]    
    def mV[B:Manifest] = manifest[DenseVector[B]] 
          
    // VectorOps
    def length = vectorview_length(elem)
    def isRow = vectorview_isrow(elem)
    def apply(n: Rep[Int]) = vectorview_apply(elem,n)
    def sort(implicit o: Ordering[A]) = elem.cloneL.sort    
    def t = vectorview_transpose(x)
    
    // generic
    type VPLUSR = DenseVector[A]
    val mVPLUSR = manifest[VPLUSR]
    val vplusBuilder = denseVectorBuilder[A]
    def vplusToIntf(x: Rep[VPLUSR]) = denseToInterface(x)
    
    type VMINUSR = DenseVector[A]
    val mVMINUSR = manifest[VMINUSR]
    val vminusBuilder = denseVectorBuilder[A]
    def vminusToIntf(x: Rep[VMINUSR]) = denseToInterface(x)    
    
    type VTIMESR = DenseVector[A]
    val mVTIMESR = manifest[VTIMESR]
    val vtimesBuilder = denseVectorBuilder[A]
    def vtimesToIntf(x: Rep[VTIMESR]) = denseToInterface(x)        
        
    def mt() = throw new UnsupportedOperationException("VectorViews cannot be updated")    
    def update(n: Rep[Int], y: Rep[A]): Rep[Unit] = throw new UnsupportedOperationException("VectorViews cannot be updated")
    def +=(y: Rep[A]) = throw new UnsupportedOperationException("VectorViews cannot be updated")
    def copyFrom(pos: Rep[Int], y: Rep[DenseVector[A]]) = throw new UnsupportedOperationException("VectorViews cannot be updated")
    def insert(pos: Rep[Int], y: Rep[A]) = throw new UnsupportedOperationException("VectorViews cannot be updated")
    def insertAll(pos: Rep[Int], y: Rep[DenseVector[A]]) = throw new UnsupportedOperationException("VectorViews cannot be updated")
    def removeAll(pos: Rep[Int], len: Rep[Int]) = throw new UnsupportedOperationException("VectorViews cannot be updated")
    def trim() = throw new UnsupportedOperationException("VectorViews cannot be updated")
    def clear() = throw new UnsupportedOperationException("VectorViews cannot be updated")        
  } 
  
  def vectorview_length[A:Manifest](x: Rep[VectorView[A]]): Rep[Int]
  def vectorview_isrow[A:Manifest](x: Rep[VectorView[A]]): Rep[Boolean]
  def vectorview_apply[A:Manifest](x: Rep[VectorView[A]], n: Rep[Int]): Rep[A]
  def vectorview_transpose[A:Manifest](x: Rep[VectorView[A]]): Rep[DenseVector[A]]
  // def vectorview_times_matrix(x: Rep[VectorView[A]], y: Rep[Matrix[Int]]): Rep[DenseVector[Int]]
  // def vectorview_flatmap[B:Manifest](x: Rep[VectorView[A]], f: Rep[A] => Rep[DenseVector[B]]): Rep[DenseVector[B]]
}

trait VectorViewOpsExp extends VectorViewOps with DeliteCollectionOpsExp { this: OptiLAExp =>
  
  def vectorview_length[A:Manifest](x: Exp[VectorView[A]]): Exp[Int] = throw new UnsupportedOperationException("tbd")
  
  def vectorview_isrow[A:Manifest](x: Exp[VectorView[A]]): Exp[Boolean] = throw new UnsupportedOperationException("tbd")
  
  def vectorview_apply[A:Manifest](x: Exp[VectorView[A]], n: Exp[Int]): Exp[A] = throw new UnsupportedOperationException("tbd")
  
  def vectorview_transpose[A:Manifest](x: Exp[VectorView[A]]): Exp[DenseVector[A]] = throw new UnsupportedOperationException("tbd")
}

trait VectorViewOpsExpOpt extends VectorViewOpsExp { this: OptiLAExp =>
  
  override def vectorview_length[A:Manifest](x: Exp[VectorView[A]]) = x match {
    case Def(MatrixVView(a, start, stride, l, r)) => l
    case Def(MatrixGetRow(a,i)) => a.numCols
    case _ => super.vectorview_length(x)
  }  
  
  override def vectorview_isrow[A:Manifest](x: Exp[VectorView[A]]) = x match {
    case Def(MatrixVView(a, start, stride, l, r)) => r
    case Def(MatrixGetRow(a,i)) => Const(true)
    case _ => super.vectorview_isrow(x)
  }
  
  // and this one also helps in the example:
  def vectorview_optimize_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]): Option[Exp[A]] = x match {
    case Def(MatrixGetRow(x, i)) => Some(matrix_apply(x,i,n))
    case _ => None
  }
  
  override def vectorview_apply[A:Manifest](x: Exp[VectorView[A]], n: Exp[Int]) = {
    vectorview_optimize_apply(x.asInstanceOf[Exp[DeliteCollection[A]]],n) getOrElse super.vectorview_apply(x,n)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]) = {
    vectorview_optimize_apply(x,n) getOrElse super.dc_apply(x,n)
  }
}
  
