package ppl.dsl.optila.matrix

import java.io.{PrintWriter}

import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.common.{CudaGenBase, ScalaGenBase, OpenCLGenBase, CGenBase}
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.Config
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.Util._

import ppl.dsl.optila._

trait MatrixBuildableOps extends Variables {
  this: OptiLA =>

  // clients use Interface[MatrixBuildable]
  class MBuildableInterface[A:Manifest](val ops: MatBuildableOpsCls[A]) extends Interface[MatrixBuildable[A]] {
    override def toString = "MBuildableInterface(" + ops.elem.toString + "  [manifest: " + ops.mA.toString + "])"
  }

  // then we convert from a Interface[MatrixBuildable[T]] to an InterfaceMatBuildableOpsCls, providing all of the original matrix methods  
  implicit def interfaceMatBuildableToOps[A:Manifest](intf: Interface[MatrixBuildable[A]]): InterfaceMatBuildableOpsCls[A] 
    = new InterfaceMatBuildableOpsCls(intf.asInstanceOf[MBuildableInterface[A]]) // all Interface[MatrixBuildable] should be instances of MInterfaceBuildable, but can we enforce this?
    
  trait MatBuildableOpsCls[A] extends InterfaceOps[MatrixBuildable[A]] {
    type Self <: MatrixBuildable[A]
    implicit def wrap(x: Rep[Self]): Interface[MatrixBuildable[A]]
    val elem: Rep[Self] 
    val x = elem    
    
    type V[X]
    type M[X]
    type VA = V[A]
    type MA = M[A]   
    implicit def mA: Manifest[A] 
    implicit def toIntf[B:Manifest](x: Rep[M[B]]): Interface[MatrixBuildable[B]]            
    implicit def vecToIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]]
    
    // HACK --
    // TODO: how can we allow shared methods between interfaces without creating ambiguous conversions?
    // do we need an ancestor interface to hold them?
    // def numRows(implicit ctx: SourceContext): Rep[Int]
    // def numCols(implicit ctx: SourceContext): Rep[Int]
    protected def _numRows(implicit ctx: SourceContext): Rep[Int]
    protected def _numCols(implicit ctx: SourceContext): Rep[Int]
      
    def update(i: Rep[Int], j: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
    def insertRow(pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit]
    def insertAllRows(pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[Unit]
    def insertCol(pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit]
    def insertAllCols(pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[Unit]    
    def removeRows(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
    def removeCols(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]    
    
    // data operations
    def update(i: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit] = updateRow(i, y)
    def updateRow(row: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit] = matrix_updaterow(x,row,y)
    def +=(y: Rep[VA])(implicit ctx: SourceContext): Rep[Unit] = this.+=(vecToIntf(y))
    def +=(y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit] = insertRow(_numRows,y)
    def ++=(y: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[Unit] = insertAllRows(_numRows,y)
    def removeRow(pos: Rep[Int])(implicit ctx: SourceContext): Rep[Unit] = removeRows(pos, unit(1))
    def removeCol(pos: Rep[Int])(implicit ctx: SourceContext): Rep[Unit] = removeCols(pos, unit(1))    
  }
  
  class InterfaceMatBuildableOpsCls[A:Manifest](val intf: MBuildableInterface[A]) {
    def update(i: Rep[Int], j: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = intf.ops.update(i,j,y)
    def update(i: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = intf.ops.update(i,y)
    def updateRow(row: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = intf.ops.updateRow(row,y)
    def +=(y: Interface[Vector[A]])(implicit ctx: SourceContext) = intf.ops.+=(y) 
    def ++=(y: Interface[Matrix[A]])(implicit ctx: SourceContext) = intf.ops.++=(y)
    def insertRow(pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = intf.ops.insertRow(pos,y)
    def insertAllRows(pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = intf.ops.insertAllRows(pos,y)
    def insertCol(pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = intf.ops.insertCol(pos,y)
    def insertAllCols(pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = intf.ops.insertAllCols(pos,y)
    def removeRow(pos: Rep[Int])(implicit ctx: SourceContext) = intf.ops.removeRow(pos)
    def removeRows(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = intf.ops.removeRows(pos,len)
    def removeCol(pos: Rep[Int])(implicit ctx: SourceContext) = intf.ops.removeCol(pos)
    def removeCols(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = intf.ops.removeCols(pos,len)    
  }  
}