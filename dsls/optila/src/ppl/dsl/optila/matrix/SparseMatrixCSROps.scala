package ppl.dsl.optila.matrix

import java.io.{PrintWriter}

import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.common.{CudaGenBase, ScalaGenBase, OpenCLGenBase, CGenBase}
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.datastructures.DeliteArray
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.Config
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.Util._

import ppl.dsl.optila._

trait SparseMatrixCSRCompilerOps extends SparseMatrixOps {
  this: OptiLA =>
 
  def sparsematrix_csr_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[SparseMatrix[A]]
  def sparsematrix_csr_raw_data[A:Manifest](x: Rep[SparseMatrix[A]])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
  def sparsematrix_csr_raw_colindices[A:Manifest](x: Rep[SparseMatrix[A]])(implicit ctx: SourceContext): Rep[DeliteArray[Int]]  
  def sparsematrix_csr_raw_rowptr[A:Manifest](x: Rep[SparseMatrix[A]])(implicit ctx: SourceContext): Rep[DeliteArray[Int]]  
  def sparsematrix_csr_set_raw_data[A:Manifest](x: Rep[SparseMatrix[A]], newVal: Rep[DeliteArray[A]])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_csr_set_raw_colindices[A:Manifest](x: Rep[SparseMatrix[A]], newVal: Rep[DeliteArray[Int]])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_csr_set_raw_rowptr[A:Manifest](x: Rep[SparseMatrix[A]], newVal: Rep[DeliteArray[Int]])(implicit ctx: SourceContext): Rep[Unit]
}

trait SparseMatrixCSROpsExp extends SparseMatrixCSRCompilerOps with DeliteCollectionOpsExp with VariablesExp {
  this: SparseMatrixCSRImplOps with OptiLAExp  =>
  
  //////////////////////////////////////////////////
  // implemented via method on real data structure
  
  case class SparseMatrixCSRNew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) extends DefWithManifest[A,SparseMatrix[A]]   
  case class SparseMatrixCSRRawData[A:Manifest](x: Exp[SparseMatrix[A]]) extends DefWithManifest[A,DeliteArray[A]]
  case class SparseMatrixCSRRawColIndices[A:Manifest](x: Exp[SparseMatrix[A]]) extends DefWithManifest[A,DeliteArray[Int]]
  case class SparseMatrixCSRRawRowPtr[A:Manifest](x: Exp[SparseMatrix[A]]) extends DefWithManifest[A,DeliteArray[Int]]
  case class SparseMatrixCSRSetRawData[A:Manifest](x: Exp[SparseMatrix[A]], newVal: Exp[DeliteArray[A]]) extends DefWithManifest[A,Unit]
  case class SparseMatrixCSRSetRawColIndices[A:Manifest](x: Exp[SparseMatrix[A]], newVal: Exp[DeliteArray[Int]]) extends DefWithManifest[A,Unit]
  case class SparseMatrixCSRSetRawRowPtr[A:Manifest](x: Exp[SparseMatrix[A]], newVal: Exp[DeliteArray[Int]]) extends DefWithManifest[A,Unit]
    
  //////////////////
  // internal

  def sparsematrix_csr_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(SparseMatrixCSRNew[A](numRows, numCols)) 
  def sparsematrix_csr_raw_data[A:Manifest](x: Exp[SparseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixCSRRawData(x))//reflectMutable(SparseMatrixRawData(x.unsafeImmutable))  
  def sparsematrix_csr_raw_colindices[A:Manifest](x: Exp[SparseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixCSRRawColIndices(x))
  def sparsematrix_csr_raw_rowptr[A:Manifest](x: Exp[SparseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixCSRRawRowPtr(x))
  def sparsematrix_csr_set_raw_data[A:Manifest](x: Exp[SparseMatrix[A]], data: Exp[DeliteArray[A]])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixCSRSetRawData(x,data))
  def sparsematrix_csr_set_raw_colindices[A:Manifest](x: Exp[SparseMatrix[A]], newVal: Exp[DeliteArray[Int]])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixCSRSetRawColIndices(x,newVal))
  def sparsematrix_csr_set_raw_rowptr[A:Manifest](x: Exp[SparseMatrix[A]], newVal: Exp[DeliteArray[Int]])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixCSRSetRawRowPtr(x,newVal))
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@SparseMatrixCSRNew(r,c) => reflectPure(SparseMatrixCSRNew(f(r),f(c))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixCSRRawData(x) => reflectPure(SparseMatrixCSRRawData(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixCSRRawColIndices(x) => reflectPure(SparseMatrixCSRRawColIndices(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixCSRRawRowPtr(x) => reflectPure(SparseMatrixCSRRawRowPtr(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])

    // reflected
    case Reflect(e@SparseMatrixCSRNew(x,y), u, es) => reflectMirrored(Reflect(SparseMatrixCSRNew(f(x),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixCSRRawData(x), u, es) => reflectMirrored(Reflect(SparseMatrixCSRRawData(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixCSRRawColIndices(x), u, es) => reflectMirrored(Reflect(SparseMatrixCSRRawColIndices(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixCSRRawRowPtr(x), u, es) => reflectMirrored(Reflect(SparseMatrixCSRRawRowPtr(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixCSRSetRawData(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixCSRSetRawData(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))       
    case Reflect(e@SparseMatrixCSRSetRawColIndices(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixCSRSetRawColIndices(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))       
    case Reflect(e@SparseMatrixCSRSetRawRowPtr(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixCSRSetRawRowPtr(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))       
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??  
}

trait ScalaGenSparseMatrixCSROps extends ScalaGenBase {
  val IR: SparseMatrixCSROpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case m@SparseMatrixCSRNew(numRows, numCols) => emitValDef(sym, "new " + remap("generated.scala.SparseMatrixCSR[" + remap(m.mA) + "]")+"(" + quote(numRows) + "," + quote(numCols) + ")")    
    case SparseMatrixCSRRawData(x) => emitValDef(sym, quote(x) + "._data")
    case SparseMatrixCSRRawColIndices(x) => emitValDef(sym, quote(x) + "._colIndices") 
    case SparseMatrixCSRRawRowPtr(x) => emitValDef(sym, quote(x) + "._rowPtr")  
    case SparseMatrixCSRSetRawData(x,v) => emitValDef(sym, quote(x) + "._data = " + quote(v))
    case SparseMatrixCSRSetRawColIndices(x,v) => emitValDef(sym, quote(x) + "._colIndices = " + quote(v))
    case SparseMatrixCSRSetRawRowPtr(x,v) => emitValDef(sym, quote(x) + "._rowPtr = " + quote(v))
    case _ => super.emitNode(sym, rhs)
  }
  
  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SparseMatrix" => remap("generated.scala.SparseMatrixCSR[" + remap(m.typeArguments(0)) + "]")
    case _ => super.remap(m)
  }  
}

trait CudaGenSparseMatrixCSROps extends CudaGenBase with CudaGenDataStruct {
  val IR: SparseMatrixCSROpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait OpenCLGenSparseMatrixCSROps extends OpenCLGenBase with OpenCLGenDataStruct {
  val IR: SparseMatrixCSROpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenSparseMatrixCSROps extends CGenBase {
  val IR: SparseMatrixCSROpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}
