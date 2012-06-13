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

/**
 * Implements the SparseMatrixBuildable interface with a COO sparse matrix.
 */ 

trait SparseMatrixCOOCompilerOps extends SparseMatrixBuildableOps {
  this: OptiLA =>
  
  def sparsematrix_coo_raw_data[A:Manifest](x: Rep[SparseMatrixBuildable[A]])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
  def sparsematrix_coo_raw_colindices[A:Manifest](x: Rep[SparseMatrixBuildable[A]])(implicit ctx: SourceContext): Rep[DeliteArray[Int]]  
  def sparsematrix_coo_raw_rowindices[A:Manifest](x: Rep[SparseMatrixBuildable[A]])(implicit ctx: SourceContext): Rep[DeliteArray[Int]]  
  def sparsematrix_coo_set_raw_data[A:Manifest](x: Rep[SparseMatrixBuildable[A]], newVal: Rep[DeliteArray[A]])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_coo_set_raw_colindices[A:Manifest](x: Rep[SparseMatrixBuildable[A]], newVal: Rep[DeliteArray[Int]])(implicit ctx: SourceContext): Rep[Unit]
  def sparsematrix_coo_set_raw_rowindices[A:Manifest](x: Rep[SparseMatrixBuildable[A]], newVal: Rep[DeliteArray[Int]])(implicit ctx: SourceContext): Rep[Unit]
}

trait SparseMatrixCOOOpsExp extends SparseMatrixCOOCompilerOps with DeliteCollectionOpsExp with VariablesExp {
  this: SparseMatrixCOOImplOps with OptiLAExp  =>
  
  //////////////////////////////////////////////////
  // implemented via method on real data structure
  
  case class SparseMatrixCOONew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) extends DefWithManifest[A,SparseMatrixBuildable[A]]   
  case class SparseMatrixCOORawData[A:Manifest](x: Exp[SparseMatrixBuildable[A]]) extends DefWithManifest[A,DeliteArray[A]]
  case class SparseMatrixCOORawColIndices[A:Manifest](x: Exp[SparseMatrixBuildable[A]]) extends DefWithManifest[A,DeliteArray[Int]]
  case class SparseMatrixCOORawRowIndices[A:Manifest](x: Exp[SparseMatrixBuildable[A]]) extends DefWithManifest[A,DeliteArray[Int]]
  case class SparseMatrixCOOSetRawData[A:Manifest](x: Exp[SparseMatrixBuildable[A]], newVal: Exp[DeliteArray[A]]) extends DefWithManifest[A,Unit]
  case class SparseMatrixCOOSetRawColIndices[A:Manifest](x: Exp[SparseMatrixBuildable[A]], newVal: Exp[DeliteArray[Int]]) extends DefWithManifest[A,Unit]
  case class SparseMatrixCOOSetRawRowIndices[A:Manifest](x: Exp[SparseMatrixBuildable[A]], newVal: Exp[DeliteArray[Int]]) extends DefWithManifest[A,Unit]
  
  /////////////////////////////////////
  // implemented via kernel embedding
  
  case class SparseMatrixCOOToCSR[A:Manifest](x: Exp[SparseMatrixBuildable[A]])
    extends DeliteOpSingleWithManifest[A,SparseMatrix[A]](reifyEffectsHere(sparsematrix_coo_to_csr_impl(x)))
  
    
  ///////////////////
  // class interface
  
  def sparsematrix_obj_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(SparseMatrixCOONew[A](numRows, numCols)) 
  def sparsematrix_buildable_finish[A:Manifest](x: Rep[SparseMatrixBuildable[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixCOOToCSR[A](x)) // note: COO tied to CSR here
  
  //////////////////
  // internal

  def sparsematrix_coo_raw_data[A:Manifest](x: Exp[SparseMatrixBuildable[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixCOORawData(x))
  def sparsematrix_coo_raw_colindices[A:Manifest](x: Exp[SparseMatrixBuildable[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixCOORawColIndices(x))
  def sparsematrix_coo_raw_rowindices[A:Manifest](x: Exp[SparseMatrixBuildable[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixCOORawRowIndices(x))
  def sparsematrix_coo_set_raw_data[A:Manifest](x: Exp[SparseMatrixBuildable[A]], data: Exp[DeliteArray[A]])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixCOOSetRawData(x,data))
  def sparsematrix_coo_set_raw_colindices[A:Manifest](x: Exp[SparseMatrixBuildable[A]], newVal: Exp[DeliteArray[Int]])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixCOOSetRawColIndices(x,newVal))
  def sparsematrix_coo_set_raw_rowindices[A:Manifest](x: Exp[SparseMatrixBuildable[A]], newVal: Exp[DeliteArray[Int]])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixCOOSetRawRowIndices(x,newVal))  
  
  /////////////////////
  // delite collection
  
  override def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext) = {
    if (isSparseMatBuildable(x)) Const(()) // allocN(0) for sparse matrices allocates the correct logical size,
                                           // and matrix conditional ops are currently not supported
    else super.dc_set_logical_size(x,y)        
  }
  
  override def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext): Exp[CA] = {
    if (isSparseMatBuildable(x)) {
      val m = asSparseMatBuildable(x)
      val out = SparseMatrix[A](unit(0), unit(0))
      sparsematrix_coo_set_raw_data(out, DeliteArray[A](size).unsafeImmutable)      
      sparsematrix_coo_set_raw_colindices(out, DeliteArray[Int](size).unsafeImmutable)
      sparsematrix_coo_set_raw_rowindices(out, DeliteArray[Int](size).unsafeImmutable)
      sparsematrix_buildable_set_nnz(out, size)
      sparsematrix_buildable_set_numrows(out, m.numRows)
      sparsematrix_buildable_set_numcols(out, m.numCols)
      out.asInstanceOf[Exp[CA]]
    }
    else super.dc_alloc[A,CA](x,size)
  }  
  
  override def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    if (isSparseMatBuildable(src) && isSparseMatBuildable(dst)) {
      darray_unsafe_copy(sparsematrix_coo_raw_rowindices(asSparseMatBuildable(src)), srcPos, sparsematrix_coo_raw_rowindices(asSparseMatBuildable(dst)), dstPos, size)
      darray_unsafe_copy(sparsematrix_coo_raw_colindices(asSparseMatBuildable(src)), srcPos, sparsematrix_coo_raw_colindices(asSparseMatBuildable(dst)), dstPos, size)
      darray_unsafe_copy(sparsematrix_coo_raw_data(asSparseMatBuildable(src)), srcPos, sparsematrix_coo_raw_data(asSparseMatBuildable(dst)), dstPos, size)
    }
    else super.dc_copy(src,srcPos,dst,dstPos,size)
  }        
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@SparseMatrixCOONew(r,c) => reflectPure(SparseMatrixCOONew(f(r),f(c))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixCOORawData(x) => reflectPure(SparseMatrixCOORawData(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixCOORawColIndices(x) => reflectPure(SparseMatrixCOORawColIndices(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixCOORawRowIndices(x) => reflectPure(SparseMatrixCOORawRowIndices(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    
    // delite ops
    case e@SparseMatrixCOOToCSR(x) => reflectPure(new { override val original = Some(f,e) } with SparseMatrixCOOToCSR(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    
    // reflected
    case Reflect(e@SparseMatrixCOONew(x,y), u, es) => reflectMirrored(Reflect(SparseMatrixCOONew(f(x),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixCOORawData(x), u, es) => reflectMirrored(Reflect(SparseMatrixCOORawData(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixCOORawColIndices(x), u, es) => reflectMirrored(Reflect(SparseMatrixCOORawColIndices(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixCOORawRowIndices(x), u, es) => reflectMirrored(Reflect(SparseMatrixCOORawRowIndices(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))        
    case Reflect(e@SparseMatrixCOOSetRawData(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixCOOSetRawData(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))       
    case Reflect(e@SparseMatrixCOOSetRawColIndices(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixCOOSetRawColIndices(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))       
    case Reflect(e@SparseMatrixCOOSetRawRowIndices(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixCOOSetRawRowIndices(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))           
    case Reflect(e@SparseMatrixCOOToCSR(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixCOOToCSR(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))      
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
    
}


trait ScalaGenSparseMatrixCOOOps extends ScalaGenBase {
  val IR: SparseMatrixCOOOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case m@SparseMatrixCOONew(numRows, numCols) => emitValDef(sym, "new " + remap("generated.scala.SparseMatrixCOO[" + remap(m.mA) + "]")+"(" + quote(numRows) + "," + quote(numCols) + ")")    
    case SparseMatrixCOORawData(x) => emitValDef(sym, quote(x) + "._data")
    case SparseMatrixCOORawColIndices(x) => emitValDef(sym, quote(x) + "._colIndices") 
    case SparseMatrixCOORawRowIndices(x) => emitValDef(sym, quote(x) + "._rowIndices")  
    case SparseMatrixCOOSetRawData(x,v) => emitValDef(sym, quote(x) + "._data = " + quote(v))
    case SparseMatrixCOOSetRawColIndices(x,v) => emitValDef(sym, quote(x) + "._colIndices = " + quote(v))
    case SparseMatrixCOOSetRawRowIndices(x,v) => emitValDef(sym, quote(x) + "._rowIndices = " + quote(v))    
    case _ => super.emitNode(sym, rhs)
  }
  
  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SparseMatrixBuildable" => remap("generated.scala.SparseMatrixCOO[" + remap(m.typeArguments(0)) + "]")
    case _ => super.remap(m)
  }  
}

trait CudaGenSparseMatrixCOOOps extends CudaGenBase with CudaGenDataStruct {
  val IR: SparseMatrixCOOOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait OpenCLGenSparseMatrixCOOOps extends OpenCLGenBase with OpenCLGenDataStruct {
  val IR: SparseMatrixCOOOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenSparseMatrixCOOOps extends CGenBase {
  val IR: SparseMatrixCOOOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}