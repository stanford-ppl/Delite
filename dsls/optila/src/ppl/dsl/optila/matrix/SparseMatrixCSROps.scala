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
import ppl.delite.framework.transform.LoweringTransform
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

trait SparseMatrixCSROpsExp extends SparseMatrixCSRCompilerOps with DeliteCollectionOpsExp with VariablesExp with SparseTransform {
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

  case class SparseMatrixCSRRowIndices(rowPtr: Rep[DeliteArray[Int]])
    extends DeliteOpSingleTask[DeliteArray[Int]](reifyEffectsHere(sparsematrix_csr_rowindices_impl(rowPtr)))
  
  case class SparseMatrixCSRZipNZUnion[A:Manifest,B:Manifest,R:Manifest](ma: Rep[SparseMatrix[A]], mb: Rep[SparseMatrix[B]], f: (Rep[A],Rep[B]) => Rep[R])
    extends DeliteOpSingleTask[SparseMatrix[R]](reifyEffectsHere(sparsematrix_csr_zip_nz_union_impl(ma, mb, f))) {
      val mA = manifest[A]
      val mB = manifest[B]
      val mR = manifest[R]
    }

  case class SparseMatrixCSRZipNZIntersection[A:Manifest,B:Manifest,R:Manifest](ma: Rep[SparseMatrix[A]], mb: Rep[SparseMatrix[B]], f: (Rep[A],Rep[B]) => Rep[R])
    extends DeliteOpSingleTask[SparseMatrix[R]](reifyEffectsHere(sparsematrix_csr_zip_nz_intersection_impl(ma, mb, f))) {
      val mA = manifest[A]
      val mB = manifest[B]
      val mR = manifest[R]      
    }
  
  def sparsematrix_csr_rowindices(rowPtr: Rep[DeliteArray[Int]]) = reflectPure(SparseMatrixCSRRowIndices(rowPtr))
  def sparsematrix_csr_zip_nz_union[A:Manifest,B:Manifest,R:Manifest](ma: Rep[SparseMatrix[A]], mb: Rep[SparseMatrix[B]], f: (Rep[A],Rep[B]) => Rep[R]) = reflectPure(SparseMatrixCSRZipNZUnion(ma,mb,f))
  def sparsematrix_csr_zip_nz_intersection[A:Manifest,B:Manifest,R:Manifest](ma: Rep[SparseMatrix[A]], mb: Rep[SparseMatrix[B]], f: (Rep[A],Rep[B]) => Rep[R]) = reflectPure(SparseMatrixCSRZipNZIntersection(ma,mb,f))
      
  //////////////////
  // internal

  def sparsematrix_csr_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(SparseMatrixCSRNew[A](numRows, numCols)) 
  def sparsematrix_csr_raw_data[A:Manifest](x: Exp[SparseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixCSRRawData(x))//reflectMutable(SparseMatrixRawData(x.unsafeImmutable))  
  def sparsematrix_csr_raw_colindices[A:Manifest](x: Exp[SparseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixCSRRawColIndices(x))
  def sparsematrix_csr_raw_rowptr[A:Manifest](x: Exp[SparseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixCSRRawRowPtr(x))
  def sparsematrix_csr_set_raw_data[A:Manifest](x: Exp[SparseMatrix[A]], data: Exp[DeliteArray[A]])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixCSRSetRawData(x,data))
  def sparsematrix_csr_set_raw_colindices[A:Manifest](x: Exp[SparseMatrix[A]], newVal: Exp[DeliteArray[Int]])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixCSRSetRawColIndices(x,newVal))
  def sparsematrix_csr_set_raw_rowptr[A:Manifest](x: Exp[SparseMatrix[A]], newVal: Exp[DeliteArray[Int]])(implicit ctx: SourceContext) = reflectWrite(x)(SparseMatrixCSRSetRawRowPtr(x,newVal))
  
  
  /////////////////////////////////////////
  // data parallel ops on underlying arrays
   
  def sparsematrix_mapnz[A:Manifest,B:Manifest](x: Exp[SparseMatrix[A]], f: Exp[A] => Exp[B]) = {
    val out = sparsematrix_csr_new[B](x.numRows, x.numCols)
    sparsematrix_csr_set_raw_data(out, sparsematrix_csr_raw_data(x).map(f))
    x match {
      case Def(Reflect(_, u, _)) if mustMutable(u) => 
        sparsematrix_csr_set_raw_colindices(out, sparsematrix_csr_raw_colindices(x).Clone)
        sparsematrix_csr_set_raw_rowptr(out, sparsematrix_csr_raw_rowptr(x).Clone)
      case _ => 
        sparsematrix_csr_set_raw_colindices(out, sparsematrix_csr_raw_colindices(x))  
        sparsematrix_csr_set_raw_rowptr(out, sparsematrix_csr_raw_rowptr(x))
    }    
    sparsematrix_set_nnz(out, x.nnz)
    out.unsafeImmutable
  }  
  
  def sparsematrix_zipnz[A:Manifest,B:Manifest,R:Manifest](ma: Exp[SparseMatrix[A]], mb: Exp[SparseMatrix[B]], f: (Exp[A],Exp[B]) => Exp[R], side: SparseZipWithSpecialization): Exp[SparseMatrix[R]] = side match {
    case SparseLeft =>
      // need to zip only places where ma is non-zero
      val colIndices = sparsematrix_csr_raw_colindices(ma)   
      val rowPtr = sparsematrix_csr_raw_rowptr(ma)   
      val data = sparsematrix_csr_raw_data(ma)
      // this essentially converts from CSR back to COO format - do we need this step?
      // means binary ops are now O(numRows + nnz) instead of just O(nnz) ...      
      val rowIndices = sparsematrix_csr_rowindices(rowPtr)      
      val outData = darray_range(unit(0),colIndices.length).map(i => f(data(i), mb(rowIndices(i),colIndices(i))))
      val out = sparsematrix_csr_new[R](ma.numRows, ma.numCols)
      sparsematrix_csr_set_raw_data(out, outData)
      ma match {
        case Def(Reflect(_, u, _)) if mustMutable(u) => 
          sparsematrix_csr_set_raw_colindices(out, colIndices.Clone)
          sparsematrix_csr_set_raw_rowptr(out, rowPtr.Clone)
        case _ => 
          sparsematrix_csr_set_raw_colindices(out, colIndices.unsafeImmutable)  
          sparsematrix_csr_set_raw_rowptr(out, rowPtr.unsafeImmutable)
      }                
      sparsematrix_set_nnz(out, ma.nnz)
      out.unsafeImmutable
                
    case SparseRight =>
      // need to zip only places where mb is non-zero
      val colIndices = sparsematrix_csr_raw_colindices(mb)   
      val rowPtr = sparsematrix_csr_raw_rowptr(mb)   
      val data = sparsematrix_csr_raw_data(mb)
      val rowIndices = sparsematrix_csr_rowindices(rowPtr)      
      val outData = darray_range(unit(0),colIndices.length).map(i => f(ma(rowIndices(i),colIndices(i)), data(i)))
      val out = sparsematrix_csr_new[R](mb.numRows, mb.numCols)
      sparsematrix_csr_set_raw_data(out, outData)
      mb match {
        case Def(Reflect(_, u, _)) if mustMutable(u) => 
          sparsematrix_csr_set_raw_colindices(out, colIndices.Clone)
          sparsematrix_csr_set_raw_rowptr(out, rowPtr.Clone)
        case _ => 
          sparsematrix_csr_set_raw_colindices(out, colIndices.unsafeImmutable)  
          sparsematrix_csr_set_raw_rowptr(out, rowPtr.unsafeImmutable)
      }                
      sparsematrix_set_nnz(out, mb.nnz)
      out.unsafeImmutable

    case SparseUnion =>    
      // need to zip only places where either ma or mb are non-zero    
      sparsematrix_csr_zip_nz_union(ma,mb,f)
                     
    case SparseIntersect =>
      // need to zip only places where both ma and mb are non-zero
      
      // could be parallelized much more straightforwardly than union, but is the overhead worth it? (how large does nnz have to be?)
      sparsematrix_csr_zip_nz_intersection(ma,mb,f)
  }
  
  def sparsematrix_reducenz[A:Manifest](x: Exp[SparseMatrix[A]], f: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A]) = {
    sparsematrix_csr_raw_data(x).reduce(f, zero) // need to trim? any extra values should have no effect..
  }  
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@SparseMatrixCSRNew(r,c) => reflectPure(SparseMatrixCSRNew(f(r),f(c))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixCSRRawData(x) => reflectPure(SparseMatrixCSRRawData(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixCSRRawColIndices(x) => reflectPure(SparseMatrixCSRRawColIndices(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixCSRRawRowPtr(x) => reflectPure(SparseMatrixCSRRawRowPtr(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixCSRRowIndices(x) => reflectPure(new { override val original = Some(f,e) } with SparseMatrixCSRRowIndices(f(x)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixCSRZipNZUnion(x,y,g) => reflectPure(new { override val original = Some(f,e) } with SparseMatrixCSRZipNZUnion(f(x),f(y),f(g))(e.mA,e.mB,e.mR))(mtype(manifest[A]),implicitly[SourceContext])
    case e@SparseMatrixCSRZipNZIntersection(x,y,g) => reflectPure(new { override val original = Some(f,e) } with SparseMatrixCSRZipNZIntersection(f(x),f(y),f(g))(e.mA,e.mB,e.mR))(mtype(manifest[A]),implicitly[SourceContext])
    
    // reflected
    case Reflect(e@SparseMatrixCSRNew(x,y), u, es) => reflectMirrored(Reflect(SparseMatrixCSRNew(f(x),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixCSRRawData(x), u, es) => reflectMirrored(Reflect(SparseMatrixCSRRawData(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixCSRRawColIndices(x), u, es) => reflectMirrored(Reflect(SparseMatrixCSRRawColIndices(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixCSRRawRowPtr(x), u, es) => reflectMirrored(Reflect(SparseMatrixCSRRawRowPtr(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixCSRSetRawData(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixCSRSetRawData(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))       
    case Reflect(e@SparseMatrixCSRSetRawColIndices(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixCSRSetRawColIndices(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))       
    case Reflect(e@SparseMatrixCSRSetRawRowPtr(x,v), u, es) => reflectMirrored(Reflect(SparseMatrixCSRSetRawRowPtr(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))       
    case Reflect(e@SparseMatrixCSRRowIndices(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixCSRRowIndices(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@SparseMatrixCSRZipNZUnion(x,y,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixCSRZipNZUnion(f(x),f(y),f(g))(e.mA,e.mB,e.mR), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@SparseMatrixCSRZipNZIntersection(x,y,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixCSRZipNZIntersection(f(x),f(y),f(g))(e.mA,e.mB,e.mR), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??  
  
  
  //////////////
  // transforms
    
  def sparsematrix_mapnz_manifest[A:Manifest,B:Manifest] = sparsematrix_mapnz[A,B] _
  def sparsematrix_zipnz_manifest[A:Manifest,B:Manifest,R:Manifest] = sparsematrix_zipnz[A,B,R] _
  def sparsematrix_reducenz_manifest[A:Manifest] = sparsematrix_reducenz[A] _
  
  override def onCreate[A:Manifest](s: Sym[A], d: Def[A]) = d match {    
    // map         
    case e:DeliteOpMapI[_,_,_,_] if (Config.optimize > 0 && isSparseMat(e.in)) =>
      specializeSparseMap(s, e, asSparseMat(e.in), sparsematrix_mapnz_manifest(e.dmA,e.dmB))(e.dmA,e.dmB).map(_.asInstanceOf[Exp[A]]) getOrElse super.onCreate(s,d)
    case Reflect(e:DeliteOpMapI[_,_,_,_], u, es) if (Config.optimize > 0 && isSparseMat(e.in)) =>
      reflectSpecialized(specializeSparseMap(s, e, asSparseMat(e.in), sparsematrix_mapnz_manifest(e.dmA,e.dmB))(e.dmA,e.dmB), u, es)(super.onCreate(s,d))
        
    // zip
    case e:DeliteOpZipWithI[_,_,_,_,_] if (Config.optimize > 0 && isSparseMat(e.inA) && isSparseMat(e.inB)) =>
      specializeSparseZip(s, e, asSparseMat(e.inA), asSparseMat(e.inB), sparsematrix_zipnz_manifest(e.dmA,e.dmB,e.dmR))(e.dmA,e.dmB,e.dmR).map(_.asInstanceOf[Exp[A]]) getOrElse super.onCreate(s,d)
    case Reflect(e:DeliteOpZipWithI[_,_,_,_,_], u, es) if (Config.optimize > 0 && isSparseMat(e.inA) && isSparseMat(e.inB)) =>
      reflectSpecialized(specializeSparseZip(s, e, asSparseMat(e.inA), asSparseMat(e.inB), sparsematrix_zipnz_manifest(e.dmA,e.dmB,e.dmR))(e.dmA,e.dmB,e.dmR), u, es)(super.onCreate(s,d))
      
    // reduce  
    case e:DeliteOpReduce[_] if (Config.optimize > 0 && isSparseMat(e.in)) =>
      specializeSparseReduce(s, e, asSparseMat(e.in), sparsematrix_reducenz_manifest(e.dmA))(e.dmA).map(_.asInstanceOf[Exp[A]]) getOrElse super.onCreate(s,d)
    case Reflect(e:DeliteOpReduce[_], u, es) if (Config.optimize > 0 && isSparseMat(e.in)) =>
      reflectSpecialized(specializeSparseReduce(s, e, asSparseMat(e.in), sparsematrix_reducenz_manifest(e.dmA))(e.dmA), u, es)(super.onCreate(s,d))
    
    // TODO: filter
    // MapReduce, ReduceFold, .. ?
    
    // what about dense-sparse, sparse-dense?
    
    case _ => super.onCreate(s,d)
  }
     
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
