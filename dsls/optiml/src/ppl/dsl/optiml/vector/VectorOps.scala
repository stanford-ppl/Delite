package ppl.dsl.optiml.vector

import ppl.dsl.optiml.{CudaGenDataStruct, OpenCLGenDataStruct}
import java.io.{PrintWriter}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import scala.reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.dsl.optiml._
import scala.reflect.SourceContext

trait OptiMLDenseVectorOps extends VectorOps with ppl.dsl.optila.vector.DenseVectorOps {
  this: OptiML =>
  
  //implicit def repToOptiMLVecOverrides[A:Manifest](x: Rep[A])(implicit toIntf: Rep[A] => Interface[Vector[A]]) = new OptiMLVecOpsOverrides(x)
  
  // overrides for OptiLA types - we have to override each OptiLA type conversion, otherwise the implicit priorities tie :(
  implicit def denseToVecOverrides[A:Manifest](x: Rep[DenseVector[A]]) = new OptiMLVecOpsOverrides(x)  
}

trait OptiMLVectorViewOps extends VectorOps with ppl.dsl.optila.vector.VectorViewOps {
  this: OptiML =>
  
  implicit def viewToVecOverrides[A:Manifest](x: Rep[VectorView[A]]) = new OptiMLVecOpsOverrides(x)  
}

trait OptiMLRangeVectorOps extends VectorOps with ppl.dsl.optila.vector.RangeVectorOps {
  this: OptiML =>
  
  implicit def rangeToVecOverrides(x: Rep[RangeVector]) = new OptiMLVecOpsOverrides(x)  
}

trait VectorOps extends ppl.dsl.optila.vector.VectorOps {
  this: OptiML =>

  // object Vector {
  //     // no way to extend a super-type object
  //   }

  // overrides for OptiLA types - we would have to override each OptiLA type conversion :(
  // override implicit def repToDenseVecOps[A:Manifest](x: Rep[DenseVector[A]]): OptiMLDenseVecOpsCls[A] = new OptiMLDenseVecOpsCls(x)
  // override implicit def varToDenseVecOps[A:Manifest](x: Var[DenseVector[A]]): OptiMLDenseVecOpsCls[A] = new OptiMLDenseVecOpsCls(readVar(x))
  //   
  
  // class OptiMLDenseVecOpsCls[A:Manifest](elem: Rep[DenseVector[A]]) extends DenseVecOpsCls[A](elem) with OptiMLVecOpsCls[A]    
  // 
  // trait OptiMLVecOpsCls[A] extends VecOpsCls[A] {
  //   def update(i: Interface[IndexVector], y: Rep[A])(implicit o: Overloaded1) = vector_update_indices(x,i,y)    
  // 
  //   override type VFINDR = IndexVectorDense
  //   override val mVFINDR = manifest[VFINDR]
  //   override val vfindBuilder = indexVecDenseBuilder
  //   override def vfindToIntf(x: Rep[VFINDR]) = indexVecDenseToInterface(x)
  //   //override def find(pred: Rep[A] => Rep[Boolean]) = vector_find_override(x,pred)
  // }
  
  class OptiMLVecOpsOverrides[A:Manifest](x: Interface[Vector[A]]) {
    def update(i: Interface[IndexVector], y: Rep[A])(implicit o: Overloaded1, ctx: SourceContext) = vector_update_indices(x,i,y)    
    def update(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = x.update(n, y) // ?
    def find(pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext) = vector_find_override(x, pred)
  }

  // class defs
  def vector_update_indices[A:Manifest](x: Interface[Vector[A]], i: Interface[IndexVector], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def vector_find_override[A:Manifest](x: Interface[Vector[A]], pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[IndexVectorDense]
}

trait VectorOpsExp extends ppl.dsl.optila.vector.VectorOpsExp with VectorOps with VariablesExp with BaseFatExp {

  this: VectorImplOps with OptiMLExp =>


  ///////////////////////////////////////////////////
  // implemented via method on real data structure

    
  ////////////////////////////////
  // implemented via delite ops
  
  case class VectorUpdateIndices[A:Manifest](x: Interface[Vector[A]], intf: Interface[IndexVector], y: Exp[A])
    extends DeliteOpForeach[Int] {

    val in = intf.ops.elem.asInstanceOf[Exp[Vector[Int]]]
    def func = i => x(i) = y
    def sync = i => List()
    val size = intf.length
  } 
  
  case class VectorFindOverride[A:Manifest](intf: Interface[Vector[A]], cond: Exp[A] => Exp[Boolean])
    extends DeliteOpFilter[A,Int,IndexVectorDense] {
      
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]
    def alloc = IndexVector(unit(0))
    def func = e => v // should we make available and use a helper function like index(e)?
    val size = intf.length

    def m = manifest[A]  
  }

  /////////////////////
  // object interface

/*
  def vector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectMutable(VectorNew[A](len, isRow)(manifest[VectorImpl[A]])) //XXX
  def vector_obj_fromseq[A:Manifest](xs: Exp[Seq[A]]) = reflectPure(VectorObjectFromSeq(xs)) //XXX
  def vector_obj_ones(len: Exp[Int]) = reflectPure(VectorObjectOnes(len))
  def vector_obj_onesf(len: Exp[Int]) = reflectPure(VectorObjectOnesF(len))
  def vector_obj_zeros(len: Exp[Int]) = reflectPure(VectorObjectZeros(len))
  def vector_obj_zerosf(len: Exp[Int]) = reflectPure(VectorObjectZerosF(len))
  def vector_obj_rand(len: Exp[Int]) = reflectEffect(VectorObjectRand(len)) // somehow causes recursive schedules -- looks like a lazy eval problem: internal IndexVectorConstruct depends on enclosing VectorObjectRand
  def vector_obj_randf(len: Exp[Int]) = reflectEffect(VectorObjectRandF(len)) // same here
  def vector_obj_range(start: Exp[Int], end: Exp[Int], stride: Exp[Int], isRow: Exp[Boolean]) = reflectPure(VectorObjectRange(start, end, stride, isRow))
  def vector_obj_uniform(start: Exp[Double], step_size: Exp[Double], end: Exp[Double], isRow: Exp[Boolean]) = reflectPure(VectorObjectUniform(start, step_size, end, isRow))
  def vector_obj_flatten[A:Manifest](pieces: Exp[Vector[Vector[A]]]) = reflectPure(VectorObjectFlatten(pieces))
*/


  /////////////////////
  // class interface

  def vector_update_indices[A:Manifest](x: Interface[Vector[A]], i: Interface[IndexVector], y: Exp[A])(implicit ctx: SourceContext) = reflectWrite(x.ops.elem)(VectorUpdateIndices(x,i,y))
  def vector_find_override[A:Manifest](x: Interface[Vector[A]], pred: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(VectorFindOverride(x, pred))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case Reflect(e@VectorUpdateIndices(x,i,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorUpdateIndices(f(x),f(i),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??



  /////////////////////
  // aliases and sharing

  // TODO: precise sharing info for other IR types (default is conservative)
  
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case VectorUpdateIndices(a,is,x) => Nil   // syms(a) <-- any use to return a?
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case VectorUpdateIndices(a,is,x) => syms(x)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case VectorUpdateIndices(a,is,x) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case VectorUpdateIndices(a,is,x) => syms(a)
    case _ => super.copySyms(e)
  }
}

/**
 * Optimizations for composite VectorOps operations.
 */

// have to extend DeliteCollectionOps to override dc_apply...
trait VectorOpsExpOpt extends ppl.dsl.optila.vector.VectorOpsExp with VectorOpsExp with DeliteCollectionOpsExp {
  this: VectorImplOps with OptiMLExp =>

  // override def vector_slice[A:Manifest,VA:Manifest](x: Interface[Vector[A]], start: Rep[Int], end: Rep[Int])(implicit b: VectorBuilder[A,VA]): Rep[VA] = x match {
  //   case Intf(Def(IndexVectorRange(s,e))) => indexvector_range(s+start,s+end).asInstanceOf[Rep[VA]]
  //   case _ => super.vector_slice(x,start,end)
  // }
    
  // override def vector_slice[A:Manifest](x: Rep[Vector[A]], start: Rep[Int], end: Rep[Int]): Rep[Vector[A]] = x match {
  //     case Def(IndexVectorRange(s,e)) => indexvector_range(s+start,s+end).asInstanceOf[Rep[Vector[A]]] // TODO: assert s+end < e!
  //     case _ => super.vector_slice(x,start,end)
  //   }
  // 
  // 
  //   override def vector_length[A:Manifest](x: Exp[Vector[A]]) = x match {
  //     /* these are essential for fusing:    */
  //     case Def(IndexVectorRange(s,e)) => e - s
  //     case Def(StreamChunkRow(x, i, offset)) => x.numCols
  //     case Def(StreamChunkRowFusable(x, i, offset)) => x.numCols // necessary, it's not a DeliteVectorLoop
  //       
  //     case _ => 
  //       //printerr("could not short-circuit call to " + x.toString + ".length")
  //       //printerr(findDefinition(x.asInstanceOf[Sym[Vector[A]]]))
  //       super.vector_length(x)
  //   }
  // 
  //   override def vector_isRow[A:Manifest](x: Exp[Vector[A]]) = x match {
  //     case Def(IndexVectorRange(s,e)) => Const(true)
  //     case _ => super.vector_isRow(x)
  //   }
  //   
  //   // and this one also helps in the example:
  //   override def vector_optimize_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]): Option[Exp[A]] = x match {
  //     case Def(IndexVectorRange(s,e)) => Some((s + n).asInstanceOf[Exp[A]])
  //     case Def(StreamChunkRow(x, i, offset)) => Some(stream_chunk_elem(x,i,n))
  //     //case Def(StreamChunkRowFusable(x, i, offset)) => Some(stream_chunk_elem(x,i,n)) <-- enabling this will remove the computation altogether
  //     case _ => super.vector_optimize_apply(x,n)
  //   }
}

trait BaseGenVectorOps extends GenericFatCodegen {
  val IR: VectorOpsExp
  import IR._

}

trait ScalaGenVectorOps extends BaseGenVectorOps with ScalaGenFat {
  val IR: VectorOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //     // these are the ops that call through to the underlying real data structure
  //     case _ => super.emitNode(sym, rhs)
  //   }
}


trait CudaGenVectorOps extends BaseGenVectorOps with CudaGenFat with CudaGenDataStruct {
  val IR: VectorOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  // 
  //     case _ => super.emitNode(sym, rhs)
  //   }
}

trait OpenCLGenVectorOps extends BaseGenVectorOps with OpenCLGenFat with OpenCLGenDataStruct {
  val IR: VectorOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //
  //     case _ => super.emitNode(sym, rhs)
  //   }
}

trait CGenVectorOps extends BaseGenVectorOps with CGenFat {
  val IR: VectorOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

