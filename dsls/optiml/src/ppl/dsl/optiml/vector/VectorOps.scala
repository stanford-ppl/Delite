package ppl.dsl.optiml.vector

import ppl.dsl.optiml.CudaGenDataStruct
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.dsl.optiml._

trait VectorOps extends ppl.dsl.optila.vector.VectorOps {
  this: OptiML =>

  // object Vector {
  //     // no way to extend a super-type object
  //   }

  implicit def repVecToOVecOps[A:Manifest](x: Rep[Vector[A]]) = new OptiMLVecOpsCls(x)
  implicit def varToOVecOps[A:Manifest](x: Var[Vector[A]]) = new OptiMLVecOpsCls(readVar(x))

  class OptiMLVecOpsCls[A:Manifest](x: Rep[Vector[A]]) extends vecOpsCls[A](x) {
    def update(i: Rep[IndexVector], y: Rep[A])(implicit o: Overloaded1) = vector_update_indices(x,i,y)    
    override def find(pred: Rep[A] => Rep[Boolean]) = vector_find_override(x,pred)
  }

  // class defs
  def vector_update_indices[A:Manifest](x: Rep[Vector[A]], i: Rep[IndexVector], y: Rep[A]): Rep[Unit]
  def vector_find_override[A:Manifest](x: Rep[Vector[A]], pred: Rep[A] => Rep[Boolean]): Rep[IndexVector]
}

trait VectorOpsExp extends ppl.dsl.optila.vector.VectorOpsExp with VectorOps with VariablesExp with BaseFatExp {

  this: VectorImplOps with OptiMLExp =>


  ///////////////////////////////////////////////////
  // implemented via method on real data structure

    
  ////////////////////////////////
  // implemented via delite ops
  
  case class VectorUpdateIndices[A:Manifest](x: Exp[Vector[A]], in: Exp[IndexVector], y: Exp[A])
    extends DeliteOpForeach[Int] {

    def sync = n => List()
    def func = i => x(i) = y
    val size = in.length
  } 
  
  case class VectorFindOverride[A:Manifest](in: Exp[Vector[A]], cond: Exp[A] => Exp[Boolean])
    extends DeliteOpFilter[A,Int,IndexVector] {
      
    def alloc = IndexVector(0)
    def func = e => v // should we make available and use a helper function like index(e)?
    val size = in.length

    def m = manifest[A]  
  }

  /////////////////////
  // object interface



  /////////////////////
  // class interface

  def vector_update_indices[A:Manifest](x: Exp[Vector[A]], i: Exp[IndexVector], y: Exp[A]) = reflectWrite(x)(VectorUpdateIndices(x,i,y))
  def vector_find_override[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) = reflectPure(VectorFindOverride(x, pred))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
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
trait VectorOpsExpOpt extends ppl.dsl.optila.vector.VectorOpsExpOpt with VectorOpsExp with DeliteCollectionOpsExp {
  this: VectorImplOps with OptiMLExp =>

  override def vector_slice[A:Manifest](x: Rep[Vector[A]], start: Rep[Int], end: Rep[Int]): Rep[Vector[A]] = x match {
    case Def(IndexVectorRange(s,e)) => indexvector_range(s+start,s+end).asInstanceOf[Rep[Vector[A]]] // TODO: assert s+end < e!
    case _ => super.vector_slice(x,start,end)
  }


  override def vector_length[A:Manifest](x: Exp[Vector[A]]) = x match {
    /* these are essential for fusing:    */
    case Def(IndexVectorRange(s,e)) => e - s
    case Def(StreamChunkRow(x, i, offset)) => x.numCols
    case Def(StreamChunkRowFusable(x, i, offset)) => x.numCols // necessary, it's not a DeliteVectorLoop
      
    case _ => 
      //printerr("could not short-circuit call to " + x.toString + ".length")
      //printerr(findDefinition(x.asInstanceOf[Sym[Vector[A]]]))
      super.vector_length(x)
  }

  override def vector_isRow[A:Manifest](x: Exp[Vector[A]]) = x match {
    case Def(IndexVectorRange(s,e)) => Const(true)
    case _ => super.vector_isRow(x)
  }
  
  // and this one also helps in the example:
  override def vector_optimize_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]): Option[Exp[A]] = x match {
    case Def(IndexVectorRange(s,e)) => Some((s + n).asInstanceOf[Exp[A]])
    case Def(StreamChunkRow(x, i, offset)) => Some(stream_chunk_elem(x,i,n))
    //case Def(StreamChunkRowFusable(x, i, offset)) => Some(stream_chunk_elem(x,i,n)) <-- enabling this will remove the computation altogether
    case _ => super.vector_optimize_apply(x,n)
  }  
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

trait CGenVectorOps extends BaseGenVectorOps with CGenFat {
  val IR: VectorOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

