package ppl.dsl.optiml.vector

import ppl.dsl.optiml.{DenseVector,Vector, IndexVector, IndexVectorRange, IndexVectorDense, Matrix}
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase}
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter

trait IndexVectorOps extends DSLType with Base with OverloadHack { this: OptiML =>
  
  object IndexVector {
    def apply(len: Rep[Int]) = indexvector_obj_new(len)
    def apply(xs: Interface[Vector[Int]])(implicit o: Overloaded1) = indexvector_obj_fromvec(xs)
  }

  trait IndexVecOpsCls extends VecOpsCls[Int] {
    def mA = manifest[Int]
    implicit def toOps(x: Rep[VA]): IndexVecOpsCls
    implicit def toIntf(x: Rep[VA]): Interface[IndexVector]
    
    type V[X] = DenseVector[X] // conversion operations on IndexVectors will return a DenseVector
    def toOpsB[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)
    def toIntfB[B:Manifest](x: Rep[DenseVector[B]]): Interface[Vector[B]] = denseToInterface(x)
    def builderB[B:Manifest]: VectorBuilder[B,V[B]] = denseVectorBuilder[B]    
    def mVB[B:Manifest] = manifest[DenseVector[B]] 
    
    // VectorOps generic - math on an IndexVector turns it into a DenseVector (is this the right thing to do?)
    type VPLUSR = DenseVector[Int]
    val mVPLUSR = manifest[VPLUSR]
    val vplusBuilder = denseVectorBuilder[Int]
    def vplusToIntf(x: Rep[VPLUSR]) = denseToInterface(x)
    
    type VMINUSR = DenseVector[Int]
    val mVMINUSR = manifest[VMINUSR]
    val vminusBuilder = denseVectorBuilder[Int]
    def vminusToIntf(x: Rep[VMINUSR]) = denseToInterface(x)    
    
    type VTIMESR = DenseVector[Int]
    val mVTIMESR = manifest[VTIMESR]
    val vtimesBuilder = denseVectorBuilder[Int]
    def vtimesToIntf(x: Rep[VTIMESR]) = denseToInterface(x)            
        
    def apply[A:Manifest](block: Rep[Int] => Rep[A]): Rep[V[A]] = indexvector_construct(x, block)
    
    def *(y: Rep[Matrix[Int]])(implicit a: Arith[Int],o: Overloaded2) = throw new UnsupportedOperationException("tbd")
    def flatMap[B:Manifest](f: Rep[Int] => Rep[DenseVector[B]]) = throw new UnsupportedOperationException("tbd")
    def partition(pred: Rep[Int] => Rep[Boolean]) = throw new UnsupportedOperationException("tbd")
    def groupBy[K:Manifest](pred: Rep[Int] => Rep[K]) = throw new UnsupportedOperationException("tbd")    
  }
    
  class IVInterface(override val ops: IndexVecOpsCls) extends VInterface[Int](ops) with Interface[IndexVector]  
  implicit def interfaceToIndexVecOps(intf: Interface[IndexVector]): InterfaceIndexVecOpsCls = new InterfaceIndexVecOpsCls(intf.asInstanceOf[IVInterface])
  
  class InterfaceIndexVecOpsCls(override val intf: IVInterface) extends InterfaceVecOpsCls[Int](intf) {
    def apply[A:Manifest](block: Rep[Int] => Rep[A]) = intf.ops.apply(block)    
    
    // this is unfortunately required to get the static return type right... TODO: any solution?
    override def slice(start: Rep[Int], end: Rep[Int]) = intf.ops.toIntf(intf.ops.slice(start,end))
  }
  
  // impl defs
  def indexvector_range(start: Rep[Int], end: Rep[Int]): Rep[IndexVectorRange]
  def indexvector_obj_new(len: Rep[Int]): Rep[IndexVectorDense]
  def indexvector_obj_fromvec(xs: Interface[Vector[Int]]): Rep[IndexVectorDense]

  // class defs
  def indexvector_construct[A:Manifest](x: Interface[IndexVector], block: Rep[Int] => Rep[A]): Rep[DenseVector[A]]
}

trait IndexVectorOpsExp extends IndexVectorOps with EffectExp { this: OptiMLExp with IndexVectorImplOps =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class IndexVectorRangeNew(start: Exp[Int], end: Exp[Int]) extends Def[IndexVectorRange]
  case class IndexVectorObjectNew(len: Exp[Int]) extends Def[IndexVectorDense]

  ////////////////////////////////
  // implemented via delite ops

  case class IndexVectorObjectFromVec(xs: Interface[Vector[Int]]) extends DeliteOpSingleTask[IndexVectorDense](reifyEffectsHere(index_vector_obj_fromvec_impl(xs)))

  // Note: Construction from a discrete index vector set will curently return a contiguous (non-sparse) vector.
  // Is this what we want?
  case class IndexVectorConstruct[B:Manifest](in: Interface[IndexVector], func: Exp[Int] => Exp[B])
    extends DeliteOpMap2[Int,B,DenseVector[B]] {

    val size = copyTransformedOrElse(_.size)(in.length)
    
    def alloc = DenseVector[B](in.length, in.isRow)

    def m = manifest[B]
  }
  
  // impl defs
  def indexvector_range(start: Exp[Int], end: Exp[Int]) = reflectPure(IndexVectorRangeNew(start, end))
  def indexvector_obj_new(len: Exp[Int]) = reflectMutable(IndexVectorObjectNew(len))
  def indexvector_obj_fromvec(xs: Interface[Vector[Int]]) = reflectPure(IndexVectorObjectFromVec(xs))

  // class defs
  def indexvector_construct[A:Manifest](x: Interface[IndexVector], block: Exp[Int] => Exp[A]): Exp[DenseVector[A]] = {
    reflectPure(IndexVectorConstruct(x, block))
    // HACK -- better scheduling performance in our apps, forces some expensive dependencies to be hoisted
    // TR TODO: use effect summary of func
    //reflectEffect(IndexVectorConstruct(x, block))
  }

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    //case e@IndexVectorConstruct(in,b) => reflectPure(new { override val original = Some(f,e) } with IndexVectorConstruct(f(in),f(b))(e.m))(mtype(manifest[A]))
    //case Reflect(e@IndexVectorConstruct(in,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with IndexVectorConstruct(f(in),f(b))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenIndexVectorOps extends ScalaGenBase {
  val IR: IndexVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case v@IndexVectorRangeNew(start, end) =>
      emitValDef(sym, "new generated.scala.IndexVectorRangeImpl(" + quote(start) +  "," + quote(end) + ")")
    case v@IndexVectorObjectNew(len) =>
      emitValDef(sym, "new generated.scala.IndexVectorSeqImpl(" + quote(len) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}
