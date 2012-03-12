package ppl.dsl.optiml.vector

import ppl.dsl.optiml.{DenseVector,Vector, IndexVector, IndexVectorRange, IndexVectorDense, Matrix, DenseMatrix}
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase}
import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext
import java.io.PrintWriter

trait IndexVectorOps extends Base with OverloadHack { this: OptiML =>
  
  object IndexVector {
    def apply(len: Rep[Int])(implicit ctx: SourceContext) = indexvector_obj_new(len)
    def apply(xs: Interface[Vector[Int]])(implicit o: Overloaded1, ctx: SourceContext) = indexvector_obj_fromvec(xs)
  }

  trait IndexVecOpsCls extends VecOpsCls[Int] with InterfaceOps[IndexVector] {
    //implicit def toOps(x: Rep[VA]): IndexVecOpsCls
    //implicit def toIntf(x: Rep[VA]): Interface[IndexVector]    
    type V[X] = DenseVector[X] // conversion operations on IndexVectors will return a DenseVector
    type M[X] = DenseMatrix[X]
    def toOps[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)
    def toIntf[B:Manifest](x: Rep[DenseVector[B]]): Interface[Vector[B]] = denseVecToInterface(x)
    def matToIntf[B:Manifest](x: Rep[DenseMatrix[B]]): Interface[Matrix[B]] = denseMatToInterface(x)
    def wrap(x: Rep[Self]): Interface[IndexVector]
    def builder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]] = denseVectorBuilder[B]    
    def matBuilder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,M[B]] = denseMatrixBuilder[B]
    def mV[B:Manifest] = manifest[DenseVector[B]]
    def mM[B:Manifest] = manifest[DenseMatrix[B]]
    def mA = manifest[Int]
    
    // VectorOps generic - math on an IndexVector turns it into a DenseVector (is this the right thing to do?)
    type VPLUSR = DenseVector[Int]
    val mVPLUSR = manifest[VPLUSR]
    def vplusBuilder(implicit ctx: SourceContext) = denseVectorBuilder[Int]
    def vplusToIntf(x: Rep[VPLUSR]) = denseVecToInterface(x)
    
    type VMINUSR = DenseVector[Int]
    val mVMINUSR = manifest[VMINUSR]
    def vminusBuilder(implicit ctx: SourceContext) = denseVectorBuilder[Int]
    def vminusToIntf(x: Rep[VMINUSR]) = denseVecToInterface(x)    
    
    type VTIMESR = DenseVector[Int]
    val mVTIMESR = manifest[VTIMESR]
    def vtimesBuilder(implicit ctx: SourceContext) = denseVectorBuilder[Int]
    def vtimesToIntf(x: Rep[VTIMESR]) = denseVecToInterface(x)            
        
    def apply[A:Manifest](block: Rep[Int] => Rep[A])(implicit ctx: SourceContext): Rep[V[A]] = indexvector_construct(wrap(x), block)    
    def *(y: Rep[Matrix[Int]])(implicit a: Arith[Int], o: Overloaded2, ctx: SourceContext) = throw new UnsupportedOperationException("tbd")
    def flatMap[B:Manifest](f: Rep[Int] => Rep[DenseVector[B]])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("tbd")
    def partition(pred: Rep[Int] => Rep[Boolean])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("tbd")
    def groupBy[K:Manifest](pred: Rep[Int] => Rep[K])(implicit ctx: SourceContext) = throw new UnsupportedOperationException("tbd")    
  }
    
  class IVInterface(override val ops: IndexVecOpsCls) extends VInterface[Int](ops) with Interface[IndexVector]  
  implicit def interfaceToIndexVecOps(intf: Interface[IndexVector]): InterfaceIndexVecOpsCls = new InterfaceIndexVecOpsCls(intf.asInstanceOf[IVInterface])
  
  class InterfaceIndexVecOpsCls(override val intf: IVInterface) extends InterfaceVecOpsCls[Int](intf) {
    def apply[A:Manifest](block: Rep[Int] => Rep[A])(implicit ctx: SourceContext) = intf.ops.apply(block)    
    
    // this is unfortunately required to get the static return type right... TODO: any solution?
    override def slice(start: Rep[Int], end: Rep[Int])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.slice(start,end))
  }
  
  // impl defs
  def indexvector_range(start: Rep[Int], end: Rep[Int])(implicit ctx: SourceContext): Rep[IndexVectorRange]
  def indexvector_obj_new(len: Rep[Int])(implicit ctx: SourceContext): Rep[IndexVectorDense]
  def indexvector_obj_fromvec(xs: Interface[Vector[Int]])(implicit ctx: SourceContext): Rep[IndexVectorDense]

  // class defs
  def indexvector_construct[A:Manifest](x: Interface[IndexVector], block: Rep[Int] => Rep[A])(implicit ctx: SourceContext): Rep[DenseVector[A]]
}

trait IndexVectorOpsExp extends IndexVectorOps with EffectExp { this: OptiMLExp with IndexVectorImplOps =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class IndexVectorRangeNew(start: Exp[Int], end: Exp[Int]) extends Def[IndexVectorRange]
  case class IndexVectorDenseNew(len: Exp[Int]) extends Def[IndexVectorDense]

  ////////////////////////////////
  // implemented via delite ops

  case class IndexVectorObjectFromVec(xs: Interface[Vector[Int]]) extends DeliteOpSingleTask[IndexVectorDense](reifyEffectsHere(index_vector_obj_fromvec_impl(xs)))

  // Note: Construction from a discrete index vector set will curently return a contiguous (non-sparse) vector.
  // Is this what we want?
  case class IndexVectorConstruct[B:Manifest](intf: Interface[IndexVector], func: Exp[Int] => Exp[B])
    extends DeliteOpMap[Int,B,DenseVector[B]] {

    val in = intf.ops.elem.asInstanceOf[Exp[Vector[Int]]]
    val size = copyTransformedOrElse(_.size)(intf.length)    
    def alloc = DenseVector[B](intf.length, intf.isRow)
    def m = manifest[B]
  }
  
  // impl defs
  def indexvector_range(start: Exp[Int], end: Exp[Int])(implicit ctx: SourceContext) = reflectPure(IndexVectorRangeNew(start, end))
  def indexvector_obj_new(len: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(IndexVectorDenseNew(len))
  def indexvector_obj_fromvec(xs: Interface[Vector[Int]])(implicit ctx: SourceContext) = reflectPure(IndexVectorObjectFromVec(xs))

  // class defs
  def indexvector_construct[A:Manifest](x: Interface[IndexVector], block: Exp[Int] => Exp[A])(implicit ctx: SourceContext): Exp[DenseVector[A]] = {
    reflectPure(IndexVectorConstruct(x, block))
    // HACK -- better scheduling performance in our apps, forces some expensive dependencies to be hoisted
    // TR TODO: use effect summary of func
    //reflectEffect(IndexVectorConstruct(x, block))
  }

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@IndexVectorConstruct(in,b) => reflectPure(new { override val original = Some(f,e) } with IndexVectorConstruct(f(in),f(b))(e.m))(mtype(manifest[A]), implicitly[SourceContext])
    case Reflect(e@IndexVectorConstruct(in,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with IndexVectorConstruct(f(in),f(b))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenIndexVectorOps extends ScalaGenBase {
  val IR: IndexVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // should not be required -- pattern matches in IndexVectorRangeOps.scala should always take precedence
    // case v@IndexVectorRangeNew(start, end) =>
    //   emitValDef(sym, "new generated.scala.IndexVectorRange(" + quote(start) +  "," + quote(end) + ")")
    case v@IndexVectorDenseNew(len) =>
      emitValDef(sym, "new generated.scala.IndexVectorDense(" + quote(len) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}
