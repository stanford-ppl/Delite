package ppl.dsl.optiml.vector

import ppl.dsl.optiml.{DenseVector,Vector, IndexVector}
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase}
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter

trait IndexVectorOps extends DSLType with Base with OverloadHack { this: OptiML =>

  implicit def repToIndexVecOps(x: Rep[IndexVector]) = new IndexVecOpsCls(x)
  implicit def varToIndexVecOps(x: Var[IndexVector]) = new IndexVecOpsCls(readVar(x))
  implicit def indexToInterface(lhs: Rep[IndexVector]) = VInterface[Int](new IndexVecOpsCls(lhs))

  def indexVectorBuilder = new VectorBuilder[Int,IndexVector] {
    def alloc(length: Rep[Int], isRow: Rep[Boolean]) = IndexVector(length, isRow)
    def toIntf(x: Rep[IndexVector]): Interface[Vector[Int]] = indexToInterface(x)
  }  
  
  object IndexVector {
    def apply(len: Rep[Int]) = indexvector_obj_new(len)
    def apply(xs: Interface[Vector[Int]])(implicit o: Overloaded1) = indexvector_obj_fromvec(xs)
  }

  class IndexVecOpsCls(val elem: Rep[IndexVector]) extends OptiMLVecOpsCls[Int] {
    type VA = IndexVector
    def toOps(x: Rep[IndexVector]) = repToIndexVecOps(x)
    def toIntf(x: Rep[IndexVector]): Interface[Vector[Int]] = indexToInterface(x)
    def builder: VectorBuilder[Int,IndexVector] = indexVectorBuilder
    def mVA = manifest[IndexVector]
  
    type V[X] = DenseVector[X] // conversion operations on IndexVectors will return a DenseVector
    def toOpsB[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)
    def toIntfB[B:Manifest](x: Rep[DenseVector[B]]): Interface[Vector[B]] = denseToInterface(x)
    def builderB[B:Manifest]: VectorBuilder[B,V[B]] = denseVectorBuilder[B]    
    def mVB[B:Manifest] = manifest[DenseVector[B]] 
        
    //def apply(index: Rep[Int]) = vector_apply(x,index)
    def apply[A:Manifest](block: Rep[Int] => Rep[A]) = indexvector_construct(x, block)
  }

  // impl defs
  def indexvector_range(start: Rep[Int], end: Rep[Int]): Rep[IndexVector]
  def indexvector_obj_new(len: Rep[Int]): Rep[IndexVector]
  def indexvector_obj_fromvec(xs: Interface[Vector[Int]]): Rep[IndexVector]

  // class defs
  def indexvector_construct[A:Manifest](x: Rep[IndexVector], block: Rep[Int] => Rep[A]): Rep[DenseVector[A]]
}

trait IndexVectorOpsExp extends IndexVectorOps with EffectExp { this: OptiMLExp with IndexVectorImplOps =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class IndexVectorRange(start: Exp[Int], end: Exp[Int]) extends Def[IndexVector]
  case class IndexVectorObjectNew(len: Exp[Int]) extends Def[IndexVector]

  ////////////////////////////////
  // implemented via delite ops

  case class IndexVectorObjectFromVec(xs: Interface[Vector[Int]]) extends DeliteOpSingleTask[IndexVector](reifyEffectsHere(index_vector_obj_fromvec_impl(xs)))

  // Note: Construction from a discrete index vector set will curently return a contiguous (non-sparse) vector.
  // Is this what we want?
  case class IndexVectorConstruct[B:Manifest](in: Exp[IndexVector], func: Exp[Int] => Exp[B])
    extends DeliteOpMap[Int,B,DenseVector[B]] {

    val size = copyTransformedOrElse(_.size)(in.length)
    
    def alloc = DenseVector[B](in.length, in.isRow)

    def m = manifest[B]
  }
  
  // impl defs
  def indexvector_range(start: Exp[Int], end: Exp[Int]) = reflectPure(IndexVectorRange(start, end))
  def indexvector_obj_new(len: Exp[Int]) = reflectMutable(IndexVectorObjectNew(len))
  def indexvector_obj_fromvec(xs: Interface[Vector[Int]]) = reflectPure(IndexVectorObjectFromVec(xs))

  // class defs
  def indexvector_construct[A:Manifest](x: Exp[IndexVector], block: Exp[Int] => Exp[A]): Exp[Vector[A]] = {
    reflectPure(IndexVectorConstruct(x, block))
    // HACK -- better scheduling performance in our apps, forces some expensive dependencies to be hoisted
    // TR TODO: use effect summary of func
    //reflectEffect(IndexVectorConstruct(x, block))
  }

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case e@IndexVectorConstruct(in,b) => reflectPure(new { override val original = Some(f,e) } with IndexVectorConstruct(f(in),f(b))(e.m))(mtype(manifest[A]))
    case Reflect(e@IndexVectorConstruct(in,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with IndexVectorConstruct(f(in),f(b))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenIndexVectorOps extends ScalaGenBase {
  val IR: IndexVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case v@IndexVectorRange(start, end) =>
      emitValDef(sym, "new generated.scala.IndexVectorRangeImpl(" + quote(start) +  "," + quote(end) + ")")
    case v@IndexVectorObjectNew(len) =>
      emitValDef(sym, "new generated.scala.IndexVectorSeqImpl(" + quote(len) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}
