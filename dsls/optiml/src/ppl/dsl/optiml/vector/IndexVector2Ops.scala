package ppl.dsl.optiml.vector

import java.io.PrintWriter
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase}
import scala.reflect.SourceContext
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.transform.LoweringTransform
import ppl.dsl.optiml._

trait IndexVector2Ops extends Base { this: OptiML =>

  /**
   * Interface for different kinds of IndexVector2
   */  
  class IV2Interface(override val ops: IndexVec2OpsCls) extends VInterface[(Int,Int)](ops) with Interface[IndexVector2]     
  
  trait IndexVec2OpsCls extends VecOpsCls[(Int,Int)] with InterfaceOps[IndexVector2] {
    type V[X] = DenseVector[X] 
    type M[X] = DenseMatrix[X]
    type I[X] = DenseMatrix[X]
    def wrap(x: Rep[Self]): Interface[IndexVector2]    
    def vecToOps[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)
    def vecToIntf[B:Manifest](x: Rep[DenseVector[B]]): Interface[Vector[B]] = denseVecToInterface(x)
    def vecBuilder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]] = denseVectorBuilder[B]        
    def matToIntf[B:Manifest](x: Rep[DenseMatrix[B]]): Interface[Matrix[B]] = denseMatToInterface(x)
    def matBuilder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,I[B],M[B]] = denseMatrixBuilder[B]
    def mV[B:Manifest] = manifest[DenseVector[B]]
    def mM[B:Manifest] = manifest[DenseMatrix[B]]
    def mA = manifest[(Int,Int)]            
  }
  
  
  /**
   * matrix constructor syntax
   */
        
  // chained implicits from LanguageOps
  implicit def tuple2ToIndexVectorOpsWC[A <% Interface[IndexVector]](tup: (A, IndexWildcard))
      = indexvector2_new_wc(tup._1, tup._2)
//    = repIndexVector2ToIndexVectorOps(indexvector2_new(tup._1, indexvector2_wildcard()))
//  implicit def tuple2ToIndexVectorOps(tup: (IndexWildcard, Rep[IndexVector]))(implicit overloaded2 : Overloaded2)
//    = repIndexVector2ToIndexVectorOps(indexvector2_new(indexvector2_wildcard(), tup._2))
  implicit def tuple2ToIndexVectorOps[A <% Interface[IndexVector]](tup: (A, A))
      = indexvector2_new(tup._1, tup._2)
//    = repIndexVector2ToIndexVectorOps(indexvector2_new(tup._1, tup._2))

  //implicit def repIndexVector2ToIndexVectorOps(x: Rep[IndexVector2]) = new IndexVector2OpsCls(x)
  
  // class IndexVector2OpsCls(x: Rep[IndexVector2]){
  //     def apply[A:Manifest](block: Rep[Int] => Interface[Vector[A]]) = indexvector2_construct_vectors(x, block)
  //     def apply[A:Manifest](block: (Rep[Int],Rep[Int]) => Rep[A]) = indexvector2_construct(x, block)
  //     def rowInd = indexvector2_rowind(x)
  //     def colInd = indexvector2_colind(x)
  //   }

  
  case class IndexVector2Tup(rowInd: Interface[IndexVector], colInd: Interface[IndexVector]) {
    def apply[A:Manifest](block: Rep[Int] => Interface[Vector[A]]) = indexvector2_construct_vectors(rowInd, colInd, block)
    def apply[A:Manifest](block: (Rep[Int],Rep[Int]) => Rep[A]) = indexvector2_construct(rowInd, colInd, block)    
  }
  
  case class IndexVector2WC(rowInd: Interface[IndexVector], colInd: IndexWildcard) {
    def apply[A:Manifest](block: Rep[Int] => Interface[Vector[A]]) = indexvector2_construct_vectors_wildcard(rowInd, block)
  }
  
  
  // impl defs
  //def indexvector2_new(rowInd: Interface[IndexVector], colInd: Interface[IndexVector]): Rep[IndexVector2]
  def indexvector2_new(rowInd: Interface[IndexVector], colInd: Interface[IndexVector]) = new IndexVector2Tup(rowInd, colInd)
  def indexvector2_new_wc(rowInd: Interface[IndexVector], colInd: IndexWildcard) = new IndexVector2WC(rowInd, colInd)
//  def indexvector2_wildcard(): Interface[IndexVector] = wcToInterface()
//  def indexvector2_isWildcard(x: Interface[IndexVector]): Rep[Boolean]

  // class defs
  def indexvector2_construct_vectors[A:Manifest](rowInd: Interface[IndexVector], colInd: Interface[IndexVector], block: Rep[Int] => Interface[Vector[A]]): Rep[DenseMatrix[A]]
  def indexvector2_construct_vectors_wildcard[A:Manifest](rowInd: Interface[IndexVector], block: Rep[Int] => Interface[Vector[A]]): Rep[DenseMatrix[A]]
  def indexvector2_construct[A:Manifest](rowInd: Interface[IndexVector], colInd: Interface[IndexVector], block: (Rep[Int],Rep[Int]) => Rep[A]): Rep[DenseMatrix[A]]
  // def indexvector2_rowind(x: Rep[IndexVector2]): Interface[IndexVector]
  // def indexvector2_colind(x: Rep[IndexVector2]): Interface[IndexVector]
}

trait IndexVector2OpsExp extends IndexVector2Ops with EffectExp with LoweringTransform { this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  //case class IndexVector2New(rowInd: Exp[IndexVector], colInd: Exp[IndexVector]) extends Def[IndexVector2]
  // case class IndexVector2Wildcard() extends Def[IndexVector]
  // case class IndexVector2IsWildcard() extends Def[IndexVector]
  // case class IndexVector2RowInd(x: Exp[IndexVector2]) extends Def[IndexVector]
  // case class IndexVector2ColInd(x: Exp[IndexVector2]) extends Def[IndexVector]
  
  
  ////////////////////////////////
  // composite: to be transformed  
  
  // we keep the unwrapped function f around so that it cna be used directly in rewrites
  case class IndexVector2Construct[A:Manifest](rowInd: Interface[IndexVector], colInd: Interface[IndexVector], f: (Exp[Int],Exp[Int]) => Exp[A], block: Block2[Int,Int,A]) extends Def[DenseMatrix[A]]{
    val m = manifest[A]
  }
  
  ////////////////////////////////
  // implemented via delite ops

  /*
  case class IndexVector2ConstructVectors[A:Manifest](in: Exp[IndexVector], block: Rep[Int] => Rep[Vector[A]])
    extends DeliteOpMap[Int,Vector[A],Vector] {

    val alloc = reifyEffects(Vector[Vector[A]](in.length, unit(true)))
    val v = fresh[Int]
    val func = reifyEffects(block(v))
  }

  case class IndexVector2Construct[A:Manifest](x: Exp[IndexVector2], block: (Rep[Int],Rep[Int]) => Rep[A])
    extends DeliteOpMap[Int,Vector[A],Vector] {

    val in = x.rowInd
    val alloc = reifyEffects(Vector[Vector[A]](in.length, unit(true)))
    val v = fresh[Int]
    val func = reifyEffects(x.colInd map { j => block(v, j) })
  }
  */
  
  case class IndexVector2ConstructRows[A:Manifest](intf: Interface[Vector[Int]], block: Exp[Int] => Interface[Vector[A]], out: Exp[DenseMatrix[A]])
    extends DeliteOpForeach[Int] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[Int]]]
    val size = copyTransformedOrElse(_.size)(intf.length)
    def sync = i => List()
    def func = i => { out(i) = block(i) } // updateRow should be fused with function application
    
    def m = manifest[A]
  }

//  case class IndexVector2ConstructCols[A:Manifest](in: Exp[IndexVector], block: Exp[Int] => Exp[Vector[A]], out: Exp[Matrix[A]])
//    extends DeliteOpForeach[Int]

  case class IndexVector2ConstructMutable[A:Manifest](rowInd: Interface[IndexVector], colInd: Interface[IndexVector], block: (Exp[Int],Exp[Int]) => Exp[A], out: Exp[DenseMatrix[A]])
    extends DeliteOpForeach[Int] {
  
    val in = rowInd.ops.elem.asInstanceOf[Exp[Vector[Int]]]
    val size = rowInd.length
    def sync = i => List()
    def func = i => out(i) = colInd map { j => block(i,j) } // updateRow should be fused with function application
     
    def m = manifest[A]
  } 
  
  // impl defs
  //def indexvector2_new(rowInd: Exp[IndexVector], colInd: Exp[IndexVector]) = reflectPure(IndexVector2New(rowInd, colInd))
  // def indexvector2_wildcard() = IndexVector2Wildcard()
  // def indexvector2_isWildcard(x: Interface[IndexVector]): Exp[Boolean] = x.ops.elem match {
  //   case Def(IndexVector2Wildcard()) => Const(true)
  //   case Def(IndexVectorRange(_,_)) => Const(false)
  //   case Def(IndexVectorObjectFromVec(_)) => Const(false)
  //   case _ => x.IsInstanceOf[IndexVectorWC]
  // }

  // class defs
  def indexvector2_construct_vectors_wildcard[A:Manifest](in: Interface[IndexVector], block: Exp[Int] => Interface[Vector[A]]): Exp[DenseMatrix[A]] = {
    val first = block(in(unit(0))) 
    val out = DenseMatrix[A](in.length, first.length)
    out(unit(0)) = first 
    reflectWrite(out)(IndexVector2ConstructRows(in.slice(unit(1),in.length),block,out))
    out.unsafeImmutable
  }
  
  def indexvector2_construct_vectors[A:Manifest](inr: Interface[IndexVector], inc: Interface[IndexVector], block: Exp[Int] => Interface[Vector[A]]): Exp[DenseMatrix[A]] = {
    val out = DenseMatrix[A](inr.length, inc.length)
    reflectWrite(out)(IndexVector2ConstructRows(inr,block,out))
    out.unsafeImmutable    
  }
    
  // TODO: verify these are zero-based and ranges, or generalize them to work otherwise
//   def indexvector2_construct_vectors[A:Manifest](x: IndexVector2, block: Exp[Int] => Interface[Vector[A]]): Exp[Matrix[A]] = {
// /*
//   an alternative approach:
//   val in = x.rowInd
//   val data = x.flatMap { i=> block(i) }
//   matrix_reshape(in.length)
// */
//     if (/*!indexvector2_isWildcard(x.rowInd) &&*/ indexvector2_isWildcard(x.colInd)) {  //FIXME: check rowInd but make sure check is remove from code
//       val in = x.rowInd
//       //if (in.length > 0){
//         val first = block(in(0)) 
//         val out = matrix_obj_new[A](in.length, first.length)
//         out(0) = first 
//         reflectWrite(out)(IndexVector2ConstructRows(in.slice(1,in.length),block,out))
//         out.unsafeImmutable     
//       //}
//       //else matrix_obj_new[A](0,0).unsafeImmutable
//     }
//     // should we allow this? it is rather inefficient...
//     //     else if ((x.colInd.IsInstanceOf[IndexVector]) && (x.rowInd.IsInstanceOf[IndexVectorWC])) {
//     //       //Matrix(IndexVector2ConstructVectors(x.colInd, block))
//     //  val in = x.colInd
//     //  if (in.length > 0){
//     //    val out = matrix_obj_new[B](0,0)
//     //    out.updateCol(0) = first 
//     //    reflectWrite(out)(IndexVector2ConstructCols(in.slice(1,in.length),block,out)) 
//     //    out.unsafeImmutable     
//     //  }
//     //  else matrix_obj_new[B](0,0)
//     // }
//     else { //if (!indexvector2_isWildcard(x.rowInd) && !indexvector2_isWildcard(x.colInd)) {
//       val inr = x.rowInd
//       val inc = x.colInd
//       val out = matrix_obj_new[A](inr.length, inc.length)
//       reflectWrite(out)(IndexVector2ConstructRows(inr,block,out))
//       out.unsafeImmutable
//     //} else {
//     //  println(unit("optiml runtime error: illegal matrix constructor"))
//     //  exit(-1)
//     }
//   }
//  def indexvector2_construct[A:Manifest](x: IndexVector2, block: (Exp[Int],Exp[Int]) => Exp[A]): Exp[Matrix[A]] = {
  
  def indexvector2_construct[A:Manifest](rowInd: Interface[IndexVector], colInd: Interface[IndexVector], block: (Exp[Int],Exp[Int]) => Exp[A]): Exp[DenseMatrix[A]] = {
    // in order for the transformation to be successful, we have to evaluate block to expose (free) dependencies    
    reflectPure(IndexVector2Construct(rowInd,colInd,block,block))
    
    // val out = DenseMatrix[A](rowInd.length, colInd.length)
    // reflectWrite(out)(IndexVector2ConstructMutable(rowInd,colInd,block,out)) 
    // out.unsafeImmutable
  }
  
  // def indexvector2_rowind(x: Exp[IndexVector2]) = x match {
  //   case Def(IndexVector2New(rowInd, colInd)) => rowInd
  //   case _ => reflectPure(IndexVector2RowInd(x))
  // }
  // def indexvector2_colind(x: Exp[IndexVector2]) = x match {
  //   case Def(IndexVector2New(rowInd, colInd)) => colInd
  //   case _ => reflectPure(IndexVector2ColInd(x))
  // }


  //////////////
  // transforms
  
  override def onCreate[A:Manifest](s: Sym[A], d: Def[A]) = d match {
    case c@IndexVector2Construct(rowInd,colInd,f,blk) => 
      s.atPhase(deviceIndependentLowering) {
        val t = deviceIndependentLowering
        val out = DenseMatrix(t(rowInd).length, t(colInd).length)(c.m,implicitly[SourceContext])
        // make sure that we transform the symbolically evaluated blk, and not the original lambda
        reflectWrite(out)(IndexVector2ConstructMutable(t(rowInd), t(colInd), t(blk), out)(c.m))
        // don't forget - have to be explicit with manifests inside transformers
        (object_unsafe_immutable(out)(out.tp,implicitly[SourceContext])).asInstanceOf[Exp[A]]              
      }
    case _ => super.onCreate(s,d)
  }
          
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case Reflect(e@IndexVector2ConstructMutable(r,c,g,out), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with IndexVector2ConstructMutable(f(r),f(c),f(g),f(out))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@IndexVector2ConstructRows(in,g,out), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with IndexVector2ConstructRows(f(in),f(g),f(out))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait ScalaGenIndexVector2Ops extends ScalaGenBase {
  val IR: IndexVector2OpsExp
  import IR._

  //override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // case IndexVector2New(rowInd, colInd) =>
    //   emitValDef(sym, "new generated.scala.IndexVector2Impl(" + quote(rowInd) +  "," + quote(colInd) + ")")
    // case IndexVector2Wildcard() => emitValDef(sym, "generated.scala.IndexVectorWCImpl")
    // case IndexVector2RowInd(x) => emitValDef(sym, quote(x) + ".rowInd")
    // case IndexVector2ColInd(x) => emitValDef(sym, quote(x) + ".colInd")
  // case _ => super.emitNode(sym, rhs)
  // }
}
