package ppl.dsl.optiml.vector

import ppl.dsl.optiml.{Vector, DenseVector, RangeVector, IndexVector, IndexVectorDense}
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext
import java.io.PrintWriter

trait IndexVectorDenseOps extends Base with OverloadHack { this: OptiML =>

  implicit def repToIndexVecDenseOps(x: Rep[IndexVectorDense]) = new IndexVecDenseOpsCls(x)
  implicit def varToIndexVecDenseOps(x: Var[IndexVectorDense]) = new IndexVecDenseOpsCls(readVar(x))
  implicit def indexVecDenseToInterface(lhs: Rep[IndexVectorDense]) = new IVInterface(new IndexVecDenseOpsCls(lhs))
  
  implicit def indexVecDenseBuilder(implicit ctx: SourceContext) = new VectorBuilder[Int,IndexVectorDense] {
    def alloc(length: Rep[Int], isRow: Rep[Boolean]) = IndexVector(length, isRow)
    def toIntf(x: Rep[IndexVectorDense]): Interface[IndexVector] = indexVecDenseToInterface(x)
  }  
  
  // copying the interface from DenseVectorOps instead of inheriting so that we preserve the IndexVector type
  // in generic operations (i.e. the type alias declarations).
  class IndexVecDenseOpsCls(val elem: Rep[IndexVectorDense]) extends IndexVecOpsCls {    
    type Self = IndexVectorDense    
    type VA = Self    
    def wrap(x: Rep[IndexVectorDense]) = indexVecDenseToInterface(x)
    def vaToOps(x: Rep[VA]) = repToIndexVecDenseOps(x)
    def vaToIntf(x: Rep[VA]) = indexVecDenseToInterface(x)
    def vaBuilder(implicit ctx: SourceContext) = indexVecDenseBuilder
    def mVA = manifest[VA]    
          
    // VectorOps
    def length(implicit ctx: SourceContext) = indexvectordense_length(elem)
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = indexvectordense_apply(elem,n)
    
    def isRow(implicit ctx: SourceContext) = densevector_isrow(elem)
    def sort(implicit o: Ordering[Int], ctx: SourceContext) = indexvectordense_new_unsafe(densevector_sort(elem))
    def t(implicit ctx: SourceContext) = indexvectordense_new_unsafe(densevector_trans(elem))
    def mt()(implicit ctx: SourceContext) = {densevector_mutable_trans(elem); elem}
    def update(n: Rep[Int], y: Rep[Int])(implicit ctx: SourceContext): Rep[Unit] = densevector_update(elem,n,y)
    def copyFrom(pos: Rep[Int], y: Rep[IndexVectorDense])(implicit ctx: SourceContext) = densevector_copyfrom(elem,pos,y)
    def insert(pos: Rep[Int], y: Rep[Int])(implicit ctx: SourceContext) = densevector_insert(elem,pos,y)
    def insertAll(pos: Rep[Int], y: Rep[IndexVectorDense])(implicit ctx: SourceContext) = densevector_insertall(elem,pos,y)
    def removeAll(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = densevector_removeall(elem,pos,len)
    def trim()(implicit ctx: SourceContext) = densevector_trim(elem)
    def clear()(implicit ctx: SourceContext) = densevector_clear(elem)
  }   
  
  def indexvectordense_length(x: Rep[IndexVectorDense])(implicit ctx: SourceContext): Rep[Int]
  def indexvectordense_apply(x: Rep[IndexVectorDense], n: Rep[Int])(implicit ctx: SourceContext): Rep[Int]
  
  // internal
  def indexvectordense_new_unsafe(x: Rep[DenseVector[Int]])(implicit ctx: SourceContext): Rep[IndexVectorDense]  
}

trait IndexVectorDenseOpsExp extends IndexVectorDenseOps with DeliteCollectionOpsExp with VariablesExp with BaseFatExp { this: OptiMLExp => 
  case class IndexVectorDenseLength(x: Exp[IndexVectorDense]) extends Def[Int]
  case class IndexVectorDenseApply(x: Exp[IndexVectorDense], n: Exp[Int]) extends Def[Int]
  //case class IndexVectorDenseNewUnsafe(x: Exp[DenseVector[Int]]) extends Def[IndexVectorDense]
    
  def indexvectordense_length(x: Exp[IndexVectorDense])(implicit ctx: SourceContext) = reflectPure(IndexVectorDenseLength(x))
  def indexvectordense_apply(x: Exp[IndexVectorDense], n: Exp[Int])(implicit ctx: SourceContext) = reflectPure(IndexVectorDenseApply(x,n))  
  def indexvectordense_new_unsafe(x: Exp[DenseVector[Int]])(implicit ctx: SourceContext) = {
    val a = indexvector_obj_new(unit(0), unit(true))
    densevector_set_raw_data(a,densevector_raw_data(x))
    densevector_set_length(a,densevector_length(x))
    densevector_set_isrow(a,densevector_isrow(x))
    a.unsafeImmutable    
    //reflectPure(IndexVectorDenseNewUnsafe(x))
  }
    
  /////////////////////
  // delite collection
  
  def isIndexDense[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.tp.erasure == classOf[IndexVectorDense]
  def asIndexDense[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[IndexVectorDense]]
    
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isIndexDense(x)) asIndexDense(x).length
    else super.dc_size(x)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isIndexDense(x)) (asIndexDense(x).apply(n)).asInstanceOf[Exp[A]]
    else super.dc_apply(x,n)    
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isIndexDense(x)) asIndexDense(x).update(n,y.asInstanceOf[Exp[Int]])
    else super.dc_update(x,n,y)        
  }  
  
  override def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext): Exp[CA] = {
    if (isIndexDense(x)) {
      val out = IndexVector(size, asIndexDense(x).isRow)
      out.asInstanceOf[Exp[CA]]
    }
    else super.dc_alloc[A,CA](x,size)
  }
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@IndexVectorDenseLength(x) => reflectPure(IndexVectorDenseLength(f(x)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@IndexVectorDenseApply(x,n) => reflectPure(IndexVectorDenseApply(f(x),f(n)))(mtype(manifest[A]),implicitly[SourceContext])
    
    // delite ops
    case Reflect(e@IndexVectorDenseLength(x), u, es) => reflectMirrored(Reflect(IndexVectorDenseLength(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@IndexVectorDenseApply(x,n), u, es) => reflectMirrored(Reflect(IndexVectorDenseApply(f(x),f(n)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
  
}

trait IndexVectorDenseOpsExpOpt extends IndexVectorDenseOpsExp { this: OptiMLExp => 
  
  override def indexvectordense_length(x: Rep[IndexVectorDense])(implicit ctx: SourceContext) = x match {
    case Def(IndexVectorDenseNew(l,r)) => l
    case Def(v@Reflect(IndexVectorDenseNew(l,r), u, es)) if context.contains(v) => l
    case Def(IndexVectorObjectFromVec(xs)) => xs.length
    case Def(v@Reflect(IndexVectorObjectFromVec(xs), u, es)) if context.contains(v) => xs.length
    case Def(VectorSlice(a, start, end)) => end - start
    case _ => super.indexvectordense_length(x) // densevector_length(x) // should work
  }


  override def indexvectordense_apply(x: Rep[IndexVectorDense], n: Rep[Int])(implicit ctx: SourceContext) = x match {
    case Def(IndexVectorObjectFromVec(xs)) => xs(n)
    case Def(v@Reflect(IndexVectorObjectFromVec(xs), u, es)) if context.contains(v) => xs(n)    
    case _ => super.indexvectordense_apply(x,n)
  }  
}


trait ScalaGenIndexVectorDenseOps extends ScalaGenFat {
  val IR: IndexVectorDenseOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IndexVectorDenseLength(x) => emitValDef(sym, quote(x) + "._length")
    case IndexVectorDenseApply(x,n) => emitValDef(sym, quote(x) + "._data(" + quote(n) + ")")
    // case IndexVectorDenseNewUnsafe(x) => 
    //   stream.println("val " + quote(sym) + " = {")
    //   stream.println("val res = new generated.scala.IndexVectorDense(0)")
    //   stream.println("res._data = " + quote(x) + "._data")
    //   stream.println("res._length = " + quote(x) + "._length")
    //   stream.println("res._isRow = " + quote(x) + "._isRow")
    //   stream.println("res")
    //   stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}
  
