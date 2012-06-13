package ppl.dsl.optila.vector

import java.io.PrintWriter
import scala.reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base, ScalaGenBase, ScalaGenFat, CudaGenBase, CudaGenFat}
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{GenericFatCodegen,GenerationFailedException}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.dsl.optila._

/**
 * Implements SparseVectorView with a SparseMatrixCSR source.
 */
trait SparseVectorViewCSROpsExp extends SparseVectorViewCompilerOps with DeliteCollectionOpsExp { this: OptiLAExp =>
  case class SparseVectorViewNew[A:Manifest](x: Exp[SparseMatrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean]) extends DefWithManifest[A,SparseVectorView[A]]
  
  def sparse_vectorview_obj_new[A:Manifest](x: Exp[SparseMatrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean]) = reflectPure(SparseVectorViewNew(x,start,stride,length,isRow))  
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@SparseVectorViewNew(x,s,str,l,r) => reflectPure(SparseVectorViewNew(f(x),f(s),f(str),f(l),f(r))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@SparseVectorViewNew(x,s,str,l,r), u, es) => reflectMirrored(Reflect(SparseVectorViewNew(f(x),f(s),f(str),f(l),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))        
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??  
}


trait BaseGenSparseVectorViewCSROps extends GenericFatCodegen {
  val IR: SparseVectorViewCSROpsExp
  import IR._
  
}

trait ScalaGenSparseVectorViewCSROps extends BaseGenSparseVectorViewCSROps with ScalaGenFat {
  val IR: SparseVectorViewCSROpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case v@SparseVectorViewNew(x,start,stride,length,isRow) => emitValDef(sym, "new " + remap("generated.scala.SparseVectorViewCSR[" + remap(v.mA) + "]") + "(" + quote(x) + "," + quote(start) + "," + quote(stride) + "," + quote(length) + "," + quote(isRow) + ")")
    case _ => super.emitNode(sym, rhs)
  }
  
  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SparseVectorView" => remap("generated.scala.SparseVectorViewCSR[" + remap(m.typeArguments(0)) + "]")
    case _ => super.remap(m)
  }  
  
}  
