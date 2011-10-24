package ppl.dsl.optiml.vector

import java.io.PrintWriter
import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optiml._

trait LabelsOps extends Base with OverloadHack {
  this: OptiML =>

  object Labels {
    def apply[A:Manifest](length: Rep[Int]) = labels_obj_fromVec(Vector[A](length, unit(false)).unsafeImmutable)
    def apply[A](xs: Rep[Vector[A]])(implicit mA: Manifest[A], o: Overloaded1) = labels_obj_fromVec(xs)
  }

  // object defs
  def labels_obj_fromVec[A:Manifest](xs: Rep[Vector[A]]): Rep[Labels[A]]

  implicit def repLabelsToLabelsOps[A:Manifest](x: Rep[Labels[A]]) = new LabelsOpsCls(x)

  class LabelsOpsCls[A:Manifest](x: Rep[Labels[A]]){
    def mmap(f: Rep[A] => Rep[A]) = labels_mmap(x,f)
  }
  def labels_mmap[A:Manifest](x: Rep[Labels[A]], f: Rep[A] => Rep[A]): Rep[Labels[A]]
}

trait LabelsOpsExp extends LabelsOps with BaseExp { this: OptiMLExp =>

  // implemented via method on real data structure
  case class LabelsObjectFromVec[A:Manifest](xs: Exp[Vector[A]]) extends Def[Labels[A]] {
    val mA = manifest[A]
  }
  case class LabelsMutableMap[A:Manifest](in: Exp[Labels[A]], func: Exp[A] => Exp[A])
    extends DeliteOpMap[A,A,Labels[A]] {

    def alloc = in
    val size = in.length    
  }  

  def labels_obj_fromVec[A:Manifest](xs: Exp[Vector[A]]) = reflectEffect(LabelsObjectFromVec(xs))

  def labels_mmap[A:Manifest](x: Exp[Labels[A]], f: Exp[A] => Exp[A]) = {
    reflectWrite(x)(LabelsMutableMap(x, f)) //reflectReadWrite(x)
  }
}

trait ScalaGenLabelsOps extends ScalaGenBase {
  val IR: LabelsOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    // TODO aks: this should create a Struct with the same fields as a DenseVector (Data,IsRow) is the new data structure implementation
    //           DenseVector operations will read the struct to extract the same fields, leaving no dependencies on the Labels struct
    //           NOTE: any leftover dependencies on the Labels struct should be erased / remapped to be a dependency on a Map  
    //case l@LabelsObjectFromVec(xs) => emitValDef(sym, "new generated.scala.LabelsVectorImpl(" + quote(xs) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
