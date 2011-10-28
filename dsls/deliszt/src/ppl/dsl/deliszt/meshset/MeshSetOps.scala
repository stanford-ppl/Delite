package ppl.dsl.deliszt.meshset

import java.io.PrintWriter
import ppl.dsl.deliszt._

import ppl.delite.framework.DSLType
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

trait MeshSetOps extends DSLType with Variables {
  this: DeLiszt =>

  implicit def repMeshSetToMeshSetOps[MO <: MeshObj:Manifest](x: Rep[MeshSet[MO]]) = new meshSetOpsCls[MO](x)
  implicit def varToMeshSetOps[MO <: MeshObj:Manifest](x: Var[MeshSet[MO]]) = new meshSetOpsCls[MO](readVar(x))

  /**
   * This class defines the public interface for the Field[T] class.
   */
  class meshSetOpsCls[MO<:MeshObj:Manifest](x: Rep[MeshSet[MO]]) {
    def foreach(block: Rep[MO] => Rep[Unit]) = meshset_foreach(x, block)
  }

  def meshset_foreach[MO<:MeshObj:Manifest](x: Rep[MeshSet[MO]], block: Rep[MO] => Rep[Unit]) : Rep[Unit]
}

trait MeshSetOpsExp extends MeshSetOps with VariablesExp with BaseFatExp {
  this: DeLisztExp =>

  ////////////////////////////////
  // implemented via delite ops

  case class MeshSetForeach[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], func: Exp[MO] => Exp[Unit])
    extends DeliteOpForeach[MO] {

    def sync = n => List()
    val in = copyTransformedOrElse(_.in)(x)
    val size = copyTransformedOrElse(_.size)(x.size)
  }


  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case Reflect(e@MeshSetForeach(a,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MeshSetForeach(f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
  
  /////////////////////
  // object interface

  /////////////////////
  // class interface
  
  def meshset_foreach[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], block: Exp[MO] => Exp[Unit]) = {
    val t = MeshSetForeach(x,block)
    reflectEffect(t, summarizeEffects(t.body.asInstanceOf[DeliteForeachElem[MO]].func).star)
  }
}

trait MeshSetOpsExpOpt extends MeshSetOpsExp {
  this: DeLisztExp =>
}

trait ScalaGenMeshSetOps extends ScalaGenBase {
  val IR: MeshSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      // these are the ops that call through to the underlying real data structure
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenMeshSetOps extends CudaGenBase with CudaGenDataStruct {
  val IR: MeshSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMeshSetOps extends CGenBase {
  val IR: MeshSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
