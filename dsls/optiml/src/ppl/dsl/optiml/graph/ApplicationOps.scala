package ppl.dsl.optiml.graph

import ppl.delite.framework.DSLType
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.dsl.optiml.datastruct.scala._
import java.io.PrintWriter
import scala.virtualization.lms.common.{EffectExp, Variables}
import scala.virtualization.lms.internal.{ScalaGenBase, GenericNestedCodegen}

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Jan 23, 2011
 * Time: 5:49:57 PM
 * To change this template use File | Settings | File Templates.
 */

trait DenoiseVertexDataOps extends DSLType with Variables {
  this: OptiML =>
  object DenoiseVertexData {
    def apply(id: Rep[Int], b: Rep[Vector[Double]], p: Rep[Vector[Double]]) = denoise_vertex_data_obj_new(id, b, p)
  }

  implicit def repDenoiseVertexDataToDenoiseVertexDataOps(v: Rep[DenoiseVertexData]) = new denoiseVertexDataOpsCls(v)

  class denoiseVertexDataOpsCls(v: Rep[DenoiseVertexData]) {
    def id = denoise_vertex_data_id(v)
    def belief = denoise_vertex_data_belief(v)
    def belief_=(b: Rep[Vector[Double]]) = denoise_vertex_data_belief_update(v, b)
    def potential = denoise_vertex_data_potential(v)
  }

  // object defs
  def denoise_vertex_data_obj_new(id: Rep[Int], b: Rep[Vector[Double]], p: Rep[Vector[Double]]): Rep[DenoiseVertexData]

  // class defs
  def denoise_vertex_data_id(v: Rep[DenoiseVertexData]): Rep[Int]
  def denoise_vertex_data_belief(v: Rep[DenoiseVertexData]): Rep[Vector[Double]]
  def denoise_vertex_data_belief_update(v: Rep[DenoiseVertexData], b: Rep[Vector[Double]])
  def denoise_vertex_data_potential(v: Rep[DenoiseVertexData]): Rep[Vector[Double]]
}

trait DenoiseVertexDataOpsExp extends DenoiseVertexDataOps with EffectExp {
  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class DenoiseVertexDataObjectNew(id: Exp[Int], belief: Exp[Vector[Double]], potential: Exp[Vector[Double]])
    extends Def[DenoiseVertexData] {
    val vD = manifest[DenoiseVertexDataImpl]
  }
  case class DenoiseVertexDataId(v: Exp[DenoiseVertexData]) extends Def[Int]
  case class DenoiseVertexDataBelief(v: Exp[DenoiseVertexData]) extends Def[Vector[Double]]
  case class DenoiseVertexDataBeliefUpdate(v: Exp[DenoiseVertexData], b: Exp[Vector[Double]]) extends Def[Unit]
  case class DenoiseVertexDataPotential(v: Exp[DenoiseVertexData]) extends Def[Vector[Double]]

  /////////////////////
  // object interface
  def denoise_vertex_data_obj_new(id: Exp[Int], b: Exp[Vector[Double]], p: Exp[Vector[Double]]) = DenoiseVertexDataObjectNew(id, b, p)

  /////////////////////
  // class interface

  def denoise_vertex_data_id(v: Exp[DenoiseVertexData]) = DenoiseVertexDataId(v)
  def denoise_vertex_data_belief(v: Exp[DenoiseVertexData]) = DenoiseVertexDataBelief(v)
  def denoise_vertex_data_belief_update(v: Exp[DenoiseVertexData], b: Exp[Vector[Double]]) = DenoiseVertexDataBeliefUpdate(v, b)
  def denoise_vertex_data_potential(v: Exp[DenoiseVertexData]) = DenoiseVertexDataPotential(v)
}

trait BaseGenDenoiseVertexDataOps extends GenericNestedCodegen {
  val IR: DenoiseVertexDataOpsExp
  import IR._
}

trait ScalaGenDenoiseVertexDataOps extends BaseGenDenoiseVertexDataOps with ScalaGenBase {
  val IR: DenoiseVertexDataOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case v@DenoiseVertexDataObjectNew(id,b,p) => emitValDef(sym, "new " + remap(v.vD) + "(" + quote(id) + "," + quote(b) + "," + quote(p) + ")")
      case DenoiseVertexDataBelief(v) => emitValDef(sym, quote(v) + ".belief")
      case DenoiseVertexDataBeliefUpdate(v,b) => emitValDef(sym, quote(v) + ".belief = (" + quote(b) + ")")
      case DenoiseVertexDataPotential(v) => emitValDef(sym, quote(v) + ".potential")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait DenoiseEdgeDataOps extends DSLType with Variables {
  this: OptiML =>

  object DenoiseEdgeData {
    def apply(m: Rep[Vector[Double]], oM: Rep[Vector[Double]]) = denoise_edge_data_obj_new(m, oM)
  }

  implicit def repDenoiseEdgeDataToDenoiseEdgeDataOps(e: Rep[DenoiseEdgeData]) = new denoiseEdgeDataOpsCls(e)

  class denoiseEdgeDataOpsCls(e: Rep[DenoiseEdgeData]) {
    def message = denoise_edge_data_message(e)
    def message_=(m: Rep[Vector[Double]]) = denoise_edge_data_message_update(e,m)
    def oldMessage = denoise_edge_data_old_message(e)
    def oldMessage_=(m: Rep[Vector[Double]]) = denoise_edge_data_old_message_update(e,m)
    def cloneL = denoise_edge_data_cloneL(e)
  }

  // object defs
  def denoise_edge_data_obj_new(m: Rep[Vector[Double]], oM: Rep[Vector[Double]]): Rep[DenoiseEdgeData]

  // class defs
  def denoise_edge_data_message(e: Rep[DenoiseEdgeData]): Rep[Vector[Double]]
  def denoise_edge_data_message_update(e: Rep[DenoiseEdgeData], m: Rep[Vector[Double]])
  def denoise_edge_data_old_message(e: Rep[DenoiseEdgeData]): Rep[Vector[Double]]
  def denoise_edge_data_old_message_update(e: Rep[DenoiseEdgeData], m: Rep[Vector[Double]])
  def denoise_edge_data_cloneL(e: Rep[DenoiseEdgeData]): Rep[DenoiseEdgeData]
}

trait DenoiseEdgeDataOpsExp extends DenoiseEdgeDataOps with EffectExp {
  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class DenoiseEdgeDataObjectNew(m: Exp[Vector[Double]], oM: Exp[Vector[Double]])
    extends Def[DenoiseEdgeData] {
    val eD = manifest[DenoiseEdgeDataImpl]
  }
  case class DenoiseEdgeDataMessage(e: Exp[DenoiseEdgeData]) extends Def[Vector[Double]]
  case class DenoiseEdgeDataMessageUpdate(e: Exp[DenoiseEdgeData], m: Exp[Vector[Double]]) extends Def[Unit]
  case class DenoiseEdgeDataOldMessage(e: Exp[DenoiseEdgeData]) extends Def[Vector[Double]]
  case class DenoiseEdgeDataOldMessageUpdate(e: Exp[DenoiseEdgeData], m: Exp[Vector[Double]]) extends Def[Unit]
  case class DenoiseEdgeDataCloneL(e: Exp[DenoiseEdgeData]) extends Def[DenoiseEdgeData]

  /////////////////////
  // object interface

  def denoise_edge_data_obj_new(m: Exp[Vector[Double]], oM: Exp[Vector[Double]]) = DenoiseEdgeDataObjectNew(m, oM)

  /////////////////////
  // class interface

  def denoise_edge_data_message(e: Exp[DenoiseEdgeData]) = DenoiseEdgeDataMessage(e)
  def denoise_edge_data_message_update(e: Exp[DenoiseEdgeData], m: Exp[Vector[Double]]) = DenoiseEdgeDataMessageUpdate(e, m)
  def denoise_edge_data_old_message(e: Exp[DenoiseEdgeData]) = DenoiseEdgeDataOldMessage(e)
  def denoise_edge_data_old_message_update(e: Exp[DenoiseEdgeData], m: Exp[Vector[Double]]) = DenoiseEdgeDataOldMessageUpdate(e, m)
  def denoise_edge_data_cloneL(e: Exp[DenoiseEdgeData]) = DenoiseEdgeDataCloneL(e) 
}

trait BaseGenDenoiseEdgeDataOps extends GenericNestedCodegen {
  val IR: DenoiseEdgeDataOpsExp
}

trait ScalaGenDenoiseEdgeDataOps extends BaseGenDenoiseEdgeDataOps with ScalaGenBase {
  val IR: DenoiseEdgeDataOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case e@DenoiseEdgeDataObjectNew(m,oM) => emitValDef(sym, "new " + remap(e.eD) + "(" + quote(m) + "," + quote(oM) + ")")
      case DenoiseEdgeDataMessage(e) => emitValDef(sym, quote(e) + ".message")
      case DenoiseEdgeDataMessageUpdate(e,m) => emitValDef(sym, quote(e) + ".message = (" + quote(m) + ")")
      case DenoiseEdgeDataOldMessage(e) => emitValDef(sym, quote(e) + ".oldMessage")
      case DenoiseEdgeDataOldMessageUpdate(e,m) => emitValDef(sym, quote(e) + ".oldMessage = (" + quote(m) + ")")
      case DenoiseEdgeDataCloneL(e) => emitValDef(sym, quote(e) + ".cloneL")
      case _ => super.emitNode(sym, rhs)
    }
  }
}