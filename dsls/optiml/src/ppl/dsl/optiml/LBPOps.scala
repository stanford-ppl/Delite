package ppl.dsl.optiml

import java.io.{PrintWriter}
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._

/**
 * This file should be auto-generated!
 */

trait DenoiseVertexData extends MessageData
trait DenoiseEdgeData extends MessageData

trait LBPOps extends DenoiseVertexDataOps with DenoiseEdgeDataOps
trait LBPOpsExp extends DenoiseVertexDataOpsExp with DenoiseEdgeDataOpsExp
trait ScalaGenLBPOps extends ScalaGenDenoiseVertexDataOps with ScalaGenDenoiseEdgeDataOps

trait DenoiseVertexDataOps extends Variables {
  object DenoiseVertexData {
    def apply(id: Rep[Int], b: Rep[DenseVector[Double]], p: Rep[DenseVector[Double]]) = denoise_vertex_data_obj_new(id, b, p)
  }

  implicit def repDenoiseVertexDataToDenoiseVertexDataOps(v: Rep[DenoiseVertexData]) = new denoiseVertexDataOpsCls(v)

  class denoiseVertexDataOpsCls(v: Rep[DenoiseVertexData]) {
    def id = denoise_vertex_data_id(v)
    def belief = denoise_vertex_data_belief(v)
    def setBelief(b: Rep[DenseVector[Double]]) = denoise_vertex_data_belief_update(v, b)
    def potential = denoise_vertex_data_potential(v)
  }

  // object defs
  def denoise_vertex_data_obj_new(id: Rep[Int], b: Rep[DenseVector[Double]], p: Rep[DenseVector[Double]]): Rep[DenoiseVertexData]

  // class defs
  def denoise_vertex_data_id(v: Rep[DenoiseVertexData]): Rep[Int]
  def denoise_vertex_data_belief(v: Rep[DenoiseVertexData]): Rep[DenseVector[Double]]
  def denoise_vertex_data_belief_update(v: Rep[DenoiseVertexData], b: Rep[DenseVector[Double]])
  def denoise_vertex_data_potential(v: Rep[DenoiseVertexData]): Rep[DenseVector[Double]]
}

trait DenoiseVertexDataOpsExp extends DenoiseVertexDataOps with VariablesExp with BaseFatExp {
  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  
  case class DenoiseVertexDataObjectNew(id: Exp[Int], belief: Exp[DenseVector[Double]], potential: Exp[DenseVector[Double]])
    extends Def[DenoiseVertexData] {
    //val vD = manifest[DenoiseVertexDataImpl]
  }
  case class DenoiseVertexDataId(v: Exp[DenoiseVertexData]) extends Def[Int]
  case class DenoiseVertexDataBelief(v: Exp[DenoiseVertexData]) extends Def[DenseVector[Double]]
  case class DenoiseVertexDataBeliefUpdate(v: Exp[DenoiseVertexData], b: Exp[DenseVector[Double]]) extends Def[Unit]
  case class DenoiseVertexDataPotential(v: Exp[DenoiseVertexData]) extends Def[DenseVector[Double]]

  /////////////////////
  // object interface
  def denoise_vertex_data_obj_new(id: Exp[Int], b: Exp[DenseVector[Double]], p: Exp[DenseVector[Double]]) = reflectMutable(DenoiseVertexDataObjectNew(id, b, p))

  /////////////////////
  // class interface

  def denoise_vertex_data_id(v: Exp[DenoiseVertexData]) = toAtom(DenoiseVertexDataId(v))
  def denoise_vertex_data_belief(v: Exp[DenoiseVertexData]) = reflectMutable(DenoiseVertexDataBelief(v))
  def denoise_vertex_data_belief_update(v: Exp[DenoiseVertexData], b: Exp[DenseVector[Double]]) = reflectWrite(v)(DenoiseVertexDataBeliefUpdate(v, b))
  def denoise_vertex_data_potential(v: Exp[DenoiseVertexData]) = toAtom(DenoiseVertexDataPotential(v))
}

trait ScalaGenDenoiseVertexDataOps extends ScalaGenBase {
  val IR: LBPOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case v@DenoiseVertexDataObjectNew(id,b,p) => emitValDef(sym, "new generated.scala.DenoiseVertexDataImpl(" + quote(id) + "," + quote(b) + "," + quote(p) + ")")
      case DenoiseVertexDataId(v) => emitValDef(sym, quote(v) + ".id")
      case DenoiseVertexDataBelief(v) => emitValDef(sym, quote(v) + ".belief")
      case DenoiseVertexDataBeliefUpdate(v,b) => emitValDef(sym, quote(v) + ".setBelief(" + quote(b) + ")")
      case DenoiseVertexDataPotential(v) => emitValDef(sym, quote(v) + ".potential")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait DenoiseEdgeDataOps extends Variables {
  object DenoiseEdgeData {
    def apply(m: Rep[DenseVector[Double]], oM: Rep[DenseVector[Double]]) = denoise_edge_data_obj_new(m, oM)
  }

  implicit def repDenoiseEdgeDataToDenoiseEdgeDataOps(e: Rep[DenoiseEdgeData]) = new denoiseEdgeDataOpsCls(e)

  class denoiseEdgeDataOpsCls(e: Rep[DenoiseEdgeData]) {
    def message = denoise_edge_data_message(e)
    def setMessage(m: Rep[DenseVector[Double]]) = denoise_edge_data_message_update(e,m)
    def oldMessage = denoise_edge_data_old_message(e)
    def setOldMessage(m: Rep[DenseVector[Double]]) = denoise_edge_data_old_message_update(e,m)
    def cloneL = denoise_edge_data_cloneL(e)
  }

  // object defs
  def denoise_edge_data_obj_new(m: Rep[DenseVector[Double]], oM: Rep[DenseVector[Double]]): Rep[DenoiseEdgeData]

  // class defs
  def denoise_edge_data_message(e: Rep[DenoiseEdgeData]): Rep[DenseVector[Double]]
  def denoise_edge_data_message_update(e: Rep[DenoiseEdgeData], m: Rep[DenseVector[Double]])
  def denoise_edge_data_old_message(e: Rep[DenoiseEdgeData]): Rep[DenseVector[Double]]
  def denoise_edge_data_old_message_update(e: Rep[DenoiseEdgeData], m: Rep[DenseVector[Double]])
  def denoise_edge_data_cloneL(e: Rep[DenoiseEdgeData]): Rep[DenoiseEdgeData]
}

trait DenoiseEdgeDataOpsExp extends DenoiseEdgeDataOps with VariablesExp with BaseFatExp {
  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  
  case class DenoiseEdgeDataObjectNew(m: Exp[DenseVector[Double]], oM: Exp[DenseVector[Double]])
    extends Def[DenoiseEdgeData] {
    //val eD = manifest[DenoiseEdgeDataImpl]
  }
  case class DenoiseEdgeDataMessage(e: Exp[DenoiseEdgeData]) extends Def[DenseVector[Double]]
  case class DenoiseEdgeDataMessageUpdate(e: Exp[DenoiseEdgeData], m: Exp[DenseVector[Double]]) extends Def[Unit]
  case class DenoiseEdgeDataOldMessage(e: Exp[DenoiseEdgeData]) extends Def[DenseVector[Double]]
  case class DenoiseEdgeDataOldMessageUpdate(e: Exp[DenoiseEdgeData], m: Exp[DenseVector[Double]]) extends Def[Unit]
  case class DenoiseEdgeDataCloneL(e: Exp[DenoiseEdgeData]) extends Def[DenoiseEdgeData]

  /////////////////////
  // object interface

  def denoise_edge_data_obj_new(m: Exp[DenseVector[Double]], oM: Exp[DenseVector[Double]]) = reflectMutable(DenoiseEdgeDataObjectNew(m, oM))

  /////////////////////
  // class interface

  def denoise_edge_data_message(e: Exp[DenoiseEdgeData]) = reflectMutable(DenoiseEdgeDataMessage(e))
  def denoise_edge_data_message_update(e: Exp[DenoiseEdgeData], m: Exp[DenseVector[Double]]) = reflectWrite(e)(DenoiseEdgeDataMessageUpdate(e, m))
  def denoise_edge_data_old_message(e: Exp[DenoiseEdgeData]) = reflectMutable(DenoiseEdgeDataOldMessage(e))
  def denoise_edge_data_old_message_update(e: Exp[DenoiseEdgeData], m: Exp[DenseVector[Double]]) = reflectWrite(e)(DenoiseEdgeDataOldMessageUpdate(e, m))
  def denoise_edge_data_cloneL(e: Exp[DenoiseEdgeData]) = reflectMutable(DenoiseEdgeDataCloneL(e))
}

trait ScalaGenDenoiseEdgeDataOps extends ScalaGenBase {
  val IR: LBPOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case e@DenoiseEdgeDataObjectNew(m,oM) => emitValDef(sym, "new generated.scala.DenoiseEdgeDataImpl(" + quote(m) + "," + quote(oM) + ")")
      case DenoiseEdgeDataMessage(e) => emitValDef(sym, quote(e) + ".message")
      case DenoiseEdgeDataMessageUpdate(e,m) => emitValDef(sym, quote(e) + ".setMessage(" + quote(m) + ")")
      case DenoiseEdgeDataOldMessage(e) => emitValDef(sym, quote(e) + ".oldMessage")
      case DenoiseEdgeDataOldMessageUpdate(e,m) => emitValDef(sym, quote(e) + ".setOldMessage(" + quote(m) + ")")
      case DenoiseEdgeDataCloneL(e) => emitValDef(sym, quote(e) + ".cloneL")
      case _ => super.emitNode(sym, rhs)
    }
  }
}
