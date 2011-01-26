package ppl.dsl.optiml.graph

import ppl.delite.framework.DSLType
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.dsl.optiml.datastruct.scala.{Vertex, Vertices}

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Jan 24, 2011
 * Time: 12:36:36 AM
 * To change this template use File | Settings | File Templates.
 */

trait VerticesOps extends DSLType with Variables {
  this: OptiML =>
  //class verOpsCls[V <: Vertex: Manifest](x: Rep[Vertices[V]]) {
    //def foreach(block: Rep[V] => Rep[Unit]) = vertices_foreach(x, block)
  //}

  //def vertices_foreach[V<: Vertex: Manifest](x: Rep[Vertices[V]], block: Rep[V] => Rep[Unit]): Rep[Unit]
}

trait VerticesOpsExp extends VerticesOps with VariablesExp {
  this: OptiMLExp =>
  /*case class VerticesForeach[V <:Vertex : Manifest](in: Exp[Vertices[V]], v: Exp[V], func: Exp[Unit])
          extends DeliteOpForeach[V, Vertices] {
    val i = fresh[Int]
    val sync = reifyEffects(in(i).neighbors.toList)
  }

  def vertices_foreach[V <: Vertex : Manifest](x: Exp[Vertices[V]], block: Exp[V] => Exp[Unit]) = {
    val v = fresh[V]
    val func = reifyEffects(block(v))
    reflectEffect(VerticesForeach(reflectRead(x), v, func))
  } */
}