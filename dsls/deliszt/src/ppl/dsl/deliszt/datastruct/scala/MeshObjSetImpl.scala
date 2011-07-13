package ppl.dsl.deliszt.datastruct.scala

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: 6/13/11
 * Time: 12:54 AM
 * To change this template use File | Settings | File Templates.
 */

object MeshObjSetImpl {
  def apply[MO<:MeshObj:MeshObjConstruct](size: Int) = new MeshObjSetImpl[MO](size)
}

class MeshObjSetImpl[MO<:MeshObj:MeshObjConstruct](val size : Int) extends MeshObjSet[MO] {
  def apply(i : Int) = {
    //TODO:bounds check here?
    MeshObjImpl[MO](i)
  }
}