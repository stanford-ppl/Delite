package ppl.dsl.deliszt.datastruct.scala

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: 6/13/11
 * Time: 12:54 AM
 * To change this template use File | Settings | File Templates.
 */

object MeshSetImpl {
  def apply[MO<:MeshObj:MeshObjConstruct](size: Int) = new MeshSetImpl[MO](size)
}

class MeshSetImpl[MO<:MeshObj:MeshObjConstruct](val size : Int) extends MeshSet[MO] {
  def apply(i : Int) = {
    //TODO:bounds check here?
    MeshObjImpl[MO](i)
  }
}

// No zero cell
class CellSetImpl(val size : Int) extends MeshSet[Cell] {
  def apply(i : Int) = {
    new CellImpl(i+1)
  }
}