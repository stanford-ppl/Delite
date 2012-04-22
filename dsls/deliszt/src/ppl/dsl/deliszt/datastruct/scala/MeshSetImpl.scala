package ppl.dsl.deliszt.datastruct.scala

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: 6/13/11
 * Time: 12:54 AM
 * To change this template use File | Settings | File Templates.
 */

object MeshSetImpl {
  def apply(size: Int) = new MeshSetImpl(size)
}

class MeshSetImpl(override val size : Int) extends MeshSet {
  def apply(i : Int) = {
    // TODO bounds check here?
    i
  }
}

// No zero cell
class CellSetImpl(val ncells : Int) extends MeshSet {
  override val size = ncells - 1

  def apply(i : Int) = {
    i+1
  }
}