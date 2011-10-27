package ppl.dsl.deliszt.analysis

import scala.collection.mutable.{Set => MSet, Map => MMap, ArrayBuilder}

import ppl.dsl.deliszt.datastruct.scala._

object Stencil {
  type StencilMap = MMap[Int,ReadWriteSet]
  type ForMap = MMap[Int,StencilMap]
  type MeshSetMap = MMap[Int,MeshSet]
}

class MultipleMeshSetImpl(objs: IndexedSeq[Int]) extends MeshSet {
  def apply(i : Int) = {
    objs(i)
  }
  
  override val size = objs.size
}

class MultipleMeshSetBuilder {
  val builder = ArrayBuilder.make[Int]()
  val members = MSet[Int]()
  
  def addSet(ms: MeshSet) {
    for(mo <- ms) {
      if(!members.contains(Mesh.internal(mo))) {
        builder += Mesh.internal(mo)
      }
    }
  }
  
  def +=(ms: MeshSet) = addSet(ms)
  
  def result = new MultipleMeshSetImpl(builder.result)
}

trait MultipleMeshObj {
  def +(b: Int) : MultipleMeshObj = new MultipleMeshObjImpl(objs + b)
  def ++(b: MultipleMeshObj) : MultipleMeshObj = new MultipleMeshObjImpl(objs ++ b.objs)
  val objs : Set[Int]
}

class MultipleMeshObjImpl(val objs : Set[Int] = Set[Int]()) extends MultipleMeshObj {
}

case class NoObjs() extends MultipleMeshObj {
  val objs = Set[Int]()
  
  override def ++(b: MultipleMeshObj) = b
}

case class OneObj(mo: Int) extends MultipleMeshObj {
  val objs = Set[Int](mo)
}

case class MultipleMeshObjSet(val ms : MeshSet) extends MultipleMeshObj {
  val objs = ms.toSet
} 

case class FieldAccess(field: Int, mo: Int)

class ReadWriteSet {
  var read = MSet[FieldAccess]()
  var write = MSet[FieldAccess]()
}
