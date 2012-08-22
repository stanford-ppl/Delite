package ppl.dsl.deliszt.analysis

import scala.collection.immutable.{Set => ISet}
import scala.collection.mutable.{Set => MSet, Map => MMap, ArrayBuilder}

import ppl.dsl.deliszt.datastruct.scala._

object Stencil {
  type StencilMap = MMap[Int,ReadWriteSet]
  type ForMap = MMap[Int,StencilMap]
  type MeshSetMap = MMap[Int,MeshSet]
  
  def ifnull(x: AnyRef) {
    x == null
  }
}

object MultipleMeshSet {
  def apply(in: MultipleMeshObj, f: Int => MeshSet) : MeshSet = {
    val builder = new MultipleMeshSetBuilder()
    
    for(mo <- in.objs) {
      builder += f(mo)
    }
    
    builder.result
  }
  
  override def toString = "MultipleMeshSet"
}

object MultipleMeshObj {
  def apply(mo: Int) = new MultipleMeshObjImpl(ISet(mo))
  def apply(ms: MeshSet) = new MultipleMeshObjImpl(ms.toSet)
  
  def apply(e: Any) = {
    e match {
      case mo: Int => OneObj(mo)
      case _ => e
    }
  }

  def multi(in: MultipleMeshObj, f: Int => MultipleMeshObj) : MultipleMeshObj = {
    var out: MultipleMeshObj = NoObjs()
    
    if(null != in) {
      for(mo <- in.objs) {
        out = out ++ f(mo)
      }
    }
    
    out
  }

  def multi(e: Int, f: Int => MultipleMeshObj) : MultipleMeshObj = {
    multi(OneObj(e), f)
  }
  
  def apply(in: MultipleMeshObj, f: Int => Int) : MultipleMeshObj = {
    var out: MultipleMeshObj = NoObjs()
    
    if(null != in) {
      for(mo <- in.objs) {
        out = out + f(mo)
      }
    }
    
    out
  }
  
  def apply(e: Int, f: Int => Int) : MultipleMeshObj = {
    apply(OneObj(e), f)
  }
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
  override def toString = { "MeshObjs(" + objs.mkString(",") + ")"}
}

class MultipleMeshObjImpl(val objs : Set[Int] = Set[Int]()) extends MultipleMeshObj {
}

case class NoObjs() extends MultipleMeshObj {
  val objs = Set[Int]()
  
  override def ++(b: MultipleMeshObj) = b
  override def toString = { "NoObjs()" }
}

case class OneObj(mo: Int) extends MultipleMeshObj {
  val objs = Set[Int](mo)
  
  override def toString = { "OneObj(" + mo + ")" }
}

case class MultipleMeshObjSet(val ms : MeshSet) extends MultipleMeshObj {
  val objs = ms.toSet
} 

case class FieldAccess(field: Int, mo: Int)

class ReadWriteSet {
  var read = MSet[FieldAccess]()
  var write = MSet[FieldAccess]()
}
