package ppl.delite.framework.datastructures

import scala.virtualization.lms.common._
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.{SourceContext, RefinedManifest}


trait DeliteMap[K,V]

trait DeliteMapOps extends Base {

  implicit def repDMapToDMapOps[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]]) = new DeliteMapOpsCls(dm)

  class DeliteMapOpsCls[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]]) {
    def size: Rep[Int] = dmap_size(dm)
    def get(key: Rep[K]): Rep[V] = dmap_get(dm, key)
    def apply(key: Rep[K]): Rep[V] = dmap_get(dm, key)
    def contains(key: Rep[K]): Rep[Boolean] = dmap_contains(dm, key)
    def keys: Rep[DeliteArray[K]] = dmap_keys(dm)
    def values: Rep[DeliteArray[V]] = dmap_values(dm)
    //def toArray: Rep[DeliteArray[(K,V)]] = dmap_toArray(dm)
  }

  def dmap_size[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext): Rep[Int]
  def dmap_get[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[V]
  def dmap_contains[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[Boolean]
  def dmap_keys[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext): Rep[DeliteArray[K]]
  def dmap_values[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext): Rep[DeliteArray[V]]
  //def dmap_toArray[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext): Rep[DeliteArray[(K,V)]]

}

trait DeliteMapOpsExp extends DeliteMapOps with DeliteStructsExp { this: DeliteOpsExp with PrimitiveOpsExp with OrderingOpsExp with EqualExp =>

  trait DeliteIndex[K]

  case class DeliteIndexGet[K:Manifest](index: Rep[DeliteIndex[K]], key: Rep[K]) extends DefWithManifest[K,Int]

  def dindex_get[K:Manifest](index: Rep[DeliteIndex[K]], key: Rep[K])(implicit ctx: SourceContext): Rep[Int] = reflectPure(DeliteIndexGet(index, key))

  case class DeliteMapNewImm[K:Manifest,V:Manifest](keys: Rep[DeliteArray[K]], values: Rep[DeliteArray[V]], index: Rep[DeliteIndex[K]], size: Rep[Int]) extends DeliteStruct[DeliteMap[K,V]] {
    val elems = copyTransformedElems(Seq("keys" -> keys, "values" -> values, "index" -> index, "size" -> size))
    val mK = manifest[K]
    val mV = manifest[V]
  }

  /*case class DeliteMapMap[K1:Manifest,V1:Manifest,K2:Manifest,V2:Manifest](in: Rep[DeliteMap[K1,V1]], func: Rep[(K1,V1)] => (Rep[K2],Rep[V2]), conflictRes: (Rep[V2],Rep[V2]) => Rep[V2]) extends DeliteOpMappedGroupByReduce[(K1,V1), K2, V2] {
    def keyFunc = a => func(a)._1
    def valFunc = a => func(a)._2
    def reduceFunc = conflictRes
    def zero = ??
  }*/

  def dmap_keys[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext) = field[DeliteArray[K]](dm, "keys")
  def dmap_values[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext) = field[DeliteArray[V]](dm, "values")
  def dmap_size[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext) = field[Int](dm, "size")

  protected def dmap_index[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext) = field[DeliteIndex[K]](dm, "index")

  /*def dmap_toArray[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext) = {
    dmap_keys(dm).zip(dmap_values(dm)){ (k,v) => t2(k,v) } //note: this should be eliminated if SoA transformations are enabled
  }*/

  def dmap_get[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]], key: Rep[K])(implicit ctx: SourceContext) = {
    val idx = dindex_get(dmap_index(dm), key)
    dmap_values(dm).apply(idx)
  }

  def dmap_contains[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]], key: Rep[K])(implicit ctx: SourceContext) = {
    dindex_get(dmap_index(dm), key) != unit(-1)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DeliteMapNewImm(k,v,i,s) => reflectPure(new {override val original = Some(f,e) } with DeliteMapNewImm(f(k),f(v),f(i),f(s))(e.mK,e.mV))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DeliteIndexGet(i,k) => dindex_get(f(i),f(k))(e.mA,ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait ScalaGenDeliteMapOps extends ScalaGenEffect {
  val IR: DeliteMapOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteIndexGet(index, key) => emitValDef(sym, quote(index) + ".get(" + quote(key) + ")")
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteIndex" => "generated.scala.container.HashMapImpl[" + remap(m.typeArguments(0)) + "]"
    case _ => super.remap(m)
  }
}
