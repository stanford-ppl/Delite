package ppl.delite.framework.datastructures

import scala.virtualization.lms.common._
import ppl.delite.framework.ops.{DeliteCollection, DeliteOpsExp}
import reflect.{SourceContext, RefinedManifest}


trait DeliteMap[K,V]

trait DeliteMapOps extends Base {

  object DeliteMap {
    def apply[A:Manifest,K:Manifest,V:Manifest](
      coll: Rep[DeliteCollection[A]],
      keyFunc: Rep[A] => Rep[K],
      valFunc: Rep[A] => Rep[V] = (e:Rep[A]) => e,
      conflictRes: (Rep[V],Rep[V]) => Rep[V] = (a:Rep[V],b:Rep[V]) => a
    ) = dmap_fromCollection(coll,keyFunc,valFunc,conflictRes)

    def apply[A:Manifest,K:Manifest,V:Manifest](coll: Rep[DeliteCollection[A]], keyFunc: Rep[A] => Rep[K], values: Rep[DeliteArray[V]]) = dmap_fromCollection(coll,keyFunc,values)
  }

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

  def dmap_fromCollection[A:Manifest,K:Manifest,V:Manifest](in: Rep[DeliteCollection[A]], keyFunc: Rep[A] => Rep[K], valFunc: Rep[A] => Rep[V], conflictRes: (Rep[V],Rep[V]) => Rep[V]): Rep[DeliteMap[K,V]]
  def dmap_fromCollection[A:Manifest,K:Manifest,V:Manifest](in: Rep[DeliteCollection[A]], keyFunc: Rep[A] => Rep[K], values: Rep[DeliteArray[V]]): Rep[DeliteMap[K,V]]
  def dmap_size[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext): Rep[Int]
  def dmap_get[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[V]
  def dmap_contains[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[Boolean]
  def dmap_keys[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext): Rep[DeliteArray[K]]
  def dmap_values[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext): Rep[DeliteArray[V]]
  //def dmap_toArray[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext): Rep[DeliteArray[(K,V)]]

}

trait DeliteMapOpsExp extends DeliteMapOps with DeliteStructsExp with EqualExpBridgeOpt with PrimitiveOpsExp with OrderingOpsExp { this: DeliteOpsExp =>

  trait DeliteIndex[K]

  case class DeliteIndexGet[K:Manifest](index: Rep[DeliteIndex[K]], key: Rep[K]) extends DefWithManifest[K,Int]

  case class DeliteMapNewImm[K:Manifest,V:Manifest](keys: Rep[DeliteArray[K]], values: Rep[DeliteArray[V]], index: Rep[DeliteIndex[K]], size: Rep[Int]) extends DeliteStruct[DeliteMap[K,V]] {
    val elems = copyTransformedElems(Seq("keys" -> keys, "values" -> values, "index" -> index, "size" -> size))
    val mK = manifest[K]
    val mV = manifest[V]
  }

  case class DeliteMapValues[A:Manifest,K:Manifest,V:Manifest](in: Exp[DeliteCollection[A]], keyFunc: Exp[A] => Exp[K], valFunc: Exp[A] => Exp[V], reduceFunc: (Exp[V],Exp[V]) => Exp[V])
    extends DeliteOpMappedGroupByReduce[A,K,V,DeliteArray[V]] {

    def alloc(len: Exp[Int]) = DeliteArray[V](len)
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    def zero = unit(null).asInstanceOf[Exp[V]]
  }

  // FIXME: it looks like in some cases, this can cause an identical loop to be emitted twice, 
  //        since valFunc and keyFunc are the same and used in different fields of the resulting elem. 
  //        in particular, if the keyFunc has an effect somewhere, then it will not be removed by .distinct in getMultiLoopFuncs.
  case class DeliteMapKeys[A:Manifest,K:Manifest](in: Exp[DeliteCollection[A]], keyFunc: Exp[A] => Exp[K])
    extends DeliteOpMappedGroupByReduce[A,K,K,DeliteArray[K]] {

    def alloc(len: Exp[Int]) = DeliteArray[K](len)
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    def zero = unit(null).asInstanceOf[Exp[K]]
    def valFunc = keyFunc
    def reduceFunc = (a,b) => a
  }

  case class DeliteMapBuildIndex[A:Manifest,K:Manifest](in: Exp[DeliteCollection[A]], keyFunc: Exp[A] => Exp[K])
    extends DeliteOpBuildIndex[A,K,DeliteIndex[K]] {

    def cond = null
    val size = copyTransformedOrElse(_.size)(dc_size(in))
  }


  def dmap_fromCollection[A:Manifest,K:Manifest,V:Manifest](coll: Exp[DeliteCollection[A]], keyFunc: Exp[A] => Exp[K], valFunc: Exp[A] => Exp[V], conflictRes: (Exp[V],Exp[V]) => Exp[V]) = {
    dmap_fromCollection(coll, keyFunc, reflectPure(DeliteMapValues(coll,keyFunc,valFunc,conflictRes)))
  }

  def dmap_fromCollection[A:Manifest,K:Manifest,V:Manifest](coll: Rep[DeliteCollection[A]], keyFunc: Rep[A] => Rep[K], values: Rep[DeliteArray[V]]) = {
    val keys = reflectPure(DeliteMapKeys(coll,keyFunc))
    val index = reflectPure(DeliteMapBuildIndex(coll,keyFunc))
    reflectPure(DeliteMapNewImm(keys, values, index, values.length))
  }

  def dmap_keys[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext) = field[DeliteArray[K]](dm, "keys")
  def dmap_values[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext) = field[DeliteArray[V]](dm, "values")
  def dmap_size[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext) = field[Int](dm, "size")
  protected def dmap_index[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext) = field[DeliteIndex[K]](dm, "index")
  def dindex_get[K:Manifest](index: Rep[DeliteIndex[K]], key: Rep[K])(implicit ctx: SourceContext): Rep[Int] = reflectPure(DeliteIndexGet(index, key))

  /*def dmap_toArray[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]])(implicit ctx: SourceContext) = {
    dmap_keys(dm).zip(dmap_values(dm)){ (k,v) => t2(k,v) } //note: this should be eliminated if SoA transformations are enabled
  }*/

  def dmap_get[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]], key: Rep[K])(implicit ctx: SourceContext) = {
    val idx = dindex_get(dmap_index(dm), key)
    dmap_values(dm).apply(idx)
  }

  def dmap_contains[K:Manifest,V:Manifest](dm: Rep[DeliteMap[K,V]], key: Rep[K])(implicit ctx: SourceContext) = {
    notequals(dindex_get(dmap_index(dm), key), unit(-1))
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DeliteMapNewImm(k,v,i,s) => reflectPure(new {override val original = Some(f,e) } with DeliteMapNewImm(f(k),f(v),f(i),f(s))(e.mK,e.mV))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteMapNewImm(k,v,i,s), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMapNewImm(f(k),f(v),f(i),f(s))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case e@DeliteIndexGet(i,k) => dindex_get(f(i),f(k))(e.mA,ctx)
    case Reflect(e@DeliteIndexGet(i,k), u, es) => reflectMirrored(Reflect(DeliteIndexGet(f(i),f(k)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case e@DeliteMapValues(in,k,v,r) => reflectPure(new { override val original = Some(f,e) } with DeliteMapValues(f(in),f(k),f(v),f(r))(mtype(e.dmA),mtype(e.dmK),mtype(e.dmV)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DeliteMapKeys(in,k) => reflectPure(new { override val original = Some(f,e) } with DeliteMapKeys(f(in),f(k))(mtype(e.dmA),mtype(e.dmK)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DeliteMapBuildIndex(in,k) => reflectPure(new { override val original = Some(f,e) } with DeliteMapBuildIndex(f(in),f(k))(mtype(e.dmA),mtype(e.dmK)))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteMapValues(in,k,v,r), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMapValues(f(in),f(k),f(v),f(r))(mtype(e.dmA),mtype(e.dmK),mtype(e.dmV)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMapKeys(in,k), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMapKeys(f(in),f(k))(mtype(e.dmA),mtype(e.dmK)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMapBuildIndex(in,k), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMapBuildIndex(f(in),f(k))(mtype(e.dmA),mtype(e.dmK)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = manifest[T] match {
    case t if t.erasure == classOf[DeliteMap[_,_]] => Some((classTag(t), List("keys" -> darrayManifest(t.typeArguments(0)), "values" -> darrayManifest(t.typeArguments(1)), "index" -> makeManifest(classOf[DeliteIndex[_]], List(t.typeArguments(0))), "size" -> manifest[Int])))
    case _ => super.unapplyStructType
  }

}

trait ScalaGenDeliteMapOps extends ScalaGenEffect with ScalaGenEqual {
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

trait CGenDeliteMapOps extends CGenEffect with CGenEqual {
  val IR: DeliteMapOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteIndexGet(index, key) => emitValDef(sym, quote(index) + "->get(" + quote(key) + ")")
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteIndex" if (cppMemMgr == "refcnt") => wrapSharedPtr("cppHashMap<" + unwrapSharedPtr(remapWithRef(m.typeArguments(0))) + ">")
    case "DeliteIndex" => "cppHashMap<" + remapWithRef(m.typeArguments(0)) + ">"
    case _ => super.remap(m)
  }

  override def getDataStructureHeaders(): String = {
    val out = new StringBuilder
    out.append("#include \"cppHashMap.h\"\n")
    super.getDataStructureHeaders() + out.toString
  }
}
