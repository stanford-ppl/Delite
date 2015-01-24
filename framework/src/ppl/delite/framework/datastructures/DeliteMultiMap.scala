package ppl.delite.framework.datastructures

import scala.virtualization.lms.common._
import ppl.delite.framework.ops.{DeliteCollection, DeliteOpsExp}
import reflect.{SourceContext, RefinedManifest}


trait DeliteMultiMap[K,V]

/*trait DeliteMultiMapOps extends Base {

  object DeliteMultiMap {
    def apply[A:Manifest,K:Manifest,V:Manifest](
      ma: Rep[DeliteMultiArray[A]],
      keyFunc: Rep[A] => Rep[K],
      valFunc: Rep[A] => Rep[V] = (e:Rep[A]) => e,
      conflictRes: (Rep[V],Rep[V]) => Rep[V] = (a:Rep[V],b:Rep[V]) => a     
    ) = dmulmap_create(ma,keyFunc,valFunc,conflictRes)
  
    def apply[A:Manifest,K:Manifest,V:Manifest](
      ma: Rep[DeliteMultiArray[A]],
      keyFunc: Rep[A] => Rep[K],
      values: Rep[DeliteArray1D[V]]
    ) = dmulmap_create(ma,keyFunc,values)
  }

  implicit def repDMultiMapToDMultiMapOps[K:Manifest,V:Manifest](mm: Rep[DeliteMultiMap[K,V]]) = new DeliteMultiMapOpsCls(mm)

  class DeliteMultiMapOpsCls[K:Manifest,V:Manifest](mm: Rep[DeliteMultiMap[K,V]]) {
    def size: Rep[Int] = dmulmap_size(mm)
    def get(key: Rep[K]): Rep[V] = dmulmap_get(mm, key)
    def apply(key: Rep[K]): Rep[V] = dmulmap_get(mm, key)
    def contains(key: Rep[K]): Rep[Boolean] = dmulmap_contains(mm, key)
    def keys: Rep[DeliteArray1D[K]] = dmulmap_keys(mm)
    def values: Rep[DeliteArray1D[V]] = dmulmap_values(mm)
  }

  def dmulmap_create[A:Manifest,K:Manifest,V:Manifest](ma: Rep[DeliteMultiArray[A]], keyFunc: Rep[A] => Rep[K], valFunc: Rep[A] => Rep[V], conflictRes: (Rep[V],Rep[V]) => Rep[V]): Rep[DeliteMultiMap[K,V]]
  def dmulmap_create[A:Manifest,K:Manifest,V:Manifest](ma: Rep[DeliteMultiArray[A]], keyFunc: Rep[A] => Rep[K], values: Rep[DeliteArray1D[V]]): Rep[DeliteMultiMap[K,V]]
  def dmulmap_size[K:Manifest,V:Manifest](mm: Rep[DeliteMultiMap[K,V]])(implicit ctx: SourceContext): Rep[Int]
  def dmulmap_get[K:Manifest,V:Manifest](mm: Rep[DeliteMultiMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[V]
  def dmulmap_contains[K:Manifest,V:Manifest](mm: Rep[DeliteMultiMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[Boolean]
  def dmulmap_keys[K:Manifest,V:Manifest](mm: Rep[DeliteMultiMap[K,V]])(implicit ctx: SourceContext): Rep[DeliteArray1D[K]]
  def dmulmap_values[K:Manifest,V:Manifest](mm: Rep[DeliteMultiMap[K,V]])(implicit ctx: SourceContext): Rep[DeliteArray1D[V]]
}

trait DeliteMultiMapOpsExp extends DeliteMultiMapOps { 
  this: DeliteOpsExp with DeliteMapOpsExp with DeliteMultiArrayOpsExp => 

  // --- Constructor
  case class DeliteMultiMapNewImm[K:Manifest,V:Manifest](keys: Rep[DeliteArray1D[K]], values: Rep[DeliteArray1D[V]], index: Rep[DeliteIndex[K]], size: Rep[Int])

  // --- Fields
  case class DeliteMultiMapSize[K:Manifest,V:Manifest](mm: Exp[DeliteMultiMap[K,V]])(implicit ctx: SourceContext) extends DefWithManifest2[K,V,Int]
  case class DeliteMultiMapKeys[K:Manifest,V:Manifest](mm: Exp[DeliteMultiMap[K,V]])(implicit ctx: SourceContext) extends DefWithManifest2[K,V,DeliteArray1D[K]]
  case class DeliteMultiMapValues[K:Manifest,V:Manifest](mm: Exp[DeliteMultiMap[K,V]])(implicit ctx: SourceContext) extends DefWithManifest2[K,V,DeliteArray1D[V]]

  case class DeliteMultiMapCreateKeys[A:Manifest,K:Manifest](in: Exp[Delite])

  def dmulmap_create[A:Manifest,K:Manifest,V:Manifest](ma: Exp[DeliteMultiArray[A]], keyFunc: Exp[A] => Exp[K], valFunc: Exp[A] => Exp[V], conflictRes: (Exp[V],Exp[V]) => Exp[V]) = {

  }

  def dmulmap_create[A:Manifest,K:Manifest,V:Manifest](ma: Exp[DeliteMultiArray[A]], keyFunc: Exp[A] => Exp[K], values: Exp[DeliteArray1D[V]]) = {
  }


}*/
