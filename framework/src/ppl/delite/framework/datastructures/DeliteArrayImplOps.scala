package ppl.delite.framework.datastructures

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}

// TODO: rename to DeliteArrayBufferImplOps?
trait DeliteArrayImplOps { this: DeliteArrayOps =>
  // def delitearray_apply_impl[A:Manifest](v: Rep[DeliteArray[A]], pos: Rep[Int]): Rep[A]
  // def delitearray_update_impl[A:Manifest](v: Rep[DeliteArray[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit]
  /*
  def delitearray_insert_impl[A:Manifest](v: Rep[DeliteArray[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit]
  def delitearray_insertall_impl[A:Manifest](v: Rep[DeliteArray[A]], pos: Rep[Int], xs: Rep[DeliteArray[A]]): Rep[Unit]
  def delitearray_copyfrom_impl[A:Manifest](v: Rep[DeliteArray[A]], pos: Rep[Int], xs: Rep[DeliteArray[A]]): Rep[Unit]  
  def delitearray_removeall_impl[A:Manifest](v: Rep[DeliteArray[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit]
  */
}

trait DeliteArrayImplOpsStandard extends DeliteArrayImplOps {
  this: DeliteArrayOps =>

  //////////////////////////
  // kernel implementations
    
  // def delitearray_apply_impl[A:Manifest](d: Rep[DeliteArray[A]], pos: Rep[Int]): Rep[A] = {
  //   d(pos)
  // }
  // 
  // def delitearray_update_impl[A:Manifest](d: Rep[DeliteArray[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit] = {
  //   delitearray_unsafe_update(d,pos,x) 
  // }
  
  /*
  def delitearray_insert_impl[A:Manifest](d: Rep[DeliteArray[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit] = {
    delitearray_insertspace(d,pos,1)
    delitearray_update(d,pos,x)
  }

  def delitearray_insertall_impl[A:Manifest](d: Rep[DeliteArray[A]], pos: Rep[Int], xs: Rep[DeliteArray[A]]): Rep[Unit] = {
    delitearray_insertspace(d,pos,xs.length)
    delitearray_copyfrom(d, pos, xs)
  }

  def delitearray_copyfrom_impl[A:Manifest](d: Rep[DeliteArray[A]], pos: Rep[Int], xs: Rep[DeliteArray[A]]): Rep[Unit] = {
    //chkRange(pos, pos + xs.length)
    var i = 0
    val d = delitearray_raw_data(d)
    while (i < xs.length) {
      array_unsafe_update(d,pos+i,xs(i))
      i += 1
    }
  }

  def delitearray_remodeall_impl[A:Manifest](d: Rep[DeliteArray[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    //chkRange(pos, pos + len)
    val data = delitearray_raw_data(d)
    array_unsafe_copy(data, pos + len, data, pos, d.length - (pos + len))
    delitearray_set_length(d, d.length - len)
  }

  protected def delitearray_insertspace[A:Manifest](d: Rep[DeliteArray[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    delitearray_ensureextra(d,len)
    val data = delitearray_raw_data(d)
    array_unsafe_copy(data, pos, data, pos + len, d.length - pos)
    delitearray_set_length(d, d.length + len)
  }

  protected def delitearray_ensureextra[A:Manifest](d: Rep[DeliteArray[A]], extra: Rep[Int]): Rep[Unit] = {
    val data = delitearray_raw_data(d)
    if (data.length - d.length < extra) {
      delitearray_realloc(d, d.length + extra)
    }
  }

  protected def delitearray_realloc[A:Manifest](d: Rep[DeliteArray[A]], minLen: Rep[Int]): Rep[Unit] = {  
    val data = delitearray_raw_data(d)
    var n = Math.max(4, data.length * 2)
    while (n < minLen) n = n*2
    val d = NewArray[A](n)
    array_unsafe_copy(data, 0, d, 0, d.length)
    delitearray_set_raw_data(d, d.unsafeImmutable)
  }
  */

}