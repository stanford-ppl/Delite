package ppl.dsl.optiql.datastruct.scala.container


final class BufferImpl[@specialized K: Manifest] {
  private var data = new Array[K](128)
  private var sz = 0
  
  def +=(x: K): this.type = {
    if (sz >= data.length) {
      val ndata = new Array[K](data.length * 2)
      System.arraycopy(data, 0, ndata, 0, data.length)
      data = ndata
    }
    data(sz) = x
    sz += 1
    this
  }

  def ++=(x: BufferImpl[K]): this.type = {
/*
    FIXME: compile error
    val nsz = sz + x.sz
    if (nsz > data.length) {
      var nlen = data.length * 2
      while (nsz >= nlen)
        nlen = nlen * 2
      val ndata = new Array[K](nlen)
      System.arraycopy(data, 0, ndata, 0, sz)
      System.arraycopy(x.data, 0, ndata, sz, x.sz)
      data = ndata
      this
    } else {
      System.arraycopy(x.data, 0, data, sz, x.sz)
      sz = nsz
      this
    }
*/
    assert(false, "FIXME: BUFFER ++= NOT IMPLEMENTED")
    this
  }
  
  def toArray: Array[K] = {
    val ndata = new Array[K](sz)
    System.arraycopy(data, 0, ndata, 0, sz)
    ndata
  }
  
}
