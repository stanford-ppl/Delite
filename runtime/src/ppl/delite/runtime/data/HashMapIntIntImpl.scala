package ppl.delite.runtime.data

// specialization bug on multiple ctors: (_indices: Array[Int], _keys: Array[K], _values: Array[V], _sz: Int)
final class HashMapIntIntImpl(indsz: Int, datasz: Int) {
  private val loadfactor_d2 = 0.4f / 2
  private var indices = Array.fill[Int](HashMapIntIntImpl.nextPow2(indsz))(-1)
  private var keys = new Array[Int](datasz)
  private var blocksizes: Array[Int] = _
  private var sz = 0
  private var relbits = Integer.numberOfTrailingZeros(indices.length / 2)
  
  import HashMapIntIntImpl.nextPow2
  
  // def this(indsz: Int, datasz: Int) = this(
  //   Array.fill[Int](HashMapIntIntImpl.nextPow2(indsz))(-1),
  //   new Array[K](datasz),
  //   new Array[V](datasz), 
  //   0)
  def this() = this(128, 52)
  
  @inline private def absolute(hc: Int) = {
    val mask = hc >> 31
    (hc + mask) ^ mask
  }
  
  def size = sz
  
  def get(k: Int): Int = {
    val hc = k.## * 0x9e3775cd
    val relbits0 = relbits
    var pos = (hc >>> (32 - relbits0)) * 2
    var currelem = indices(pos)
    var currhash = indices(pos + 1)
    
    val mask = indices.length - 1
    while (currelem != -1 && (currhash != hc || keys(currelem) != k)) {
      pos = (pos + 2) & mask
      currelem = indices(pos)
      currhash = indices(pos + 1)
    }
    
    currelem
  }
  
  def put(k: Int): Int = {
    val hc = k.## * 0x9e3775cd
    val relbits0 = relbits
    var pos = (hc >>> (32 - relbits0)) * 2
    var currelem = indices(pos)
    var currhash = indices(pos + 1)
    
    val mask = indices.length - 1
    while (currelem != -1 && (currhash != hc || keys(currelem) != k)) {
      pos = (pos + 2) & mask
      currelem = indices(pos)
      currhash = indices(pos + 1)
    }
    
    if (currelem == -1) {
      val datapos = sz
      indices(pos) = datapos
      indices(pos + 1) = hc
      keys(datapos) = k
      sz += 1
      
      grow()
      datapos
    } else {
      val datapos = currelem
      keys(datapos) = k
      datapos
    }
  }
  
  def statistics = {
"""size: %d
indices length: %d
data length: %d
growth threshold: %d
""".format(sz, indices.length, keys.length, (loadfactor_d2 * indices.length).toInt)
  }
  
  private def grow() = if (sz > (loadfactor_d2 * indices.length)) {
    val nindices = Array.fill[Int](indices.length * 2)(-1)
    val nkeys = new Array[Int](keys.length * 2)
    relbits = Integer.numberOfTrailingZeros(nindices.length / 2)
    val mask = nindices.length - 1

    // copy raw data
    System.arraycopy(keys, 0, nkeys, 0, sz)
    
    // copy indices
    var i = 0
    val relbits0 = relbits
    while (i < indices.length) {
      val elem = indices(i)
      if (elem != -1) {
        val hash = indices(i + 1)
        var pos = (hash >>> (32 - relbits0)) * 2
        
        // insert it into nindices
        var currelem = nindices(pos)
        var currhash = nindices(pos + 1)
        while (currelem != -1) {
          pos = (pos + 2) & mask
          currelem = nindices(pos)
          currhash = nindices(pos + 1)
        }
        nindices(pos) = elem
        nindices(pos + 1) = hash
      }
      i += 2
    }
    
    indices = nindices
    keys = nkeys
  }
  
  override def toString = "HashMapIntIntImpl(sz: %d; indices: %s; keys: %s)".format(sz, if (indices != null) indices.mkString(", ") else "null", if (keys != null) keys.mkString(", ") else "null")
  
  def unsafeIndices: Array[Int] = indices
  
  def unsafeKeys: Array[Int] = keys
  
  def unsafeSize = sz
  
  def unsafeBlockSizes = blocksizes
  
  def unsafeSetBlockSizes(_blkszs: Array[Int]) = blocksizes = _blkszs
  
  def unsafeSetKeys(_keys: Array[Int]) {
    keys = _keys
  }
  
  def unsafeSetSize(_sz: Int) {
    sz = _sz
  }
  
  def unsafeSetInternal(_ind: Array[Int], _keys: Array[Int], _sz: Int) {
    indices = _ind
    keys = _keys
    sz = _sz
  }
}



object HashMapIntIntImpl {
  def range(n: Int) = {
    val hm = new HashMapIntIntImpl(n * 5 + 1, n * 3)
    for (i <- 0 until n) hm.put(i)
    hm
  }
  def nextPow2(x: Int) = {
    var c = x - 1;
    c |= c >>>  1;
    c |= c >>>  2;
    c |= c >>>  4;
    c |= c >>>  8;
    c |= c >>> 16;
    c + 1;
  }
}