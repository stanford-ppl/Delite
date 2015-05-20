package generated.scala.container


// specialization bug on multiple ctors: (_indices: Array[Int], _keys: Array[K], _values: Array[V], _sz: Int)
final class HashMapImpl[@specialized K: Manifest](indsz: Int, datasz: Int) {
  private val loadfactor_d2 = 0.4f / 2
  private var indices = fill(HashMapImpl.nextPow2(indsz))(-1)
  private var keys = new Array[K](datasz)
  private var blocksizes: Array[Int] = _
  private var sz = 0
  private var relbits = Integer.numberOfTrailingZeros(indices.length / 2)

  // JVM specific, but we generally cannot allocate an array of size 2^31,
  // and the implementation expects the index array to be aligned to a power of 2.
  private val MAX_ARRAY_SIZE = Math.pow(2,30).toInt

  import HashMapImpl.nextPow2

  def this() = this(128, 52)

  @inline private def absolute(hc: Int) = {
    val mask = hc >> 31
    (hc + mask) ^ mask
  }

  def size = sz

  def get(k: K): Int = {
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

  def put(k: K): Int = {
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

      if (datapos > keys.length - 1) {
        if (datapos > MAX_ARRAY_SIZE - 1) {
          throw new NegativeArraySizeException("HashMapImpl already at max size, cannot grow")
        }
        growKeys()
      }

      keys(datapos) = k
      sz += 1
      growIndices()
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

  private def fill(length: Int)(value: Int) = {
    val a = new Array[Int](length)
    java.util.Arrays.fill(a, value) //scala fill is much more expensive
    a
  }

  private def growKeys() = {
    val nkeys = new Array[K](Math.min(keys.length * 2L, MAX_ARRAY_SIZE).toInt)
    // copy raw data
    System.arraycopy(keys, 0, nkeys, 0, sz)
    keys = nkeys
  }

  private def growIndices() = {
    if (sz > (loadfactor_d2 * indices.length) && (indices.length < MAX_ARRAY_SIZE)) {
      val nindices = fill(Math.min(indices.length * 2L, MAX_ARRAY_SIZE).toInt)(-1)
      relbits = Integer.numberOfTrailingZeros(nindices.length / 2)
      val mask = nindices.length - 1

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
    }
  }

  override def toString = "HashMapImpl(sz: %d; indices: %s; keys: %s)".format(sz, if (indices != null) indices.mkString(", ") else "null", if (keys != null) keys.mkString(", ") else "null")

  def unsafeIndices: Array[Int] = indices

  def unsafeKeys: Array[K] = keys

  def unsafeSize = sz

  def unsafeBlockSizes = blocksizes

  def unsafeSetBlockSizes(_blkszs: Array[Int]) = blocksizes = _blkszs

  def unsafeSetKeys(_keys: Array[K]) {
    keys = _keys
  }

  def unsafeSetSize(_sz: Int) {
    sz = _sz
  }

  def unsafeSetInternal(_ind: Array[Int], _keys: Array[K], _sz: Int) {
    indices = _ind
    keys = _keys
    sz = _sz
  }
}


final class Bucket[@specialized T] {
  var array: Array[T] = _
  var size = 0
  //var next: Bucket[T] = _

  //def dcSize = size
  def dcApply(idx: Int) = array(idx)
  def dcUpdate(idx: Int, x: T) = array(idx) = x

  override def toString = "Bucket(size: %d; values: %s)".format(size, array.take(size).mkString(", "))
}


object HashMapImpl {
  def range(n: Int) = {
    val hm = new HashMapImpl[Int](n * 5 + 1, n * 3)
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
