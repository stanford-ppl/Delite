package ppl.dsl.optiml.baseline

class SparseVectorView[@specialized T:Numeric:Manifest]
  (val _source: SparseMatrix[T], 
   val _start: Int,
   val _stride: Int,
   val _length: Int) extends Vector[T] {
                                        
  def length: Int = _length
  def newVector[B:Numeric:Manifest](len: Int): Vector[B] = SparseVector[B](len)

  def apply(pos: Int): T = {
    val idx = pos*_stride + _start
    _source(idx/_source.numCols,idx%_source.numCols)    
  }

  def update(pos: Int, x: T): Unit = {
    throw new UnsupportedOperationException("tbd")
  }
  
}
