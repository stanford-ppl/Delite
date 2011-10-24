package ppl.dsl.optila.datastruct.scala


// lots of violations of the LSP here, that can end up exposed to the user (TODO: fix)

/**
 * Vector
 */

 trait Vector[@specialized(Boolean, Int, Long, Float, Double) T] { //extends ppl.delite.framework.datastruct.scala.DeliteCollection[T] {
   def length: Int
//   def isRow: Boolean
//   def mtrans: Vector[T]  
   def apply(n: Int): T
//   def update(index: Int, x: T)
//   def cloneL: Vector[T]
//   def toList: List[T]
//   def sort(implicit o: Ordering[T]): Vector[T] // because we use the underlying data field to sort
//   def copyFrom(pos: Int, xs: Vector[T])
//   def insert(pos: Int, x: T)
//   def insertAll(pos: Int, xs: Vector[T])
//   def removeAll(pos: Int, len: Int)
//   def clear()
//   def trim
//     
//   // DeliteCollection
//  def dcApply(idx: Int) = apply(idx)
//  def dcUpdate(idx: Int, x: T) = update(idx, x)
//  def dcSize = length
//   

  // AKS: how does this work with Struct?
  // value equality is needed for DeliteCollection zero
  // override def equals(rhs: Any): Boolean = {
  //   if (!rhs.isInstanceOf[Vector[T]]) return false
  //       
  //   val rv = rhs.asInstanceOf[Vector[T]]
  //   if (dcSize != rv.dcSize) return false
  //   var i = 0
  //   while (i < rv.dcSize) {
  //     if (dcApply(i) != rv.dcApply(i)) return false
  //     i += 1        
  //   }
  //   true    
  }
  
  // TO BE MOVED TO DenseVector
//  def data: Array[T]
//  def unsafeSetData(xs: Array[T], len: Int)
//}

// TODO: enable this after switching VectorOps to DenseVectorOps
// trait DenseVector[@specialized(Boolean, Int, Long, Float, Double) T] extends Vector[T] {
//   def cloneL: DenseVector[T]
//   def sort(implicit o: Ordering[T]): DenseVector[T] // because we use the underlying data field to sort
// //   def data: Array[T]
// //   def unsafeSetData(xs: Array[T], len: Int)  
// }

// trait ZeroVector[T] extends DenseVector[T]
// 
// trait EmptyVector[T] extends DenseVector[T] {
//   def length : Int = 0
//   def isRow: Boolean = true // shouldn't matter
//   def mtrans = this  
//   def toList = List[T]()
//   def cloneL = new EmptyVectorImpl[T]  
//   def trim = ()
//   def clear() = ()  
//   def sort(implicit o: Ordering[T]) = this
//   
//   def apply(i: Int): T = throw new IndexOutOfBoundsException()
//   def update(index: Int, x: T) = throw new IndexOutOfBoundsException()
//   def removeAll(pos: Int, len: Int) = throw new IndexOutOfBoundsException()
//   def copyFrom(pos: Int, xs: Vector[T]) = throw new IndexOutOfBoundsException()    
//   
//   // TODO: implement these - return a non-empty Vector  
//   def insert(pos: Int, x: T) = throw new UnsupportedOperationException()
//   def insertAll(pos: Int, xs: Vector[T]) = throw new UnsupportedOperationException()
//   
//   // to be removed
//   def data: Array[T] = throw new UnsupportedOperationException()
//   def unsafeSetData(xs: Array[T], len: Int) = throw new UnsupportedOperationException()
// }

//trait VectorView[@specialized(Boolean, Int, Long, Float, Double) T] extends Vector[T]

// trait MatrixRow[@specialized(Boolean, Int, Long, Float, Double) T] extends VectorView[T] {
//   def index: Int
// }
// trait MatrixCol[@specialized(Boolean, Int, Long, Float, Double) T] extends VectorView[T] {
//   def index: Int
// }

//trait RangeVector extends Vector[Int]

/**
 * Matrix
 */
trait Matrix[@specialized(Boolean, Int, Long, Float, Double) T] extends ppl.delite.framework.datastruct.scala.DeliteCollection[T] {
  def numRows: Int
  def numCols: Int
  def size: Int
  def apply(i: Int, j: Int): T
  def update(row: Int, col: Int, x: T)
  
  def data: Array[T]  
  def vview(start: Int, stride: Int, length: Int, isRow: Boolean): VectorView[T]
  def getRow(i: Int): VectorView[T]
  def getCol(j: Int): VectorView[T]
  def insertRow(pos: Int, x: Vector[T])
  def insertAllRows(pos: Int, xs: Matrix[T])
  def insertCol(pos: Int, x: Vector[T])
  def insertAllCols(pos: Int, xs: Matrix[T])
  def removeRows(pos: Int, len: Int)
  def removeCols(pos: Int, len: Int)
  def cloneL: Matrix[T]

  // DeliteCollection
  def dcApply(idx: Int): T
  def dcUpdate(idx: Int, x: T): Unit
  def dcSize = size
  
  // value equality is needed for DeliteCollection zero
  override def equals(rhs: Any): Boolean = {
    if (!rhs.isInstanceOf[Matrix[T]]) return false
        
    val rv = rhs.asInstanceOf[Matrix[T]]
    if (size != rv.size) return false
    var i = 0
    while (i < rv.size) {
      if (dcApply(i) != rv.dcApply(i)) return false
      i += 1        
    }
    true    
  }
}

//trait SymmetricMatrix[@specialized(Boolean, Int, Float, Double) T] extends Matrix[T] 