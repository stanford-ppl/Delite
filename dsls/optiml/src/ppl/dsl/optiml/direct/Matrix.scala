package ppl.dsl.optiml.direct

object Matrix {

  def apply[A : ClassManifest](numRows : Int, numCols : Int) : Matrix[A] = newMatrix[A](numRows, numCols)

  def zeros(numRows : Int, numCols : Int) : Matrix[Double] = {
    OP_zeros(numRows, numCols).task
  }

  private def newMatrix[A](numRows: Int, numCols: Int)(implicit m: ClassManifest[A]): Matrix[A] = {
    new MatrixImpl[A](numRows, numCols)
  }


  /**
   *  these are the kernels that could be pre-processed by a generator to generate other device kernels off to the side
   */

  protected[optiml] case class OP_zeros( numRows : Int, numCols : Int) {
    def task = newMatrix[Double](numRows, numCols)
  }

  protected[optiml] case class OP_+[A](val collA: Matrix[A], val collB: Matrix[A])(implicit n: Numeric[A], c: ClassManifest[A])
    /* extends GPUable */ {

    def task : Matrix[A] = {
      assert(collA.numRows == collB.numRows && collA.numCols == collB.numCols)

      val out = newMatrix[A](collA.numRows, collA.numCols)
      for (i <- 0 until collA.numRows) {
        for (j <- 0 until collA.numCols) {
          out(i,j) = n.plus(collA(i,j), collB(i,j))
        }
      }
      out
    }

    // call to auto-generated JNI stub?
    //def gpu_task =
  }

}

trait Matrix[T] {
  import Matrix._

  def pprint = {
    for (i <- 0 until numRows){
      print("[ ")
      for (j <- 0 until numCols){
        print(this(i,j))
        print(" ")
      }
      print("]\n")
    }
  }

  // public (read-only) properties
  def numRows : Int
  def numCols : Int

  def apply(i: Int) = getRow(i)
  def apply(i: Int, j: Int) : T

  def update(i: Int, j: Int, x: T)

  def vview(start: Int, stride: Int, length: Int, is_row: Boolean) : Vector[T]

  def +=(x: Vector[T]): Matrix[T] = insertRow(numRows, x)
  def insertRow[A <: T](pos: Int, x: Vector[A]): Matrix[T]

  def getRow(row: Int) : Vector[T] = {
    vview(row*numCols, 1, numCols, true)
  }

  def +(m: Matrix[T])(implicit n: Numeric[T], c: ClassManifest[T]) : Matrix[T] = {
    OP_+(this, m).task
  }


}
