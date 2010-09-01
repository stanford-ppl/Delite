package ppl.dsl.optiml.direct

object Vector {

  def apply[A : ClassManifest](len: Int, is_row: Boolean = true) : Vector[A] = newVector[A](len, is_row)

  def zeros(length: Int) : Vector[Double] = {
    OP_zeros(length).task
  }

  private def newVector[A](len: Int, is_row: Boolean = true)(implicit m: ClassManifest[A]): Vector[A] = {
    new VectorImpl[A](is_row, len)
  }

  protected[optiml] case class OP_zeros(len: Int){
    def task : Vector[Double] = newVector[Double](len)
  }

  protected[optiml] case class OP_trans[A : ClassManifest](val v: Vector[A]){

    def task : Vector[A] = {
      // no mutable trans or initialization from vector yet
      val out = newVector[A](v.length, !v.is_row)
      for (i <- 0 until v.length){
        out(i) = v(i)
      }
      out
    }
  }

  protected[optiml] case class OP_toBoolean[A](val v: Vector[A])(implicit conv: A => Boolean) {
    def task: Vector[Boolean] = {
      val out = newVector[Boolean](v.length, v.is_row)
      for (i <- 0 until v.length){
        out(i) = conv(v(i))
      }
      out
    }
  }

  protected[optiml] case class OP_+[A](val collA: Vector[A], val collB: Vector[A])(implicit n: Numeric[A], c: ClassManifest[A]) {

    def task : Vector[A] = {
      assert(collA.length == collB.length)

      val out = newVector[A](collA.length)
      for (i <- 0 until collA.length){
        out(i) = n.plus(collA(i), collB(i))
      }
      out
    }
  }

  protected[optiml] case class OP_-[A](val collA: Vector[A], val collB: Vector[A])(implicit n: Numeric[A], c: ClassManifest[A]) {

    def task : Vector[A] = {
      assert(collA.length == collB.length)

      val out = newVector[A](collA.length)
      for (i <- 0 until collA.length){
        out(i) = n.minus(collA(i), collB(i))
      }
      out
    }
  }

  protected[optiml] case class OP_/[A](val collA: Vector[A], val x: A)(implicit f: Fractional[A], c: ClassManifest[A]) {

    def task : Vector[A] = {
      val out = newVector[A](collA.length)
      for (i <- 0 until collA.length){
        out(i) = f.div(collA(i), x)
      }
      out
    }
  }

  protected[optiml] case class OP_outer[A](collA: Vector[A], collB: Vector[A])(implicit val n: Numeric[A], c: ClassManifest[A]) {

    def task = {
      assert(collA.length == collB.length && collA.is_row != collB.is_row && !collA.is_row)
      val out = Matrix[A](collA.length, collA.length)
      var i = 0
      var j = 0
      while( i < collA.length ){
        while (j < collB.length) {
          out(i,j) = n.times(collA(i),collB(j))
          j += 1
        }
        j = 0
        i += 1
      }
      out
    }
  }
}


trait Vector[T] {
  import Vector._

  def pprint = {
    if (is_row){
      print("[ ")
      this.foreach(e => {  print(e); print(" ") })
      print("]\n")
    }
    else{
      this.foreach(e => { print("[ ")
                          print(e)
                          print(" ]\n")
      })
    }
  }

  /* Real length of the vector; underlying raw data may be allocated more. */
  def length : Int

  /* Defines vector orientation (row-wise or column-wise) */
  def is_row : Boolean

  def toBoolean(implicit conv: T => Boolean) : Vector[Boolean] =  /*map(e => conv(e)*/ OP_toBoolean(this).task

  def apply(n: Int) : T

  def trans(implicit c: ClassManifest[T]) : Vector[T] = {
    OP_trans(this).task
  }

  def update(n: Int, x: T)

  /* Adds a single element to the vector */
  def +=[A <: T](x: A): Vector[T]

  def +(v: Vector[T])(implicit n: Numeric[T], c: ClassManifest[T]) : Vector[T] = {
    OP_+(this, v).task
  }

  def -(v: Vector[T])(implicit n: Numeric[T], c: ClassManifest[T]) : Vector[T] = {
    OP_-(this, v).task
  }

  def /(x: T)(implicit f: Fractional[T], c: ClassManifest[T]) : Vector[T] = {
    OP_/(this, x).task
  }

  /* vector-vector outer product */
  def outer(v: Vector[T])(implicit n: Numeric[T], c: ClassManifest[T]) : Matrix[T] = {
    OP_outer(this, v).task
  }

  def foreach(block: T => Unit) = {
    for (i <- 0 until length){
      block(this(i))
    }
  }

}
