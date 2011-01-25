package ppl.dsl.optiml.datastruct.scala

class ImageImpl[T](nRows: Int, nCols: Int) extends MatrixImpl[T](nRows, nCols) with Image[T]