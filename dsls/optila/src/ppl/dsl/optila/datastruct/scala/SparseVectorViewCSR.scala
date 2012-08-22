package ppl.dsl.optila.datastruct.scala

class SparseVectorViewCSR[T:Manifest](val _source: SparseMatrixCSR[T], 
                                      val _start: Int,
                                      val _stride: Int,
                                      val _length: Int,
                                      val _isRow: Boolean) 