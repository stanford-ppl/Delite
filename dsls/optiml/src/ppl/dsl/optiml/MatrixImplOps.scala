package ppl.dsl.optiml

import datastruct.scala.{Vector,Matrix}
import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}

trait MatrixImplOps { this: OptiML =>
  def matrix_obj_fromseq_impl[A:Manifest](xs: Rep[Seq[Rep[Vector[A]]]]): Rep[Matrix[A]]
  def matrix_obj_diag_impl[A:Manifest](w: Rep[Int], vals: Rep[Vector[A]]): Rep[Matrix[A]]
  def matrix_obj_identity_impl(w: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_zeros_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_zerosf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_ones_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_onesf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_rand_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_randf_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_randn_impl(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]

  def matrix_get_row_impl[A:Manifest](m: Rep[Matrix[A]], row: Rep[Int]): Rep[Vector[A]]
  def matrix_get_col_impl[A:Manifest](m: Rep[Matrix[A]], col: Rep[Int]): Rep[Vector[A]]
  def matrix_slice_rows_impl[A:Manifest](m: Rep[Matrix[A]], begin: Rep[Int], end: Rep[Int]): Rep[Matrix[A]]
  def matrix_update_row_impl[A:Manifest](m: Rep[Matrix[A]], row: Rep[Int], y: Rep[Vector[A]]): Rep[Unit]
  def matrix_pprint_impl[A:Manifest](m: Rep[Matrix[A]]): Rep[Unit]

}

trait MatrixImplOpsStandard extends MatrixImplOps {
  this: OptiML =>
  

  ///////////////
  // kernels

  def matrix_obj_identity_impl(w: Rep[Int]) = Matrix.diag(w, Vector.ones(w))
  def matrix_obj_zeros_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Double](numRows, numCols)
  def matrix_obj_zerosf_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Float](numRows, numCols)
  def matrix_obj_ones_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Double](numRows, numCols) mmap { e => 1. }
  def matrix_obj_onesf_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Float](numRows, numCols) mmap { e => 1f}
  def matrix_obj_rand_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Double](numRows, numCols) mmap { e => random[Double] }
  def matrix_obj_randf_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Float](numRows, numCols) mmap { e => random[Float] }
  def matrix_obj_randn_impl(numRows: Rep[Int], numCols: Rep[Int]) = Matrix[Double](numRows, numCols) mmap { e => randomGaussian }

  def matrix_obj_diag_impl[A:Manifest](w: Rep[Int], vals: Rep[Vector[A]]) = {
    val out = Matrix[A](w,w)
    var i = unit(0)
    while (i < w){
      out(i,i) = vals(i)
      i += 1
    }
    out
  }

  def matrix_obj_fromseq_impl[A:Manifest](xs: Rep[Seq[Rep[Vector[A]]]]): Rep[Matrix[A]] = {
    throw new UnsupportedOperationException("this is currently broken")
//    val m = Matrix[A](0,0)
//    for (i <- 0 until xs.length){
//      m += xs.applyRaw(i)
//    }
//    m
  }

  def matrix_get_row_impl[A:Manifest](m: Rep[Matrix[A]], row: Rep[Int]) = m.vview(row*m.numCols, 1, m.numCols, true)
  def matrix_get_col_impl[A:Manifest](m: Rep[Matrix[A]], col: Rep[Int]) = m.vview(col, m.numCols, m.numRows, false)

  def matrix_slice_rows_impl[A:Manifest](m: Rep[Matrix[A]], begin: Rep[Int], end: Rep[Int]) = {
    //m.chkRange(begin, end)
    val out = Matrix[A](end-begin, m.numCols)
    var i = begin
    while (i < end) {
      var j = unit(0)
      while (j < m.numCols) {
        out(i-begin, j) = m(i,j)
        j += 1
      }
      i += 1
    }
    out
  }

  def matrix_update_row_impl[A:Manifest](m: Rep[Matrix[A]], row: Rep[Int], y: Rep[Vector[A]]) = {
    //chkEquals(x.length, numCols)
    // TODO: could be parallelized using a view
    var j = unit(0)
    while(j < m.numCols){
      m(row,j) = y(j)
      j += 1
    }
  }

  def matrix_pprint_impl[A:Manifest](m: Rep[Matrix[A]]) = {
    for (i <- 0 until m.numRows){
      print("[ ")
      for (j <- 0 until m.numCols){
        print(m(i,j))
        print(" ")
      }
      print("]\\n")
    }
  }


}