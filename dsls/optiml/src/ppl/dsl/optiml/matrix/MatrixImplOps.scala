package ppl.dsl.optiml.matrix

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml._

trait MatrixImplOps { this: OptiML =>
  def matrix_apply_row_indices_impl[A:Manifest,I:Manifest,MA:Manifest](m: Interface[Matrix[A]], rowIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA]): Rep[MA]
  def matrix_apply_col_indices_impl[A:Manifest,I:Manifest,MA:Manifest](m: Interface[Matrix[A]], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA]): Rep[MA]
  def matrix_apply_block_indices_impl[A:Manifest,I:Manifest,MA:Manifest](m: Interface[Matrix[A]], rowIndices: Interface[IndexVector], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA]): Rep[MA]
  def sparsematrix_csr_nz_row_indices_impl[A:Manifest](x: Rep[SparseMatrix[A]]): Rep[IndexVectorDense]
}

trait MatrixImplOpsStandard extends MatrixImplOps {
  this: OptiMLCompiler with OptiMLLift =>

  //////////////////////////
  // kernel implementations

  def matrix_apply_row_indices_impl[A:Manifest,I:Manifest,MA:Manifest](m: Interface[Matrix[A]], rowIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA]) = {
    val resultOut = b.alloc(rowIndices.length, m.numCols)
    val result = b.toBuildableIntf(resultOut)    
    for (i <- 0 until rowIndices.length){
      for (j <- 0 until m.numCols) {
        result(i,j) = m(rowIndices(i),j)
      }
    }
    b.finalizer(resultOut)
  }
  
  def matrix_apply_col_indices_impl[A:Manifest,I:Manifest,MA:Manifest](m: Interface[Matrix[A]], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA]) = {
    val resultOut = b.alloc(m.numRows, colIndices.length)
    val result = b.toBuildableIntf(resultOut)    
    for (i <- 0 until m.numRows){
      for (j <- 0 until colIndices.length) {
        result(i,j) = m(i,colIndices(j))
      }
    }
    b.finalizer(resultOut)
  }
  
  def matrix_apply_block_indices_impl[A:Manifest,I:Manifest,MA:Manifest](m: Interface[Matrix[A]], rowIndices: Interface[IndexVector], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA]) = {
    val resultOut = b.alloc(rowIndices.length, colIndices.length)
    val result = b.toBuildableIntf(resultOut)    
    for (i <- 0 until rowIndices.length){
      for (j <- 0 until colIndices.length) {
        result(i,j) = m(rowIndices(i),colIndices(j))
      }
    }
    b.finalizer(resultOut)
  }  
  
  // FIXME: this implicitly assumes CSR, breaking the abstraction in OptiLA
  def sparsematrix_csr_nz_row_indices_impl[A:Manifest](x: Rep[SparseMatrix[A]]) = {
    val out = IndexVector(0,true)
    val rowPtr = sparsematrix_csr_raw_rowptr(x)
    var i = 0
    var lastRowOff = 0
    // if (rowPtr(0) != 0) fatal("sparsematrix_csr_nz_row_indices: rowPtr(0) should always be 0")
    while (i < rowPtr.length - 1) {
      val rowOff = rowPtr(i)
      if (rowOff != lastRowOff) {
        out += i-1
      }
      lastRowOff = rowOff
      i += 1
    }  
    out.unsafeImmutable
  }
}