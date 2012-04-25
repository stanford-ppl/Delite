package ppl.dsl.optiml.matrix

import ppl.dsl.optiml.{Vector,Matrix,IndexVector}
import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml.{OptiMLLift, OptiMLCompiler, OptiML}

trait MatrixImplOps { this: OptiML =>
  def matrix_apply_row_indices_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], rowIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,MA]): Rep[MA]
  def matrix_apply_col_indices_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,MA]): Rep[MA]
  def matrix_apply_block_indices_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], rowIndices: Interface[IndexVector], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,MA]): Rep[MA]
}

trait MatrixImplOpsStandard extends MatrixImplOps {
  this: OptiMLCompiler with OptiMLLift =>

  //////////////////////////
  // kernel implementations

  def matrix_apply_row_indices_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], rowIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,MA]) = {
    val resultOut = b.alloc(rowIndices.length, m.numCols)
    val result = b.toIntf(resultOut)    
    for (i <- 0 until rowIndices.length){
      for (j <- 0 until m.numCols) {
        result(i,j) = m(rowIndices(i),j)
      }
    }
    resultOut.unsafeImmutable        
  }
  
  def matrix_apply_col_indices_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,MA]) = {
    val resultOut = b.alloc(m.numRows, colIndices.length)
    val result = b.toIntf(resultOut)    
    for (i <- 0 until m.numRows){
      for (j <- 0 until colIndices.length) {
        result(i,j) = m(i,colIndices(j))
      }
    }
    resultOut.unsafeImmutable            
  }
  
  def matrix_apply_block_indices_impl[A:Manifest,MA:Manifest](m: Interface[Matrix[A]], rowIndices: Interface[IndexVector], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,MA]) = {
    val resultOut = b.alloc(rowIndices.length, colIndices.length)
    val result = b.toIntf(resultOut)    
    for (i <- 0 until rowIndices.length){
      for (j <- 0 until colIndices.length) {
        result(i,j) = m(rowIndices(i),colIndices(j))
      }
    }
    resultOut.unsafeImmutable    
  }  
}