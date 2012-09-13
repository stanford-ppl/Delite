package ppl.dsl.optiml.matrix

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml._

trait GrayscaleImageImplOps { this: OptiML =>
  def grayscaleimage_histogram_impl(x: Rep[GrayscaleImage]): Rep[DenseVector[Double]]
}

trait GrayscaleImageImplOpsStandard extends GrayscaleImageImplOps {
  this: OptiMLCompiler with OptiMLLift =>

  //////////////////////////
  // kernel implementations

  def grayscaleimage_histogram_impl(x: Rep[GrayscaleImage]): Rep[DenseVector[Double]] = {
    // 17 34 100
    // 100 55 17
    // 1   99 200

    // strategy: chunk-local histograms, and then reduce buckets
    // how do we express this with delite ops?  
    
    // sequential for now:
    val out = DenseVector[Double](256, true)
    
    var row = 0
    while (row < x.numRows) {
      var col = 0
      while (col < x.numCols) {
        //out(x(row, col)) += 1
        val bucket = x(row,col).AsInstanceOf[Int]
        out(bucket) = out(bucket)+1
        col += 1
      }
      row += 1
    }
    
    out.unsafeImmutable
  }
}


