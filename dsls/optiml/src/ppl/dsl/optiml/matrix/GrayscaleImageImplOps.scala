package ppl.dsl.optiml.matrix

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml._

trait GrayscaleImageImplOps { this: OptiML =>
  def grayscaleimage_histogram_impl(x: Rep[GrayscaleImage]): Rep[DenseVector[Int]]
}

trait GrayscaleImageImplOpsStandard extends GrayscaleImageImplOps {
  this: OptiMLCompiler with OptiMLLift =>

  //////////////////////////
  // kernel implementations

  def grayscaleimage_histogram_impl(x: Rep[GrayscaleImage]): Rep[DenseVector[Int]] = {
    // 17 34 100
    // 100 55 17
    // 1   99 200

    // strategy: chunk-local histograms, and then reduce buckets
    // how do we express this with delite ops?  
    
    // sequential for now:
    val out = DenseVector[Int](256, true)
    
    var row = 0
    while (row < x.numRows) {
      var col = 0
      while (col < x.numCols) {
        //out(x(row, col)) += 1
        out(x(row,col)) = out(x(row,col))+1
        col += 1
      }
      row += 1
    }
    
    out.unsafeImmutable
  }
}


