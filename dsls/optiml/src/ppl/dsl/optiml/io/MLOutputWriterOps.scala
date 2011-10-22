package ppl.dsl.optiml.io

import ppl.dsl.optiml.{Vector,Matrix,TrainingSet}
import java.io.{PrintWriter}
import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common.Base
import ppl.delite.framework.ops.DeliteOpsExp

// file format is m lines with n floats per line, each float separated by whitespaces
// (same as matlab .dat)

trait MLOutputWriterOps extends Base {
  object MLOutputWriter {
    def writeImgPgm(img: Rep[Matrix[Double]], filename: Rep[String]) = obj_mloutput_write_img_pgm(img,filename)
  }

  def obj_mloutput_write_img_pgm(img: Rep[Matrix[Double]], filename: Rep[String]): Rep[Unit]
}

trait MLOutputWriterOpsExp extends MLOutputWriterOps { this: MLOutputWriterImplOps with DeliteOpsExp =>
  case class MLOutputWriteImgPgm(img: Exp[Matrix[Double]], filename: Exp[String]) extends DeliteOpSingleTask(reifyEffects(mloutput_write_img_pgm_impl(img,filename)))
  
  def obj_mloutput_write_img_pgm(img: Exp[Matrix[Double]], filename: Exp[String]) = reflectEffect(MLOutputWriteImgPgm(img,filename))
}
