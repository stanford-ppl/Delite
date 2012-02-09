package ppl.dsl.optiml.io

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common.Base
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optiml._

// file format is m lines with n floats per line, each float separated by whitespaces
// (same as matlab .dat)
trait MLOutputWriterOps extends Base {
  object MLOutputWriter {
    def writeImgPgm(img: Rep[Image[Double]], filename: Rep[String])(implicit ctx: SourceContext) = obj_mloutput_write_img_pgm(img,filename)
  }

  def obj_mloutput_write_img_pgm(img: Rep[Image[Double]], filename: Rep[String])(implicit ctx: SourceContext): Rep[Unit]
}

trait MLOutputWriterOpsExp extends MLOutputWriterOps { this: MLOutputWriterImplOps with DeliteOpsExp =>
  case class MLOutputWriteImgPgm(img: Exp[Image[Double]], filename: Exp[String]) extends DeliteOpSingleTask(reifyEffects(mloutput_write_img_pgm_impl(img,filename)))
  
  def obj_mloutput_write_img_pgm(img: Exp[Image[Double]], filename: Exp[String])(implicit ctx: SourceContext) = reflectEffect(MLOutputWriteImgPgm(img,filename))
}
