package ppl.dsl.optiml.io

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common.{Base, BaseFatExp}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optiml._

// file format is m lines with n floats per line, each float separated by whitespaces
// (same as matlab .dat)
trait MLOutputWriterOps extends Base {
  this: OptiML =>
  
  object MLOutputWriter {
    def writeImgPgm(img: Rep[GrayscaleImage], filename: Rep[String])(implicit ctx: SourceContext) = obj_mloutput_write_img_pgm(img,filename)
  }

  def obj_mloutput_write_img_pgm(img: Rep[GrayscaleImage], filename: Rep[String])(implicit ctx: SourceContext): Rep[Unit]
}

trait MLOutputWriterOpsExp extends MLOutputWriterOps with BaseFatExp { this: MLOutputWriterImplOps with OptiMLExp =>  
  case class MLOutputWriteImgPgm(img: Exp[GrayscaleImage], filename: Exp[String]) extends DeliteOpSingleTask(reifyEffects(mloutput_write_img_pgm_impl(img,filename)))
  
  def obj_mloutput_write_img_pgm(img: Exp[GrayscaleImage], filename: Exp[String])(implicit ctx: SourceContext) = reflectEffect(MLOutputWriteImgPgm(img,filename))
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(d@MLOutputWriteImgPgm(img,fn), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,d) } with MLOutputWriteImgPgm(f(img),f(fn)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]      
}
