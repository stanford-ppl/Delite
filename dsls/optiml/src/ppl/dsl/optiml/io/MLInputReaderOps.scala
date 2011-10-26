package ppl.dsl.optiml.io

import scala.reflect.SourceContext
import java.io.{PrintWriter}
import scala.virtualization.lms.common.{TupleOpsExp, Base, BaseFatExp}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optiml._
import ppl.dsl.optiml.application.{BinarizedGradientTemplate}

// TODO: we need to support an escape hatch, or move application-specific i/o to application ops. Either
// way, they shouldn't be here.
trait MLInputReaderOps extends Base {
  object MLInputReader {
    // file format is m lines with n floats per line, each float separated by whitespaces
    // (same as matlab .dat)
    def readGrayscaleImage(filename: Rep[String]) = obj_mlinput_read_grayscale_image(filename)

    // app specific! to be removed
    def readTokenMatrix(filename: Rep[String]) = obj_mlinput_read_tokenmatrix(filename)
    def readTemplateModels(directory: Rep[String]) = obj_mlinput_read_template_models(directory)
  }

  def obj_mlinput_read_grayscale_image(filename: Rep[String]) : Rep[GrayscaleImage]
  
  //def obj_mlinput_read_tokenmatrix(filename: Rep[String]): Rep[TrainingSet[Double,Double]]
  def obj_mlinput_read_tokenmatrix(filename: Rep[String]): (Rep[Matrix[Double]],Rep[DenseVector[Double]])
  def obj_mlinput_read_template_models(directory: Rep[String]): Rep[DenseVector[(String, DenseVector[BinarizedGradientTemplate])]]
}

trait MLInputReaderOpsExp extends MLInputReaderOps with BaseFatExp { this: MLInputReaderImplOps with DeliteOpsExp with TupleOpsExp =>
  case class MLInputReadGrayscaleImage(filename: Exp[String])
    extends DeliteOpSingleTask(reifyEffects(mlinput_read_grayscale_image_impl(filename)))

  case class MLInputReadTokenMatrix(filename: Exp[String])
    extends DeliteOpSingleTask(reifyEffects(mlinput_read_tokenmatrix_impl(filename)))

  case class MLInputReadTemplateModels(directory: Exp[String])
    extends DeliteOpSingleTask(reifyEffects(mlinput_read_template_models_impl(directory)))


  def obj_mlinput_read_grayscale_image(filename: Exp[String]) = reflectEffect(MLInputReadGrayscaleImage(filename))

  def obj_mlinput_read_tokenmatrix(filename: Exp[String]) = reflectEffect(MLInputReadTokenMatrix(filename))
  def obj_mlinput_read_template_models(directory: Exp[String]) = reflectEffect(MLInputReadTemplateModels(directory))

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case Reflect(e@MLInputReadTokenMatrix(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MLInputReadTokenMatrix(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}


//trait ScalaGenMLInputReaderOps extends ScalaGenBase {
//  val IR: MLInputReaderOpsExp
//  import IR._
//
//  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
//    case MLInputRead(filename) => emitValDef(sym, base + ".read(" + quote(filename) + ")")
//    case MLInputReadVector(filename) => emitValDef(sym, base + ".readVector(" + quote(filename) + ")")
//    case _ => super.emitNode(sym, rhs)
//  }
//}
