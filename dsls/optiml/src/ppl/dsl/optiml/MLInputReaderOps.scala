package ppl.dsl.optiml

import datastruct.scala._
import java.io.{PrintWriter}
import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.common.{TupleOpsExp, Base}

// TODO: we need to support an escape hatch, or move application-specific i/o to application ops. Either
// way, they shouldn't be here.
trait MLInputReaderOps extends DSLType with Base {
  object MLInputReader {
    // file format is m lines with n floats per line, each float separated by whitespaces
    // (same as matlab .dat)
    def read(filename: Rep[String]) = obj_mlinput_read(filename)
    def readVector(filename: Rep[String]) = obj_mlinput_read_vector(filename)
    def readGrayscaleImage(filename: Rep[String]) = obj_mlinput_read_grayscale_image(filename)

    // app specific! to be removed
    def readTokenMatrix(filename: Rep[String]) = obj_mlinput_read_tokenmatrix(filename)
    def readTemplateModels(filename: Rep[String]) = obj_mlinput_read_template_models(filename)
  }

  def obj_mlinput_read(filename: Rep[String]) : Rep[Matrix[Double]]
  def obj_mlinput_read_vector(filename: Rep[String]) : Rep[Vector[Double]]
  def obj_mlinput_read_grayscale_image(filename: Rep[String]) : Rep[GrayscaleImage]

  def obj_mlinput_read_tokenmatrix(filename: Rep[String]): Rep[TrainingSet[Double,Double]]
  def obj_mlinput_read_template_models(filename: Rep[String]): (Rep[String], Rep[Vector[BinarizedGradientTemplate]])
}

trait MLInputReaderOpsExp extends MLInputReaderOps { this: MLInputReaderImplOps with DeliteOpsExp with TupleOpsExp =>
  case class MLInputRead(filename: Exp[String])
    extends DeliteOpSingleTask(reifyEffects(mlinput_read_impl(filename)))

  case class MLInputReadVector(filename: Exp[String])
    extends DeliteOpSingleTask(reifyEffects(mlinput_read_vector_impl(filename)))

  case class MLInputReadGrayscaleImage(filename: Exp[String])
    extends DeliteOpSingleTask(reifyEffects(mlinput_read_grayscale_image_impl(filename)))

  case class MLInputReadTokenMatrix(filename: Exp[String])
    extends DeliteOpSingleTask(reifyEffects(mlinput_read_tokenmatrix_impl(filename)))

  case class MLInputReadTemplateModels(filename: Exp[String])
    extends DeliteOpSingleTask(reifyEffects(mlinput_read_template_models_impl(filename)))


  def obj_mlinput_read(filename: Exp[String]) = reflectEffect(MLInputRead(filename))
  def obj_mlinput_read_vector(filename: Exp[String]) = reflectEffect(MLInputReadVector(filename))
  def obj_mlinput_read_grayscale_image(filename: Exp[String]) = reflectEffect(MLInputReadGrayscaleImage(filename))

  def obj_mlinput_read_tokenmatrix(filename: Exp[String]) = reflectEffect(MLInputReadTokenMatrix(filename))
  def obj_mlinput_read_template_models(filename: Exp[String]) = t2(reflectEffect(MLInputReadTemplateModels(filename)))
}


//trait ScalaGenMLInputReaderOps extends ScalaGenBase {
//  val IR: MLInputReaderOpsExp
//  import IR._
//
//  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
//    case MLInputRead(filename) => emitValDef(sym, base + ".read(" + quote(filename) + ")")
//    case MLInputReadVector(filename) => emitValDef(sym, base + ".readVector(" + quote(filename) + ")")
//    case _ => super.emitNode(sym, rhs)
//  }
//}
