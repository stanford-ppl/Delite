package ppl.dsl.optiml.io

import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix,TrainingSet}
import java.io.{PrintWriter}
import ppl.delite.framework.{DSLType, DeliteApplication}
import scala.virtualization.lms.common.Base
import ppl.delite.framework.ops.DeliteOpsExp

// file format is m lines with n floats per line, each float separated by whitespaces
// (same as matlab .dat)

trait MLOutputWriterOps extends DSLType with Base {
  object MLOutputWriter {
    def write[A](m: Rep[Matrix[A]], filename: Rep[String])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]) = obj_mloutput_write(m,filename,conv)
    def writeVector[A](v: Rep[Vector[A]], filename: Rep[String])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]) = obj_mloutput_write_vector(v,filename,conv)
    def writeImgPgm(img: Rep[Matrix[Double]], filename: Rep[String]) = obj_mloutput_write_img_pgm(img,filename)
  }

  def obj_mloutput_write[A:Manifest](m: Rep[Matrix[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit]
  def obj_mloutput_write_vector[A:Manifest](v: Rep[Vector[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit]
  def obj_mloutput_write_img_pgm(img: Rep[Matrix[Double]], filename: Rep[String]): Rep[Unit]
}

trait MLOutputWriterOpsExp extends MLOutputWriterOps { this: MLOutputWriterImplOps with DeliteOpsExp =>
  case class MLOutputWrite[A:Manifest](m: Exp[Matrix[A]], filename: Exp[String], conv: Exp[A] => Exp[Double]) extends DeliteOpSingleTask(reifyEffects(mloutput_write_impl(m,filename,conv)))
  case class MLOutputWriteVector[A:Manifest](v: Exp[Vector[A]], filename: Exp[String], conv: Exp[A] => Exp[Double]) extends DeliteOpSingleTask(reifyEffects(mloutput_write_vector_impl(v,filename,conv)))
  case class MLOutputWriteImgPgm(img: Exp[Matrix[Double]], filename: Exp[String]) extends DeliteOpSingleTask(reifyEffects(mloutput_write_img_pgm_impl(img,filename)))
  
  def obj_mloutput_write[A:Manifest](m: Exp[Matrix[A]], filename: Exp[String], conv: Exp[A] => Exp[Double]) = reflectEffect(MLOutputWrite(m,filename,conv))
  def obj_mloutput_write_vector[A:Manifest](v: Exp[Vector[A]], filename: Exp[String], conv: Exp[A] => Exp[Double]) = reflectEffect(MLOutputWriteVector(v,filename,conv))
  def obj_mloutput_write_img_pgm(img: Exp[Matrix[Double]], filename: Exp[String]) = reflectEffect(MLOutputWriteImgPgm(img,filename))
}
