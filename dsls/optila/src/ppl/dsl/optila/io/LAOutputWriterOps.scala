package ppl.dsl.optila.io

import ppl.dsl.optila.{Vector,Matrix}
import java.io.{PrintWriter}
import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common.Base
import ppl.delite.framework.ops.DeliteOpsExp

// file format is m lines with n floats per line, each float separated by whitespaces
// (same as matlab .dat)

trait LAOutputWriterOps extends Base {
  object LAOutputWriter {
    def write[A](m: Rep[Matrix[A]], filename: Rep[String])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]) = obj_laoutput_write(m,filename,conv)
    def writeVector[A](v: Interface[Vector[A]], filename: Rep[String])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]) = obj_laoutput_write_vector(v,filename,conv)
  }

  def obj_laoutput_write[A:Manifest](m: Rep[Matrix[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit]
  def obj_laoutput_write_vector[A:Manifest](v: Interface[Vector[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit]
}

trait LAOutputWriterOpsExp extends LAOutputWriterOps { this: LAOutputWriterImplOps with DeliteOpsExp =>
  case class LAOutputWrite[A:Manifest](m: Exp[Matrix[A]], filename: Exp[String], conv: Exp[A] => Exp[Double]) extends DeliteOpSingleTask(reifyEffects(laoutput_write_impl(m,filename,conv)))
  case class LAOutputWriteVector[A:Manifest](v: Interface[Vector[A]], filename: Exp[String], conv: Exp[A] => Exp[Double]) extends DeliteOpSingleTask(reifyEffects(laoutput_write_vector_impl(v,filename,conv)))
  
  def obj_laoutput_write[A:Manifest](m: Exp[Matrix[A]], filename: Exp[String], conv: Exp[A] => Exp[Double]) = reflectEffect(LAOutputWrite(m,filename,conv))
  def obj_laoutput_write_vector[A:Manifest](v: Interface[Vector[A]], filename: Exp[String], conv: Exp[A] => Exp[Double]) = reflectEffect(LAOutputWriteVector(v,filename,conv))
}
