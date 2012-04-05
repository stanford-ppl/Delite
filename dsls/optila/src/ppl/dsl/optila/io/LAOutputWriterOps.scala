package ppl.dsl.optila.io

import ppl.dsl.optila.{Vector,Matrix}
import java.io.{PrintWriter}
import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common.Base
import scala.reflect.SourceContext
import ppl.delite.framework.ops.DeliteOpsExp

trait LAOutputWriterOps extends Base {
  // file format is m lines with n floats per line, each float separated by whitespaces
  // (same as matlab .dat)
  
  def writeMatrix[A](x: Interface[Matrix[A]], filename: Rep[String])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double], ctx: SourceContext)
    = obj_laoutput_write(x,filename,conv)
  def writeVector[A](x: Interface[Vector[A]], filename: Rep[String])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double], ctx: SourceContext)
    = obj_laoutput_write_vector(x,filename,conv)
  
  def obj_laoutput_write[A:Manifest](m: Interface[Matrix[A]], filename: Rep[String], conv: Rep[A] => Rep[Double])(implicit ctx: SourceContext): Rep[Unit]
  def obj_laoutput_write_vector[A:Manifest](v: Interface[Vector[A]], filename: Rep[String], conv: Rep[A] => Rep[Double])(implicit ctx: SourceContext): Rep[Unit]
}

trait LAOutputWriterOpsExp extends LAOutputWriterOps { this: LAOutputWriterImplOps with DeliteOpsExp =>
  case class LAOutputWrite[A:Manifest](m: Interface[Matrix[A]], filename: Exp[String], conv: Exp[A] => Exp[Double]) extends DeliteOpSingleTask(reifyEffects(laoutput_write_impl(m,filename,conv)))
  case class LAOutputWriteVector[A:Manifest](v: Interface[Vector[A]], filename: Exp[String], conv: Exp[A] => Exp[Double]) extends DeliteOpSingleTask(reifyEffects(laoutput_write_vector_impl(v,filename,conv)))
  
  def obj_laoutput_write[A:Manifest](m: Interface[Matrix[A]], filename: Exp[String], conv: Exp[A] => Exp[Double])(implicit ctx: SourceContext) = reflectEffect(LAOutputWrite(m,filename,conv))
  def obj_laoutput_write_vector[A:Manifest](v: Interface[Vector[A]], filename: Exp[String], conv: Exp[A] => Exp[Double])(implicit ctx: SourceContext) = reflectEffect(LAOutputWriteVector(v,filename,conv))
}
