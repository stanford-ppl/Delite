package ppl.dsl.optila.io

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common.{Base, BaseFatExp}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.{Interfaces,InterfacesExp}
import ppl.dsl.optila.{Vector,Matrix}

trait LAOutputWriterOps extends Base {
  this: Interfaces =>
  
  // file format is m lines with n floats per line, each float separated by whitespaces
  // (same as matlab .dat)
  
  def writeMatrix[A](x: Interface[Matrix[A]], filename: Rep[String])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double], ctx: SourceContext)
    = obj_laoutput_write_matrix(x,filename,conv)
  def writeVector[A](x: Interface[Vector[A]], filename: Rep[String])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double], ctx: SourceContext)
    = obj_laoutput_write_vector(x,filename,conv)
  
  def obj_laoutput_write_matrix[A:Manifest](m: Interface[Matrix[A]], filename: Rep[String], conv: Rep[A] => Rep[Double])(implicit ctx: SourceContext): Rep[Unit]
  def obj_laoutput_write_vector[A:Manifest](v: Interface[Vector[A]], filename: Rep[String], conv: Rep[A] => Rep[Double])(implicit ctx: SourceContext): Rep[Unit]
}

trait LAOutputWriterOpsExp extends LAOutputWriterOps with BaseFatExp { this: LAOutputWriterImplOps with InterfacesExp with DeliteOpsExp =>
  case class LAOutputWriteMatrix[A:Manifest](m: Interface[Matrix[A]], filename: Exp[String], conv: Exp[A] => Exp[Double]) extends DeliteOpSingleTask(reifyEffects(laoutput_write_impl(m,filename,conv)))
  case class LAOutputWriteVector[A:Manifest](v: Interface[Vector[A]], filename: Exp[String], conv: Exp[A] => Exp[Double]) extends DeliteOpSingleTask(reifyEffects(laoutput_write_vector_impl(v,filename,conv)))
  
  def obj_laoutput_write_matrix[A:Manifest](m: Interface[Matrix[A]], filename: Exp[String], conv: Exp[A] => Exp[Double])(implicit ctx: SourceContext) = reflectEffect(LAOutputWriteMatrix(m,filename,conv))
  def obj_laoutput_write_vector[A:Manifest](v: Interface[Vector[A]], filename: Exp[String], conv: Exp[A] => Exp[Double])(implicit ctx: SourceContext) = reflectEffect(LAOutputWriteVector(v,filename,conv))
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(d@LAOutputWriteVector(x,fn,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,d) } with LAOutputWriteVector(f(x),f(fn),f(c)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(d@LAOutputWriteMatrix(x,fn,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,d) } with LAOutputWriteMatrix(f(x),f(fn),f(c)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]    
}
