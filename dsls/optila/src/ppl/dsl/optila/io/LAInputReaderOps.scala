package ppl.dsl.optila.io

import java.io.{PrintWriter}
import scala.virtualization.lms.common.{TupleOpsExp, Base, BaseFatExp}
import scala.reflect.SourceContext
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optila._

trait LAInputReaderOps extends Base {
  // file format is m lines with n floats per line, each float separated by whitespaces
  // (same as matlab .dat)  
  
  def readMatrix(filename: Rep[String], delim: Rep[String] = unit("\\\\s+"))(implicit ctx: SourceContext) = obj_lainput_read(filename, delim)
  def readVector(filename: Rep[String])(implicit ctx: SourceContext) = obj_lainput_read_vector(filename)
  
  def obj_lainput_read(filename: Rep[String], delim: Rep[String])(implicit ctx: SourceContext): Rep[DenseMatrix[Double]]
  def obj_lainput_read_vector(filename: Rep[String])(implicit ctx: SourceContext): Rep[DenseVector[Double]]
}

trait LAInputReaderOpsExp extends LAInputReaderOps with BaseFatExp { this: LAInputReaderImplOps with DeliteOpsExp with TupleOpsExp =>
  case class LAInputRead(filename: Exp[String], delim: Exp[String])
    extends DeliteOpSingleTask(reifyEffects(lainput_read_impl(filename, delim)))

  case class LAInputReadVector(filename: Exp[String])
    extends DeliteOpSingleTask(reifyEffects(lainput_read_vector_impl(filename)))


  def obj_lainput_read(filename: Exp[String], delim: Exp[String])(implicit ctx: SourceContext) = reflectEffect(LAInputRead(filename, delim))
  def obj_lainput_read_vector(filename: Exp[String])(implicit ctx: SourceContext) = reflectEffect(LAInputReadVector(filename))
}


//trait ScalaGenLAInputReaderOps extends ScalaGenBase {
//  val IR: LAInputReaderOpsExp
//  import IR._
//
//  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
//    case LAInputRead(filename) => emitValDef(sym, base + ".read(" + quote(filename) + ")")
//    case LAInputReadVector(filename) => emitValDef(sym, base + ".readVector(" + quote(filename) + ")")
//    case _ => super.emitNode(sym, rhs)
//  }
//}
