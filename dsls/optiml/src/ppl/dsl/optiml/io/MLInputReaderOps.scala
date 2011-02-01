package ppl.dsl.optiml.io

import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix,TrainingSet}
import java.io.{PrintWriter}
import ppl.delite.framework.{DSLType, DeliteApplication}
import scala.virtualization.lms.common.Base
import ppl.delite.framework.ops.DeliteOpsExp

// file format is m lines with n floats per line, each float separated by whitespaces
// (same as matlab .dat)

trait MLInputReaderOps extends DSLType with Base {
  object MLInputReader {
    def read(filename: Rep[String]) = obj_mlinput_read(filename)
    def readVector(filename: Rep[String]) = obj_mlinput_read_vector(filename)
    def readTokenMatrix(filename: Rep[String]) = obj_mlinput_read_tokenmatrix(filename)
  }

  def obj_mlinput_read(filename: Rep[String]) : Rep[Matrix[Double]]
  def obj_mlinput_read_vector(filename: Rep[String]) : Rep[Vector[Double]]
  def obj_mlinput_read_tokenmatrix(filename: Rep[String]): Rep[TrainingSet[Double,Double]]
}

trait MLInputReaderOpsExp extends MLInputReaderOps { this: MLInputReaderImplOps with DeliteOpsExp =>
  case class MLInputRead(filename: Exp[String]) extends DeliteOpSingleTask(reifyEffects(mlinput_read_impl(filename)))
  case class MLInputReadVector(filename: Exp[String]) extends DeliteOpSingleTask(reifyEffects(mlinput_read_vector_impl(filename)))
  case class MLInputReadTokenMatrix(filename: Exp[String])
    extends DeliteOpSingleTask(reifyEffects(mlinput_read_tokenmatrix_impl(filename)))

  def obj_mlinput_read(filename: Exp[String]) = reflectEffect(MLInputRead(filename))
  def obj_mlinput_read_vector(filename: Exp[String]) = reflectEffect(MLInputReadVector(filename))
  def obj_mlinput_read_tokenmatrix(filename: Exp[String]) = reflectEffect(MLInputReadTokenMatrix(filename))
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
