package ppl.dsl.optiml

import datastruct.scala.{Vector,Matrix}
import java.io.{PrintWriter}
import ppl.delite.framework.{DSLType, DeliteApplication}
import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.Base

// file format is m lines with n floats per line, each float separated by whitespaces
// (same as matlab .dat)

trait MLInputReaderOps extends DSLType with Base {
  object MLInputReader {
    def read(filename: Rep[String]) : Rep[Matrix[Double]] = obj_mlinput_read(filename)
    def readVector(filename: Rep[String]) : Rep[Vector[Double]] = obj_mlinput_read_vector(filename)
  }

  def obj_mlinput_read(filename: Rep[String]) : Rep[Matrix[Double]]
  def obj_mlinput_read_vector(filename: Rep[String]) : Rep[Vector[Double]]
}

trait MLInputReaderOpsExp extends MLInputReaderOps { this: MLInputReaderImplOps with DSLOpsExp =>
  case class MLInputRead(filename: Exp[String]) extends DSLOp(reifyEffects(mlinput_read_impl(filename)))
  case class MLInputReadVector(filename: Exp[String]) extends DSLOp(reifyEffects(mlinput_read_vector_impl(filename)))

  def obj_mlinput_read(filename: Exp[String]) = reflectEffect(MLInputRead(filename))
  def obj_mlinput_read_vector(filename: Exp[String]) = reflectEffect(MLInputReadVector(filename))
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
