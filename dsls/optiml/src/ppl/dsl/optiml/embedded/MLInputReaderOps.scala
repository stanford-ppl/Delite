package ppl.dsl.optiml.embedded

import java.io.{PrintWriter}
import scala.virtualization.lms.internal.ScalaCodegen
import scala.virtualization.lms.common.{BaseExp, Base}


// file format is m lines with n floats per line, each float separated by whitespaces
// (same as matlab .dat)

trait MLInputReaderOps extends Base {

  object MLInputReader {
    def read(filename: Rep[String]) : Rep[Matrix[Double]] = obj_mlinput_read(filename)
    def readVector(filename: Rep[String]) : Rep[Vector[Double]] = obj_mlinput_read_vector(filename)
  }

  def obj_mlinput_read(filename: Rep[String]) : Rep[Matrix[Double]]
  def obj_mlinput_read_vector(filename: Rep[String]) : Rep[Vector[Double]]
}

/* Eliminated by the move to common embedding framework
trait MLInputReaderOpsRepString extends MLInputReaderOps with StringExp {
  def obj_mlinput_read(filename: String) = "MLInputReader.read(" + filename + ")"
  def obj_mlinput_read_vector(filename: String) = "MLInputReader.readVector(" + filename + ")"
}
*/

trait MLInputReaderOpsRepExp extends MLInputReaderOps with BaseExp {
  case class MLInputRead(filename: Exp[String]) extends Def[Matrix[Double]]
  case class MLInputReadVector(filename: Exp[String]) extends Def[Vector[Double]]

  def obj_mlinput_read(filename: Exp[String]) = MLInputRead(filename)
  def obj_mlinput_read_vector(filename: Exp[String]) = MLInputReadVector(filename)
}


trait ScalaGenMLInputReader extends ScalaCodegen { this: MLInputReaderOpsRepExp =>
  private val base = "ppl.delite.polymorphic.dsl.optiml.direct.MLInputReader"

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case MLInputRead(filename) => emitValDef(sym, base + ".read(" + quote(filename) + ")")
    case MLInputReadVector(filename) => emitValDef(sym, base + ".readVector(" + quote(filename) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
