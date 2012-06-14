/*package ppl.dsl.optigraph.io

import java.io.{PrintWriter}
import scala.virtualization.lms.common.{TupleOpsExp, Base, BaseFatExp}
import ppl.delite.framework.{DeliteApplication}
import ppl.dsl.optigraph.{OptiGraphExp, OptiGraph}
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optigraph.datastruct.scala._

trait GraphInputReaderOps extends Base {
  object GraphInputReader {
    // TODO: define input format
    def read(filename: Rep[String], delim: Rep[String] = unit("\\\\s+")):Rep[Graph] = read(filename, delim)
  }
  def read(filename: Rep[String], delim: Rep[String]) : Rep[Graph]
}

trait GraphInputReaderOpsExp extends GraphInputReaderOps with BaseFatExp { 
  this: OptiGraphExp =>
    
  case class GraphInputRead(filename: Exp[String], delim: Exp[String]) extends Def[Unit]
  
  def read(filename: Exp[String], delim: Exp[String]) = reflectPure(GraphInputRead(filename, delim))

}


trait ScalaGenGraphInputReaderOps extends ScalaGenBase {
  val IR: GraphInputReaderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case GraphInputRead(filename, delim) => emitValDef(sym, base + "GraphReaderImpl.read(" + quote(filename) + "," + quote(delim) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}*/
