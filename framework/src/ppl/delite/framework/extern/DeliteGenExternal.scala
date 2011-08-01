package ppl.delite.framework.extern

import _root_.scala.virtualization.lms.internal._
import collection.mutable.{ListBuffer}
import collection.mutable.HashMap
import java.io.{FileWriter, BufferedWriter, File, PrintWriter}

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.ops._
import ppl.delite.framework.codegen.delite._

trait DeliteGenExternal extends DeliteCodegen {
  val IR: DeliteOpsExp
  import IR._

  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter): Unit = rhs match {
    // TODO: only called once per op (even across repeated occurences)
    case ThinDef(e:DeliteOpExternal[_]) =>
      var foundTarget = false
      for (g <- generators) {
        try{
           g.emitExternalLib(e)
           foundTarget = true
        }
        catch {
          case g:GenerationFailedException => 
        }
      }
      if (!foundTarget) throw new GenerationFailedException("No generator could be found for external lib: " + e)

      super.emitFatNode(sym, rhs) // pass on to DeliteGenTaskGraph
      
    case _ => super.emitFatNode(sym, rhs)
  }
  
}