package ppl.delite.framework.extern

import _root_.scala.virtualization.lms.internal._
import collection.mutable.{ListBuffer, HashMap, HashSet}
import java.io.{FileWriter, BufferedWriter, File, PrintWriter}

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.ops._
import ppl.delite.framework.codegen.delite._

trait DeliteGenExternal extends DeliteCodegen {
  val IR: DeliteOpsExp
  import IR._

  val generatedOps = HashSet[String]()

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case e:DeliteOpExternal[_] if !generatedOps.contains(e.funcName) =>
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

      generatedOps += e.funcName
      super.emitNode(sym, rhs) // pass on to DeliteGenTaskGraph
      
    case _ => super.emitNode(sym, rhs)
  }
  
}
