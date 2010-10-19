package ppl.delite.framework.embedded.scala

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.codegen.scala.CodeGeneratorScalaBase
import java.io.PrintWriter

/**
 * Author: Kevin J. Brown
 * Date: Oct 19, 2010
 * Time: 3:57:52 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait EffectOps { this: DeliteApplication =>

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaEffects { val intermediate: EffectOps.this.type = EffectOps.this })
}

trait CodeGeneratorScalaEffects extends CodeGeneratorScalaBase {
  val intermediate: DeliteApplication
  import intermediate._

  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Boolean = {
    rhs match {
      case Reflect(s, effects) => intermediate.targets.get("Scala").get.emitTargetNode(sym, s)
      case Reify(s, effects) => //just ignore -- handled by emitBlock
      case _ => return false
    }
    true
  }
}