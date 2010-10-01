package ppl.delite.framework.codegen

import collection.mutable.{HashMap, ListBuffer}

object Target {
  val targets = new HashMap[String, Target]

  def addTarget(tgt: Target) {
    targets += tgt.name -> tgt
  }

  def addGeneratorToTarget(tgtname: String, gen: CodeGenerator) {
    val tgt = targets.get(tgtname).getOrElse{
      throw new RuntimeException("Accessing a non-exisiting Target")
    }
    tgt.generators += genb
  }
}

/**
 * This trait encodes a target for code generation, the target can have multiple code generators registered
 */
abstract class Target {
  val name: String
  val generators = new ListBuffer[CodeGenerator]
}