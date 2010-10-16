package ppl.delite.framework

import codegen.Target
import datafields.Field
import ops.Op

class DSLTypeRepresentation(val name: String) {

  val fields: List[Field] = Nil
  val ops: List[Op] = Nil
  val targets: List[Target] = Nil
}