package ppl.delite.framework

import codegen.Target
import collection.mutable.ListBuffer
import datastructures.Field
import ops.Op

class DSLTypeRepresentation(val name: String) {

  val fields = new ListBuffer[Field]()
  val ops = new ListBuffer[Op]()
  val targets = new ListBuffer[Target]()
}