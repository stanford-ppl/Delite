package ppl.delite.framework.datastructures

import java.io.{File,FileWriter,PrintWriter}
import scala.virtualization.lms.common.ScalaGenStructBase

trait ScalaGenDeliteStruct extends ScalaGenStructBase {
  override def emitDataStructures(path: String) {
    val out = new PrintWriter(new FileWriter(path + File.separator + "Structs.scala"))
    out.println("package generated.scala")
    emitDataStructures(out)
    out.close()
    super.emitDataStructures(path)
  }
}
