package ppl.delite.framework.datastructures

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.virtualization.lms.common._

trait RecordOps extends StructOps

trait RecordOpsExp extends RecordOps with StructExp

trait ScalaGenRecordOps extends ScalaGenStruct
