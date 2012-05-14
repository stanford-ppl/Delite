package ppl.dsl.optiql.ops

import scala.virtualization.lms.common._


trait ResultOps extends StructOps {
  abstract class Result extends Record
}

trait ResultOpsExp extends ResultOps with BaseExp

trait ScalaGenResultOps extends ScalaGenBase {
    val IR:ResultOpsExp
    import IR._
}
