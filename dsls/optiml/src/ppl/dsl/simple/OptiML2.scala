package ppl.dsl.simple

import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common.embedded.scala.ScalaOpsPkgExp

trait OptiML2 extends ScalaOpsPkgExp with VectorOpsExp2 with MatrixOpsExp2 { this: DeliteApplication => }