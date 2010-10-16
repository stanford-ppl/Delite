package ppl.dsl.simple

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.embedded.scala.ScalaOpsPkgExp3

trait OptiML2 extends ScalaOpsPkgExp3 with VectorOpsExp2 with MatrixOpsExp2 { this: DeliteApplication => }