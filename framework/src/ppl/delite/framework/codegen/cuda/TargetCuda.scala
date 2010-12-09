package ppl.delite.framework.codegen.cuda

import ppl.delite.framework.codegen.Target

trait TargetCuda extends Target {
  import IR._

  val name = "Cuda"
}