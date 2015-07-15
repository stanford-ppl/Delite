package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.internal.GenericFatCodegen

import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.hw._
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.cpp.TargetCpp

import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.ops._

// SMAL - Small Multidimensional Array Language

// --- SMAL Ops
trait SMALOps extends DeliteDSLOps
  with MultiArrayOps with SimpleProfileOps {
  this: SMALApp =>
}

trait SMALOpsExp extends DeliteDSLOpsExp with SMALOps
  with MultiArrayTransform with DeliteSimpleOpsExp with SimpleProfileOpsExp {
  this: SMALCompiler => 

  override def getCodeGenPkg(t: Target{val IR: SMALOpsExp.this.type}) : GenericFatCodegen{val IR: SMALOpsExp.this.type} = t match {
    case _:TargetScala => new ScalaGenSMAL{val IR: SMALOpsExp.this.type = SMALOpsExp.this}
    case _:TargetCuda => new CudaGenSMAL{val IR: SMALOpsExp.this.type = SMALOpsExp.this}
    case _:TargetCpp => new CGenSMAL{val IR: SMALOpsExp.this.type = SMALOpsExp.this}
    case _:TargetHw => new HwGenSMAL{val IR: SMALOpsExp.this.type = SMALOpsExp.this}
    case _ => throw new Exception("SMAL does not support this target")
  }  
}

// --- SMAL Code Generators
trait ScalaGenSMAL extends ScalaGenDeliteDSL
  with ScalaGenMultiArray   with ScalaGenSimpleProfileOps
  { val IR: SMALOpsExp }

trait CudaGenSMAL extends CudaGenDeliteDSL
  with CudaGenMultiArray    with CudaGenSimpleProfileOps
  { val IR: SMALOpsExp }

trait CGenSMAL extends CGenDeliteDSL
  with CGenMultiArray       with CGenSimpleProfileOps
  { val IR: SMALOpsExp }

trait HwGenSMAL extends HwGenDeliteDSL
  /*with HWGenMultiArray*/  /*with HWGenSimpleProfileOps*/
  { val IR: SMALOpsExp }

// --- SMAL Stubs
trait SMALCompiler extends SMALOpsExp with DeliteDSLCompiler with SMALApp
trait SMALApp extends DeliteDSLApplication with SMALOps

