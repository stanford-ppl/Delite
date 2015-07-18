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

// PPL - Parallel Pattern Language

// --- PPL Ops
trait PPLOps extends DeliteDSLOps
  with FlattenedArrayOps with SimpleProfileOps with PPLBlockingOps {
  this: PPLApp =>
}

trait PPLOpsExp extends DeliteDSLOpsExp with PPLOps
  with FlattenedArrayOpsExp with DeliteSimpleOpsExp with PPLBlockingOpsExp with SimpleProfileOpsExp {
  this: PPLCompiler => 

  override def getCodeGenPkg(t: Target{val IR: PPLOpsExp.this.type}) : GenericFatCodegen{val IR: PPLOpsExp.this.type} = t match {
    case _:TargetScala => new ScalaGenPPL{val IR: PPLOpsExp.this.type = PPLOpsExp.this}
    case _:TargetCuda => new CudaGenPPL{val IR: PPLOpsExp.this.type = PPLOpsExp.this}
    case _:TargetCpp => new CGenPPL{val IR: PPLOpsExp.this.type = PPLOpsExp.this}
    case _:TargetHw => new HwGenPPL{val IR: PPLOpsExp.this.type = PPLOpsExp.this}
    case _ => throw new Exception("PPL does not support this target")
  }  
}

// --- PPL Code Generators
trait ScalaGenPPL extends ScalaGenDeliteDSL
  with ScalaGenSimpleProfileOps with ScalaGenPPLBlocking
  { val IR: PPLOpsExp }

trait CudaGenPPL extends CudaGenDeliteDSL
  with CudaGenSimpleProfileOps
  { val IR: PPLOpsExp }

trait CGenPPL extends CGenDeliteDSL
  with CGenSimpleProfileOps
  { val IR: PPLOpsExp }

trait HwGenPPL extends HwGenDeliteDSL
  /*with HWGenMultiArray*/  /*with HWGenSimpleProfileOps*/
  { val IR: PPLOpsExp }

// --- PPL Stubs
trait PPLCompiler extends PPLOpsExp with DeliteDSLCompiler with PPLApp
trait PPLApp extends DeliteDSLApplication with PPLOps
