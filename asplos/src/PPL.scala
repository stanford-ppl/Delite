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
  with FlattenedArrayOps with PPLNestedOps {
  this: PPLApp =>

  // Configuration file for test input dimensions (matmult, outer product)
  val CONFIG_FILE: Rep[String] = unit("/home/david/PPL/hyperdsl/delite/asplos/apps/config.txt")
  // Folder for benchmark datasets
  val DATA_FOLDER: Rep[String] = unit("/home/david/PPL/data/")
}

// Note: FlattenedArrayLowerableOpsExp includes transformer, so mix-in order matters here
// In particular, it must come before DeliteDSLOpsExp (which includes SOA transformer)
trait PPLOpsExp extends FlattenedArrayOpsExpOpt with DeliteDSLOpsExp with PPLOps
  with PPLNestedOpsExp with SimpleProfileOpsExp {
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
  with ScalaGenSimpleProfileOps with ScalaGenNestedOps with ScalaGenFlattenedArrayOps
  { val IR: PPLOpsExp }

trait CudaGenPPL extends CudaGenDeliteDSL
  with CudaGenSimpleProfileOps
  { val IR: PPLOpsExp }

trait CGenPPL extends CGenDeliteDSL
  with CGenSimpleProfileOps
  { val IR: PPLOpsExp }

trait HwGenPPL extends HwCodegen with HwGenDeliteDSL
  with HwGenAsplos with HwGenDeliteOps
  with HwGenDeliteInternalOps
  with HwGenDeliteArrayOps
  /*with HWGenMultiArray*/  /*with HWGenSimpleProfileOps*/
  { val IR: PPLOpsExp }

// --- PPL Stubs
trait PPLCompiler extends PPLOpsExp with DeliteDSLCompiler with PPLApp
trait PPLApp extends DeliteDSLApplication with PPLOps
