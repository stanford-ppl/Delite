package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.internal.{GenericFatCodegen, IRPrinter}

import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.hw._
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.cpp.TargetCpp

import ppl.delite.framework.Config
import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.ops._

// PPL - Parallel Pattern Language

// --- PPL Ops
trait PPLOps extends DeliteDSLOps
  with FlattenedArrayOps with FlattenedArrayIO with PPLNestedOps with AbstractGroupByReduceOps {
  this: PPLApp =>

  // Configuration file for test input dimensions (matmult, outer product)
  val CONFIG_FILE: Rep[String] = unit("/home/david/PPL/hyperdsl/delite/asplos/apps/config.txt")
  // Folder for benchmark datasets
  val DATA_FOLDER: Rep[String] = unit("/home/david/PPL/data/")
}

abstract class CompileStop extends IRPrinter {
  override val name = "STOP"
  override def postprocess[A:Manifest](b: Block[A]): Block[A] = { 
    sys.exit
    b 
  }
}

trait PPLOpsExp extends PPLOps 
  with StripMiningExp with PatternPromotingExp with PatternFlatteningExp /*with ReductionPromotingExp*/
  with SliceInterchangingExp with SlicePushingExp with AbstractGroupByReduceExp
  with FlattenedArrayOpsExpOpt with ManualFatLoopNestOpsExp with DeliteDSLOpsExp 
  with PPLNestedOpsExp with DeliteSimpleOpsExp with SimpleProfileOpsExp {
  this: PPLCompiler => 

  val stop = new CompileStop{val IR: PPLOpsExp.this.type = PPLOpsExp.this}
  val printer = new IRPrinter{val IR: PPLOpsExp.this.type = PPLOpsExp.this}

  if (Config.debugCodegen) {
    appendVisitor(printer)
  }

  // TODO: These should eventually move back to their respective traits
  if (Config.blockLoops > 0) appendVisitor(stripMiner)
  if (Config.blockLoops > 0 && Config.debugCodegen) {
    appendVisitor(printer)
  }

  if (Config.blockLoops > 1) appendVisitor(patternPromotion)
  if (Config.blockLoops > 1 && Config.debugCodegen) {
    appendVisitor(printer)
  }

  if (Config.blockLoops > 2) appendVisitor(patternFlatten)
  if (Config.blockLoops > 2 && Config.debugCodegen) {
    appendVisitor(printer)
  }

  // These aren't really blocking transformations...
  if (Config.blockLoops > 0) appendVisitor(slicePush)  
  if (Config.blockLoops > 0 && Config.debugCodegen) {
    appendVisitor(printer)
  }

  if (Config.blockLoops > 0) appendVisitor(sliceInterchange)
  if (Config.blockLoops > 0 && Config.debugCodegen) {
    appendVisitor(printer)
  }

  appendVisitor(applyLowering)  // Always run apply lowering (can't codegen otherwise)
  if (Config.debugCodegen) {
    appendVisitor(printer)
  }

  //appendVisitor(stop)
  if (Config.soaEnabled) appendVisitor(soaTransform)
  if (Config.soaEnabled && Config.debugCodegen) {
    appendVisitor(printer)
  }

  // TODO: Check Config to see if we're generating HW?
  appendVisitor(lowerGroupByReduce) // Enable for CPU tests only (only used in kNN)
  if (Config.debugCodegen) {
    appendVisitor(printer)
  }

  override def getCodeGenPkg(t: Target{val IR: PPLOpsExp.this.type}) : GenericFatCodegen{val IR: PPLOpsExp.this.type} = t match {
    case _:TargetScala => new ScalaGenPPL{val IR: PPLOpsExp.this.type = PPLOpsExp.this}
    //case _:TargetCuda => new CudaGenPPL{val IR: PPLOpsExp.this.type = PPLOpsExp.this}
    //case _:TargetCpp => new CGenPPL{val IR: PPLOpsExp.this.type = PPLOpsExp.this}
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
