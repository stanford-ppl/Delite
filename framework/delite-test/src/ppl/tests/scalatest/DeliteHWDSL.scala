package ppl.tests.scalatest

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}
import ppl.delite.framework.{Config, ExpressionsOpt, SchedulingOpt, DeliteApplication, DeliteInteractive, DeliteInteractiveRunner}
import ppl.delite.framework.datastructures._
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.hw._
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.cpp.TargetCpp
import ppl.delite.framework.codegen.opencl.TargetOpenCL
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.transform._
import ppl.delite.framework.ops._
import ppl.delite.framework.visit._
import ppl.delite.framework.{Interfaces,InterfacesExp}

trait DeliteHWDSLApplicationRunner extends DeliteHWDSLApplication with DeliteApplication with DeliteHWDSLExp

trait DeliteHWDSLApplication extends DeliteHWDSL with DeliteHWDSLLift {
  var args: Rep[Array[String]]
  def main(): Unit
}


trait DeliteHWDSLLift extends LiftVariables with LiftEquals with LiftString with LiftBoolean with LiftNumeric with LiftPrimitives {
  this: DeliteHWDSL =>
}

trait DeliteHWDSLScalaOpsPkg extends Base
  with Equal with IfThenElse with Variables with While with TupledFunctions
  with ImplicitOps with OrderingOps with StringOps with RangeOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with CastingOps with ObjectOps with IOOps
  with ArrayOps with ExceptionOps

trait DeliteHWDSLScalaOpsPkgExp extends DeliteHWDSLScalaOpsPkg with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with TupleOpsExp with TupledFunctionsExp
  with ImplicitOpsExp with OrderingOpsExp with StringOpsExp with RangeOpsExp with IOOpsExp
  with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExpOpt with MiscOpsExp 
  with ListOpsExp with SeqOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp
  with SynchronizedArrayBufferOpsExp with HashMapOpsExp with IterableOpsExp with ArrayBufferOpsExp with ExceptionOpsExp

trait DeliteHWDSLScalaCodeGenPkg extends ScalaGenDSLOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenTupleOps with ScalaGenTupledFunctions
  with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps
  with ScalaGenArrayOps with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps 
  with ScalaGenListOps with ScalaGenSeqOps with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenSetOps with ScalaGenObjectOps
  with ScalaGenSynchronizedArrayBufferOps with ScalaGenHashMapOps with ScalaGenIterableOps with ScalaGenArrayBufferOps with ScalaGenExceptionOps
  { val IR: DeliteHWDSLScalaOpsPkgExp  }

trait DeliteHWDSLCudaCodeGenPkg extends CudaGenDSLOps with CudaGenImplicitOps with CudaGenOrderingOps
  with CudaGenEqual with CudaGenIfThenElse with CudaGenVariables with CudaGenWhile with CudaGenTupleOps /*with CudaGenTupledFunctions*/
  with CudaGenStringOps with CudaGenRangeOps with CudaGenIOOps with CudaGenArrayOps with CudaGenBooleanOps
  with CudaGenPrimitiveOps with CudaGenMiscOps
  with CudaGenListOps with CudaGenSeqOps with CudaGenMathOps with CudaGenCastingOps with CudaGenSetOps with CudaGenObjectOps
  with CudaGenSynchronizedArrayBufferOps with CudaGenHashMapOps with CudaGenIterableOps with CudaGenArrayBufferOps with CudaGenExceptionOps
  { val IR: DeliteHWDSLScalaOpsPkgExp  }

trait DeliteHWDSLOpenCLCodeGenPkg extends OpenCLGenDSLOps with OpenCLGenImplicitOps with OpenCLGenOrderingOps
  with OpenCLGenEqual with OpenCLGenIfThenElse with OpenCLGenVariables with OpenCLGenWhile /*with OpenCLGenFunctions*/
  with OpenCLGenStringOps with OpenCLGenRangeOps with OpenCLGenIOOps with OpenCLGenArrayOps with OpenCLGenBooleanOps
  with OpenCLGenPrimitiveOps with OpenCLGenMiscOps //with OpenCLGenTupleOps
  with OpenCLGenListOps with OpenCLGenSeqOps with OpenCLGenMathOps with OpenCLGenCastingOps with OpenCLGenSetOps with OpenCLGenObjectOps
  with OpenCLGenSynchronizedArrayBufferOps with OpenCLGenHashMapOps with OpenCLGenIterableOps with OpenCLGenArrayBufferOps
  { val IR: DeliteHWDSLScalaOpsPkgExp  }

trait DeliteHWDSLCCodeGenPkg extends CGenDSLOps with CGenImplicitOps with CGenOrderingOps
  with CGenEqual with CGenIfThenElse with CGenVariables with CGenWhile with CGenTupleOps with CGenTupledFunctions
  with CGenStringOps with CGenRangeOps with CGenIOOps with CGenArrayOps with CGenBooleanOps
  with CGenPrimitiveOps with CGenMiscOps
  with CGenListOps with CGenSeqOps with CGenMathOps with CGenCastingOps with CGenSetOps with CGenObjectOps
  with CGenSynchronizedArrayBufferOps with CGenHashMapOps with CGenIterableOps with CGenArrayBufferOps with CGenExceptionOps
  { val IR: DeliteHWDSLScalaOpsPkgExp  }

/*
 * Every trait here is described in LMS for other backends
 */
trait DeliteHWDSLHwCodeGenPkg
  extends HwGenOrderingOps
//    with HwGenEqual
//    with HwGenIfTheElse
    with HwGenVariables
    with HwGenWhile
//    with HwGenTupleOps
//    with HwGenTupledFunctions
    with HwGenStringOps
    with HwGenRangeOps
//    with HwGenIOOps
    with HwGenArrayOps
    with HwGenBooleanOps
    with HwGenPrimitiveOps
//    with HwGenMiscOps
//    with HwGenListOps
//    with HwGenSeqOps
//    with HwGenMathOps
//    with HwGenCastingOps
//    with HwGenSetOps
    with HwGenObjectOps
//    with HwGenSynchronizedArrayBufferOps
//    with HwGenHashMapOps
//    with HwGenIterableOps
//    with HwGenArrayBufferOps
//    with HwGenExceptionOps
  { val IR: DeliteHWDSLScalaOpsPkgExp  }


trait DeliteHWDSL extends DeliteHWDSLScalaOpsPkg with StructOps with DeliteArrayOps with DeliteArrayBufferOps with DeliteMapOps with DeliteFileReaderOps  with DeliteAnalysesOps {
  this: DeliteHWDSLApplication =>
}

trait DeliteHWDSLCompiler extends DeliteHWDSL 
  with DeliteArrayCompilerOps with DeliteArrayBufferCompilerOps {
  this: DeliteHWDSLApplication with DeliteHWDSLExp =>
}

trait DeliteHWDSLTransformExp extends DeliteVisit
//    with MultiloopSoATransformWithReduceExp -- Removing SOA transform on reduces for now (needs to be fixed)
    with MultiloopSoATransformExp
    with HwLoweringTransformExp
    with DotPrintTransformExp

trait DeliteHWDSLExp extends DeliteHWDSLCompiler with DeliteHWDSLScalaOpsPkgExp 
  with FunctionBlocksExp with DeliteStructsExp with DeliteOpsExp with DeliteArrayFatExp with DeliteArrayBufferOpsExp with DeliteMapOpsExp with DeliteFileReaderOpsExp
  with ExpressionsOpt with DeliteHWDSLTransformExp with DeliteTestOpsExp with DeliteLMSForwarderExp with DeliteAllOverridesExp {

  this: DeliteApplication with DeliteHWDSLApplication with DeliteHWDSLExp =>

  def getCodeGenPkg(t: Target{val IR: DeliteHWDSLExp.this.type}) : GenericFatCodegen{val IR: DeliteHWDSLExp.this.type} = {
    t match {
      case _:TargetScala => new DeliteHWDSLCodeGenScala{val IR: DeliteHWDSLExp.this.type = DeliteHWDSLExp.this}
      case _:TargetCuda => new DeliteHWDSLCodeGenCuda{val IR: DeliteHWDSLExp.this.type = DeliteHWDSLExp.this}
      case _:TargetOpenCL => new DeliteHWDSLCodeGenOpenCL{val IR: DeliteHWDSLExp.this.type = DeliteHWDSLExp.this}
      case _:TargetCpp => new DeliteHWDSLCodeGenC{val IR: DeliteHWDSLExp.this.type = DeliteHWDSLExp.this}
      case _:TargetHw => new DeliteHWDSLCodeGenHw{val IR: DeliteHWDSLExp.this.type = DeliteHWDSLExp.this}
      case _ => throw new Exception("DeliteHWDSL does not support this target")
    }
  }  
}


/**
 * DeliteHWDSL code generators
 */
trait DeliteHWDSLCodeGenBase extends GenericFatCodegen with SchedulingOpt {
  val IR: DeliteApplication with DeliteHWDSLExp
  override def initialDefs = IR.deliteGenerator.availableDefs
}

trait DeliteHWDSLCodeGenScala extends DeliteHWDSLCodeGenBase with DeliteHWDSLScalaCodeGenPkg
  with ScalaGenDeliteOps with ScalaGenDeliteStruct with ScalaGenDeliteArrayOps with ScalaGenDeliteArrayBufferOps with ScalaGenDeliteMapOps with ScalaGenDeliteFileReaderOps
  with ScalaGenDeliteTest with DeliteScalaGenAllOverrides {
  
  val IR: DeliteApplication with DeliteHWDSLExp
}

trait DeliteHWDSLCodeGenCuda extends DeliteHWDSLCudaCodeGenPkg with DeliteHWDSLCodeGenBase
  with CudaGenDeliteOps with CudaGenDeliteStruct with CudaGenDeliteArrayOps with CudaGenDeliteArrayBufferOps 
  with DeliteCudaGenAllOverrides with DeliteCppHostTransfer with DeliteCudaDeviceTransfer {
  
  val IR: DeliteApplication with DeliteHWDSLExp
}

trait DeliteHWDSLCodeGenOpenCL extends DeliteHWDSLCodeGenBase with DeliteHWDSLOpenCLCodeGenPkg with OpenCLGenDeliteOps
  with OpenCLGenDeliteArrayOps /*with OpenCLGenDeliteArrayBufferOps*/
  with DeliteOpenCLGenAllOverrides {
  
  val IR: DeliteApplication with DeliteHWDSLExp
}

trait DeliteHWDSLCodeGenC extends DeliteHWDSLCodeGenBase with DeliteHWDSLCCodeGenPkg
  with CGenDeliteOps with CGenDeliteStruct with CGenDeliteArrayOps /*with CGenDeliteArrayBufferOps*/
  with DeliteCGenAllOverrides with DeliteCppHostTransfer {
  
  val IR: DeliteApplication with DeliteHWDSLExp
}

trait DeliteHWDSLCodeGenHw
  extends DeliteHWDSLCodeGenBase  /* Common to all generators */
  with DeliteHWDSLHwCodeGenPkg    /* Lump of traits usually implemented in LMS */
  with HwGenDeliteInternalOps
  with HwGenDeliteOps               /* Codegen for all Delite Ops */
  with HwGenDeliteArrayOps          /* Codegen for all Delite Array Ops */
{
  val IR: DeliteApplication with DeliteHWDSLExp
}

