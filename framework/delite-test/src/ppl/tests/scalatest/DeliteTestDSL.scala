package ppl.tests.scalatest

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}
import ppl.delite.framework.{Config, ExpressionsOpt, SchedulingOpt, DeliteApplication, DeliteInteractive, DeliteInteractiveRunner}
import ppl.delite.framework.datastructures._
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.cpp.TargetCpp
import ppl.delite.framework.codegen.opencl.TargetOpenCL
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.transform._
import ppl.delite.framework.ops._
import ppl.delite.framework.{Interfaces,InterfacesExp}

trait DeliteTestDSLApplicationRunner extends DeliteTestDSLApplication with DeliteApplication with DeliteTestDSLExp

trait DeliteTestDSLApplication extends DeliteTestDSL with DeliteTestDSLLift {
  var args: Rep[Array[String]]
  def main(): Unit
}


trait DeliteTestDSLLift extends LiftVariables with LiftEquals with LiftString with LiftBoolean with LiftNumeric with LiftPrimitives {
  this: DeliteTestDSL =>
}

trait DeliteTestDSLScalaOpsPkg extends Base
  with Equal with IfThenElse with Variables with While with TupledFunctions
  with ImplicitOps with OrderingOps with StringOps with RangeOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with CastingOps with ObjectOps with IOOps
  with ArrayOps with ExceptionOps

trait DeliteTestDSLScalaOpsPkgExp extends DeliteTestDSLScalaOpsPkg with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with TupleOpsExp with TupledFunctionsExp
  with ImplicitOpsExp with OrderingOpsExp with StringOpsExp with RangeOpsExp with IOOpsExp
  with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExpOpt with MiscOpsExp 
  with ListOpsExp with SeqOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp
  with SynchronizedArrayBufferOpsExp with HashMapOpsExp with IterableOpsExp with ArrayBufferOpsExp with ExceptionOpsExp

trait DeliteTestDSLScalaCodeGenPkg extends ScalaGenDSLOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenTupleOps with ScalaGenTupledFunctions
  with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps
  with ScalaGenArrayOps with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps 
  with ScalaGenListOps with ScalaGenSeqOps with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenSetOps with ScalaGenObjectOps
  with ScalaGenSynchronizedArrayBufferOps with ScalaGenHashMapOps with ScalaGenIterableOps with ScalaGenArrayBufferOps with ScalaGenExceptionOps
  { val IR: DeliteTestDSLScalaOpsPkgExp  }

trait DeliteTestDSLCudaCodeGenPkg extends CudaGenDSLOps with CudaGenImplicitOps with CudaGenOrderingOps
  with CudaGenEqual with CudaGenIfThenElse with CudaGenVariables with CudaGenWhile with CudaGenTupleOps /*with CudaGenTupledFunctions*/
  with CudaGenStringOps with CudaGenRangeOps with CudaGenIOOps with CudaGenArrayOps with CudaGenBooleanOps
  with CudaGenPrimitiveOps with CudaGenMiscOps
  with CudaGenListOps with CudaGenSeqOps with CudaGenMathOps with CudaGenCastingOps with CudaGenSetOps with CudaGenObjectOps
  with CudaGenSynchronizedArrayBufferOps with CudaGenHashMapOps with CudaGenIterableOps with CudaGenArrayBufferOps with CudaGenExceptionOps
  { val IR: DeliteTestDSLScalaOpsPkgExp  }

trait DeliteTestDSLOpenCLCodeGenPkg extends OpenCLGenDSLOps with OpenCLGenImplicitOps with OpenCLGenOrderingOps
  with OpenCLGenEqual with OpenCLGenIfThenElse with OpenCLGenVariables with OpenCLGenWhile /*with OpenCLGenFunctions*/
  with OpenCLGenStringOps with OpenCLGenRangeOps with OpenCLGenIOOps with OpenCLGenArrayOps with OpenCLGenBooleanOps
  with OpenCLGenPrimitiveOps with OpenCLGenMiscOps //with OpenCLGenTupleOps
  with OpenCLGenListOps with OpenCLGenSeqOps with OpenCLGenMathOps with OpenCLGenCastingOps with OpenCLGenSetOps with OpenCLGenObjectOps
  with OpenCLGenSynchronizedArrayBufferOps with OpenCLGenHashMapOps with OpenCLGenIterableOps with OpenCLGenArrayBufferOps
  { val IR: DeliteTestDSLScalaOpsPkgExp  }

trait DeliteTestDSLCCodeGenPkg extends CGenDSLOps with CGenImplicitOps with CGenOrderingOps
  with CGenEqual with CGenIfThenElse with CGenVariables with CGenWhile with CGenTupleOps with CGenTupledFunctions
  with CGenStringOps with CGenRangeOps with CGenIOOps with CGenArrayOps with CGenBooleanOps
  with CGenPrimitiveOps with CGenMiscOps
  with CGenListOps with CGenSeqOps with CGenMathOps with CGenCastingOps with CGenSetOps with CGenObjectOps
  with CGenSynchronizedArrayBufferOps with CGenHashMapOps with CGenIterableOps with CGenArrayBufferOps with CGenExceptionOps
  { val IR: DeliteTestDSLScalaOpsPkgExp  }


trait DeliteTestDSL extends DeliteTestDSLScalaOpsPkg with StructOps with DeliteCollectionOps with DeliteArrayOps with DeliteArrayBufferOps with DeliteMapOps with DeliteFileReaderOps {
  this: DeliteTestDSLApplication =>
}

trait DeliteTestDSLCompiler extends DeliteTestDSL 
  with DeliteArrayCompilerOps with DeliteArrayBufferCompilerOps {
  this: DeliteTestDSLApplication with DeliteTestDSLExp =>
}

trait DeliteTestDSLExp extends DeliteTestDSLCompiler with DeliteTestDSLScalaOpsPkgExp with FunctionBlocksExp with DeliteStructsExp with DeliteOpsExp with DeliteArrayFatExp with DeliteArrayBufferOpsExp with DeliteMapOpsExp with DeliteFileReaderOpsExp
  with ExpressionsOpt with DeliteTransform with MultiloopSoATransformWithReduceExp with DeliteAllOverridesExp {

  this: DeliteApplication with DeliteTestDSLApplication with DeliteTestDSLExp =>

  def getCodeGenPkg(t: Target{val IR: DeliteTestDSLExp.this.type}) : GenericFatCodegen{val IR: DeliteTestDSLExp.this.type} = {
    t match {
      case _:TargetScala => new DeliteTestDSLCodeGenScala{val IR: DeliteTestDSLExp.this.type = DeliteTestDSLExp.this}
      case _:TargetCuda => new DeliteTestDSLCodeGenCuda{val IR: DeliteTestDSLExp.this.type = DeliteTestDSLExp.this}
      case _:TargetOpenCL => new DeliteTestDSLCodeGenOpenCL{val IR: DeliteTestDSLExp.this.type = DeliteTestDSLExp.this}
      case _:TargetCpp => new DeliteTestDSLCodeGenC{val IR: DeliteTestDSLExp.this.type = DeliteTestDSLExp.this}
      case _ => sys.error("DeliteTestDSL does not support this target")
    }
  }  
}


/**
 * DeliteTestDSL code generators
 */
trait DeliteTestDSLCodeGenBase extends GenericFatCodegen with SchedulingOpt {
  val IR: DeliteApplication with DeliteTestDSLExp
  override def initialDefs = IR.deliteGenerator.availableDefs
}

trait DeliteTestDSLCodeGenScala extends DeliteTestDSLCodeGenBase with DeliteTestDSLScalaCodeGenPkg
  with ScalaGenDeliteOps with ScalaGenDeliteCollectionOps with ScalaGenDeliteStruct with ScalaGenDeliteArrayOps with ScalaGenDeliteArrayBufferOps with ScalaGenDeliteMapOps with ScalaGenDeliteFileReaderOps
  with DeliteScalaGenAllOverrides {
  
  val IR: DeliteApplication with DeliteTestDSLExp
}

trait DeliteTestDSLCodeGenCuda extends DeliteTestDSLCudaCodeGenPkg with DeliteTestDSLCodeGenBase
  with CudaGenDeliteOps with CudaGenDeliteCollectionOps with CudaGenDeliteStruct with CudaGenDeliteArrayOps with CudaGenDeliteArrayBufferOps 
  with DeliteCudaGenAllOverrides with DeliteCppHostTransfer with DeliteCudaDeviceTransfer {
  
  val IR: DeliteApplication with DeliteTestDSLExp
}

trait DeliteTestDSLCodeGenOpenCL extends DeliteTestDSLCodeGenBase with DeliteTestDSLOpenCLCodeGenPkg with OpenCLGenDeliteOps
  with OpenCLGenDeliteCollectionOps with OpenCLGenDeliteArrayOps /*with OpenCLGenDeliteArrayBufferOps*/
  with DeliteOpenCLGenAllOverrides {
  
  val IR: DeliteApplication with DeliteTestDSLExp
}

trait DeliteTestDSLCodeGenC extends DeliteTestDSLCodeGenBase with DeliteTestDSLCCodeGenPkg
  with CGenDeliteOps with CGenDeliteCollectionOps with CGenDeliteStruct with CGenDeliteArrayOps /*with CGenDeliteArrayBufferOps*/
  with DeliteCGenAllOverrides with DeliteCppHostTransfer {
  
  val IR: DeliteApplication with DeliteTestDSLExp
}
