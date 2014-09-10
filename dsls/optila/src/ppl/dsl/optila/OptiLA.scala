package ppl.dsl.optila

import java.io._
import scala.reflect.SourceContext
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
import ppl.dsl.optila.extern._
import ppl.dsl.optila.vector._
import ppl.dsl.optila.matrix._
import ppl.dsl.optila.capabilities._
import ppl.dsl.optila.io._
import ppl.dsl.optila.generic.GenericDefs


/**
 * These separate OptiLA applications from the Exp world.
 */

// ex. object GDARunner extends OptiLAApplicationRunner with GDA
trait OptiLAApplicationRunner extends OptiLAApplication with DeliteApplication with OptiLAExp

// ex. trait GDA extends OptiLAApplication
trait OptiLAApplication extends OptiLA with OptiLALift {
  var args: Rep[Array[String]]
  def main(): Unit
}

trait OptiLAInteractive extends OptiLAApplication with DeliteInteractive

trait OptiLAInteractiveRunner[R] extends OptiLAApplicationRunner with DeliteInteractiveRunner[R]

object OptiLA {
  def apply[R](b: => R) = new Scope[OptiLAInteractive, OptiLAInteractiveRunner[R], R](b)
}


/**
 * These are the portions of Scala imported into OptiLA's scope.
 */
trait OptiLALift extends LiftVariables with LiftEquals with LiftString with LiftBoolean /*with LiftNumeric*/ with LiftPrimitives {
  this: OptiLA =>
}

trait OptiLAScalaOpsPkg extends Base
  with Equal with IfThenElse with Variables with While with TupledFunctions
  with ImplicitOps with OrderingOps with StringOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with CastingOps with ObjectOps with IOOps
  // only included because of args. TODO: investigate passing args as a vector
  with ArrayOps with ExceptionOps with RangeOps

trait OptiLAScalaOpsPkgExp extends OptiLAScalaOpsPkg with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with TupleOpsExp with TupledFunctionsExp
  with ImplicitOpsExp with OrderingOpsExp with StringOpsExp with RangeOpsExp with IOOpsExp
  with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExpOpt with MiscOpsExp 
  with ListOpsExp with SeqOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp
  with SynchronizedArrayBufferOpsExp with HashMapOpsExp with IterableOpsExp with ArrayBufferOpsExp with ExceptionOpsExp

trait OptiLAScalaCodeGenPkg extends ScalaGenDSLOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenTupleOps with ScalaGenTupledFunctions
  with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps
  with ScalaGenArrayOps with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps 
  with ScalaGenListOps with ScalaGenSeqOps with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenSetOps with ScalaGenObjectOps
  with ScalaGenSynchronizedArrayBufferOps with ScalaGenHashMapOps with ScalaGenIterableOps with ScalaGenArrayBufferOps with ScalaGenExceptionOps
  { val IR: OptiLAScalaOpsPkgExp  }

trait OptiLACudaCodeGenPkg extends CudaGenDSLOps with CudaGenImplicitOps with CudaGenOrderingOps
  with CudaGenEqual with CudaGenIfThenElse with CudaGenVariables with CudaGenWhile with CudaGenTupleOps /*with CudaGenTupledFunctions*/
  with CudaGenStringOps with CudaGenRangeOps with CudaGenIOOps with CudaGenArrayOps with CudaGenBooleanOps
  with CudaGenPrimitiveOps with CudaGenMiscOps
  with CudaGenListOps with CudaGenSeqOps with CudaGenMathOps with CudaGenCastingOps with CudaGenSetOps with CudaGenObjectOps
  with CudaGenSynchronizedArrayBufferOps with CudaGenHashMapOps with CudaGenIterableOps with CudaGenArrayBufferOps with CudaGenExceptionOps
  { val IR: OptiLAScalaOpsPkgExp  }

trait OptiLAOpenCLCodeGenPkg extends OpenCLGenDSLOps with OpenCLGenImplicitOps with OpenCLGenOrderingOps
  with OpenCLGenEqual with OpenCLGenIfThenElse with OpenCLGenVariables with OpenCLGenWhile /*with OpenCLGenFunctions*/
  with OpenCLGenStringOps with OpenCLGenRangeOps with OpenCLGenIOOps with OpenCLGenArrayOps with OpenCLGenBooleanOps
  with OpenCLGenPrimitiveOps with OpenCLGenMiscOps //with OpenCLGenTupleOps
  with OpenCLGenListOps with OpenCLGenSeqOps with OpenCLGenMathOps with OpenCLGenCastingOps with OpenCLGenSetOps with OpenCLGenObjectOps
  with OpenCLGenSynchronizedArrayBufferOps with OpenCLGenHashMapOps with OpenCLGenIterableOps with OpenCLGenArrayBufferOps
  { val IR: OptiLAScalaOpsPkgExp  }

trait OptiLACCodeGenPkg extends CGenDSLOps with CGenImplicitOps with CGenOrderingOps
  with CGenEqual with CGenIfThenElse with CGenVariables with CGenWhile with CGenTupleOps with CGenTupledFunctions
  with CGenStringOps with CGenRangeOps with CGenIOOps with CGenArrayOps with CGenBooleanOps
  with CGenPrimitiveOps with CGenMiscOps
  with CGenListOps with CGenSeqOps with CGenMathOps with CGenCastingOps with CGenSetOps with CGenObjectOps
  with CGenSynchronizedArrayBufferOps with CGenHashMapOps with CGenIterableOps with CGenArrayBufferOps with CGenExceptionOps
  { val IR: OptiLAScalaOpsPkgExp  }

/**
 * This the trait that every OptiLA application must extend.
 */
trait OptiLA extends OptiLATypes with Interfaces with OptiLAScalaOpsPkg with StructOps with DeliteCollectionOps with DeliteArrayOps with DeliteFileReaderOps with DeliteMapOps
  with GenericDefs with LanguageOps with ArithOps with CloneableOps with HasMinMaxOps
  with VectorOps with DenseVectorOps with SparseVectorOps with RangeVectorOps with DenseVectorViewOps with SparseVectorViewOps //with MatrixRowOps with MatrixColOps
  with MatrixOps with MatrixBuildableOps with DenseMatrixOps with SparseMatrixOps with SparseMatrixBuildableOps
  with LAInputReaderOps with LAOutputWriterOps {

  this: OptiLAApplication =>
}

trait OptiLACompiler extends OptiLA with OptiLAUtilities 
  // -- ops only available to the compiler (they are restricted from application use)
  with DeliteArrayCompilerOps 
  with DenseVectorCompilerOps with SparseVectorCompilerOps with SparseVectorViewCompilerOps with DenseMatrixCompilerOps with SparseMatrixCompilerOps with SparseMatrixBuildableCompilerOps
  with MathOps with RangeOps with IOOps with SeqOps with SetOps with ListOps with HashMapOps with IterableOps with ArrayBufferOps with ExceptionOps 
  // --  kernel implementations
  with LanguageImplOpsStandard
  with VectorImplOpsStandard with DenseVectorImplOpsStandard with SparseVectorImplOpsStandard with DenseVectorViewImplOpsStandard with SparseVectorViewImplOps
  with MatrixImplOpsStandard with DenseMatrixImplOpsStandard with SparseMatrixImplOps with SparseMatrixBuildableImplOps
  with LAInputReaderImplOpsStandard with LAOutputWriterImplOpsStandard
  // -- designates the choice of sparse matrix formats for SparseMatrix and SparseMatrixBuildable respectively
  with SparseMatrixCSRCompilerOps with SparseMatrixCSRImplOps with SparseMatrixCOOCompilerOps with SparseMatrixCOOImplOps with SparseVectorViewCSRImplOps {
  
  this: OptiLAApplication with OptiLAExp =>
}


/**
 * These are the corresponding IR nodes for OptiLA.
 */
trait OptiLAExp extends OptiLACompiler with InterfacesExp with OptiLAScalaOpsPkgExp with FunctionBlocksExp with DeliteOpsExp with DeliteArrayFatExp with StructExp with DeliteFileReaderOpsExp with DeliteMapOpsExp
  with LanguageOpsExp with ArithOpsExpOpt with CloneableOpsExp
  with VectorOpsExpOpt with DenseVectorOpsExpOpt with SparseVectorOpsExp with RangeVectorOpsExp with DenseVectorViewOpsExpOpt with SparseVectorViewOpsExpOpt //with MatrixRowOpsExpOpt with MatrixColOpsExpOpt
  with MatrixOpsExpOpt with DenseMatrixOpsExpOpt with SparseMatrixOpsExp with SparseMatrixBuildableOpsExp
  with LAInputReaderOpsExp with LAOutputWriterOpsExp
  with ExceptionOpsExp
  // -- choice of sparse matrix repr
  with SparseMatrixCSROpsExp with SparseMatrixCOOOpsExp with SparseVectorViewCSROpsExp
  with ExpressionsOpt with DeliteTransform with MultiloopSoATransformExp with DeliteAllOverridesExp {

  // this: OptiLAApplicationRunner => why doesn't this work?
  this: DeliteApplication with OptiLAApplication with OptiLAExp => // can't be OptiLAApplication right now because code generators depend on stuff inside DeliteApplication (via IR)

  def getCodeGenPkg(t: Target{val IR: OptiLAExp.this.type}) : GenericFatCodegen{val IR: OptiLAExp.this.type} = {
    t match {
      case _:TargetScala => new OptiLACodeGenScala{val IR: OptiLAExp.this.type = OptiLAExp.this}
      case _:TargetCuda => new OptiLACodeGenCuda{val IR: OptiLAExp.this.type = OptiLAExp.this}
      case _:TargetOpenCL => new OptiLACodeGenOpenCL{val IR: OptiLAExp.this.type = OptiLAExp.this}
      case _:TargetCpp => new OptiLACodeGenC{val IR: OptiLAExp.this.type = OptiLAExp.this}
      case _ => err("optila does not support this target")
    }
  }  
}

trait OptiLAUtilities {  
  def err(s: String)(implicit ctx: SourceContext) = {
    println("[optila error]: " + s)
    println("  at " + (ctx.fileName.split("/").last + ":" + ctx.line).mkString("//").mkString(";"))
    exit(1)
  }
  def warn(s: String) = println("[optila warning]: " + s)  
}


/**
 * OptiLA code generators
 */
trait OptiLACodeGenBase extends GenericFatCodegen with SchedulingOpt {

  val IR: DeliteApplication with OptiLAExp
  override def initialDefs = IR.deliteGenerator.availableDefs

  def dsmap(line: String) = line

  val specialize = Set[String]()
  val specialize2 = Set[String]()
  def genSpec(f: File, outPath: String) = {}

  def getFiles(d: File): Array[File] = {
    d.listFiles flatMap { f => if (f.isDirectory()) getFiles(f) else Array(f) }
  }
  
  override def emitDataStructures(path: String) {
    val s = File.separator
    val dsRoot = Config.homeDir + s+"dsls"+s+"optila"+s+"src"+s+"ppl"+s+"dsl"+s+"optila"+s+"datastruct"+s + this.toString

    val dsDir = new File(dsRoot)
    if (!dsDir.exists) return
    val outDir = new File(path)
    outDir.mkdirs()

    val files = getFiles(dsDir)    
    for (f <- files) {
      if (f.isDirectory){
        emitDataStructures(f.getPath())
      }
      else {
        if (specialize contains (f.getName.substring(0, f.getName.indexOf(".")))) {
          genSpec(f, path)
        }
        val outFile = path + s + f.getName
        val out = new BufferedWriter(new FileWriter(outFile))
        for (line <- scala.io.Source.fromFile(f).getLines) {
          out.write(dsmap(line) + "\n")
        }
        out.close()
      }
    }
  }
}

trait OptiLACodeGenScala extends OptiLACodeGenBase with OptiLAScalaCodeGenPkg with OptiLAScalaGenExternal 
  with ScalaGenDeliteOps with ScalaGenDeliteCollectionOps with ScalaGenDeliteStruct with ScalaGenDeliteArrayOps with ScalaGenDeliteArrayBufferOps with ScalaGenDeliteFileReaderOps with ScalaGenDeliteMapOps
  with ScalaGenLanguageOps with ScalaGenArithOps with ScalaGenVectorOps with ScalaGenDenseVectorOps with ScalaGenSparseVectorOps
  with ScalaGenDenseVectorViewOps with ScalaGenSparseVectorViewOps with ScalaGenMatrixOps with ScalaGenDenseMatrixOps with ScalaGenSparseMatrixOps with ScalaGenSparseMatrixBuildableOps  
  //with ScalaGenMatrixRowOps with ScalaGenMatrixColOps
  // -- choice of sparse matrix repr
  with ScalaGenSparseMatrixCSROps with ScalaGenSparseMatrixCOOOps with ScalaGenSparseVectorViewCSROps
  with DeliteScalaGenAllOverrides { //with ScalaGenMLInputReaderOps {
  
  val IR: DeliteApplication with OptiLAExp

  override val specialize = Set("DenseVector", "DenseMatrix", "SparseVector", "SparseMatrixCSR", "SparseMatrixCOO", "DenseVectorView", "SparseVectorViewCSR")

  override def genSpec(f: File, dsOut: String) {
    for (s <- List("Double","Int","Float","Long","Boolean")) {
      val outFile = dsOut + s + f.getName
      val out = new BufferedWriter(new FileWriter(outFile))
      for (line <- scala.io.Source.fromFile(f).getLines) {
        out.write(specmap(line, s) + "\n")
      }
      out.close()
    }
  }

  def specmap(line: String, t: String) : String = {
    var res = line.replaceAll("object ", "object " + t)
    res = res.replaceAll("import ", "import " + t)
    res = res.replaceAll("@specialized T: Manifest", t)
    res = res.replaceAll("T:Manifest", t)
    res = res.replaceAll("\\bT\\b", t)
    dsmap(res)
  }

  override def remap(s: String) = parmap(s)
  override def remap[A](m: Manifest[A]): String = {
    var res = super.remap(m)
    res = res.replaceAllLiterally("package$", "")
    dsmap(res)
  }

  def parmap(line: String): String = {
    var res = line
    for(tpe1 <- List("Int","Long","Double","Float","Boolean")) {
      for (s <- specialize) {
        res = res.replaceAll(s+"\\["+tpe1+"\\]", tpe1+s)
      }
    }
    res
  }

  override def dsmap(line: String) : String = {
    // AKS TODO: fix remap to handle stuff like
    // ppl.tests.scalatest.dsl.optiml.SparseSimpleMatrixArithmeticRunner$@5a3aa30b.type#generated.scala.OptiLATypes$DoubleSparseVector
    
    var res = line.replaceAll("ppl.dsl.optila.datastruct", "generated")
    res = res.replaceAll("ppl.delite.framework.datastruct", "generated")
    res = res.replaceAll(".*\\$", "generated.scala.")
    parmap(res)
  }
}


trait OptiLACodeGenCuda extends OptiLACudaCodeGenPkg with OptiLACodeGenBase with OptiLACudaGenExternal 
  with CudaGenDeliteOps with CudaGenDeliteCollectionOps with CudaGenDeliteStruct with CudaGenDeliteArrayOps with CudaGenDeliteArrayBufferOps 
  /*with CudaGenLanguageOps*/ with CudaGenArithOps with CudaGenVectorOps with CudaGenDenseVectorOps with CudaGenSparseVectorOps
  with CudaGenDenseVectorViewOps with CudaGenSparseVectorViewOps with CudaGenMatrixOps with CudaGenDenseMatrixOps with CudaGenSparseMatrixOps with CudaGenSparseMatrixBuildableOps  
  with CudaGenSparseMatrixCSROps with CudaGenSparseMatrixCOOOps with CudaGenSparseVectorViewCSROps
  with DeliteCudaGenAllOverrides with DeliteCppHostTransfer with OptiLACppHostTransfer with DeliteCudaDeviceTransfer with OptiLACudaDeviceTransfer { //with CudaGenMLInputReaderOps  //TODO:DeliteCodeGenOverrideScala needed?
  val IR: DeliteApplication with OptiLAExp

  import IR._
}

trait OptiLACodeGenOpenCL extends OptiLACodeGenBase with OptiLAOpenCLCodeGenPkg with OptiLAOpenCLGenExternal with OpenCLGenDeliteOps
  with OpenCLGenArithOps with OpenCLGenVectorOps with OpenCLGenDenseVectorOps with OpenCLGenDenseVectorViewOps with OpenCLGenMatrixOps with OpenCLGenDenseMatrixOps 
  with OpenCLGenDeliteCollectionOps with OpenCLGenDeliteArrayOps
  with DeliteOpenCLGenAllOverrides
{
  val IR: DeliteApplication with OptiLAExp
  import IR._
}

trait OptiLACodeGenC extends OptiLACodeGenBase with OptiLACCodeGenPkg with OptiLACGenExternal  
  with CGenDeliteOps with CGenDeliteCollectionOps with CGenDeliteStruct with CGenDeliteArrayOps with CGenDeliteFileReaderOps /*with CGenDeliteArrayBufferOps*/ with CGenDeliteMapOps
  /*with CGenLanguageOps*/ with CGenArithOps with CGenVectorOps with CGenDenseVectorOps with CGenSparseVectorOps
  with CGenDenseVectorViewOps /*with CGenSparseVectorViewOps*/ with CGenMatrixOps with CGenDenseMatrixOps with CGenSparseMatrixOps with CGenSparseMatrixBuildableOps  
  with CGenSparseMatrixCSROps with CGenSparseMatrixCOOOps /*with CGenSparseVectorViewCSROps*/
  with DeliteCGenAllOverrides with DeliteCppHostTransfer with OptiLACppHostTransfer
{
  val IR: DeliteApplication with OptiLAExp
  import IR._
}
