package feattest.smal
import feattest._

import scala.reflect.SourceContext
import scala.virtualization.lms.internal.GenericFatCodegen

import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.cpp.TargetCpp

import ppl.delite.framework.Config
import ppl.delite.framework.datastructures._
import ppl.delite.framework.analysis._
import ppl.delite.framework.transform._
import ppl.delite.framework.ops._

// SMAL - Simple Multidimensional Array Language

// --- SMAL Ops
trait SMALOps extends DeliteDSLOps with SimpleProfileOps with DeliteMultiArrayOps {
  this: SMALApp =>

  // Configuration file for test input dimensions (matmult, outer product)
  //val CONFIG_FILE: Rep[String] = unit("/home/david/PPL/hyperdsl/delite/feattest/tests/config.txt")
  // Folder for benchmark datasets
  val DATA_FOLDER: Rep[String] = unit("/home/david/PPL/data/")
  type MultiArray[T] = DeliteMultiArray[T]
  type Array1D[T] = DeliteArray1D[T]
  type Array2D[T] = DeliteArray2D[T]
  type Array3D[T] = DeliteArray3D[T]
  type Array4D[T] = DeliteArray4D[T]
  type Array5D[T] = DeliteArray5D[T]

  implicit def multiArrayManifest[T:Manifest] = manifest[DeliteMultiArray[T]]
  implicit def array1DManifest[T:Manifest] = manifest[DeliteArray1D[T]]
  implicit def array2DManifest[T:Manifest] = manifest[DeliteArray2D[T]]
  implicit def array3DManifest[T:Manifest] = manifest[DeliteArray3D[T]]
  implicit def array4DManifest[T:Manifest] = manifest[DeliteArray4D[T]]
  implicit def array5DManifest[T:Manifest] = manifest[DeliteArray5D[T]]

  implicit def array1DtoMultiArray[T:Manifest](x: Rep[DeliteArray1D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
  implicit def array2DtoMultiArray[T:Manifest](x: Rep[DeliteArray2D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
  implicit def array3DtoMultiArray[T:Manifest](x: Rep[DeliteArray3D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
  implicit def array4DtoMultiArray[T:Manifest](x: Rep[DeliteArray4D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
  implicit def array5DtoMultiArray[T:Manifest](x: Rep[DeliteArray5D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
}

trait MultiArrayExp extends DeliteMultiArrayOpsExp with MultiArrayWrapExp with RankMetadataOps with LayoutMetadataOps {this: DeliteOpsExp => }

trait SMALOpsExp extends SMALOps with DeliteDSLOpsExp with SimpleProfileOpsExp with MultiArrayExp {
  this: SMALCompiler =>

  val stop = new CompileStop{val IR: SMALOpsExp.this.type = SMALOpsExp.this}

  class StepPrinter(val step: String) extends Printer {
    val IR: SMALOpsExp.this.type = SMALOpsExp.this
    override val name = step
  }
  def prependPrinter(stepName: String) { prependTraversal(new StepPrinter(stepName)) }

  val rankAnalyzer = new RankAnalyzer{val IR: SMALOpsExp.this.type = SMALOpsExp.this}
  val rankChecker  = new RankChecker{val IR: SMALOpsExp.this.type = SMALOpsExp.this}
  val wrapTransformer = new MultiArrayWrapTransformer{val IR: SMALOpsExp.this.type = SMALOpsExp.this}
  val layoutAnalyzer = new LayoutAnalyzer{val IR: SMALOpsExp.this.type = SMALOpsExp.this}
  // Transformers prepended in reverse order to avoid interfering with default Delite transformer order
  prependTraversal(stop)
  prependTraversal(layoutAnalyzer)
  prependTraversal(wrapTransformer)
  prependTraversal(rankChecker)
  prependTraversal(rankAnalyzer)
  prependPrinter("Inital IR")
}

// --- SMAL Application Code Generators
trait ScalaGenSMAL extends ScalaGenDeliteDSL with ScalaGenSimpleProfileOps
  { val IR: SMALOpsExp }

trait CudaGenSMAL extends CudaGenDeliteDSL with CudaGenSimpleProfileOps
  { val IR: SMALOpsExp }

trait CGenSMAL extends CGenDeliteDSL with CGenSimpleProfileOps
  { val IR: SMALOpsExp }

// --- SMAL Application Stubs
trait SMALApp extends DeliteDSLApplication with SMALOps
trait SMALCompiler extends SMALOpsExp with DeliteDSLCompiler with SMALApp {
  override def getCodeGenPkg(t: Target{val IR: SMALCompiler.this.type}) : GenericFatCodegen{val IR: SMALCompiler.this.type} = t match {
    case _:TargetScala => new ScalaGenSMAL{val IR: SMALCompiler.this.type = SMALCompiler.this}
    //case _:TargetCuda => new CudaGenSMAL{val IR: SMALOpsExp.this.type = SMALOpsExp.this}
    //case _:TargetCpp => new CGenSMAL{val IR: SMALOpsExp.this.type = SMALOpsExp.this}
    case _ => throw new Exception("SMAL does not support this target")
  }
}

// --- SMAL Testbench Stubs
trait ScalaGenSMALTest extends ScalaGenSMAL with ScalaGenDeliteTest { val IR: SMALTest }

trait SMALTest extends DSLTest with SMALCompiler {
   override def getCodeGenPkg(t: Target{val IR: SMALTest.this.type}) : GenericFatCodegen{val IR: SMALTest.this.type} = t match {
    case _: TargetScala => new ScalaGenSMALTest{val IR: SMALTest.this.type = SMALTest.this }
    case _ => super.getCodeGenPkg(t)
  }
}
