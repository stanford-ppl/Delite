package ppl.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import scala.virtualization.lms.common.{ScalaOpsPkgExp, ScalaOpsPkg, ScalaCodeGenPkg, CudaCodeGenPkg}
import scala.virtualization.lms.internal.{GenericNestedCodegen, GenericCodegen}
import ppl.delite.framework.codegen.delite.DeliteCodeGenOverridesScala
import ppl.delite.framework.ops.{CudaGenDeliteOps, DeliteOpsExp, ScalaGenDeliteOps}

trait OptiML extends ScalaOpsPkg with LanguageOps with ArithImplicits with VectorOps with MatrixOps with MLInputReaderOps {
  this: DeliteApplication =>
}

trait OptiMLExp extends OptiML with ScalaOpsPkgExp with LanguageOpsExp with DeliteOpsExp
  with VectorOpsExpOpt with VectorViewOpsExp with MatrixOpsExpOpt with MLInputReaderOpsExp
  with LanguageImplOpsStandard with VectorImplOpsStandard with VectorViewImplOpsStandard
  with MatrixImplOpsStandard with MLInputReaderImplOpsStandard {
  this: DeliteApplication =>

  def getCodeGenPkg(t: Target{val IR: OptiMLExp.this.type}) : GenericNestedCodegen{val IR: OptiMLExp.this.type} = {
    t match {
      case _:TargetScala => new OptiMLCodeGenScala{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _:TargetCuda => new OptiMLCodeGenCuda{val IR: OptiMLExp.this.type = OptiMLExp.this} 
      case _ => throw new RuntimeException("optiml does not support this target")
    }
  }

}

trait OptiMLCodeGenScala extends ScalaCodeGenPkg with ScalaGenLanguageOps with ScalaGenVectorOps with ScalaGenVectorViewOps with ScalaGenMatrixOps
  with ScalaGenDeliteOps with DeliteCodeGenOverridesScala { //with ScalaGenMLInputReaderOps {

  val IR: DeliteApplication with OptiMLExp
  import IR._

  override def remap[A](m: Manifest[A]) : String = m.toString match {
    // TODO: make more robust
    case "ppl.dsl.optiml.NilVector[Double]" => "ppl.dsl.optiml.NilVectorDoubleImpl"
    case "ppl.dsl.optiml.Vector[Double]" => "ppl.dsl.optiml.VectorImpl[Double]"
    case "ppl.dsl.optiml.Vector[Int]" => "ppl.dsl.optiml.VectorImpl[Int]"
    case "ppl.dsl.optiml.Vector[Boolean]" => "ppl.dsl.optiml.VectorImpl[Boolean]"
    case "ppl.dsl.optiml.Matrix[Double]" => "ppl.dsl.optiml.MatrixImpl[Double]"
    case _ => super.remap(m)
  }
}

trait OptiMLCodeGenCuda extends CudaCodeGenPkg with CudaGenLanguageOps with CudaGenVectorOps with CudaGenMatrixOps with CudaGenDeliteOps// with CudaGenVectorViewOps
 // with DeliteCodeGenOverrideCuda // with CudaGenMLInputReaderOps   //TODO:DeliteCodeGenOverrideScala needed?
{

  val IR: DeliteApplication with OptiMLExp

  // Maps the scala type to cuda type
  override def remap[A](m: Manifest[A]) : String = m.toString match {
    case "ppl.dsl.optiml.Matrix[Int]" => "Matrix<int>"
    case "ppl.dsl.optiml.Matrix[Long]" => "Matrix<long>"
    case "ppl.dsl.optiml.Matrix[Float]" => "Matrix<float>"
    case "ppl.dsl.optiml.Matrix[Double]" => "Matrix<double>"
    case "ppl.dsl.optiml.Matrix[Boolean]" => "Matrix<bool>"
    case "ppl.dsl.optiml.Vector[Int]" => "Vector<int>"
    case "ppl.dsl.optiml.Vector[Long]" => "Vector<long>"
    case "ppl.dsl.optiml.Vector[Float]" => "Vector<float>"
    case "ppl.dsl.optiml.Vector[Double]" => "Vector<double>"
    case "ppl.dsl.optiml.Vector[Boolean]" => "Vector<bool>"
    case "Int" => "int"
    case "Long" => "long"
    case "Float" => "float"
    case "Double" => "double"
    case "Boolean" => "bool"
    case _ => throw new Exception("Undefined CUDA type: " + m)
  }

}
