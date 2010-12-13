package ppl.dsl.optiml

import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import scala.virtualization.lms.common.{ScalaOpsPkgExp, ScalaOpsPkg, ScalaCodeGenPkg, CudaCodeGenPkg}
import ppl.delite.framework.codegen.delite.DeliteCodeGenOverridesScala
import ppl.delite.framework.ops.{CudaGenDeliteOps, DeliteOpsExp, ScalaGenDeliteOps}
import scala.virtualization.lms.internal.{ScalaGenBase, GenericNestedCodegen, GenericCodegen}
import ppl.delite.framework.{Config, DeliteApplication}
import java.io._

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

trait OptiMLCodeGenBase extends GenericCodegen {
  def dsmap(line: String) = line

  val specialize = Set[String]()
  def genSpec(f: File, outPath: String) = {}

  override def emitDataStructures() {
    val dsRoot = "dsls/optiml/src/ppl/dsl/optiml/datastruct/" + this.toString
    val dsOut = Config.build_dir + "/" + this.toString + "/"

    val dsDir = new File(dsRoot)
    if (!dsDir.exists) return
    val outDir = new File(dsOut)
    outDir.mkdirs()

    for (f <- dsDir.listFiles) {
      if (specialize contains (f.getName())) {
        genSpec(f, dsOut)
      }
      val outFile = dsOut + "/" + f.getName()
      val out = new BufferedWriter(new FileWriter(outFile))
      for (line <- scala.io.Source.fromFile(f).getLines) {
        out.write(dsmap(line) + "\n")
      }
      out.close()
    }
  }
}

trait OptiMLCodeGenScala extends OptiMLCodeGenBase with ScalaCodeGenPkg with ScalaGenLanguageOps
  with ScalaGenVectorOps with ScalaGenVectorViewOps with ScalaGenMatrixOps
  with ScalaGenDeliteOps with DeliteCodeGenOverridesScala { //with ScalaGenMLInputReaderOps {

  val IR: DeliteApplication with OptiMLExp

  override val specialize = Set("VectorImpl.scala", "MatrixImpl.scala", "VectorViewImpl.scala")

  override def genSpec(f: File, dsOut: String) {
    for (s <- List("Double","Int")) {
      val outFile = dsOut + "/" + s + f.getName()
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
    res = res.replaceAll("@specialized T: ClassManifest", t)
    res = res.replaceAll("\\bT\\b", t)
    dsmap(res)
  }

  override def remap[A](m: Manifest[A]) : String = {
    dsmap(super.remap(m))
  }

  override def dsmap(line: String) : String = {
    var res = line.replaceAll("ppl.dsl.optiml.datastruct", "generated")
    res = res.replaceAll("ppl.delite.framework", "generated.scala")
    res = res.replaceAll("VectorImpl\\[Double\\]", "DoubleVectorImpl")
    res = res.replaceAll("VectorImpl\\[Int\\]", "IntVectorImpl")
    res = res.replaceAll("VectorViewImpl\\[Double\\]", "DoubleVectorViewImpl")
    res = res.replaceAll("VectorViewImpl\\[Int\\]", "IntVectorViewImpl")
    res = res.replaceAll("MatrixImpl\\[Double\\]", "DoubleMatrixImpl")
    res = res.replaceAll("MatrixImpl\\[Int\\]", "IntMatrixImpl")
    res
  }
}

trait OptiMLCodeGenCuda extends OptiMLCodeGenBase with CudaCodeGenPkg /*with CudaGenLanguageOps*/ with CudaGenVectorOps with CudaGenMatrixOps with CudaGenDeliteOps// with CudaGenVectorViewOps
 // with DeliteCodeGenOverrideCuda // with CudaGenMLInputReaderOps   //TODO:DeliteCodeGenOverrideScala needed?
{

  val IR: DeliteApplication with OptiMLExp

  // Maps the scala type to cuda type
  override def remap[A](m: Manifest[A]) : String = m.toString match {
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Int]" => "Matrix<int>"
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Long]" => "Matrix<long>"
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Float]" => "Matrix<float>"
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Double]" => "Matrix<double>"
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Boolean]" => "Matrix<bool>"
    case "ppl.dsl.optiml.datastruct.scala.Vector[Int]" => "Vector<int>"
    case "ppl.dsl.optiml.datastruct.scala.Vector[Long]" => "Vector<long>"
    case "ppl.dsl.optiml.datastruct.scala.Vector[Float]" => "Vector<float>"
    case "ppl.dsl.optiml.datastruct.scala.Vector[Double]" => "Vector<double>"
    case "ppl.dsl.optiml.datastruct.scala.Vector[Boolean]" => "Vector<bool>"
    case "Int" => "int"
    case "Long" => "long"
    case "Float" => "float"
    case "Double" => "double"
    case "Boolean" => "bool"
    case _ => throw new Exception("Undefined CUDA type: " + m)
  }

}
