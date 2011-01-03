package ppl.dsl.optiml

import datastruct.CudaGenDataStruct
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.DeliteCodeGenOverridesScala
import ppl.delite.framework.ops.{CudaGenDeliteOps, DeliteOpsExp, ScalaGenDeliteOps}
import scala.virtualization.lms.internal.{ScalaGenBase, GenericNestedCodegen, GenericCodegen}
import ppl.delite.framework.{Config, DeliteApplication}
import java.io._
import scala.virtualization.lms.internal._

/**
 * These are the portions of Scala imported into OptiML's scope.
 */
trait OptiMLScalaOpsPkg extends Base
    with Equal with IfThenElse with Variables with While with Functions
    with ImplicitOps with OrderingOps with StringOps with RangeOps with IOOps
    with ArrayOps with BooleanOps with PrimitiveOps with MiscOps with TupleOps
    with ListOps with SeqOps with MathOps with CastingOps

trait OptiMLScalaOpsPkgExp extends OptiMLScalaOpsPkg with DSLOpsExp
    with EqualExp with IfThenElseExp with VariablesExp with WhileExp with FunctionsExp
    with ImplicitOpsExp with OrderingOpsExp with StringOpsExp with RangeOpsExp with IOOpsExp
    with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp with TupleOpsExp
    with ListOpsExp with SeqOpsExp with MathOpsExp with CastingOpsExp

trait OptiMLScalaCodeGenPkg extends ScalaGenDSLOps
    with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenFunctions
    with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps
    with ScalaGenArrayOps with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps
    with ScalaGenListOps with ScalaGenSeqOps with ScalaGenMathOps with ScalaGenCastingOps
    { val IR: OptiMLScalaOpsPkgExp  }

trait OptiMLCudaCodeGenPkg extends CudaGenDSLOps with CudaGenImplicitOps with CudaGenOrderingOps
    with CudaGenEqual with CudaGenIfThenElse with CudaGenVariables with CudaGenWhile with CudaGenFunctions
    with CudaGenStringOps with CudaGenRangeOps with CudaGenIOOps with CudaGenArrayOps with CudaGenBooleanOps
    with CudaGenPrimitiveOps with CudaGenMiscOps
    with CudaGenListOps with CudaGenSeqOps
    { val IR: OptiMLScalaOpsPkgExp  }


/**
 * This the trait that every OptiML application must extend.
 */
trait OptiML extends OptiMLScalaOpsPkg with LanguageOps with ArithOps
  with VectorOps with MatrixOps with MLInputReaderOps with VectorViewOps with IndexVectorOps  {

  this: DeliteApplication =>

}


/**
 * These are the corresponding IR nodes for OptiML.
 */
trait OptiMLExp extends OptiML with OptiMLScalaOpsPkgExp with LanguageOpsExp with ArithOpsExp
  with VectorOpsExpOpt with MatrixOpsExpOpt with MLInputReaderOpsExp with VectorViewOpsExp with IndexVectorOpsExp
  with LanguageImplOpsStandard with VectorImplOpsStandard with VectorViewImplOpsStandard
  with MatrixImplOpsStandard with MLInputReaderImplOpsStandard
  with DeliteOpsExp {
  this: DeliteApplication =>

  def getCodeGenPkg(t: Target{val IR: OptiMLExp.this.type}) : GenericNestedCodegen{val IR: OptiMLExp.this.type} = {
    t match {
      case _:TargetScala => new OptiMLCodeGenScala{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _:TargetCuda => new OptiMLCodeGenCuda{val IR: OptiMLExp.this.type = OptiMLExp.this} 
      case _ => throw new RuntimeException("optiml does not support this target")
    }
  }

}


/**
 * OptiML code generators
 */
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

trait OptiMLCodeGenScala extends OptiMLCodeGenBase with OptiMLScalaCodeGenPkg with ScalaGenLanguageOps with ScalaGenArithOps
  with ScalaGenVectorOps with ScalaGenVectorViewOps with ScalaGenMatrixOps with ScalaGenIndexVectorOps
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

trait OptiMLCodeGenCuda extends OptiMLCodeGenBase with OptiMLCudaCodeGenPkg /*with CudaGenLanguageOps*/ with CudaGenArithOps with CudaGenDeliteOps with CudaGenVectorOps with CudaGenMatrixOps with CudaGenDataStruct // with CudaGenVectorViewOps
 // with DeliteCodeGenOverrideCuda // with CudaGenMLInputReaderOps   //TODO:DeliteCodeGenOverrideScala needed?
{

  val IR: DeliteApplication with OptiMLExp with Expressions
  import IR._

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
    case "ppl.dsl.optiml.datastruct.scala.RangeVector" => "RangeVector"
    case _ => super.remap(m)
  }

  override def isObjectType(m: Manifest[_]) : Boolean = remap(m) match {
    case "Matrix<int>" => true
    case "Matrix<long>" => true
    case "Matrix<float>" => true
    case "Matrix<double>" => true
    case "Matrix<bool>" => true
    case "Vector<int>" => true
    case "Vector<long>" => true
    case "Vector<float>" => true
    case "Vector<double>" => true
    case "Vector<bool>" => true
    case "RangeVector" => true
    case _ => super.isObjectType(m)
  }

  override def copyDataStructureHtoD(sym: Sym[_]) : String = remap(sym.Type) match {
    case "Matrix<int>" => matrixCopyHtoD(sym)
    case "Matrix<long>" => matrixCopyHtoD(sym)
    case "Matrix<float>" => matrixCopyHtoD(sym)
    case "Matrix<double>" => matrixCopyHtoD(sym)
    case "Matrix<bool>" => matrixCopyHtoD(sym)
    case "Vector<int>" => vectorCopyHtoD(sym)
    case "Vector<long>" => vectorCopyHtoD(sym)
    case "Vector<float>" => vectorCopyHtoD(sym)
    case "Vector<double>" => vectorCopyHtoD(sym)
    case "Vector<bool>" => vectorCopyHtoD(sym)
    case "RangeVector" => rangevectorCopyHtoD(sym)
    case _ => super.copyDataStructureHtoD(sym)
  }

  override def copyDataStructureDtoH(sym: Sym[_]) : String = remap(sym.Type) match {
    case "Matrix<int>" => matrixCopyDtoH(sym)
    case "Matrix<long>" => matrixCopyDtoH(sym)
    case "Matrix<float>" => matrixCopyDtoH(sym)
    case "Matrix<double>" => matrixCopyDtoH(sym)
    case "Matrix<bool>" => matrixCopyDtoH(sym)
    case "Vector<int>" => vectorCopyDtoH(sym)
    case "Vector<long>" => vectorCopyDtoH(sym)
    case "Vector<float>" => vectorCopyDtoH(sym)
    case "Vector<double>" => vectorCopyDtoH(sym)
    case "Vector<bool>" => vectorCopyDtoH(sym)
    //case "RangeVector" => rangevectorCopyDtoH(sym)
    case "RangeVector" => vectorCopyDtoH(sym)
    case _ => super.copyDataStructureDtoH(sym)
  }

  override def allocOutput(newSym: Sym[_], sym: Sym[_]) : Unit = remap(sym.Type) match {
    case "Matrix<int>" => emitMatrixAllocSym(newSym,sym)
    case "Matrix<long>" => emitMatrixAllocSym(newSym,sym)
    case "Matrix<float>" => emitMatrixAllocSym(newSym,sym)
    case "Matrix<double>" => emitMatrixAllocSym(newSym,sym)
    case "Matrix<bool>" => emitMatrixAllocSym(newSym,sym)
    case "Vector<int>" => emitVectorAllocSym(newSym,sym)
    case "Vector<long>" => emitVectorAllocSym(newSym,sym)
    case "Vector<float>" => emitVectorAllocSym(newSym,sym)
    case "Vector<double>" => emitVectorAllocSym(newSym,sym)
    case "Vector<bool>" => emitVectorAllocSym(newSym,sym)
    //case "RangeVector" => emitRangeVectorAllocSym(newSym,sym)
    case "RangeVector" => emitVectorAllocSym(newSym,sym)
    case _ => super.allocOutput(newSym,sym)    
  }

  override def allocReference(newSym: Sym[_], sym: Sym[_]) : Unit = remap(sym.Type) match {
    case "Matrix<int>" => emitMatrixAllocRef(newSym,sym)
    case "Matrix<long>" => emitMatrixAllocRef(newSym,sym)
    case "Matrix<float>" => emitMatrixAllocRef(newSym,sym)
    case "Matrix<double>" => emitMatrixAllocRef(newSym,sym)
    case "Matrix<bool>" => emitMatrixAllocRef(newSym,sym)
    case "Vector<int>" => emitVectorAllocRef(newSym,sym)
    case "Vector<long>" => emitVectorAllocRef(newSym,sym)
    case "Vector<float>" => emitVectorAllocRef(newSym,sym)
    case "Vector<double>" => emitVectorAllocRef(newSym,sym)
    case "Vector<bool>" => emitVectorAllocRef(newSym,sym)
    //case "RangeVector" => emitRangeVectorAllocRef(newSym,sym)
    case "RangeVector" => emitVectorAllocRef(newSym,sym)
    case _ => super.allocReference(newSym,sym)
  }

  override def getDSLHeaders: String = {
    val out = new StringBuilder
    out.append("#include \"VectorImpl.h\"\n")
    out.append("#include \"MatrixImpl.h\"\n")
    out.append("#include \"RangeVectorImpl.h\"\n")
    out.toString
  }

}