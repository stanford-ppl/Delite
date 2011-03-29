package ppl.dsl.deliszt

import graph._
import java.io._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.ops.{CudaGenDeliteOps, DeliteOpsExp, ScalaGenDeliteOps}
import ppl.delite.framework.codegen.c.TargetC
import ppl.delite.framework.codegen.delite.overrides.{DeliteCudaGenAllOverrides, DeliteCGenAllOverrides, DeliteScalaGenAllOverrides, DeliteAllOverridesExp}
import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import ppl.dsl.deliszt.io._
import ppl.dsl.deliszt.vector._
import ppl.dsl.deliszt.matrix._
//import ppl.dsl.deliszt.graph._
import ppl.delite.framework.ops._

/**
 * These are the portions of Scala imported into DeLiszt's scope.
 */
trait OptiMLScalaOpsPkg extends Base
    with Equal with IfThenElse with Variables with While with Functions
    with ImplicitOps with OrderingOps with StringOps with RangeOps with IOOps
    with ArrayOps with BooleanOps with PrimitiveOps with MiscOps with TupleOps
    with ListOps with SeqOps with MathOps with CastingOps with SetOps

trait OptiMLScalaOpsPkgExp extends OptiMLScalaOpsPkg with DSLOpsExp
    with EqualExp with IfThenElseExp with VariablesExp with WhileExp with FunctionsExp
    with ImplicitOpsExp with OrderingOpsExp with StringOpsExp with RangeOpsExp with IOOpsExp
    with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp with TupleOpsExp
    with ListOpsExp with SeqOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp

trait OptiMLScalaCodeGenPkg extends ScalaGenDSLOps
    with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenFunctions
    with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps
    with ScalaGenArrayOps with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps
    with ScalaGenListOps with ScalaGenSeqOps with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenSetOps
    { val IR: OptiMLScalaOpsPkgExp  }

trait OptiMLCudaCodeGenPkg extends CudaGenDSLOps with CudaGenImplicitOps with CudaGenOrderingOps
    with CudaGenEqual with CudaGenIfThenElse with CudaGenVariables with CudaGenWhile with CudaGenFunctions
    with CudaGenStringOps with CudaGenRangeOps with CudaGenIOOps with CudaGenArrayOps with CudaGenBooleanOps
    with CudaGenPrimitiveOps with CudaGenMiscOps
    with CudaGenListOps with CudaGenSeqOps with CudaGenMathOps with CudaGenCastingOps with CudaGenSetOps
    { val IR: OptiMLScalaOpsPkgExp  }

trait DeLisztCCodeGenPkg extends CGenDSLOps with CGenImplicitOps with CGenOrderingOps
    with CGenStringOps with CGenRangeOps with CGenIOOps with CGenArrayOps with CGenBooleanOps
    with CGenPrimitiveOps with CGenMiscOps with CGenFunctions with CGenEqual with CGenIfThenElse
    with CGenVariables with CGenWhile with CGenListOps with CGenSeqOps { val IR: OptiMLScalaOpsPkgExp  }

/**
 * This the trait that every DeLiszt application must extend.
 */
trait DeLiszt extends OptiMLScalaOpsPkg with LanguageOps with ApplicationOps with ArithOps with CloneableOps
  with VectorOps with MatrixOps with MLInputReaderOps with MLOutputWriterOps with VectorViewOps
  with LabelsOps {

  this: DeliteApplication =>

}


/**
 * These are the corresponding IR nodes for DeLiszt.
 */
trait OptiMLExp extends DeLiszt with OptiMLScalaOpsPkgExp with LanguageOpsExp with ApplicationOpsExp with ArithOpsExpOpt
  with VectorOpsExpOpt with MatrixOpsExpOpt with MLInputReaderOpsExp 
  with MLOutputWriterOpsExp with VectorViewOpsExp
  with LabelsOpsExp
  with LanguageImplOpsStandard with VectorImplOpsStandard with VectorViewImplOpsStandard
  with MatrixImplOpsStandard with MLInputReaderImplOpsStandard with MLOutputWriterImplOpsStandard
  with DeliteOpsExp with VariantsOpsExp with DeliteAllOverridesExp {
  this: DeliteApplication =>

  def getCodeGenPkg(t: Target{val IR: OptiMLExp.this.type}) : GenericFatCodegen{val IR: OptiMLExp.this.type} = {
    t match {
      case _:TargetScala => new OptiMLCodeGenScala{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _:TargetCuda => new OptiMLCodeGenCuda{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _:TargetC => new OptiMLCodeGenC{val IR: OptiMLExp.this.type = OptiMLExp.this} 
      case _ => throw new RuntimeException("deliszt does not support this target")
    }
  }

}


/**
 * DeLiszt code generators
 */
trait DeLiszt extends GenericFatCodegen {

  val IR: DeliteApplication with OptiMLExp
  override def initialDefs = IR.deliteGenerator.availableDefs


  def dsmap(line: String) = line

  val specialize = Set[String]()
  val specialize2 = Set[String]()
  def genSpec(f: File, outPath: String) = {}
  def genSpec2(f: File, outPath: String) = {}

  override def emitDataStructures() {
    val dsRoot = Config.homeDir + "/dsls/deliszt/src/ppl/dsl/deliszt/datastruct/" + this.toString
    val dsOut = Config.buildDir + "/" + this.toString + "/"

    val dsDir = new File(dsRoot)
    if (!dsDir.exists) return
    val outDir = new File(dsOut)
    outDir.mkdirs()

    for (f <- dsDir.listFiles) {
      if (specialize contains (f.getName())) {
        genSpec(f, dsOut)
      }
	  if (specialize2 contains (f.getName())) {
		 genSpec2(f, dsOut)
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

trait OptiMLCodeGenScala extends DeLiszt with OptiMLScalaCodeGenPkg with ScalaGenDeliteOps with ScalaGenLanguageOps
  with ScalaGenApplicationOps
  with ScalaGenArithOps with ScalaGenVectorOps with ScalaGenVectorViewOps with ScalaGenMatrixOps
  with ScalaGenLabelsOps with ScalaGenVariantsOps with ScalaGenDeliteCollectionOps
  with DeliteScalaGenAllOverrides { //with ScalaGenMLInputReaderOps {
  
  val IR: DeliteApplication with OptiMLExp

  override val specialize = Set("VectorImpl.scala", "MatrixImpl.scala", "VectorViewImpl.scala", "LabelsImpl.scala")
  override val specialize2 = Set("TrainingSetImpl.scala")

  override def genSpec(f: File, dsOut: String) {
    for (s <- List("Double","Int","Float","Long","Boolean")) {
      val outFile = dsOut + "/" + s + f.getName()
      val out = new BufferedWriter(new FileWriter(outFile))
      for (line <- scala.io.Source.fromFile(f).getLines) {
        out.write(specmap(line, s) + "\n")
      }
      out.close()
    }
  }

  override def genSpec2(f: File, dsOut: String) {
    for (s1 <- List("Double","Int","Float","Long","Boolean")) {
   	  for (s2 <- List("Double","Int","Float","Long","Boolean")) {
        val outFile = dsOut + "/" + s1 + s2 + f.getName()
        val out = new BufferedWriter(new FileWriter(outFile))
        for (line <- scala.io.Source.fromFile(f).getLines) {
          out.write(specmap2(line, s1, s2) + "\n")
        }
        out.close()
	  }
    }
  }

  def specmap(line: String, t: String) : String = {
    var res = line.replaceAll("object ", "object " + t)
    res = res.replaceAll("import ", "import " + t)
    res = res.replaceAll("@specialized T: ClassManifest", t)
    res = res.replaceAll("T:Manifest", t)
    res = res.replaceAll("\\bT\\b", t)
    dsmap(res)
  }
  def specmap2(line: String, t1: String, t2: String) : String = {
    var res = line.replaceAll("object ", "object " + t1 + t2)
    res = res.replaceAll("import ", "import " + t1 + t2)
    res = res.replaceAll("@specialized T: ClassManifest", t1)
    res = res.replaceAll("@specialized L: ClassManifest", t2)
    res = res.replaceAll("T:Manifest", t1)
    res = res.replaceAll("L:Manifest", t2)
    res = res.replaceAll("\\bT\\b", t1)
    res = res.replaceAll("\\bL\\b", t2)
    dsmap(res)
  }

  override def remap[A](m: Manifest[A]) : String = {
    dsmap(super.remap(m))
  }

  override def dsmap(line: String) : String = {
    var res = line.replaceAll("ppl.dsl.deliszt.datastruct", "generated")
    res = res.replaceAll("ppl.delite.framework", "generated.scala")
	for(tpe1 <- List("Int","Long","Double","Float","Boolean")) {
    	res = res.replaceAll("VectorImpl\\["+tpe1+"\\]", tpe1+"VectorImpl")
    	res = res.replaceAll("VectorViewImpl\\["+tpe1+"\\]", tpe1+"VectorViewImpl")
    	res = res.replaceAll("MatrixImpl\\["+tpe1+"\\]", tpe1+"MatrixImpl")
    	res = res.replaceAll("LabelsImpl\\["+tpe1+"\\]", tpe1+"LabelsImpl")
	}
    res
  }
}

trait OptiMLCodeGenCuda extends DeLiszt with OptiMLCudaCodeGenPkg /*with CudaGenLanguageOps*/ with CudaGenArithOps with CudaGenDeliteOps with CudaGenVectorOps with CudaGenMatrixOps with CudaGenDataStruct with CudaGenTrainingSetOps // with CudaGenVectorViewOps
  with CudaGenVariantsOps with DeliteCudaGenAllOverrides // with DeliteCodeGenOverrideCuda // with CudaGenMLInputReaderOps  //TODO:DeliteCodeGenOverrideScala needed?
{
  val IR: DeliteApplication with OptiMLExp
  import IR._

  // Maps the scala type to cuda type
  override def remap[A](m: Manifest[A]) : String = m.toString match {
    case "ppl.dsl.deliszt.datastruct.scala.Matrix[Int]" => "Matrix<int>"
    case "ppl.dsl.deliszt.datastruct.scala.Matrix[Long]" => "Matrix<long>"
    case "ppl.dsl.deliszt.datastruct.scala.Matrix[Float]" => "Matrix<float>"
    case "ppl.dsl.deliszt.datastruct.scala.Matrix[Double]" => "Matrix<double>"
    case "ppl.dsl.deliszt.datastruct.scala.Matrix[Boolean]" => "Matrix<bool>"
    case "ppl.dsl.deliszt.datastruct.scala.Vector[Int]" => "Vector<int>"
    case "ppl.dsl.deliszt.datastruct.scala.Vector[Long]" => "Vector<long>"
    case "ppl.dsl.deliszt.datastruct.scala.Vector[Float]" => "Vector<float>"
    case "ppl.dsl.deliszt.datastruct.scala.Vector[Double]" => "Vector<double>"
    case "ppl.dsl.deliszt.datastruct.scala.Vector[Boolean]" => "Vector<bool>"
    case "ppl.dsl.deliszt.datastruct.scala.RangeVector" => "RangeVector"
    case "ppl.dsl.deliszt.datastruct.scala.IndexVector" => "IndexVector"
    case "ppl.dsl.deliszt.datastruct.scala.Labels[Int]" => "Labels<int>"
    case "ppl.dsl.deliszt.datastruct.scala.Labels[Long]" => "Labels<long>"
    case "ppl.dsl.deliszt.datastruct.scala.Labels[Float]" => "Labels<float>"
    case "ppl.dsl.deliszt.datastruct.scala.Labels[Double]" => "Labels<double>"
    case "ppl.dsl.deliszt.datastruct.scala.Labels[Boolean]" => "Labels<bool>"
    case "ppl.dsl.deliszt.datastruct.scala.TrainingSet[Double, Double]" => "TrainingSet<double,double>"
    case _ => super.remap(m)
  }

  override def isObjectType[T](m: Manifest[T]) : Boolean = remap(m) match {
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
    case "IndexVector" => true
    case "Labels<int>" => true
    case "Labels<long>" => true
    case "Labels<float>" => true
    case "Labels<double>" => true
    case "Labels<bool>" => true
    case "TrainingSet<double,double>" => true
    case _ => super.isObjectType(m)
  }

  override def copyDataStructureHtoD(sym: Sym[Any]) : String = remap(sym.Type) match {
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
    case "RangeVector" => rangeVectorCopyHtoD(sym)
    case "IndexVector" => indexVectorCopyHtoD(sym)
    case "Labels<int>" => labelsCopyHtoD(sym)
    case "Labels<long>" => labelsCopyHtoD(sym)
    case "Labels<float>" => labelsCopyHtoD(sym)
    case "Labels<double>" => labelsCopyHtoD(sym)
    case "Labels<bool>" => labelsCopyHtoD(sym)
    case "TrainingSet<double,double>" => trainingSetCopyHtoD(sym)
    case _ => super.copyDataStructureHtoD(sym)
  }

  override def copyDataStructureDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
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
    case _ => super.copyDataStructureDtoH(sym)
  }

  override def copyDataStructureDtoHBack(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Matrix<int>" => matrixCopyDtoHBack(sym)
    case "Matrix<long>" => matrixCopyDtoHBack(sym)
    case "Matrix<float>" => matrixCopyDtoHBack(sym)
    case "Matrix<double>" => matrixCopyDtoHBack(sym)
    case "Matrix<bool>" => matrixCopyDtoHBack(sym)
    case "Vector<int>" => vectorCopyDtoHBack(sym)
    case "Vector<long>" => vectorCopyDtoHBack(sym)
    case "Vector<float>" => vectorCopyDtoHBack(sym)
    case "Vector<double>" => vectorCopyDtoHBack(sym)
    case "Vector<bool>" => vectorCopyDtoHBack(sym)
    case "RangeVector" => rangeVectorCopyDtoHBack(sym)
    case "IndexVector" => indexVectorCopyDtoHBack(sym)
    case "Labels<int>" => labelsCopyDtoHBack(sym)
    case "Labels<long>" => labelsCopyDtoHBack(sym)
    case "Labels<float>" => labelsCopyDtoHBack(sym)
    case "Labels<double>" => labelsCopyDtoHBack(sym)
    case "Labels<bool>" => labelsCopyDtoHBack(sym)
    case "TrainingSet<double,double>" => trainingSetCopyDtoHBack(sym)
    case _ => super.copyDataStructureDtoHBack(sym)
  }

  override def allocOutput(newSym: Sym[_], sym: Sym[_], reset: Boolean = false) : Unit = remap(newSym.Type) match {
    case "Matrix<int>" => emitMatrixAllocSym(newSym,sym,reset)
    case "Matrix<long>" => emitMatrixAllocSym(newSym,sym,reset)
    case "Matrix<float>" => emitMatrixAllocSym(newSym,sym,reset)
    case "Matrix<double>" => emitMatrixAllocSym(newSym,sym,reset)
    case "Matrix<bool>" => emitMatrixAllocSym(newSym,sym,reset)
    case "Vector<int>" => emitVectorAllocSym(newSym,sym,reset)
    case "Vector<long>" => emitVectorAllocSym(newSym,sym,reset)
    case "Vector<float>" => emitVectorAllocSym(newSym,sym,reset)
    case "Vector<double>" => emitVectorAllocSym(newSym,sym,reset)
    case "Vector<bool>" => emitVectorAllocSym(newSym,sym,reset)
    case _ => super.allocOutput(newSym,sym,reset)
  }

  override def allocReference(newSym: Sym[Any], sym: Sym[Any]) : Unit = remap(newSym.Type) match {
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
    case "Labels<int>" => emitVectorAllocRef(newSym,sym)
    case "Labels<long>" => emitVectorAllocRef(newSym,sym)
    case "Labels<float>" => emitVectorAllocRef(newSym,sym)
    case "Labels<double>" => emitVectorAllocRef(newSym,sym)
    case "Labels<bool>" => emitVectorAllocRef(newSym,sym)
    case _ => super.allocReference(newSym,sym)
  }

  override def positionMultDimInputs(sym: Sym[Any]) : String = remap(sym.Type) match {
	//TODO: Add matrix reposition, and also do safety check for datastructures that do not have data field
    case "Vector<int>" => vectorPositionMultDimInputs(sym)
    case "Vector<long>" => vectorPositionMultDimInputs(sym)
    case "Vector<float>" => vectorPositionMultDimInputs(sym)
    case "Vector<double>" => vectorPositionMultDimInputs(sym)
    case "Vector<bool>" => vectorPositionMultDimInputs(sym)
    case _ => super.positionMultDimInputs(sym)
  }

  override def getDSLHeaders: String = {
    val out = new StringBuilder
    out.append("#include <float.h>\n")
    out.append("#include \"VectorImpl.h\"\n")
    out.append("#include \"MatrixImpl.h\"\n")
    out.append("#include \"RangeVectorImpl.h\"\n")
    out.append("#include \"IndexVectorImpl.h\"\n")
    out.append("#include \"LabelsImpl.h\"\n")
    out.append("#include \"TrainingSetImpl.h\"\n")
    out.toString
  }

}

trait OptiMLCodeGenC extends DeLiszt with DeLisztCCodeGenPkg with CGenArithOps with CGenDeliteOps with CGenVectorOps with CGenMatrixOps
  with CGenVariantsOps with DeliteCGenAllOverrides
{
  val IR: DeliteApplication with OptiMLExp
  import IR._

  override def remap[A](m: Manifest[A]) : String = m.toString match {
    case "ppl.dsl.deliszt.datastruct.scala.Matrix[Int]" => "Matrix<int>"
    case "ppl.dsl.deliszt.datastruct.scala.Matrix[Long]" => "Matrix<long>"
    case "ppl.dsl.deliszt.datastruct.scala.Matrix[Float]" => "Matrix<float>"
    case "ppl.dsl.deliszt.datastruct.scala.Matrix[Double]" => "Matrix<double>"
    case "ppl.dsl.deliszt.datastruct.scala.Matrix[Boolean]" => "Matrix<bool>"
    case "ppl.dsl.deliszt.datastruct.scala.Vector[Int]" => "Vector<int>"
    case "ppl.dsl.deliszt.datastruct.scala.Vector[Long]" => "Vector<long>"
    case "ppl.dsl.deliszt.datastruct.scala.Vector[Float]" => "Vector<float>"
    case "ppl.dsl.deliszt.datastruct.scala.Vector[Double]" => "Vector<double>"
    case "ppl.dsl.deliszt.datastruct.scala.Vector[Boolean]" => "Vector<bool>"
    case _ => super.remap(m)
  }
}
