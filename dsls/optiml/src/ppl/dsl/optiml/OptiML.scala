package ppl.dsl.optiml

import java.io._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.c.TargetC
import ppl.delite.framework.codegen.delite.overrides.{DeliteCudaGenAllOverrides, DeliteCGenAllOverrides, DeliteScalaGenAllOverrides, DeliteAllOverridesExp}
import ppl.delite.framework.ops._

import ppl.dsl.optila.{OptiLAApplication}
import ppl.dsl.optila.{OptiLAScalaOpsPkg, OptiLAScalaOpsPkgExp, OptiLA, OptiLAExp, OptiLACompiler, OptiLALift}
import ppl.dsl.optila.{OptiLAScalaCodeGenPkg, OptiLACudaCodeGenPkg, OptiLACCodeGenPkg, OptiLACodeGenBase, OptiLACodeGenScala, OptiLACodeGenCuda, OptiLACodeGenC}

import ppl.dsl.optiml.io._
import ppl.dsl.optiml.vector._
import ppl.dsl.optiml.matrix._
import ppl.dsl.optiml.graph._
import ppl.dsl.optiml.stream._
import ppl.dsl.optiml.library.cluster._
import ppl.dsl.optiml.application._


/**
 * These separate OptiML applications from the Exp world.
 */

// ex. object GDARunner extends OptiMLApplicationRunner with GDA
trait OptiMLApplicationRunner extends OptiMLApplication with DeliteApplication with OptiMLExp

// ex. trait GDA extends OptiMLApplication
trait OptiMLApplication extends OptiLAApplication with OptiML with OptiMLLift with OptiMLLibrary {
  var args: Rep[Array[String]]
  def main(): Unit
}

trait OptiMLLibrary extends OptiMLKmeans {
  this: OptiMLApplication =>
}

/**
 * These are the portions of Scala imported into OptiML's scope.
 */
trait OptiMLLift extends OptiLALift {
  this: OptiML =>
}

trait OptiMLScalaOpsPkg extends OptiLAScalaOpsPkg

trait OptiMLScalaOpsPkgExp extends OptiLAScalaOpsPkgExp with OptiMLScalaOpsPkg

trait OptiMLScalaCodeGenPkg extends OptiLAScalaCodeGenPkg 
  { val IR: OptiMLScalaOpsPkgExp  }

trait OptiMLCudaCodeGenPkg extends OptiLACudaCodeGenPkg
  { val IR: OptiMLScalaOpsPkgExp  }

trait OptiMLCCodeGenPkg extends OptiLACCodeGenPkg
  { val IR: OptiMLScalaOpsPkgExp  }

/**
 * This is the trait that every OptiML application must extend.
 */
trait OptiML extends OptiLA with OptiMLScalaOpsPkg with LanguageOps with ApplicationOps with LBPOps // TODO: LBPOpsshould be auto-generated with ApplicationOps
  with MLInputReaderOps with MLOutputWriterOps
  with VectorOps with OptiMLDenseVectorOps with OptiMLVectorViewOps with OptiMLRangeVectorOps
  with MatrixOps with IndexVectorOps with IndexVectorDenseOps with IndexVectorRangeOps with IndexVector2Ops 
  with StreamOps with StreamRowOps
  with GraphOps with VerticesOps with EdgeOps with VertexOps with MessageEdgeOps with MessageVertexOps with VSetOps
  with LabelsOps with TrainingSetOps with ImageOps with GrayscaleImageOps {

  this: OptiMLApplication =>
}

// these ops are only available to the compiler (they are restricted from application use)
trait OptiMLCompiler extends OptiLACompiler with OptiML {
    
  this: OptiMLApplication with OptiMLExp =>
}


/**
 * These are the corresponding IR nodes for OptiML.
 */
trait OptiMLExp extends OptiLAExp with OptiMLCompiler with OptiMLScalaOpsPkgExp 
  with LanguageOpsExp with ApplicationOpsExp with LBPOpsExp 
  with MLInputReaderOpsExp with MLOutputWriterOpsExp
  with VectorOpsExpOpt with MatrixOpsExpOpt with IndexVectorOpsExp with IndexVectorDenseOpsExpOpt with IndexVectorRangeOpsExp with IndexVector2OpsExp 
  with StreamOpsExpOpt with StreamRowOpsExpOpt
  with LabelsOpsExp with TrainingSetOpsExp with ImageOpsExp with GrayscaleImageOpsExp
  with LanguageImplOpsStandard with VectorImplOpsStandard with IndexVectorImplOpsStandard
  with MLInputReaderImplOpsStandard with MLOutputWriterImplOpsStandard with StreamImplOpsStandard
  with GraphOpsExp with VerticesOpsExp with EdgeOpsExp with VertexOpsExp with MessageEdgeOpsExp with MessageVertexOpsExp with VSetOpsExp
  with DeliteAllOverridesExp {

  // this: OptiMLApplicationRunner => why doesn't this work?
  this: DeliteApplication with OptiMLApplication with OptiMLExp => // can't be OptiMLApplication right now because code generators depend on stuff inside DeliteApplication (via IR)

  override def getCodeGenPkg(t: Target{val IR: OptiMLExp.this.type}) : GenericFatCodegen{val IR: OptiMLExp.this.type} = {
    t match {
      case _:TargetScala => new OptiMLCodeGenScala{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _:TargetCuda => new OptiMLCodeGenCuda{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _:TargetC => new OptiMLCodeGenC{val IR: OptiMLExp.this.type = OptiMLExp.this} 
      case _ => throw new RuntimeException("optiml does not support this target")
    }
  }

}


/**
 * OptiML code generators
 */
trait OptiMLCodeGenBase extends OptiLACodeGenBase {

  val IR: DeliteApplication with OptiMLExp
  override def initialDefs = IR.deliteGenerator.availableDefs


  def genSpec2(f: File, outPath: String) = {}
    
  override def emitDataStructures(path: String) {
    super.emitDataStructures(path) // get optila data structures
    
    val s = File.separator
    val dsRoot = Config.homeDir + s+"dsls"+s+"optiml"+s+"src"+s+"ppl"+s+"dsl"+s+"optiml"+s+"datastruct"+s + this.toString

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
        if (specialize2 contains (f.getName.substring(0, f.getName.indexOf(".")))) {
          genSpec2(f, path)
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

trait OptiMLCodeGenScala extends OptiLACodeGenScala with OptiMLCodeGenBase with OptiMLScalaCodeGenPkg
  with ScalaGenApplicationOps with ScalaGenLBPOps with ScalaGenLanguageOps 
  with ScalaGenVectorOps with ScalaGenMatrixOps with ScalaGenIndexVectorOps with ScalaGenIndexVectorDenseOps with ScalaGenIndexVector2Ops 
  with ScalaGenStreamOps with ScalaGenStreamRowOps
  with ScalaGenGraphOps with ScalaGenVerticesOps with ScalaGenEdgeOps with ScalaGenVertexOps with ScalaGenMessageEdgeOps with ScalaGenMessageVertexOps with ScalaGenVSetOps
  with ScalaGenLabelsOps with ScalaGenTrainingSetOps with ScalaGenVariantsOps with ScalaGenDeliteCollectionOps
  with ScalaGenImageOps with ScalaGenGrayscaleImageOps
  with DeliteScalaGenAllOverrides { //with ScalaGenMLInputReaderOps {
  
  val IR: DeliteApplication with OptiMLExp

  override val specialize = Set("LabelsImpl", "ImageImpl", "StreamImpl", "StreamRowImpl")
  override val specialize2 = Set("TrainingSetImpl")

  override def genSpec2(f: File, dsOut: String) {
    for (s1 <- List("Double","Int","Float","Long","Boolean")) {
      for (s2 <- List("Double","Int","Float","Long","Boolean")) {
        val outFile = dsOut + s1 + s2 + f.getName
        val out = new BufferedWriter(new FileWriter(outFile))
        for (line <- scala.io.Source.fromFile(f).getLines) {
          out.write(specmap2(line, s1, s2) + "\n")
        }
        out.close()
    }
    }
  }

  override def specmap(line: String, t: String) : String = {
    var res = line.replaceAll("import ppl.dsl.optila.datastruct.scala._", "") // ends up in the same package in generated code
    super.specmap(res, t)
  }
    
  def specmap2(line: String, t1: String, t2: String) : String = {
    var res = line.replaceAll("object ", "object " + t1 + t2)
    res = res.replaceAll("import ppl.dsl.optila.datastruct.scala._", "") 
    res = res.replaceAll("import ", "import " + t1 + t2)
    res = res.replaceAll("@specialized T: ClassManifest", t1)
    res = res.replaceAll("@specialized L: ClassManifest", t2)
    res = res.replaceAll("T:Manifest", t1)
    res = res.replaceAll("L:Manifest", t2)
    res = res.replaceAll("\\bT\\b", t1)
    res = res.replaceAll("\\bL\\b", t2)
    parmap(res)
  }
  
  override def parmap(line: String): String = {
    var res = line
    for(tpe1 <- List("Int","Long","Double","Float","Boolean")) {
      for (s <- specialize) {
        res = res.replaceAll(s+"\\["+tpe1+"\\]", tpe1+s)
      }
      for(tpe2 <- List("Int","Long","Double","Float","Boolean")) {
        for (s <- specialize2) {
          // should probably parse and trim whitespace, this is fragile
          res = res.replaceAll(s+"\\["+tpe1+","+tpe2+"\\]", tpe1+tpe2+s)
          res = res.replaceAll(s+"\\["+tpe1+", "+tpe2+"\\]", tpe1+tpe2+s)
        }
      }
    }
    dsmap(res)
  }

  override def dsmap(line: String) : String = {
    var res = line.replaceAll("ppl.dsl.optiml.datastruct", "generated")
    res = res.replaceAll("import ppl.dsl.optila.datastruct.scala._", "")     
    res = res.replaceAll("ppl.delite.framework.datastruct", "generated")
    res = res.replaceAll("ppl.dsl.optiml", "generated.scala")        
    super.dsmap(res)
  }
}

trait OptiMLCodeGenCuda extends OptiLACodeGenCuda with OptiMLCodeGenBase with OptiMLCudaCodeGenPkg 
  with CudaGenDataStruct with CudaGenVectorOps with CudaGenMatrixOps with CudaGenTrainingSetOps 
  with DeliteCudaGenAllOverrides // with DeliteCodeGenOverrideCuda // with CudaGenMLInputReaderOps  //TODO:DeliteCodeGenOverrideScala needed?
{
  val IR: DeliteApplication with OptiMLExp
  import IR._


  // Maps the scala type to cuda type
  override def remap[A](m: Manifest[A]) : String = {
    m.toString match {
      case "ppl.dsl.optiml.RangeVector" => "RangeVector"
      case "ppl.dsl.optiml.IndexVector" => "IndexVector"
      case "ppl.dsl.optiml.Labels[Int]" => "Labels<int>"
      case "ppl.dsl.optiml.Labels[Long]" => "Labels<long>"
      case "ppl.dsl.optiml.Labels[Float]" => "Labels<float>"
      case "ppl.dsl.optiml.Labels[Double]" => "Labels<double>"
      case "ppl.dsl.optiml.Labels[Boolean]" => "Labels<bool>"
      case "ppl.dsl.optiml.TrainingSet[Double, Double]" => "TrainingSet<double,double>"
      case _ => super.remap(m)
    }
  }

  override def isObjectType[T](m: Manifest[T]) : Boolean = m.toString match {
    case "ppl.dsl.optiml.RangeVector" => true
    case "ppl.dsl.optiml.IndexVector" => true
    case "ppl.dsl.optiml.Labels[Int]" => true
    case "ppl.dsl.optiml.Labels[Long]" => true
    case "ppl.dsl.optiml.Labels[Float]" => true
    case "ppl.dsl.optiml.Labels[Double]" => true
    case "ppl.dsl.optiml.Labels[Boolean]" => true
    case "ppl.dsl.optiml.TrainingSet[Double, Double]" => true
    case _ => super.isObjectType(m)
  }

  override def copyInputHtoD(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Labels<int>" | "Labels<long>" | "Labels<float>" | "Labels<double>" | "Labels<bool>" => labelsCopyInputHtoD(sym)
    case "RangeVector" => rangeVectorCopyInputHtoD(sym)
    case "IndexVector" => indexVectorCopyInputHtoD(sym)
    case "TrainingSet<double,double>" => trainingSetCopyInputHtoD(sym)
    case _ => super.copyInputHtoD(sym)
  }

  override def copyOutputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
    case _ => super.copyOutputDtoH(sym)
  }

  override def copyMutableInputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Labels<int>" | "Labels<long>" | "Labels<float>" | "Labels<double>" | "Labels<bool>" => labelsCopyMutableInputDtoH(sym)
    case "RangeVector" => rangeVectorCopyMutableInputDtoH(sym)
    case "IndexVector" => indexVectorCopyMutableInputDtoH(sym)
    case "TrainingSet<double,double>" => trainingSetCopyMutableInputDtoH(sym)
    case _ => super.copyMutableInputDtoH(sym)
  }

  /*
  override def allocOutput(newSym: Sym[_], sym: Sym[_], reset: Boolean = false) : Unit = remap(newSym.Type) match {
    case _ => super.allocOutput(newSym,sym,reset)
  }
  */

  /*
  override def allocReference(newSym: Sym[Any], sym: Sym[Any]) : Unit = remap(newSym.Type) match {
    case "Labels<int>" | "Labels<long>" | "Labels<float>" | "Labels<double>" | "Labels<bool>" => emitVectorAllocRef(newSym,sym)
    case _ => super.allocReference(newSym,sym)
  }
   */

  override def positionMultDimInputs(sym: Sym[Any]) : String = remap(sym.Type) match {
    case _ => super.positionMultDimInputs(sym)
  }

  override def getDSLHeaders: String = {
    val out = new StringBuilder
    out.append(super.getDSLHeaders)
    out.append("#include \"RangeVectorImpl.h\"\n")
    out.append("#include \"IndexVectorImpl.h\"\n")
    out.append("#include \"LabelsImpl.h\"\n")
    out.append("#include \"TrainingSetImpl.h\"\n")
    out.toString
  }

}

trait OptiMLCodeGenC extends OptiLACodeGenC with OptiMLCodeGenBase with OptiMLCCodeGenPkg 
  with CGenVectorOps with CGenMatrixOps with DeliteCGenAllOverrides
{
  val IR: DeliteApplication with OptiMLExp
  import IR._

  override def remap[A](m: Manifest[A]) : String = m.toString match {
    case _ => super.remap(m)
  }
}
