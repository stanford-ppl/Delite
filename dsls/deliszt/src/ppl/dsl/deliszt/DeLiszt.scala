package ppl.dsl.deliszt

import java.io._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.analysis.TraversalAnalysis
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.c.TargetC
import ppl.delite.framework.codegen.delite.overrides.{DeliteCudaGenAllOverrides, DeliteCGenAllOverrides, DeliteScalaGenAllOverrides, DeliteAllOverridesExp}
import ppl.delite.framework.ops._
import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import scala.util.matching.Regex

import ppl.dsl.deliszt.capabilities._
import ppl.dsl.deliszt.field._
import ppl.dsl.deliszt.intm._
import ppl.dsl.deliszt.mat._
import ppl.dsl.deliszt.vec._
import ppl.dsl.deliszt.mesh._
import ppl.dsl.deliszt.meshset._

import ppl.dsl.deliszt.analysis.{DeLisztCodeGenAnalysis, LoopColoringOpt, LoopColoringOpsExp, ScalaGenLoopColoringOps}

/**
 * These are the portions of Scala imported into DeLiszt's scope.
 */
trait DeLisztLift extends LiftVariables with LiftEquals with LiftString with LiftBoolean with LiftNumeric {
  this: DeLiszt =>
}

trait DeLisztScalaOpsPkg extends Base
  with Equal with IfThenElse with Variables with While with Functions
  with ImplicitOps with OrderingOps with StringOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with MathOps with CastingOps with ObjectOps
  // TEST
  with RangeOps
  // only included because of args. TODO: investigate passing args as a vec
  with ArrayOps

trait DeLisztScalaOpsPkgExp extends DeLisztScalaOpsPkg with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with FunctionsExp
  with ImplicitOpsExp with OrderingOpsExp with StringOpsExp with RangeOpsExp with IOOpsExp
  with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp with TupleOpsExp
  with ListOpsExp with SeqOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp
  with ArrayBufferOpsExp

trait DeLisztScalaCodeGenPkg extends ScalaGenDSLOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenFunctions
  with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps
  with ScalaGenArrayOps with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps
  with ScalaGenListOps with ScalaGenSeqOps with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenSetOps with ScalaGenObjectOps
  with ScalaGenArrayBufferOps
  { val IR: DeLisztScalaOpsPkgExp  }

trait DeLisztCudaCodeGenPkg extends CudaGenDSLOps with CudaGenImplicitOps with CudaGenOrderingOps
  with CudaGenEqual with CudaGenIfThenElse with CudaGenVariables with CudaGenWhile with CudaGenFunctions
  with CudaGenStringOps with CudaGenRangeOps with CudaGenIOOps with CudaGenArrayOps with CudaGenBooleanOps
  with CudaGenPrimitiveOps with CudaGenMiscOps
  with CudaGenListOps with CudaGenSeqOps with CudaGenMathOps with CudaGenCastingOps with CudaGenSetOps
  with CudaGenArrayBufferOps
  { val IR: DeLisztScalaOpsPkgExp  }

trait DeLisztCCodeGenPkg extends CGenDSLOps with CGenImplicitOps with CGenOrderingOps
  with CGenStringOps with CGenRangeOps with CGenIOOps with CGenArrayOps with CGenBooleanOps
  with CGenPrimitiveOps with CGenMiscOps with CGenFunctions with CGenEqual with CGenIfThenElse
  with CGenVariables with CGenWhile with CGenListOps with CGenSeqOps
  with CGenArrayBufferOps
  { val IR: DeLisztScalaOpsPkgExp  }

/**
 * This the trait that every DeLiszt application must extend.
 */
trait DeLiszt extends DeLisztScalaOpsPkg with LanguageOps
  with MeshPrivateOps with MeshSetOps
  with IntMOps
  with MathOps
  with ArithOps with FieldOps with MatOps with VecOps with OrderingOps with HasMinMaxOps {
  this: DeLisztApplication =>
}

// these ops are only available to the compiler (they are restricted from application use)
trait DeLisztCompiler extends DeLiszt with ListOps {  // FieldPrivateOps, MeshPrivateOps
  this: DeLisztApplication with DeLisztExp =>
}

/**
 * These are the corresponding IR nodes for DeLiszt.
 */
trait DeLisztExp extends DeLisztCompiler with DeLisztScalaOpsPkgExp with LanguageOpsExp
  with LanguageImplOpsStandard
  with MeshSetOpsExp
  with MeshPrivateOpsExp
  with ArithOpsExpOpt
  with OrderingOpsExp
  with MathOpsExp
  with IntMOpsExp
  with LoopColoringOpsExp
  with DeliteOpsExp with VariantsOpsExp with DeliteAllOverridesExp
  with FieldOpsExpOpt with FieldImplOpsStandard with MatOpsExp with MatImplOpsStandard with VecOpsExp with VecImplOpsStandard {

  this: DeliteApplication with DeLisztApplication with DeLisztExp => // can't be DeLisztApplication right now because code generators depend on stuff inside DeliteApplication (via IR)

  def getCodeGenPkg(t: Target{val IR: DeLisztExp.this.type}) : GenericFatCodegen{val IR: DeLisztExp.this.type} = {
    t match {
      case _:TargetScala => new DeLisztCodeGenScala{val IR: DeLisztExp.this.type = DeLisztExp.this}
      case _:TargetCuda => new DeLisztCodeGenCuda{val IR: DeLisztExp.this.type = DeLisztExp.this}
      case _:TargetC => new DeLisztCodeGenC{val IR: DeLisztExp.this.type = DeLisztExp.this} 
      case _ => throw new RuntimeException("DeLiszt does not support this target")
    }
  }

  override lazy val analyses = scala.collection.immutable.List(new DeLisztCodeGenAnalysis{val IR: DeLisztExp.this.type = DeLisztExp.this})
}


/**
 * DeLiszt code generators
 */
trait DeLisztCodeGenBase extends GenericFatCodegen {

  val IR: DeliteApplication with DeLisztExp
  override def initialDefs = IR.deliteGenerator.availableDefs


  def dsmap(line: String) = line

  val specialize = Set[String]()
  val specialize2 = Set[String]()
  def genSpec(f: File, outPath: String) = {}
  def genSpec2(f: File, outPath: String) = {}

  override def emitDataStructures(path: String) {
    val s = File.separator
    val dsRoot = Config.homeDir + s+"dsls"+s+"deliszt"+s+"src"+s+"ppl"+s+"dsl"+s+"deliszt"+s+"datastruct"+s + this.toString

    val dsDir = new File(dsRoot)
    if (!dsDir.exists) return
    val outDir = new File(path)
    outDir.mkdirs()

    for (f <- dsDir.listFiles) {
      if (specialize contains (f.getName.substring(0, f.getName.indexOf(".")))) {
        genSpec(f, path)
      }
      if (specialize2 contains (f.getName.substring(0, f.getName.indexOf(".")))) {
        genSpec2(f, path)
      }
      val outFile = path + f.getName
      val out = new BufferedWriter(new FileWriter(outFile))
      for (line <- scala.io.Source.fromFile(f).getLines) {
        out.write(dsmap(line) + "\n")
      }
      out.close()
    }
  }
}

trait DeLisztCodeGenScala extends DeLisztCodeGenBase with DeLisztScalaCodeGenPkg with ScalaGenDeliteOps with ScalaGenLanguageOps
  with ScalaGenArithOps with ScalaGenVariantsOps with ScalaGenDeliteCollectionOps
  with ScalaGenFieldOps with ScalaGenIntMOps with ScalaGenMeshPrivateOps with ScalaGenMeshSetOps
  with ScalaGenMatOps with ScalaGenVecOps with ScalaGenLoopColoringOps
  with DeliteScalaGenAllOverrides { //with ScalaGenMLInputReaderOps {
  
  val IR: DeliteApplication with DeLisztExp

  override val specialize = Set("VecImpl", "MatImpl", "MatColImpl", "MatRowImpl", "VecViewImpl")

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

  def specmap(line: String, t: String) : String = {
    var res = line.replaceAll("object ", "object " + t)
    res = res.replaceAll("@specialized T: ClassManifest", t)
    res = res.replaceAll("\\bT:Manifest\\b", t)
    res = res.replaceAll("\\bT\\b", t)
    parmap(res)
  }

  def specmap2(line: String, t1: String, t2: String) : String = {
    var res = line.replaceAll("object ", "object " + t1 + t2)
    res = res.replaceAll("@specialized T: ClassManifest", t1)
    res = res.replaceAll("@specialized L: ClassManifest", t2)
    res = res.replaceAll("T:Manifest", t1)
    res = res.replaceAll("L:Manifest", t2)
    res = res.replaceAll("\\bT\\b", t1)
    res = res.replaceAll("\\bL\\b", t2)
    parmap(res)
  }

  override def remap[A](m: Manifest[A]): String = {
    parmap(super.remap(m))
  }

  def parmap(line: String): String = {
    var res = line
    for(tpe1 <- List("Int","Long","Double","Float","Boolean")) {
      val parSub = (m: Regex.Match) => {
        val rest = (m.group(1) + m.group(3)).replaceAll("""^\s+""", "")
        if(rest.length > 0) {
          "[" + rest + "]"
        }
        else {
          ""
        }
      }

      for (s <- specialize) {
        val expr = ("\\b" + s + "\\[(.*?)(,\\s*)?\\b" + tpe1 + "\\b(.*?)\\]\\(").r  
        res = expr.replaceAllIn(res, m => tpe1 + s + parSub(m) + "(")
      }
      
      // Map methods (like in the companion)
      val expr = ("\\[(.*?)(,\\s*)?\\b" + tpe1 + "\\s*:\\s*\\w+?\\b(.*?)\\]\\(").r  
      res = expr.replaceAllIn(res, m => parSub(m) + "(")
        
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
    var res = line.replaceAll("ppl.dsl.deliszt.datastruct", "generated")
    res = res.replaceAll("ppl.delite.framework.datastruct", "generated")
    res
  }
}

trait DeLisztCodeGenCuda extends DeLisztCodeGenBase with DeLisztCudaCodeGenPkg /*with CudaGenLanguageOps*/
  with CudaGenArithOps with CudaGenDeliteOps
  with CudaGenVariantsOps with CudaGenDeliteCollectionOps
  with CudaGenFieldOps with CudaGenIntMOps with CudaGenMeshPrivateOps with CudaGenMeshSetOps
  with CudaGenVecOps with CudaGenMatOps with CudaGenDataStruct with CudaGenMatRowOps
{
  val IR: DeliteApplication with DeLisztExp
  import IR._

  // Maps the scala type to cuda type
  override def remap[A](m: Manifest[A]) : String = {
    m.toString match {
      case "ppl.dsl.deliszt.datastruct.scala.Mat[Int]" => "Mat<int>"
      case "ppl.dsl.deliszt.datastruct.scala.Mat[Long]" => "Mat<long>"
      case "ppl.dsl.deliszt.datastruct.scala.Mat[Float]" => "Mat<float>"
      case "ppl.dsl.deliszt.datastruct.scala.Mat[Double]" => "Mat<double>"
      case "ppl.dsl.deliszt.datastruct.scala.Mat[Boolean]" => "Mat<bool>"
      case "ppl.dsl.deliszt.datastruct.scala.Vec[Int]" => "Vec<int>"
      case "ppl.dsl.deliszt.datastruct.scala.Vec[Long]" => "Vec<long>"
      case "ppl.dsl.deliszt.datastruct.scala.Vec[Float]" => "Vec<float>"
      case "ppl.dsl.deliszt.datastruct.scala.Vec[Double]" => "Vec<double>"
      case "ppl.dsl.deliszt.datastruct.scala.Vec[Boolean]" => "Vec<bool>"
      case _ => super.remap(m)
    }
  }

  override def isObjectType[T](m: Manifest[T]) : Boolean = m.toString match {
    case "ppl.dsl.deliszt.datastruct.scala.Mat[Int]" => true
    case "ppl.dsl.deliszt.datastruct.scala.Mat[Long]" => true
    case "ppl.dsl.deliszt.datastruct.scala.Mat[Float]" => true
    case "ppl.dsl.deliszt.datastruct.scala.Mat[Double]" => true
    case "ppl.dsl.deliszt.datastruct.scala.Mat[Boolean]" => true
    case "ppl.dsl.deliszt.datastruct.scala.Vec[Int]" => true
    case "ppl.dsl.deliszt.datastruct.scala.Vec[Long]" => true
    case "ppl.dsl.deliszt.datastruct.scala.Vec[Float]" => true
    case "ppl.dsl.deliszt.datastruct.scala.Vec[Double]" => true
    case "ppl.dsl.deliszt.datastruct.scala.Vec[Boolean]" => true
    case _ => super.isObjectType(m)
  }

  override def copyInputHtoD(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Mat<int>" | "Mat<long>" | "Mat<float>" | "Mat<double>" | "Mat<bool>" => matCopyInputHtoD(sym)
    case "Vec<int>" | "Vec<long>" | "Vec<float>" | "Vec<double>" | "Vec<bool>" => vecCopyInputHtoD(sym)
    case _ => super.copyInputHtoD(sym)
  }

  override def copyOutputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Mat<int>" | "Mat<long>" | "Mat<float>" | "Mat<double>" | "Mat<bool>" => matCopyOutputDtoH(sym)
    case "Vec<int>" | "Vec<long>" | "Vec<float>" | "Vec<double>" | "Vec<bool>" => vecCopyOutputDtoH(sym)
    case _ => super.copyOutputDtoH(sym)
  }

  override def copyMutableInputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Mat<int>" | "Mat<long>" | "Mat<float>" | "Mat<double>" | "Mat<bool>" => matCopyMutableInputDtoH(sym)
    case "Vec<int>" | "Vec<long>" | "Vec<float>" | "Vec<double>" | "Vec<bool>" => vecCopyMutableInputDtoH(sym)
    case _ => super.copyMutableInputDtoH(sym)
  }

  override def positionMultDimInputs(sym: Sym[Any]) : String = remap(sym.Type) match {
    //TODO: Add mat reposition, and also do safety check for datastructures that do not have data field
    case "Vec<int>" | "Vec<long>" | "Vec<float>" | "Vec<double>" | "Vec<bool>" => vecPositionMultDimInputs(sym)
    case _ => super.positionMultDimInputs(sym)
  }

  override def getDSLHeaders: String = {
    val out = new StringBuilder
    out.append("#include <float.h>\n")
    out.append("#include \"VecImpl.h\"\n")
    out.append("#include \"MatImpl.h\"\n")
    out.toString
  }
}

trait DeLisztCodeGenC extends DeLisztCodeGenBase with DeLisztCCodeGenPkg with CGenArithOps with CGenDeliteOps
  with CGenVariantsOps with DeliteCGenAllOverrides
  with CGenFieldOps with CGenIntMOps with CGenMeshPrivateOps with CGenMeshSetOps
  with CGenVecOps with CGenMatOps
{
  val IR: DeliteApplication with DeLisztExp
  import IR._
}
