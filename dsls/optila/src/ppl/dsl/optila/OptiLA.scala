package ppl.dsl.optila

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
import ppl.dsl.optila.extern.{OptiLAScalaGenExternal, OptiLACudaGenExternal}
import ppl.dsl.optila.vector._
import ppl.dsl.optila.matrix._
import ppl.dsl.optila.capabilities._
import ppl.dsl.optila.io._


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

/**
 * These are the portions of Scala imported into OptiLA's scope.
 */
trait OptiLALift extends LiftVariables with LiftEquals with LiftString with LiftBoolean with LiftNumeric {
  this: OptiLA =>
}

trait OptiLAScalaOpsPkg extends Base
  with Equal with IfThenElse with Variables with While with Functions
  with ImplicitOps with OrderingOps with StringOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with MathOps with CastingOps with ObjectOps with IOOps
  // only included because of args. TODO: investigate passing args as a vector
  with ArrayOps

trait OptiLAScalaOpsPkgExp extends OptiLAScalaOpsPkg with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with FunctionsExp
  with ImplicitOpsExp with OrderingOpsExp with StringOpsExp with RangeOpsExp with IOOpsExp
  with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp with TupleOpsExp
  with ListOpsExp with SeqOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp
  with SynchronizedArrayBufferOpsExp with HashMapOpsExp with IterableOpsExp

trait OptiLAScalaCodeGenPkg extends ScalaGenDSLOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenFunctions
  with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps
  with ScalaGenArrayOps with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps
  with ScalaGenListOps with ScalaGenSeqOps with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenSetOps with ScalaGenObjectOps
  with ScalaGenSynchronizedArrayBufferOps with ScalaGenHashMapOps with ScalaGenIterableOps
  { val IR: OptiLAScalaOpsPkgExp  }

trait OptiLACudaCodeGenPkg extends CudaGenDSLOps with CudaGenImplicitOps with CudaGenOrderingOps
  with CudaGenEqual with CudaGenIfThenElse with CudaGenVariables with CudaGenWhile with CudaGenFunctions
  with CudaGenStringOps with CudaGenRangeOps with CudaGenIOOps with CudaGenArrayOps with CudaGenBooleanOps
  with CudaGenPrimitiveOps with CudaGenMiscOps
  with CudaGenListOps with CudaGenSeqOps with CudaGenMathOps with CudaGenCastingOps with CudaGenSetOps
  with CudaGenSynchronizedArrayBufferOps with CudaGenHashMapOps with CudaGenIterableOps
  { val IR: OptiLAScalaOpsPkgExp  }

trait OptiLACCodeGenPkg extends CGenDSLOps with CGenImplicitOps with CGenOrderingOps
  with CGenStringOps with CGenRangeOps with CGenIOOps with CGenArrayOps with CGenBooleanOps
  with CGenPrimitiveOps with CGenMiscOps with CGenFunctions with CGenEqual with CGenIfThenElse
  with CGenVariables with CGenWhile with CGenListOps with CGenSeqOps
  with CGenSynchronizedArrayBufferOps with CGenHashMapOps with CGenIterableOps
  { val IR: OptiLAScalaOpsPkgExp  }

/**
 * This the trait that every OptiLA application must extend.
 */
trait OptiLA extends OptiLAScalaOpsPkg 
  with LanguageOps with ArithOps with CloneableOps with HasMinMaxOps with VectorOps with MatrixOps with VectorViewOps
  with LAInputReaderOps with LAOutputWriterOps
  with MatrixRowOps with MatrixColOps {

  this: OptiLAApplication =>
}

// these ops are only available to the compiler (they are restricted from application use)
trait OptiLACompiler extends OptiLA with DeliteCollectionOps with RangeOps with IOOps with SeqOps with SetOps
  with ListOps with HashMapOps with IterableOps {
    
  this: OptiLAApplication with OptiLAExp =>
}


/**
 * These are the corresponding IR nodes for OptiLA.
 */
trait OptiLAExp extends OptiLACompiler with OptiLAScalaOpsPkgExp with DeliteOpsExp with VariantsOpsExp 
  with LanguageOpsExp with ArithOpsExpOpt 
  with VectorOpsExpOpt with MatrixOpsExpOpt with VectorViewOpsExp with MatrixRowOpsExpOpt with MatrixColOpsExpOpt
  with LAInputReaderOpsExp with LAOutputWriterOpsExp
  with LanguageImplOpsStandard
  with VectorImplOpsStandard with MatrixImplOpsStandard 
  with LAInputReaderImplOpsStandard with LAOutputWriterImplOpsStandard
  with DeliteAllOverridesExp {

  // this: OptiLAApplicationRunner => why doesn't this work?
  this: DeliteApplication with OptiLAApplication with OptiLAExp => // can't be OptiLAApplication right now because code generators depend on stuff inside DeliteApplication (via IR)

  def getCodeGenPkg(t: Target{val IR: OptiLAExp.this.type}) : GenericFatCodegen{val IR: OptiLAExp.this.type} = {
    t match {
      case _:TargetScala => new OptiLACodeGenScala{val IR: OptiLAExp.this.type = OptiLAExp.this}
      case _:TargetCuda => new OptiLACodeGenCuda{val IR: OptiLAExp.this.type = OptiLAExp.this}
      case _:TargetC => new OptiLACodeGenC{val IR: OptiLAExp.this.type = OptiLAExp.this} 
      case _ => throw new RuntimeException("optiml does not support this target")
    }
  }

}


/**
 * OptiLA code generators
 */
trait OptiLACodeGenBase extends GenericFatCodegen {

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

trait OptiLACodeGenScala extends OptiLACodeGenBase with OptiLAScalaCodeGenPkg with OptiLAScalaGenExternal with ScalaGenDeliteOps
  with ScalaGenLanguageOps with ScalaGenArithOps with ScalaGenVectorOps with ScalaGenVectorViewOps with ScalaGenMatrixOps
  with ScalaGenMatrixRowOps with ScalaGenMatrixColOps
  with ScalaGenVariantsOps with ScalaGenDeliteCollectionOps
  with DeliteScalaGenAllOverrides { //with ScalaGenMLInputReaderOps {
  
  val IR: DeliteApplication with OptiLAExp

  override val specialize = Set("VectorImpl", "MatrixImpl", "SymmetricMatrixImpl", "VectorViewImpl", "MatrixRowImpl", "MatrixColImpl")

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
    res = res.replaceAll("@specialized T: ClassManifest", t)
    res = res.replaceAll("T:Manifest", t)
    res = res.replaceAll("\\bT\\b", t)
    parmap(res)
  }

  override def remap[A](m: Manifest[A]): String = {
    var res = super.remap(m)
    res = res.replaceAllLiterally("package$", "")
    parmap(res)
  }

  def parmap(line: String): String = {
    var res = line
    for(tpe1 <- List("Int","Long","Double","Float","Boolean")) {
      for (s <- specialize) {
        res = res.replaceAll(s+"\\["+tpe1+"\\]", tpe1+s)
      }
    }
    dsmap(res)
  }

  override def dsmap(line: String) : String = {
    var res = line.replaceAll("ppl.dsl.optila.datastruct", "generated")
    res = res.replaceAll("ppl.delite.framework.datastruct", "generated")
    res = res.replaceAll("ppl.dsl.optila", "generated.scala")    
    res
  }
}

trait OptiLACodeGenCuda extends OptiLACodeGenBase with OptiLACudaCodeGenPkg with OptiLACudaGenExternal
  with CudaGenArithOps with CudaGenDeliteOps with CudaGenVectorOps with CudaGenMatrixOps with CudaGenDataStruct with CudaGenMatrixRowOps // with CudaGenVectorViewOps
  with CudaGenVariantsOps with DeliteCudaGenAllOverrides with CudaGenDeliteCollectionOps // with DeliteCodeGenOverrideCuda // with CudaGenMLInputReaderOps  //TODO:DeliteCodeGenOverrideScala needed?
{
  val IR: DeliteApplication with OptiLAExp
  import IR._


  // Maps the scala type to cuda type
  override def remap[A](m: Manifest[A]) : String = {
    m.toString match {
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
      case "ppl.dsl.optiml.RangeVector" => "RangeVector"
      case _ => super.remap(m)
    }
  }

  override def isObjectType[T](m: Manifest[T]) : Boolean = m.toString match {
    case "ppl.dsl.optiml.Matrix[Int]" => true
    case "ppl.dsl.optiml.Matrix[Long]" => true
    case "ppl.dsl.optiml.Matrix[Float]" => true
    case "ppl.dsl.optiml.Matrix[Double]" => true
    case "ppl.dsl.optiml.Matrix[Boolean]" => true
    case "ppl.dsl.optiml.Vector[Int]" => true
    case "ppl.dsl.optiml.Vector[Long]" => true
    case "ppl.dsl.optiml.Vector[Float]" => true
    case "ppl.dsl.optiml.Vector[Double]" => true
    case "ppl.dsl.optiml.Vector[Boolean]" => true
    case _ => super.isObjectType(m)
  }

  override def copyInputHtoD(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Matrix<int>" | "Matrix<long>" | "Matrix<float>" | "Matrix<double>" | "Matrix<bool>" => matrixCopyInputHtoD(sym)
    case "Vector<int>" | "Vector<long>" | "Vector<float>" | "Vector<double>" | "Vector<bool>" => vectorCopyInputHtoD(sym)
    case _ => super.copyInputHtoD(sym)
  }

  override def copyOutputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Matrix<int>" | "Matrix<long>" | "Matrix<float>" | "Matrix<double>" | "Matrix<bool>" => matrixCopyOutputDtoH(sym)
    case "Vector<int>" | "Vector<long>" | "Vector<float>" | "Vector<double>" | "Vector<bool>" => vectorCopyOutputDtoH(sym)
    case _ => super.copyOutputDtoH(sym)
  }

  override def copyMutableInputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Matrix<int>" | "Matrix<long>" | "Matrix<float>" | "Matrix<double>" | "Matrix<bool>" => matrixCopyMutableInputDtoH(sym)
    case "Vector<int>" | "Vector<long>" | "Vector<float>" | "Vector<double>" | "Vector<bool>" => vectorCopyMutableInputDtoH(sym)
    case _ => super.copyMutableInputDtoH(sym)
  }

  /*
  override def allocOutput(newSym: Sym[_], sym: Sym[_], reset: Boolean = false) : Unit = remap(newSym.Type) match {
    case "Matrix<int>" | "Matrix<long>" | "Matrix<float>" | "Matrix<double>" | "Matrix<bool>" => emitMatrixAllocSym(newSym,sym,reset)
    case "Vector<int>" | "Vector<long>" | "Vector<float>" | "Vector<double>" | "Vector<bool>" => emitVectorAllocSym(newSym,sym,reset)
    case _ => super.allocOutput(newSym,sym,reset)
  }
  */

  /*
  override def allocReference(newSym: Sym[Any], sym: Sym[Any]) : Unit = remap(newSym.Type) match {
    case "Matrix<int>" | "Matrix<long>" | "Matrix<float>" | "Matrix<double>" | "Matrix<bool>" => emitMatrixAllocRef(newSym,sym)
    case "Vector<int>" | "Vector<long>" | "Vector<float>" | "Vector<double>" | "Vector<bool>" => emitVectorAllocRef(newSym,sym)
    case _ => super.allocReference(newSym,sym)
  }
   */

  override def positionMultDimInputs(sym: Sym[Any]) : String = remap(sym.Type) match {
    //TODO: Add matrix reposition, and also do safety check for datastructures that do not have data field
    case "Vector<int>" | "Vector<long>" | "Vector<float>" | "Vector<double>" | "Vector<bool>" => vectorPositionMultDimInputs(sym)
    case _ => super.positionMultDimInputs(sym)
  }

  override def getDSLHeaders: String = {
    val out = new StringBuilder
    out.append("#include <float.h>\n")
    out.append("#include \"VectorImpl.h\"\n")
    out.append("#include \"MatrixImpl.h\"\n")
    out.append("#include \"library.h\"\n") // external library
    out.toString
  }

}

trait OptiLACodeGenC extends OptiLACodeGenBase with OptiLACCodeGenPkg with CGenDeliteOps 
  with CGenArithOps with CGenVectorOps with CGenMatrixOps with CGenMatrixRowOps
  with CGenVariantsOps with DeliteCGenAllOverrides
{
  val IR: DeliteApplication with OptiLAExp
  import IR._

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
    case _ => super.remap(m)
  }
}
