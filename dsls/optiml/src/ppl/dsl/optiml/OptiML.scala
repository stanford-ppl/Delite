package ppl.dsl.optiml

import java.io._
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{Expressions, GenericFatCodegen, GenericCodegen}
import ppl.delite.framework.{Config, DeliteApplication, DeliteInteractive, DeliteInteractiveRunner, DeliteRestageOps, DeliteRestageOpsExp, DeliteRestageRunner}
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.c.TargetC
import ppl.delite.framework.codegen.opencl.TargetOpenCL
import ppl.delite.framework.codegen.restage.{DeliteCodeGenRestage,TargetRestage}
import ppl.delite.framework.codegen.delite.overrides.{DeliteCudaGenAllOverrides, DeliteOpenCLGenAllOverrides, DeliteCGenAllOverrides, DeliteScalaGenAllOverrides, DeliteAllOverridesExp}
import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._

import ppl.dsl.optila.{OptiLAApplication}
import ppl.dsl.optila.{OptiLAScalaOpsPkg, OptiLAScalaOpsPkgExp, OptiLA, OptiLAExp, OptiLACompiler, OptiLALift, OptiLAUtilities}
import ppl.dsl.optila.{OptiLAScalaCodeGenPkg, OptiLACudaCodeGenPkg, OptiLAOpenCLCodeGenPkg, OptiLACCodeGenPkg, OptiLACodeGenBase, OptiLACodeGenScala, OptiLACodeGenCuda, OptiLACodeGenOpenCL, OptiLACodeGenC}

import ppl.dsl.optiml.io._
import ppl.dsl.optiml.vector._
import ppl.dsl.optiml.matrix._
import ppl.dsl.optiml.graph._
import ppl.dsl.optiml.stream._
import ppl.dsl.optiml.library.cluster._
import ppl.dsl.optiml.library.regression._
import ppl.dsl.optiml.application._
import ppl.dsl.optiml.capabilities._
import ppl.dsl.optiml.transform._

/**
 * Microbenchmark experiments: OptiMLApplicationRunners with optimizations disabled
 */

trait OptiMLNoCSE extends Expressions {
  override def findDefinition[T](d: Def[T]) = None
}


/**
 * These separate OptiML applications from the Exp world.
 */

trait OptiMLApplicationRunner extends OptiMLApplicationRunnerBase with OptiMLExpOpt

// ex. object GDARunner extends OptiMLApplicationRunner with GDA
trait OptiMLApplicationRunnerBase extends OptiMLApplication with DeliteApplication 
// ex. trait GDA extends OptiMLApplication
trait OptiMLApplication extends OptiLAApplication with OptiML with OptiMLLift with OptiMLLibrary with DeliteInteroperability {
  var args: Rep[Array[String]]
  def main(): Unit  
}

trait OptiMLLibrary extends OptiMLKmeans with OptiMLLinReg {
  this: OptiMLApplication with OptiMLLift =>
}


trait OptiMLInteractive extends OptiMLApplication with DeliteInteractive

trait OptiMLInteractiveRunner extends OptiMLApplicationRunner with DeliteInteractiveRunner {
  /*
  case class Materialize[A](x: Any) extends Def[A]
  
  def materialize[R[X],A](x: Facade[R,A]): Exp[A] = {
    // e.g. Materialize(OptiQL.Rep[DeliteArray[Int]])
    // later, in compose, when OptiQL.Rep == OptiML.Rep, we unwrap this.
    // the problem is the composition is static and OptiML+OptiQL scope will have a different IR than both of the original OptiML and OptiQL objects!
    Materialize[A](x.body)
  }  
  */
}

// executes scope immediately
object OptiML {
  def apply[R](b: => R) = new Scope[OptiMLInteractive, OptiMLInteractiveRunner, R](b)
  
  // allows combining with one extra scope
  // def compose[A,AR](b: => Unit) = new Scope[OptiMLInteractive with A, OptiMLInteractiveRunner with AR, Unit](b)
}

// defines stuff visible outside of the scopes
// can use structs and delite arrays as valid data to pass between scopes
trait DeliteInteroperability {
  
  // ATTEMPT 1: define some abstract data type to pass data between scopes

  // abstract class Facade
  
  // case class DeliteArrayFacade[A](a: A) extends Facade {
  //   // def length = a.asInstanceOf[s.Rep[DeliteArray[Any]]].length // length is not defined in this scope..    
  // }
  
  // lean on type inference very heavily
  /*
  class Facade[R[X],A](body: R[A])
  
  def facade[R[X],A](x: R[A]) = new Facade(x)
  */
  
  /*
  def materialize[R[X],A](x: Facade[R,A]) = {
    // type-system hack - if R is not Exp, we are in trouble
    // still won't work: it's the *wrong* Exp. what can we do?
    //  x: OptiML1.Exp[DeliteArray[Int]]
    
    
    // what if materialize constructs an IR node of the right type, which is unwrapped when the blocks are composed?
    //  -- composed block has an IR type different than either of the individual block..
    
    //  can only materialize it inside a combined scope, when the Exps are the same!
    // but this scope needs to be defined statically somehow..
    
    val rx = x.body.asInstanceOf[Rep[A]]
    Predef.println("got: " + rx)
    
    throw new RuntimeException("--")
  }
  */
  
  
  // ATTEMPT 2: try to stage the combined scopes as a single object with narrow visibility for each block
  // PROBLEM: limited visibiility is not enough. rewrites from object B can still effect object A if mixed together..
  /*
  
  def compose[A <: DeliteInterativeRunner,B <: DeliteApplication](b1: => Any)(b2: => Any)/*(implicit aToI: A => DeliteInteractive, aToIRunner: A => DeliteInteractiveRunner)*/ {
    
    val a = new Scope[A, A with DeliteStager, Any](b1) // stage b1 with A in scope only
    
    class B2 extends A with B with DeliteStager {
      // for the input data of A to be the same Exp type as B, *while* staging B, we must stage B in the same object as A
      // but now A's rewrites could possibly effect B!
      
      static = a // input data available to block 2 
    }    
  
    val b = new Scope[B, B with DeliteStager, Any](b2) // stage b2 with B in scope only
    
    
    val c = new Scope[A with B, A with B with DeliteStager, Any]
    
    // val z = () => { b1; b2 }    
    // val globalCore = new Scope[A,B with DeliteStager,Any](z) 
    
    // how do we narrow the visible type for each block?
    
    // how do we set the output of the first block as the input (args) to the second?        
  }
  */
  
  // fundamentally, we have two conflicting goals:
  //  1. we want to refer to a result from a previous staged block as a normal Rep in our scope
  //  2. but we don't want to be in the same object as the other block due to possibly conflicting rewrites
  
  // problems with single object:
  //  1. rewrites
  //  2. namespace conflicts
  //  3. transformation conflicts (can still be applied independently?)
  
  // what about the simpler problem of composing two blocks (not known ahead of time) into a single object, with narrow visibility for each block?
  // in this case, we don't need to lower each object IR and then recombine, since they are the same IR (subject to the problems above)
  //    - still no good way to limit the visibility of each block - has to be narrowed at call site (application), and then it is not isolated in a scope
  
  // the problem seems artificial though: Exp in the second scope is not the same as Exp in the first scope (except it is, more or less). The type system
  // is what's binding our hands here, not anything fundamental.
  // ideally, we want the two scopes to share the definition of Rep[T] = Exp[T], but nothing else, but to do that Exp[T] can't be object dependent
    
  
  /*
  def compose[A,B](b1: => Any, b2: => Any) {
    
  }
  */  
}

// ATTEMPT 3: stage DSL scopes individually and generate re-stageable code
// 
// what if we only save the rhs of the lowered expression from the first scope, and reconstruct the sym in the second scope? how would we reconstruct it?
// DeliteArrayNew(..., ..., ...) -> we'd actually want to execute these definitions from a serialized representation.. seems tough
//    - but we could use program generation to create a function that would execute the appropriate case class constructors to reconstruct the IR
//    - essentially serialize the IR out as a low-level Delite IR generator! When we are trying to stage block b, we first run (gen a), and set args to that.
//
// do we need a code generator for every IR node that essentially emits mirror? that seems terrible.
// 
// - first execution of the program:
//    - stage block A independently. schedule result, and emitNode on each result to recreate the relevant low-level IR nodes.
//    - set B args to some abstract placeholder for A??
//    - stage block B independently. schedule result, and emitNode on each result to recreate the relevant low-level IR nodes.
// 
// - second execution of the program:
//    - run func A to recreate A's results (now in a Delite-only base IR)
//    - in same IR, run func B to recreate B's results
// 

// stages scope and generates re-stageable code

trait OptiMLLower extends OptiMLApplication with DeliteRestageOps
trait OptiMLLowerRunner extends OptiMLApplicationRunner with DeliteRestageRunner

object OptiML_ {
  def apply[R](b: => R) = new Scope[OptiMLLower, OptiMLLowerRunner, R](b)
}

// def compose(scopes: DeliteApplication*) = {
//   // stage and execute the independent Delite apps
// }


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

trait OptiMLOpenCLCodeGenPkg extends OptiLAOpenCLCodeGenPkg
  { val IR: OptiMLScalaOpsPkgExp  }

trait OptiMLCCodeGenPkg extends OptiLACCodeGenPkg
  { val IR: OptiMLScalaOpsPkgExp  }

/**
 * This is the trait that every OptiML application must extend.
 */
trait OptiML extends OptiMLScalaOpsPkg with OptiLA with StructOps
  with LanguageOps with ApplicationOps 
  with MLInputReaderOps with MLOutputWriterOps
  with CanSumOps
  with VectorOps with OptiMLDenseVectorOps with OptiMLDenseVectorViewOps with OptiMLSparseVectorOps with OptiMLSparseVectorViewOps with OptiMLRangeVectorOps
  with MatrixOps with OptiMLDenseMatrixOps with OptiMLSparseMatrixOps 
  with IndexVectorOps with IndexVectorDenseOps with IndexVectorRangeOps with IndexVector2Ops with IndexVectorTriangularOps
  with StreamOps with StreamRowOps
  with GraphOps with EdgeOps with VertexOps with VSetOps
  with TrainingSetOps with ImageOps with GrayscaleImageOps with GrayscaleImageOpsExtension {

  this: OptiMLApplication =>
}

// these ops are only available to the compiler (they are restricted from application use)
trait OptiMLCompiler extends OptiLACompiler with OptiML with OptiMLUtilities with GraphCompilerOps with DeliteCollectionOps 
  with LanguageImplOpsStandard with VectorImplOpsStandard with IndexVectorImplOpsStandard with MatrixImplOpsStandard
  with GrayscaleImageImplOpsStandard with MLInputReaderImplOpsStandard with MLOutputWriterImplOpsStandard with StreamImplOpsStandard
  with GraphImplOpsStandard with EdgeImplOpsStandard with VertexImplOpsStandard with VerticesImplOpsStandard {

  this: OptiMLApplication with OptiMLExp =>
}


/**
 * These are the corresponding IR nodes for OptiML.
 */
trait OptiMLExp extends OptiLAExp with OptiMLCompiler with OptiMLScalaOpsPkgExp with StructExp
  with LanguageOpsExpOpt with ApplicationOpsExp
  with MLInputReaderOpsExp with MLOutputWriterOpsExp
  with VectorOpsExpOpt with MatrixOpsExpOpt with DenseMatrixOpsExpOpt 
  with IndexVectorOpsExp with IndexVectorDenseOpsExpOpt with IndexVectorRangeOpsExp with IndexVector2OpsExp with IndexVectorTriangularOpsExp
  with StreamOpsExpOpt with StreamRowOpsExpOpt
  with TrainingSetOpsExp with ImageOpsExp with GrayscaleImageOpsExp
  with GraphOpsExp with EdgeOpsExp with VertexOpsExp with VSetOpsExp
  with MultiloopTransformExp
  with DeliteRestageOpsExp
  with DeliteAllOverridesExp {

  // this: OptiMLApplicationRunner => why doesn't this work?
  this: DeliteApplication with OptiMLApplication with OptiMLExp => // can't be OptiMLApplication right now because code generators depend on stuff inside DeliteApplication (via IR)

  override def getCodeGenPkg(t: Target{val IR: OptiMLExp.this.type}) : GenericFatCodegen{val IR: OptiMLExp.this.type} = {
    t match {
      case _:TargetScala => new OptiMLCodeGenScala{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _:TargetCuda => new OptiMLCodeGenCuda{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _:TargetOpenCL => new OptiMLCodeGenOpenCL{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _:TargetC => new OptiMLCodeGenC{val IR: OptiMLExp.this.type = OptiMLExp.this} 
      case _:TargetRestage => new OptiMLCodeGenRestage{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _ => err("optiml does not support this target")
    }
  }
}

// add rewritings
trait OptiMLExpOpt extends OptiMLExp
  with VectorOpsExpOpt with MatrixOpsExpOpt with StreamOpsExpOpt with StreamRowOpsExpOpt {
    
  this: DeliteApplication with OptiMLApplication with OptiMLExp =>
}

trait OptiMLUtilities extends OptiLAUtilities {
  override def err(s: String)(implicit ctx: SourceContext) = {
    println("[optiml error]: " + s)
    println("  at " + (ctx.fileName.split("/").last + ":" + ctx.line).mkString("//").mkString(";"))
    exit(1)
  }
  override def warn(s: String) = println("[optiml warning]: " + s)
}


/**
 * OptiML code generators
 */
trait OptiMLCodeGenBase extends OptiLACodeGenBase {

  val IR: DeliteApplication with OptiMLExp
  override def initialDefs = IR.deliteGenerator.availableDefs

  val mlspecialize = Set[String]()
  val mlspecialize2 = Set[String]()  
  def genSpec2(f: File, outPath: String) = {}

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "IndexVectorDense" => IR.structName(m)
    case "SupervisedTrainingSet" => IR.structName(m)
    case "UnsupervisedTrainingSet" => IR.structName(m)
    case _ => super.remap(m)
  }
    
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
        if (mlspecialize contains (f.getName.substring(0, f.getName.indexOf(".")))) {
          genSpec(f, path)
        }
        if (mlspecialize2 contains (f.getName.substring(0, f.getName.indexOf(".")))) {
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

// strategy is to inherit all of the base Scala generators and override what we need
trait OptiMLCodeGenRestage extends OptiMLScalaCodeGenPkg with DeliteCodeGenRestage { 
  val IR: DeliteApplication with OptiMLExp
}

trait OptiMLCodeGenScala extends OptiLACodeGenScala with OptiMLCodeGenBase with OptiMLScalaCodeGenPkg
  with ScalaGenApplicationOps with ScalaGenLanguageOps 
  with ScalaGenVectorOps with ScalaGenMatrixOps with ScalaGenIndexVectorOps with ScalaGenIndexVectorDenseOps with ScalaGenIndexVector2Ops 
  with ScalaGenStreamOps with ScalaGenStreamRowOps
  with ScalaGenGraphOps with ScalaGenEdgeOps with ScalaGenVertexOps with ScalaGenVSetOps
  with ScalaGenTrainingSetOps with ScalaGenVariantsOps with ScalaGenDeliteCollectionOps
  with ScalaGenImageOps with ScalaGenGrayscaleImageOps
  with DeliteScalaGenAllOverrides { //with ScalaGenMLInputReaderOps {
  
  val IR: DeliteApplication with OptiMLExp

  override val mlspecialize = Set(/*"LabelsImpl", "Image",*/ "UnsupervisedTrainingSet", "Stream", "StreamRow")
  override val mlspecialize2 = Set("SupervisedTrainingSet")

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

  override def remap(s: String) = parmap(s)
  
  override def specmap(line: String, t: String) : String = {
    var res = line.replaceAll("import ppl.dsl.optila.datastruct.scala._", "") // ends up in the same package in generated code    
    super.specmap(res, t)
  }
    
  def specmap2(line: String, t1: String, t2: String) : String = {
    var res = line.replaceAll("object ", "object " + t1 + t2)
    res = res.replaceAll("import ppl.dsl.optila.datastruct.scala._", "") 
    res = res.replaceAll("import ", "import " + t1 + t2)
    res = res.replaceAll("@specialized T: Manifest", t1)
    res = res.replaceAll("@specialized L: Manifest", t2)
    res = res.replaceAll("T:Manifest", t1)
    res = res.replaceAll("L:Manifest", t2)
    res = res.replaceAll("\\bT\\b", t1)
    res = res.replaceAll("\\bL\\b", t2)
    dsmap(res)
  }
  
  override def parmap(line: String): String = {
    var res = line
    for(tpe1 <- List("Int","Long","Double","Float","Boolean")) {
      for (s <- mlspecialize) {
        res = res.replaceAll(s+"\\["+tpe1+"\\]", tpe1+s)
      }
      for(tpe2 <- List("Int","Long","Double","Float","Boolean")) {
        for (s <- mlspecialize2) {
          // should probably parse and trim whitespace, this is fragile
          res = res.replaceAll(s+"\\["+tpe1+","+tpe2+"\\]", tpe1+tpe2+s)
          res = res.replaceAll(s+"\\["+tpe1+", "+tpe2+"\\]", tpe1+tpe2+s)
        }
      }
    }
    super.parmap(res)
  }

  override def dsmap(line: String) : String = {
    var res = line.replaceAll("ppl.dsl.optiml.datastruct", "generated")
    res = res.replaceAll("import ppl.dsl.optila.datastruct.scala._", "")     
    res = res.replaceAll("ppl.delite.framework.datastruct", "generated")
    res = res.replaceAll("ppl.dsl.optiml", "generated.scala")        
    super.dsmap(res) // calls parmap
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
      case "ppl.dsl.optiml.datastruct.scala.IndexVector" => "IndexVector"
      case "ppl.dsl.optiml.datastruct.scala.TrainingSet[Double, Double]" => "TrainingSet<double,double>"
      case "ppl.dsl.optiml.datastruct.scala.TrainingSet[Double, Int]" => "TrainingSet<double,int>"
      case _ => super.remap(m)
    }
  }

  override def isObjectType[T](m: Manifest[T]) : Boolean = m.toString match {
    case "ppl.dsl.optiml.datastruct.scala.IndexVector" => true
    case "ppl.dsl.optiml.datastruct.scala.TrainingSet[Double, Double]" => true
    case "ppl.dsl.optiml.datastruct.scala.TrainingSet[Double, Int]" => true
    case _ => super.isObjectType(m)
  }

  override def copyInputHtoD(sym: Sym[Any]) : String = remap(sym.tp) match {
    case "IndexVector" => indexVectorCopyInputHtoD(sym)
    case "TrainingSet<double,double>" => trainingSetCopyInputHtoD(sym)
    case "TrainingSet<double,int>" => trainingSetCopyInputHtoD(sym)
    case _ => super.copyInputHtoD(sym)
  }

  override def copyOutputDtoH(sym: Sym[Any]) : String = remap(sym.tp) match {
    case _ => super.copyOutputDtoH(sym)
  }

  override def copyMutableInputDtoH(sym: Sym[Any]) : String = remap(sym.tp) match {
    case "IndexVector" => indexVectorCopyMutableInputDtoH(sym)
    case "TrainingSet<double,double>" => trainingSetCopyMutableInputDtoH(sym)
    case "TrainingSet<double,int>" => trainingSetCopyMutableInputDtoH(sym)
    case _ => super.copyMutableInputDtoH(sym)
  }

  override def getDSLHeaders: String = {
    val out = new StringBuilder
    out.append(super.getDSLHeaders)
    out.append("#include \"IndexVectorImpl.h\"\n")
    out.append("#include \"TrainingSetImpl.h\"\n")
    out.toString
  }

}

trait OptiMLCodeGenOpenCL extends OptiLACodeGenOpenCL with OptiMLCodeGenBase with OptiMLOpenCLCodeGenPkg
  with OpenCLGenDataStruct with OpenCLGenVectorOps with OpenCLGenMatrixOps with OpenCLGenTrainingSetOps
  with DeliteOpenCLGenAllOverrides
{
  val IR: DeliteApplication with OptiMLExp
  import IR._

  override def isObjectType[T](m: Manifest[T]) : Boolean = m.toString match {
    //case "ppl.dsl.optiml.IndexVector" => true
    //case "ppl.dsl.optiml.TrainingSet[Double, Double]" => true
    //case "ppl.dsl.optiml.TrainingSet[Double, Int]" => true
    case _ => super.isObjectType(m)
  }

  override def remap[A](m: Manifest[A]) : String = m.toString match {
    //case "ppl.dsl.optiml.IndexVector" => "IndexVector"
    //case "ppl.dsl.optiml.TrainingSet[Double, Double]" => "DoubleDoubleTrainingSet"
    //case "ppl.dsl.optiml.TrainingSet[Double, Int]" => "DoubleIntTrainingSet"
    case _ => super.remap(m)
  }

  override def copyInputHtoD(sym: Sym[Any]) : String = remap(sym.tp) match {
    case "IndexVector" => indexVectorCopyInputHtoD(sym)
    case "DoubleDoubleTrainingSet" | "DoubleIntTrainingSet" => trainingSetCopyInputHtoD(sym)
    case _ => super.copyInputHtoD(sym)
  }

  override def copyMutableInputDtoH(sym: Sym[Any]) : String = remap(sym.tp) match {
    case "IndexVector" => indexVectorCopyMutableInputDtoH(sym)
    case "DoubleDoubleTrainingSet" | "DoubleIntTrainingSet" => trainingSetCopyMutableInputDtoH(sym)
    case _ => super.copyMutableInputDtoH(sym)
  }

  override def unpackObject[A](sym: Sym[Any]) : Map[String,Manifest[_]] = remap(sym.tp) match {
    case "IndexVector" =>
      Map("isRow"->Manifest.Boolean, "length"->Manifest.Int, "data"->Manifest.Int.arrayManifest)
    case "DoubleDoubleTrainingSet" | "DoubleIntTrainingSet" =>
      val dataArrayType1 = sym.tp.typeArguments(0)
      val dataArrayType2 = sym.tp.typeArguments(1)
      Map("numRows"->Manifest.Int, "numCols"->Manifest.Int, "data"->dataArrayType1.arrayManifest, "data_labels"->dataArrayType2.arrayManifest)
    case _ => super.unpackObject(sym)
  }

  override def getDSLHeaders: String = {
    val out = new StringBuilder
    out.append(super.getDSLHeaders)
    out.append("#include \"IndexVectorImpl.h\"\n")
    out.append("#include \"TrainingSetImpl.h\"\n")
    out.toString
  }
}

trait OptiMLCodeGenC extends OptiLACodeGenC with OptiMLCodeGenBase with OptiMLCCodeGenPkg
{
  val IR: DeliteApplication with OptiMLExp
  import IR._
  
}
