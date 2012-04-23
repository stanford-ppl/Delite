package ppl.dsl.CVX

import java.io._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
//import ppl.delite.framework.codegen.cuda.TargetCuda
//import ppl.delite.framework.codegen.c.TargetC
import ppl.delite.framework.codegen.delite.overrides.{DeliteCudaGenAllOverrides, DeliteCGenAllOverrides, DeliteScalaGenAllOverrides, DeliteAllOverridesExp}
import ppl.delite.framework.ops._

//import ppl.dsl.optila.{OptiLAApplication}
//import ppl.dsl.optila.{OptiLAScalaOpsPkg, OptiLAScalaOpsPkgExp, OptiLA, OptiLAExp, OptiLACompiler, OptiLALift}
//import ppl.dsl.optila.{OptiLAScalaCodeGenPkg, OptiLACudaCodeGenPkg, OptiLACCodeGenPkg, OptiLACodeGenBase, OptiLACodeGenScala, OptiLACodeGenCuda, OptiLACodeGenC}

/**
 * DSL specific
 */
// import ppl.dsl.optiml.io._
// import ppl.dsl.optiml.vector._
// import ppl.dsl.optiml.matrix._
// import ppl.dsl.optiml.graph._
// import ppl.dsl.optiml.stream._
// import ppl.dsl.optiml.library.cluster._
// import ppl.dsl.optiml.application._


/**
 * These separate CVX applications from the Exp world.
 */

// ex. object GDARunner extends OptiMLApplicationRunner with GDA
trait CVXApplicationRunner extends CVXApplication with DeliteApplication with CVXExp

// ex. trait GDA extends OptiMLApplication
//trait OptiMLApplication extends OptiLAApplication with OptiML with OptiMLLift with OptiMLLibrary {
trait CVXApplication extends CVX with CVXLift {
  var args: Rep[Array[String]]
  def main(): Unit
}


/**
 * These are the portions of Scala imported into OptiML's scope.
 */
//trait OptiMLLift extends OptiLALift {
trait CVXLift extends LiftVariables with LiftEquals with LiftString with LiftBoolean with LiftNumeric {
  this: CVX =>
}

trait CVXScalaOpsPkg extends Base
  with Equal with IfThenElse with Variables with While with Functions
  with ImplicitOps with OrderingOps with StringOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with MathOps with CastingOps with ObjectOps with IOOps
  // only included because of args. TODO: investigate passing args as a vector
  with ArrayOps
  
trait CVXScalaOpsPkgExp extends CVXScalaOpsPkg with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with FunctionsExp
  with ImplicitOpsExp with OrderingOpsExp with StringOpsExp with RangeOpsExp with IOOpsExp
  with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp with TupleOpsExp
  with ListOpsExp with SeqOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp
  with SynchronizedArrayBufferOpsExp with HashMapOpsExp with IterableOpsExp

  
trait CVXScalaCodeGenPkg extends ScalaGenDSLOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenFunctions
  with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps
  with ScalaGenArrayOps with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps
  with ScalaGenListOps with ScalaGenSeqOps with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenSetOps with ScalaGenObjectOps
  with ScalaGenSynchronizedArrayBufferOps with ScalaGenHashMapOps with ScalaGenIterableOps
  { val IR: CVXScalaOpsPkgExp  }
/**
 * This is the trait that every OptiML application must extend.
 */
// trait OptiML extends OptiLA with OptiMLScalaOpsPkg with LanguageOps with ApplicationOps with LBPOps // TODO: LBPOpsshould be auto-generated with ApplicationOps
//   with MLInputReaderOps with MLOutputWriterOps
//   with VectorOps with OptiMLDenseVectorOps with OptiMLVectorViewOps with OptiMLRangeVectorOps
//   with MatrixOps with IndexVectorOps with IndexVectorDenseOps with IndexVectorRangeOps with IndexVector2Ops 
//   with StreamOps with StreamRowOps
//   with GraphOps with VerticesOps with EdgeOps with VertexOps with MessageEdgeOps with MessageVertexOps with VSetOps
//   with LabelsOps with TrainingSetOps with ImageOps with GrayscaleImageOps {
// 
//   this: OptiMLApplication =>
// }
trait CVX extends CVXScalaOpsPkg 
  with OptVarOps with ExprOps
  with ConstExprOps
  with DeliteCollectionOps
{
  this: CVXApplication =>
}


// these ops are only available to the compiler (they are restricted from application use)
//trait OptiMLCompiler extends OptiLACompiler with OptiML {
trait CVXCompiler extends CVX {
  this: CVXApplication with CVXExp =>
}


/**
 * These are the corresponding IR nodes for OptiML.
 */
trait CVXExp extends CVXCompiler with CVXScalaOpsPkgExp 
  with OptVarOpsExp with ExprOpsExp
  with ConstExprOpsExp
  with DeliteAllOverridesExp {

  // this: OptiMLApplicationRunner => why doesn't this work?
  this: DeliteApplication with CVXApplication with CVXExp => // can't be OptiMLApplication right now because code generators depend on stuff inside DeliteApplication (via IR)

  override def getCodeGenPkg(t: Target{val IR: CVXExp.this.type}) : GenericFatCodegen{val IR: CVXExp.this.type} = {
    t match {
      case _:TargetScala => new CVXCodeGenScala{val IR: CVXExp.this.type = CVXExp.this}
      case _ => throw new RuntimeException("CVX does not support this target")
    }
  }

}


/**
 * CVX code generators
 */
trait CVXCodeGenBase extends GenericFatCodegen {

  val IR: DeliteApplication with CVXExp
  override def initialDefs = IR.deliteGenerator.availableDefs

  def dsmap(line: String) = line
  
  val specialize = Set[String]()
  def genSpec(f: File, outPath: String) = {}
  
  def getFiles(d: File): Array[File] = {
    d.listFiles flatMap { f => if (f.isDirectory()) getFiles(f) else Array(f) }
  }
    
  override def emitDataStructures(path: String) {
    //super.emitDataStructures(path) // get optila data structures
    
    val s = File.separator
    val dsRoot = Config.homeDir + s+"dsls"+s+"cvx"+s+"src"+s+"ppl"+s+"dsl"+s+"cvx"+s+"datastruct"+s + this.toString

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

// insert code generators here
trait CVXCodeGenScala extends CVXCodeGenBase
  with ScalaGenOptVarOps with ScalaGenExprOps
  with ScalaGenConstExprOps
  with DeliteScalaGenAllOverrides { //with ScalaGenMLInputReaderOps {
  
  val IR: DeliteApplication with CVXExp
  
  override def remap(s: String) = dsmap(s)
  override def remap[A](m: Manifest[A]): String = {
    var res = super.remap(m)
    res = res.replaceAllLiterally("package$", "")
    dsmap(res)
  }
  
  override def dsmap(line: String) : String = {
    var res = line.replaceAll("ppl.dsl.CVX.datastruct", "generated")
    res = res.replaceAll("ppl.delite.framework.datastruct", "generated")
    res = res.replaceAll("ppl.dsl.CVX", "generated.scala")
    res      
  }

}

// trait OptiMLCodeGenCuda extends OptiLACodeGenCuda with OptiMLCodeGenBase with OptiMLCudaCodeGenPkg 
//   with CudaGenDataStruct with CudaGenVectorOps with CudaGenMatrixOps with CudaGenTrainingSetOps 
//   with DeliteCudaGenAllOverrides // with DeliteCodeGenOverrideCuda // with CudaGenMLInputReaderOps  //TODO:DeliteCodeGenOverrideScala needed?
// {
//   val IR: DeliteApplication with OptiMLExp
//   import IR._
// 
// 
//   // Maps the scala type to cuda type
//   override def remap[A](m: Manifest[A]) : String = {
//     m.toString match {
//       case "ppl.dsl.optiml.RangeVector" => "RangeVector"
//       case "ppl.dsl.optiml.IndexVector" => "IndexVector"
//       case "ppl.dsl.optiml.Labels[Int]" => "Labels<int>"
//       case "ppl.dsl.optiml.Labels[Long]" => "Labels<long>"
//       case "ppl.dsl.optiml.Labels[Float]" => "Labels<float>"
//       case "ppl.dsl.optiml.Labels[Double]" => "Labels<double>"
//       case "ppl.dsl.optiml.Labels[Boolean]" => "Labels<bool>"
//       case "ppl.dsl.optiml.TrainingSet[Double, Double]" => "TrainingSet<double,double>"
//       case _ => super.remap(m)
//     }
//   }
// 
//   override def isObjectType[T](m: Manifest[T]) : Boolean = m.toString match {
//     case "ppl.dsl.optiml.RangeVector" => true
//     case "ppl.dsl.optiml.IndexVector" => true
//     case "ppl.dsl.optiml.Labels[Int]" => true
//     case "ppl.dsl.optiml.Labels[Long]" => true
//     case "ppl.dsl.optiml.Labels[Float]" => true
//     case "ppl.dsl.optiml.Labels[Double]" => true
//     case "ppl.dsl.optiml.Labels[Boolean]" => true
//     case "ppl.dsl.optiml.TrainingSet[Double, Double]" => true
//     case _ => super.isObjectType(m)
//   }
// 
//   override def copyInputHtoD(sym: Sym[Any]) : String = remap(sym.Type) match {
//     case "Labels<int>" | "Labels<long>" | "Labels<float>" | "Labels<double>" | "Labels<bool>" => labelsCopyInputHtoD(sym)
//     case "RangeVector" => rangeVectorCopyInputHtoD(sym)
//     case "IndexVector" => indexVectorCopyInputHtoD(sym)
//     case "TrainingSet<double,double>" => trainingSetCopyInputHtoD(sym)
//     case _ => super.copyInputHtoD(sym)
//   }
// 
//   override def copyOutputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
//     case _ => super.copyOutputDtoH(sym)
//   }
// 
//   override def copyMutableInputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
//     case "Labels<int>" | "Labels<long>" | "Labels<float>" | "Labels<double>" | "Labels<bool>" => labelsCopyMutableInputDtoH(sym)
//     case "RangeVector" => rangeVectorCopyMutableInputDtoH(sym)
//     case "IndexVector" => indexVectorCopyMutableInputDtoH(sym)
//     case "TrainingSet<double,double>" => trainingSetCopyMutableInputDtoH(sym)
//     case _ => super.copyMutableInputDtoH(sym)
//   }
// 
//   /*
//   override def allocOutput(newSym: Sym[_], sym: Sym[_], reset: Boolean = false) : Unit = remap(newSym.Type) match {
//     case _ => super.allocOutput(newSym,sym,reset)
//   }
//   */
// 
//   /*
//   override def allocReference(newSym: Sym[Any], sym: Sym[Any]) : Unit = remap(newSym.Type) match {
//     case "Labels<int>" | "Labels<long>" | "Labels<float>" | "Labels<double>" | "Labels<bool>" => emitVectorAllocRef(newSym,sym)
//     case _ => super.allocReference(newSym,sym)
//   }
//    */
// 
//   override def positionMultDimInputs(sym: Sym[Any]) : String = remap(sym.Type) match {
//     case _ => super.positionMultDimInputs(sym)
//   }
// 
//   override def getDSLHeaders: String = {
//     val out = new StringBuilder
//     out.append(super.getDSLHeaders)
//     out.append("#include \"RangeVectorImpl.h\"\n")
//     out.append("#include \"IndexVectorImpl.h\"\n")
//     out.append("#include \"LabelsImpl.h\"\n")
//     out.append("#include \"TrainingSetImpl.h\"\n")
//     out.toString
//   }
// 
// }
// 
// trait OptiMLCodeGenC extends OptiLACodeGenC with OptiMLCodeGenBase with OptiMLCCodeGenPkg 
//   with CGenVectorOps with CGenMatrixOps with DeliteCGenAllOverrides
// {
//   val IR: DeliteApplication with OptiMLExp
//   import IR._
// 
//   override def remap[A](m: Manifest[A]) : String = m.toString match {
//     case _ => super.remap(m)
//   }
// }
