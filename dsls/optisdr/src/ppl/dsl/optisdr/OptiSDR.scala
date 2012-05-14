package ppl.dsl.optisdr

import java.io._

import scala.reflect.SourceContext

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala

import ppl.dsl.optila.{OptiLAApplication}
import ppl.dsl.optila.{OptiLAScalaOpsPkg, OptiLAScalaOpsPkgExp, OptiLA, OptiLAExp, OptiLACompiler, OptiLALift, OptiLAUtilities}
import ppl.dsl.optila.{OptiLAScalaCodeGenPkg, OptiLACudaCodeGenPkg, OptiLAOpenCLCodeGenPkg, OptiLACCodeGenPkg, OptiLACodeGenBase, OptiLACodeGenScala, OptiLACodeGenCuda, OptiLACodeGenOpenCL, OptiLACodeGenC}

import ppl.dsl.optisdr.capabilities._
import ppl.dsl.optisdr.primitive._
import ppl.dsl.optisdr.vector._
import ppl.dsl.optisdr.stream._

trait OptiSDRApplicationRunner extends OptiSDRApplication with DeliteApplication with OptiSDRExp

trait OptiSDRApplication extends OptiLAApplication with OptiSDR with OptiSDRLift {
  var args: Rep[Array[String]]
  def main(): Unit
}

/**
 * These are the portions of Scala imported into OptiML's scope.
 */
trait OptiSDRLift extends OptiLALift {
  this: OptiSDR =>
}

trait OptiSDRScalaOpsPkg extends OptiLAScalaOpsPkg

trait OptiSDRScalaOpsPkgExp extends OptiSDRScalaOpsPkg with OptiLAScalaOpsPkgExp

trait OptiSDRScalaCodeGenPkg extends OptiLAScalaCodeGenPkg
  { val IR: OptiSDRScalaOpsPkgExp  }

/**
 * This the trait that every OptiSDR application must extend.
 */
trait OptiSDR extends OptiSDRScalaOpsPkg with OptiLA
  with ComplexOps with UIntOps with SoftBitOps
  with BitArithOps
  with SDRArithOps
  with SDRVectorOps
  with SDRStreamOps
  with LanguageOps {
  this: OptiSDRApplication =>

  type Real = Double
  // Int is already a defined type
}
  
// these ops are only available to the compiler (they are restricted from application use)
trait OptiSDRCompiler extends OptiLACompiler with OptiSDR {
   
  this: OptiSDRApplication with OptiSDRExp =>
}

trait OptiSDRExp extends OptiLAExp with OptiSDRCompiler with OptiSDRScalaOpsPkgExp
  with ComplexOpsExpOpt with UIntOpsExpOpt with SoftBitOpsExp
  with BitArithOpsExp
  with SDRArithOpsExp
  with SDRVectorOpsExpOpt
  with SDRStreamOpsExpOpt
  with LanguageOpsExp {
  this: DeliteApplication with OptiSDRApplication with OptiSDRExp => // can't be OptiSDRApplication right now because code generators depend on stuff inside DeliteApplication (via IR)
  
  def getCodeGenPkg(t: Target{val IR: OptiSDRExp.this.type}) : GenericFatCodegen{val IR: OptiSDRExp.this.type} = {
    t match {
      case _:TargetScala => new OptiSDRCodeGenScala{val IR: OptiSDRExp.this.type = OptiSDRExp.this}
      case _ => err("optisdr does not support this target")
    }
  }
}

/**
 * OptiSDR code generators
 */
trait OptiSDRCodeGenBase extends OptiLACodeGenBase {
  val IR: DeliteApplication with OptiSDRExp
  override def initialDefs = IR.deliteGenerator.availableDefs

  val specialize = Set[String]()
  val specialize2 = Set[String]()
  
  def emitDataStructures(dsRoot: String, dest: String) {
    val s = File.separator
    val dsDir = new File(dsRoot)
    if (!dsDir.exists) return
    val outDir = new File(dest)
    outDir.mkdirs()

    val files = getFiles(dsDir)    
    for (f <- files) {
      if (f.isDirectory){
        emitDataStructures(f.getPath, dest + s + f.getName)
      }
      else {
        if (specialize contains (f.getName.substring(0, f.getName.indexOf(".")))) {
          genSpec(f, dest)
        }
        val outFile = dest + s + f.getName
        val out = new BufferedWriter(new FileWriter(outFile))
        for (line <- scala.io.Source.fromFile(f).getLines) {
          out.write(dsmap(line) + "\n")
        }
        out.close()
      }
    }
  }
  
  override def emitDataStructures(path: String) {
    val s = File.separator
    val dsRoot = Config.homeDir + s+"dsls"+s+"optisdr"+s+"src"+s+"ppl"+s+"dsl"+s+"optisdr"+s+"datastruct"+s + this.toString

    emitDataStructures(dsRoot, path)
    
    super.emitDataStructures(path)
  }
}

trait OptiSDRCodeGenScala extends OptiLACodeGenScala with OptiSDRCodeGenBase with OptiSDRScalaCodeGenPkg /* with OptiSDRScalaGenExternal */ {
  val IR: DeliteApplication with OptiSDRExp

  // override val specialize = Set("DenseVector", "DenseMatrix"/*, "VectorView"*/)

  // override def genSpec(f: File, dsOut: String)

  override def dsmap(line: String) : String = {
    var res = line.replaceAll("ppl.dsl.optisdr.datastruct", "generated")
    res = res.replaceAll("ppl.dsl.optisdr", "generated.scala")    
    
    super.dsmap(res)
  }
}