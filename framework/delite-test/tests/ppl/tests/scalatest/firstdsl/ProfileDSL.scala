package ppl.tests.scalatest.firstdsl

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import ppl.delite.framework._
import ppl.delite.framework.codegen._
import ppl.delite.framework.ops._
import ppl.delite.framework.ops.DeliteCollection
import ppl.delite.framework.datastructures._
import codegen.delite.overrides._
import codegen.scala.TargetScala
import codegen.cpp.TargetCpp
import codegen.cuda.TargetCuda
import java.io.File

/* Profile DSL front-end types */
abstract class ProfileArray extends DeliteCollection[Double]

/* Application packages */
trait ProfileApplicationRunner extends ProfileApplication 
  with DeliteApplication with ProfileExp
trait ProfileApplication extends Profile with ProfileLift {
  var args: Rep[Array[String]]
  def main(): Unit
}

trait ProfileLift extends LiftScala { // allow apps to use all of Scala
  this: Profile =>
}

/* IR packages */
trait Profile extends ScalaOpsPkg with ProfileOps with ProfileArrayOps
trait ProfileExp extends Profile with ScalaOpsPkgExp with ProfileOpsExp
  with ProfileArrayOpsExp with DeliteOpsExp
  with DeliteAllOverridesExp {

  this: DeliteApplication with ProfileApplication with ProfileExp =>

  def getCodeGenPkg(t: Target{val IR: ProfileExp.this.type}):
    GenericFatCodegen{val IR: ProfileExp.this.type} = {
    
    t match {
      case _:TargetScala => new ProfileCodeGenScala { val IR: ProfileExp.this.type = ProfileExp.this }
      case _:TargetCpp => new ProfileCodeGenC { val IR: ProfileExp.this.type = ProfileExp.this }
      case _:TargetCuda => new ProfileCodeGenCuda { val IR: ProfileExp.this.type = ProfileExp.this }
      case _ => throw new IllegalArgumentException("unsupported target")
    }
  }
}

/* Code generator packages */
trait ProfileCodeGenBase extends GenericFatCodegen with codegen.Utils {
  val IR: DeliteApplication with ProfileExp
  override def initialDefs = IR.deliteGenerator.availableDefs
}

trait ProfileCodeGenScala extends ProfileCodeGenBase with ScalaCodeGenPkg
  with ScalaGenDeliteOps with ScalaGenProfileOps with ScalaGenProfileArrayOps
  with ScalaGenDeliteCollectionOps
  with DeliteScalaGenAllOverrides {

  val IR: DeliteApplication with ProfileExp
  
  def dsmap(s: String) = {
    var res = s.replaceAll("ppl.tests.scalatest.firstdsl.datastruct", "generated")
    res.replaceAll("ppl.tests.scalatest.firstdsl", "generated.scala")
  }
  
  override def remap[A](m: Manifest[A]): String = dsmap(super.remap(m))
  
  override def emitDataStructures(path: String) {
    val s = File.separator
    val dsRoot = Config.homeDir + s+"framework"+s+"delite-test"+s+"tests"+s+
                 "ppl"+s+"tests"+s+"scalatest"+s+"firstdsl"+s+"datastruct"+s + this.toString
    
    copyDataStructures(dsRoot, path, dsmap)
  }
}

trait ProfileCodeGenC extends ProfileCodeGenBase with CCodeGenPkg
  with CGenDeliteOps with CGenDeliteStruct with CGenDeliteArrayOps with DeliteCppHostTransfer
  /*with CGenProfileOps with CGenProfileArrayOps */
  with CGenDeliteCollectionOps
  with DeliteCGenAllOverrides {
      
  val IR: DeliteApplication with ProfileExp
}

trait ProfileCodeGenCuda extends ProfileCodeGenBase with CudaCodeGenPkg
  with CudaGenDeliteOps with CudaGenDeliteStruct with CudaGenDeliteArrayOps with DeliteCppHostTransfer with DeliteCudaDeviceTransfer
  /*with CudaGenProfileOps with CudaGenProfileArrayOps */
  with CudaGenDeliteCollectionOps
  with DeliteCudaGenAllOverrides {

  val IR: DeliteApplication with ProfileExp
}
