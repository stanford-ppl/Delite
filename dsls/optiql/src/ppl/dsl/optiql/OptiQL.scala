package ppl.dsl.optiql

import scala.reflect.RefinedManifest

import ops._
import scala.virtualization.lms.common._
import ppl.delite.framework.ops._
import ppl.delite.framework.codegen.delite.overrides.DeliteAllOverridesExp
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.dsl.optiql.user.applications._
import java.io.{FileWriter, BufferedWriter, File}
import ppl.delite.framework.codegen.{Utils, Target}


/**
 * These are the lifted scala constructs that only operate on the Rep world. These are usually safe to mix in
 */
trait OptiQLScalaOpsPkg extends Base with MiscOps with OrderingOps with PrimitiveOps with TupleOps with NumericOps with ArrayOps with IfThenElse 

/**
 * This trait adds the Ops that are specific to OptiQL
 */
trait OptiQL extends OptiQLScalaOpsPkg with HackOps with DataTableOps with QueryableOps with DateOps with OptiQLMiscOps with ResultOps with ApplicationOps {
  this: OptiQLApplication =>
}

/**
 * These are the lifted scala constructs, which convert a concrete type to a Rep type.
 * These can be dangerous if you mix them in to the wrong place
 */
trait OptiQLLift extends LiftString with LiftPrimitives {
  this: OptiQL =>
}

/**
 * Scala IR nodes
 */
trait OptiQLScalaOpsPkgExp extends OptiQLScalaOpsPkg with MiscOpsExp with IOOpsExp with SeqOpsExp with OrderingOpsExp
  with PrimitiveOpsExp with TupleOpsExp with NumericOpsExp with ArrayOpsExp with IfThenElseExp with ResultOps

/**
 * Ops available only to the compiler, and not our applications
 */
trait OptiQLCompiler extends OptiQL with IOOps with SeqOps {
  this: OptiQLApplication with OptiQLExp =>
}

/**
 * This trait comprises the IR nodes for OptiQL and the code required to instantiate code generators
 */
trait OptiQLExp extends OptiQLCompiler with OptiQLScalaOpsPkgExp with HackOpsExp with DataTableOpsExp with DateOpsExp with QueryableOpsExp with OptiQLMiscOpsExp
  with ResultOpsExp with ApplicationOpsExp with DeliteOpsExp {

  this: DeliteApplication with OptiQLApplication with OptiQLExp =>

  def getCodeGenPkg(t: Target{val IR: OptiQLExp.this.type}) : GenericFatCodegen{val IR: OptiQLExp.this.type} = {
    t match {
      case _:TargetScala => new OptiQLCodeGenScala{val IR: OptiQLExp.this.type = OptiQLExp.this}
      case _ => throw new RuntimeException("OptiQL does not support this target")
    }
  }

}

/**
 * Codegen traits
 */
trait OptiQLScalaCodeGenPkg extends ScalaGenMiscOps with ScalaGenIOOps with ScalaGenSeqOps with ScalaGenOrderingOps 
  with ScalaGenPrimitiveOps with ScalaGenTupleOps with ScalaGenNumericOps with ScalaGenArrayOps with ScalaGenIfThenElse with ScalaGenImplicitOps {
  val IR: OptiQLScalaOpsPkgExp
}

trait OptiQLCodeGenBase extends GenericFatCodegen {
  val IR: DeliteApplication with OptiQLExp
  override def initialDefs = IR.deliteGenerator.availableDefs

  def dsmap(line: String) = line

  //TODO HC: This is copied and pasted from OptiML, need to be refactored
  override def emitDataStructures(path: String) {
    val s = File.separator
    val dsRoot = Config.homeDir + s+"dsls"+s+"optiql"+s+"src"+s+"ppl"+s+"dsl"+s+"optiql"+s+"datastruct"+s + this.toString
    emitDSHelper(path, dsRoot)
  }

  def emitDSHelper(path:String, dsRoot:String) {
    val s = File.separator
    val dsDir = new File(dsRoot)
    if (!dsDir.exists) return
    val outDir = new File(path)
    outDir.mkdirs()

    for (f <- dsDir.listFiles) {
      val outFile = new File(path + f.getName)
      if(f.isDirectory) {
        emitDSHelper(path + f.getName + s, dsRoot + s + f.getName)
      } else {
        val out = new BufferedWriter(new FileWriter(outFile))
        for (line <- scala.io.Source.fromFile(f).getLines) {
          out.write(dsmap(line) + "\n")
        }
        out.close()
      }

    }
  }
}

trait OptiQLCodeGenScala extends OptiQLCodeGenBase with OptiQLScalaCodeGenPkg with ScalaGenHackOps
  with ScalaGenDataTableOps with ScalaGenDateOps with ScalaGenQueryableOps with ScalaGenOptiQLMiscOps with ScalaGenResultOps with ScalaGenApplicationOps with ScalaGenDeliteCollectionOps with ScalaGenDeliteOps {
  val IR: DeliteApplication with OptiQLExp

  override def remap[A](m: Manifest[A]): String = {    
    m match {
      case m if m.toString.startsWith("ppl.dsl.optiql.datastruct.scala.container.DataTable") => "generated.scala.container.DataTable[" + remap(m.typeArguments(0)) + "]"
      case rm: RefinedManifest[A] =>  "AnyRef{" + rm.fields.foldLeft(""){(acc, f) => {val (n,mnf) = f; acc + "val " + n + ": " + remap(mnf) + ";"}} + "}"
      case _ => dsmap(super.remap(m))
    }
    
  }

  override def dsmap(line: String) : String = {
    var res = line.replaceAll("ppl.dsl.optiql.datastruct", "generated")
    res = res.replaceAll("ppl.delite.framework.datastruct", "generated")
    res
  }
}

/**
 * Traits for running applications
 */
// ex. trait TPCH extends OptiQLApplication
trait OptiQLApplication extends OptiQL with OptiQLLift {
  var args: Rep[Array[String]]
  def main(): Unit 
}

// ex. object TPCHRunner extends OptiQLApplicationRunner with  TPCH
trait OptiQLApplicationRunner extends OptiQLApplication with DeliteApplication with OptiQLExp
