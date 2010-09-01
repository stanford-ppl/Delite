package ppl.tests.apps

import java.io.PrintWriter
import scala.virtualization.lms.internal.ScalaCompile
import ppl.dsl.optiml.embedded._
import scala.virtualization.lms.common._
import scala.virtualization.lms.ppl.{ScalaGenScalaOpsPkg, ScalaOpsPkgExp}
import ppl.dsl.optiml.apps.gda.GDA


/*
object GDAString extends GDA with OptiML with VectorOpsRepString with MatrixOpsRepString with MLInputReaderOpsRepString with ScalaOpsRepString
        with FunctionsString with ControlFlowString
*/

//todo why do we need the addition EffectsExp if we are already mixing in EqualExp and FunctionsExp and so forth
object GDAExp extends GDA with OptiML
        with VectorOpsRepExp with MatrixOpsRepExp with MLInputReaderOpsRepExp with ScalaOpsPkgExp with EqualExp with VariablesExp with IfThenElseExp with FunctionsExp

//todo currently using FunctionsExpUnfoldAll from test3, shouldn't that be moved to the core embeddeding framework
object GDAScalaCompile extends GDA
        with VectorOpsRepExp with MatrixOpsRepExp with MLInputReaderOpsRepExp
        with ScalaGenScalaOpsPkg with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenFunctions with ScalaGenVariables
        with ScalaCodegenVector with ScalaCodegenMatrix with ScalaCodegenMLInputReader
        with ScalaCompile

/*
// This is just running GDAString Representation
object TestString {

  def main(args: Array[String]) {
    val a = Array("dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1x.dat",
                  "dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1y.dat")
    import GDAString._

    print("" + GDAString.run(a))
  }
}
*/

object TestExp {
   def main(args: Array[String]) {
    val a = Array("dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1x.dat",
                  "dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1y.dat")

     print("" + GDAExp.run(a))

  }
}


object TestGenCode {
   def main(args: Array[String]) {
    val a = Array("dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1x.dat",
                  "dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1y.dat")

     // need to import rep into namespace
     import GDAScalaCompile._
     val runm = (argus:Rep[Array[String]]) => GDAScalaCompile.run(argus)
     emitScalaSource(runm, "GDA", new PrintWriter(System.out));

  }
}

object TestGeneratedCode {
   def main(args: Array[String]) {
    val a = Array("dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1x.dat",
                  "dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1y.dat")

     // this is the pre-generated test
     //val prog = new generated.GDA
     //prog(a)

     // this dynamically re-generates and compiles
     import GDAScalaCompile._
     val runm = (argus:Rep[Array[String]]) => GDAScalaCompile.run(argus)
     val g = compile(runm)
     g(a)
  }
}
