package ppl.tests.apps

import java.io.PrintWriter
import ppl.dsl.optiml.apps.gda._
import ppl.dsl.optiml._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{ScalaNestedCodegen, ScalaCompile}
import scala.virtualization.lms.ppl._


object GDAExp extends GDA with OptiMLExp with ScalaOpsPkgExp with EmbeddingPkgExp

object GDAScalaCompile extends GDA with OptiMLCodeGen
        with ScalaGenScalaOpsPkg with ScalaGenEmbeddingPkg with ScalaCompile


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
     //val prog = new generated.GDA2
     //prog(a)

     //this dynamically re-generates and compiles
     import GDAScalaCompile._
     val runm = (argus:Rep[Array[String]]) => GDAScalaCompile.run(argus)
     val g = compile(runm)
     g(a)
  }
}
