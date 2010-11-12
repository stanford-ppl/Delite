package ppl.tests.apps

import scala.virtualization.lms.internal.ScalaCompile

object TestPreGeneratedCode {
  def main(args: Array[String]) {

    val a = Array("dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1x.dat",
                  "dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1y.dat")

    // this is the pre-generated test
    val prog = new generated.GDA_merged
    prog(a)
  }
}

object TestCompileCode {
  def main(args: Array[String]) {
    val a = Array("dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1x.dat",
                  "dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1y.dat")

    //this dynamically re-generates and compiles
    import ppl.dsl.optiml.apps.gda.GDA
    GDA.execute(a)
  }
}

object TestGenCode {
  def main(args: Array[String]) {
    val a = Array("dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1x.dat",
                  "dsls/optiml/src/ppl/dsl/optiml/apps/gda/q1y.dat")

    //this just generates
    import ppl.dsl.optiml.apps.gda.GDA
    GDA.main(a)
  }
}