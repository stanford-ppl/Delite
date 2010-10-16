package ppl.delite.framework

import codegen.scala.TargetScala
import codegen.Target
import collection.immutable.HashMap
import scala.virtualization.lms.common.EffectExp
import java.io.PrintWriter


trait DeliteApplication extends EffectExp {

  type DeliteApplicationTarget = Target{val intermediate: DeliteApplication.this.type}

  var args: Rep[Array[String]] = _
  private var _targets: HashMap[String, DeliteApplicationTarget] = _

  final def main(args: Array[String]) {
    println("Delite Application Being Staged:[" + this.getClass.getSimpleName + "]")
    this.args = fresh
    val main_m = {x: Rep[Array[String]] => liftedMain }


    println("******Generating the program*********")
    for(tgt <- targets.values) {
      globalDefs = List()
      tgt.emitSource(this.args, main_m, "Application", new PrintWriter(System.out))
    }

  }

  var scope: List[TP[_]] = Nil


  def addTarget(tgt: DeliteApplicationTarget) {
    _targets += tgt.name -> tgt
  }

  def targets : HashMap[String, DeliteApplicationTarget] = {
    if (_targets == null) {
      _targets = new HashMap[String, DeliteApplicationTarget]
      addTarget(new TargetScala{val intermediate: DeliteApplication.this.type = DeliteApplication.this})
      //addTarget(new TargetC{val intermediate: DeliteApplication.this.type = DeliteApplication.this})
    }
    _targets
  }

  /**
   * this is the entry method for our applications, user implement this method. Note, that it is missing the
   * args parameter, args are now accessed via the args field. This basically hides the notion of Reps from
   * user code
   */
  def main : Unit

  def liftedMain : Rep[Unit] = main


  //so that our main doesn't itself get lifted
  private def println(s:String) = Predef.println(s)

}
