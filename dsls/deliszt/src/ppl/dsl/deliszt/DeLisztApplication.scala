package ppl.dsl.deliszt

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.codegen.delite.DeliteCodeGenPkg
import ppl.dsl.deliszt.analysis.{LoopColoringOpt}

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/12/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

 /**
 * These separate DeLiszt applications from the Exp world.
 */

// ex. object GDARunner extends DeLisztApplicationRunner with GDA
trait DeLisztApplicationRunner extends DeLisztApplication with DeLisztExp {
  override def liftedMain(x: Rep[Array[String]]) = {
    _init(x)
    this.args = x
    val y = main()
    this.args = null
    unit(y)
  }
  
  override val deliteGenerator = new DeliteCodeGenPkg with LoopColoringOpt { val IR: DeLisztApplicationRunner.this.type = DeLisztApplicationRunner.this;
                                                                             val generators = DeLisztApplicationRunner.this.generators }                                                                               
}
 
trait DeLisztApplication extends DeliteApplication with DeLiszt with DeLisztLift with DeLisztLibrary

trait DeLisztLibrary {
  this: DeLisztApplication =>
}
