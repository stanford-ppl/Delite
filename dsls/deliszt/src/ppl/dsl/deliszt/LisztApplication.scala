package ppl.dsl.deliszt

import ppl.delite.framework.DeliteApplication

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/12/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait LisztApplication extends DeliteApplication with LanguageOpsExp {
  def liftedMain(x: Rep[Array[String]]) = {
    DeLisztInit()
    this.args = x;
    val y = main();
    this.args = null;
    y
  }
}