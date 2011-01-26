package ppl.apps.robotics.gradient

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala.{Vector,IndexVector}
import ppl.delite.framework.DeliteApplication

trait BinarizedGradientTemplate {
  // TODO: how do we clean this up in app code?
  val IR: DeliteApplication with OptiMLExp
  import IR._

  /**
   * Score this template against another template
   * test: the other template
   * match_thresh: allow for early exit if you are not above this
   * returns: the score
   */
  def score(test1: Rep[BinarizedGradientTemplate], test2: Rep[BinarizedGradientTemplate], match_thresh: Rep[Float]): Rep[Float] = {
    if (test1.radius != test2.radius) {
      return -1.0f
    }
    var total: Float = test1.match_list.length.asInstanceOf[Float]
    if (total == 0.0) {
      return -1.0f
    }
    val num_test: Float = test2.match_list.length.asInstanceOf[Float]
    if (repOrderingToRepOrderingCls(num_test / total) < match_thresh) {
      return num_test / total //Not enough entries in the other list to be above match_thresh
    }
    var matches: Float = 0
    var limit = (total * (1.0 - match_thresh) + 0.5).asInstanceOf[Int] //Miss more than this number and we can't be above match_thresh
    // TODO: Change this to some sort of "filterUntil" construct to allow the code to short-circuit if we cross limit
    var i = 0
    while (i < test1.match_list.length) {
      if (test1.binary_gradients(test1.match_list(i)) == 0 && test2.binary_gradients(test1.match_list(i)) == 0) {
        matches += 1
      }
      else if (((test1.binary_gradients(test1.match_list(i))) & (test2.binary_gradients(test1.match_list(i)))) > 0) {
        matches += 1
      }
      else {
        limit -= 1
        if (limit <= 0) {
          return (match_thresh - 0.000001f) //sunk below the limit of misses, early terminate
        }
      }
      i += 1
    }
    return (matches / total).asInstanceOf[Float]
  }
}

