package ppl.apps.robotics.gradient

import ppl.dsl.optiml._
import ppl.dsl.optiml.application._
import ppl.delite.framework.DeliteApplication

trait BinarizedGradientTemplateFuncs {
  this: OptiMLApplication =>

  /**
   * Score this template against another template
   * test: the other template
   * match_thresh: allow for early exit if you are not above this
   * returns: the score
   */
  def score(test1: Rep[BinarizedGradientTemplate], test2: Rep[BinarizedGradientTemplate], match_thresh: Rep[Float]): Rep[Float] = {
    if (test1.radius != test2.radius) {
      -1.0f
    } else {
    var total: Rep[Float] = test1.match_list.length.AsInstanceOf[Float]
    if (total == 0.0) {
      -1.0f
    } else {
    val num_test: Rep[Float] = test2.match_list.length.AsInstanceOf[Float]
    if ((num_test / total) < match_thresh) {
      num_test / total //Not enough entries in the other list to be above match_thresh
    } else {
    var matches = 0f
    var limit = (total * (1.0f - match_thresh) + 0.5f).AsInstanceOf[Int] //Miss more than this number and we can't be above match_thresh
    // TODO: Change this to some sort of "filterUntil" construct to allow the code to short-circuit if we cross limit
    var i = 0
    var keepRunning = true
    while (i < test1.match_list.length && keepRunning) {
      val x = test1.match_list(i)
      if (test1.binary_gradients(test1.match_list(i)) == 0 && test2.binary_gradients(test1.match_list(i)) == 0) {
        matches = matches + 1f
      }
      else if (((test1.binary_gradients(test1.match_list(i))) & (test2.binary_gradients(test1.match_list(i)))) > 0) {
        matches = matches + 1f
      }
      else {
        limit = limit - 1
        if (limit <= 0) {
          keepRunning = false
        }
      }
      i += 1
    }
if (!keepRunning) {
(match_thresh - 0.000001f) //sunk below the limit of misses, early terminate
} else {
    (matches / total).AsInstanceOf[Float]
}
}
}
}
  }
}

