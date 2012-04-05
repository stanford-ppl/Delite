package ppl.dsl.optila.datastruct.scala

/* Global values used in generated OptiLA code.
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 12/30/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

object Global {

  //////////////////////////////////////
  // random vars (these are thread-safe)

  val INITIAL_SEED = 100
  var randRef = new scala.util.Random(INITIAL_SEED)
  var intRandRef = new scala.util.Random(INITIAL_SEED)


}