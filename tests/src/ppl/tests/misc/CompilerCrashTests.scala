package ppl.tests.misc

import collection.mutable.ListBuffer
import scala.virtualization.lms.common.ScalaOpsPkg

/* Testing small cases to try to identify causes of scalac crashes.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Oct 28, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait Bar { val IR: Foo }

/* Crashes only when Foo extends ScalaOpsPkg AND the loop is uncommented */
trait Foo extends ScalaOpsPkg {
  type FooBar = Bar {val IR: Foo.this.type}

  lazy val myFooBars = ListBuffer[FooBar]()

  //for (fb <- myFooBars){
  //  println("x")
  //}
}