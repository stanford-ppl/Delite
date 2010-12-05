package ppl.dsl.optiml

import scala.virtualization.lms.common.{DSLOpsExp, Base}

/* Machinery provided by OptiML itself (language features and control structures).
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * created: Nov 29, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait LanguageOps extends Base { this: ArithImplicits =>
  def sum[A:Manifest:ArithOps](start: Rep[Int], end: Rep[Int])(block: Rep[Int] => Rep[A]) = optiml_sum(start, end, block)

  def optiml_sum[A:Manifest:ArithOps](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A]) : Rep[A]
}

trait LanguageOpsExp extends LanguageOps with DSLOpsExp { this: LanguageImplOps with ArithImplicits =>
  // implemented via kernel embedding
  case class Sum[A:Manifest:ArithOps](start: Exp[Int], end: Exp[Int], block: Rep[Int] => Rep[A])
    extends DSLOp(reifyEffects(optiml_sum_impl(start, end, block)))

  def optiml_sum[A:Manifest:ArithOps](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A]) = Sum(start, end, block)
}