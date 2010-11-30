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

trait LanguageOps extends Base {
  def sum[A](start: Rep[Int], end: Rep[Int])(block: Rep[Int] => Rep[A])(implicit mA: Manifest[A], ops: ArithOps[Rep[A]])
    = optiml_sum(start, end, block)

  def optiml_sum[A](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A])(implicit mA: Manifest[A], ops: ArithOps[Rep[A]]) : Rep[A]
}

trait LanguageOpsExp extends LanguageOps with DSLOpsExp { this: LanguageImplOps =>
  // implemented via kernel embedding
  case class Sum[A](start: Exp[Int], end: Exp[Int], block: Rep[Int] => Rep[A])(implicit mA: Manifest[A], ops: ArithOps[Rep[A]])
    extends DSLOp(reifyEffects(optiml_sum_impl(start, end, block)))

  def optiml_sum[A](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A])
                   (implicit mA: Manifest[A], ops: ArithOps[Rep[A]]) = Sum(start, end, block)
}