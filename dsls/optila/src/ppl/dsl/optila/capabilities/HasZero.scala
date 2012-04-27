package ppl.dsl.optila.capabilities

import scala.virtualization.lms.common.{Variables, Base}
import scala.reflect.SourceContext
import ppl.dsl.optila.{Vector,Matrix}
import ppl.dsl.optila.{OptiLAExp, OptiLA}

/**
 * Currently unused in favor of the more dynamic 'defaultValue' method in SparseVectorOps.
 * While HasZero would be safer, it causes complications threading the required constraint
 * through generic framework operations (e.g. vectorBuilder in VectorOps or dc_apply in 
 * DeliteCollectionOps). The 'defaultValue' method has a far smaller reach.
 */
 
// trait HasZeroInternal[Rep[X], T] {
//   def zero(implicit ctx: SourceContext): Rep[T]
// }
// 
// trait HasZeroOps extends Variables {
//   this: OptiLA =>
//   
//   type HasZero[X] = HasZeroInternal[Rep,X]
//   
//   implicit def hasZeroToHasZeroOps[T:HasZero:Manifest](n: T) = new HasZeroOpsCls(unit(n))
//   implicit def repHasZeroToHasZeroOps[T:HasZero:Manifest](n: Rep[T]) = new HasZeroOpsCls(n)
//   implicit def varHasZeroToHasZeroOps[T:HasZero:Manifest](n: Var[T]) = new HasZeroOpsCls(readVar(n))
// 
//   class HasZeroOpsCls[T:Manifest](lhs: Rep[T])(implicit hz: HasZero[T]){
//     def zero(implicit ctx: SourceContext) = hz.zero
//   }
//   
//   implicit def numericHasZero[T:Numeric:Manifest] = new HasZero[T] {
//     def zero(implicit ctx: SourceContext) = unit(0.asInstanceOf[T])
//   }  
// }
// 
