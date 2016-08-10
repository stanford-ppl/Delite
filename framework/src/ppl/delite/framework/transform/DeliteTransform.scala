package ppl.delite.framework.transform

import reflect.{SourceContext}
import scala.virtualization.lms.internal.Traversal
import ppl.delite.framework.DeliteApplication

trait DeliteTransform extends LoweringTransform {
  this: DeliteApplication =>

  // built-in phases
  object deviceIndependentLowering extends LoweringTransformer
  object deviceDependentLowering extends LoweringTransformer

  // list of all transformers to be applied
  private var _traversals: List[Traversal] = List(deviceIndependentLowering,deviceDependentLowering)
  var emitted_consts: Set[(Exp[Any], Def[Any])] = Set.empty // TODO: Put this in the right place in ExternPrimitiveOps.  Matt couldn't figure out how to mix in without errors....

  /*
   * return the set of transformers to be applied
   */
  def traversals = _traversals

  /*
   * api for registering new transformers with Delite
   */
  def prependTraversal(t: Traversal) { _traversals ::= t }
  def appendTraversal(t: Traversal) { _traversals :+= t }

  // Clear traversals - for experimental use in Forge DSLs
  def clearTraversals() { _traversals = Nil }

  /*
   * utilities
   */

   // investigate: is this necessary?
   def reflectTransformed[A:Manifest](t: Transformer, x: Exp[A], u: Summary, es: List[Exp[Any]])(implicit ctx: SourceContext): Exp[A] = {
     reflectMirrored(Reflect(DUnsafeImmutable(x), mapOver(t,u), t(es)))(mtype(manifest[A]), ctx)
   }
}
