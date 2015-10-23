package ppl.delite.framework.transform

import org.scala_lang.virtualized.{SourceContext}
import scala.virtualization.lms.common.{ObjectOpsExp,WorklistTransformer}
import ppl.delite.framework.DeliteApplication

trait DeliteTransform extends LoweringTransform {
  this: DeliteApplication =>
  
  // built-in phases   
  object deviceIndependentLowering extends LoweringTransformer
  object deviceDependentLowering extends LoweringTransformer
  
  // list of all transformers to be applied
  private var _transformers: List[WorklistTransformer] = List(deviceIndependentLowering,deviceDependentLowering) 
  
  /*
   * return the set of transformers to be applied
   */
  def transformers = _transformers
    
  /*
   * api for registering new transformers with Delite
   */     
  def prependTransformer(t: WorklistTransformer) {
    _transformers ::= t
  }
  
  def appendTransformer(t: WorklistTransformer) {
    _transformers :+= t
  }
  
  
  /*
   * utilities
   */
   
   // investigate: is this necessary?
   def reflectTransformed[A:Manifest](t: Transformer, x: Exp[A], u: Summary, es: List[Exp[Any]])(implicit ctx: SourceContext): Exp[A] = {
     reflectMirrored(Reflect(DUnsafeImmutable(x), mapOver(t,u), t(es)))(mtype(manifest[A]), ctx)        
   }    
}
