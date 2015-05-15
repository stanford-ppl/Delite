package ppl.delite.framework.transform

import reflect.{SourceContext}
import scala.virtualization.lms.common.{ObjectOpsExp,WorklistTransformer}
import ppl.delite.framework.DeliteApplication
import scala.collection.mutable.HashMap

trait DeliteTransform extends LoweringTransform {
  this: DeliteApplication =>
  
  // built-in phases   
  object deviceIndependentLowering extends LoweringTransformer
  object deviceDependentLowering extends LoweringTransformer
  
  // list of all transformers to be applied
  private var _transformers: List[WorklistTransformer] = List(deviceIndependentLowering,deviceDependentLowering) 
  private var _transformerMetadata = HashMap[String, Any]()
  
  /*
   * return the set of transformers to be applied
   */
  def transformers = _transformers
  def transformerMetadata = _transformerMetadata
    
  /*
   * api for registering new transformers with Delite
   */     
  def prependTransformer(t: WorklistTransformer) {
    _transformers ::= t
  }
  
  def appendTransformer(t: WorklistTransformer) {
    _transformers :+= t
//    println("[appendTransformer] _transformers = " + _transformers)
  }

  def appendTransformerMetadata(key: String, value: Any) {
    _transformerMetadata += (key -> value)
  }

  
  
  /*
   * utilities
   */
   
   // investigate: is this necessary?
   def reflectTransformed[A:Manifest](t: Transformer, x: Exp[A], u: Summary, es: List[Exp[Any]])(implicit ctx: SourceContext): Exp[A] = {
     reflectMirrored(Reflect(DUnsafeImmutable(x), mapOver(t,u), t(es)))(mtype(manifest[A]), ctx)        
   }    
}
