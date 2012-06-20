package ppl.delite.framework.transform

import scala.virtualization.lms.common.WorklistTransformer
import ppl.delite.framework.DeliteApplication

trait DeliteTransform extends LoweringTransform {
  this: DeliteApplication =>
  
  // built-in phases   
  object deviceIndependentLowering extends LoweringTransformer
  // deviceDependentLowering, ... ??
  
  // list of all transformers to be applied
  private var _transformers: List[WorklistTransformer] = List(deviceIndependentLowering) 
  
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
  
  
}