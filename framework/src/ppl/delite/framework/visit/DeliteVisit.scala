package ppl.delite.framework.visit

import scala.reflect.SourceContext
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.transform.LoweringTransform

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

trait DeliteVisit extends LoweringTransform {

  // built-in phases   
  object deviceIndependentLowering extends LoweringTransformer {override lazy val name = "Device-Independent Lowering"} 
  object deviceDependentLowering extends LoweringTransformer {override lazy val name = "Device-Dependent Lowering"}
  
  // list of all visitors to be applied to IR
  private var _visitors: List[IRVisitor] = List(deviceIndependentLowering,deviceDependentLowering) 
  
  /*
   * return the set of visitors to be applied
   */
  def visitors = _visitors
    
  /*
   * api for registering new visitors with Delite
   */     
  def prependVisitor(t: IRVisitor) { _visitors ::= t }
  def appendVisitor(t: IRVisitor) { _visitors :+= t }
}