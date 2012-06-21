package ppl.delite.framework.transform

import scala.virtualization.lms.common._

/*
 * This transformer is intended to be used to lower IR nodes from an abstract representation 
 * (an arbitrary Def) to some other representation. Each instance of LoweringTransformer should
 * be registered with DeliteApplication in order to be run.
 */
 
 /**
  example usage (1): custom lowering phase
    val myLoweringPhase = new LoweringTransformer
    
    appendTransformer(myLoweringPhase) // register with Delite
      
    override def onCreate[A:Manifest](s: Sym[A], d: Def[A]) = (d match {
      case VectorZeros(n)   => s.atPhase(myLoweringPhase) { vfromarray(array(myLoweringPhase(n)) { i => 0 }) }
      case _ => super.onCreate(s,d)
  }).asInstanceOf[Exp[A]]
   
  example usage (2): use global lowering phase
    override def onCreate[A:Manifest](s: Sym[A], d: Def[A]) = (d match {
      case VectorZeros(n)   => s.atPhase(deviceIndependentLowering) { vfromarray(array(deviceIndependentLowering(n)) { i => 0 }) }
      case _ => super.onCreate(s,d)
  }).asInstanceOf[Exp[A]]
    
 */
 
 /* adapted from LMS TestWorklistTransform2.scala */
trait LoweringTransform extends BaseFatExp with EffectExp with IfThenElseFatExp with LoopsFatExp { self =>  
  /* class to extend for a custom LoweringTransformer, most likely to ensure a particular phase ordering */
  class LoweringTransformer extends WorklistTransformer { val IR: self.type = self }
  
  // ---------- Exp api
  
  implicit def toAfter[A:Manifest](x: Def[A]) = new { def atPhase(t: LoweringTransformer)(y: => Exp[A]) = transformAtPhase(x)(t)(y) }
  implicit def toAfter[A](x: Exp[A]) = new { def atPhase(t: LoweringTransformer)(y: => Exp[A]) = transformAtPhase(x)(t)(y) }

  // transform x to y at the *next* iteration of t. 
  // note: if t is currently active, it will continue the current pass with x = x.
  // do we need a variant that replaces x -> y immediately if t is active?
  
  def transformAtPhase[A](x: Exp[A])(t: LoweringTransformer)(y: => Exp[A]): Exp[A] = {
    t.register(x)(y)
    x
  }
    
  
  def onCreate[A:Manifest](s: Sym[A], d: Def[A]): Exp[A] = s

  // ----------
  
  override def createDefinition[T](s: Sym[T], d: Def[T]): Stm = {
    onCreate(s,d)(s.tp)
    super.createDefinition(s,d)
  }
}
