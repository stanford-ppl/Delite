package ppl.dsl.optila

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.LoweringTransform

trait SparseTransform extends LoweringTransform {
  this: OptiLAExp => 
      
  /**
   * Specialize bulk sparse operations by testing the corresponding function to see if operations on zero values do anything.
   * If zero-value ops do nothing, we can optimize by operating directly on the underlying nonzero array.
   */
  
  // -- generic
  
  private val t = deviceIndependentLowering // all sparse transformations happen at this phase
  
  def reflectSpecialized[A:Manifest](spec: Option[Exp[Any]], u: Summary, es: List[Exp[Any]])(orElse: => Exp[A]): Exp[A] = {
    // spec.map(_.asInstanceOf[Exp[A]]) getOrElse super.onCreate(s,d)          
    if (spec.isDefined) {
      reflectTransformed(t,spec.get.asInstanceOf[Exp[A]],u,es)
    }
    else orElse
  }
  
  // -- map
  
  def specializeSparseMap[A:Manifest,B:Manifest,SC[X]](s: Sym[Any], e: DeliteOpMapI[A,B,_,_], x: Exp[SC[A]], mapnz: (Exp[SC[A]], Exp[A] => Exp[B]) => Exp[SC[B]]): Option[Exp[Any]] = {
    val f = e.func
    val repr = e.body.asInstanceOf[DeliteCollectElem[B,_,_]].func
    
    val test = reifyEffects(f(defaultValue[A]))
    // could also test for  == Const(y) and optimize that
    if (test.res == defaultValue[A]) {  // symbolically resolves to constant            
      Some(s.atPhase(t) {                
        // not ideal: we would like to use the BlockN interface
        // the issue is that storing an explicit Block1 inside DeliteOpMap creates complications by having two versions of the evaluated function around
        // in particular, we end up applying the Block1 transformed function twice, which creates two substitutions for the original bound var (with only the first valid) (see FunctionBlocks.scala)
        mapnz(t(x), { a => transformBlockWithBound(t, repr, scala.List(e.fin -> a)) })
      })
    }        
    else {
      // TODO: generate dynamic test      
      warn("performance: map function " + repr + " operates on zero values of sparse object " + findDefinition(x.asInstanceOf[Sym[Any]]).get.toString) // TODO: context for v
      None
    }      
  }
  
  
  // -- zip

  trait SparseZipWithSpecialization
  object SparseLeft extends SparseZipWithSpecialization
  object SparseRight extends SparseZipWithSpecialization
  object SparseIntersect extends SparseZipWithSpecialization
  object SparseUnion extends SparseZipWithSpecialization
  object SparseNone extends SparseZipWithSpecialization
  
  def specializeSparseZip[A:Manifest,B:Manifest,R:Manifest,SC[X]](s: Sym[Any], e: DeliteOpZipWithI[A,B,R,_,_], xA: Exp[SC[A]], xB: Exp[SC[B]], zipnz: (Exp[SC[A]], Exp[SC[B]], (Exp[A],Exp[B]) => Exp[R], SparseZipWithSpecialization) => Exp[SC[R]]): Option[Exp[Any]] = {
    val f = e.func
    val repr = e.body.asInstanceOf[DeliteCollectElem[R,_,_]].func
    
    val testLeft = reifyEffects(f(defaultValue[A],fresh[B]))          // test if left's zeros matter
    val testRight = reifyEffects(f(fresh[A],defaultValue[B]))         // test if right's zeros matter
    val testBoth = reifyEffects(f(defaultValue[A],defaultValue[B]))
    
    // workaround for unintentional lifting of == 
    val leftCond: Boolean = testLeft.res == defaultValue[R]
    val rightCond: Boolean = testRight.res == defaultValue[R]
    
    val side: SparseZipWithSpecialization = 
      if (leftCond && rightCond) SparseIntersect              // only need to evaluate where both vectors are nonzero
      else if (leftCond) SparseLeft                           // only need to evaluate left's nonzeros
      else if (rightCond) SparseRight                         // only need to evaluate right's nonzeros      
      else if (testBoth.res == defaultValue[R]) SparseUnion   // only need to evaluate where either vector is nonzero
      else SparseNone                                         // can't specialize statically
        
    if (side != SparseNone) {
      Some(s.atPhase(t) {                        
        zipnz(t(xA), t(xB), { (a,b) => transformBlockWithBound(t, repr, scala.List(e.fin._1 -> a, e.fin._2 -> b)) }, side)        
      })
    }        
    else {
      // TODO: generate dynamic test      
      warn("performance: zip function " + repr + " operates on zero values of sparse objects " + findDefinition(xA.asInstanceOf[Sym[Any]]).get.toString + ", " + findDefinition(xB.asInstanceOf[Sym[Any]]).get.toString) // TODO: context for x
      None
    }      
  }


  // -- reduce
  
  def specializeSparseReduce[A:Manifest,SC[X]](s: Sym[Any], e: DeliteOpReduce[A], x: Exp[SC[A]], reducenz: (Exp[SC[A]], (Exp[A], Exp[A]) => Exp[A], Exp[A]) => Exp[A]): Option[Exp[Any]] = {
    val f = e.func
    val repr = e.body.asInstanceOf[DeliteReduceElem[A]].rFunc
            
    val y = fresh[A]    
    val test = reifyEffects(f(y,defaultValue[A]))
    if (test.res == y) {
      Some(s.atPhase(t) {                        
        reducenz(t(x), { (a,b) => transformBlockWithBound(t, repr, scala.List(e.rV._1 -> a, e.rV._2 -> b)) }, t(e.zero))        
      })
    }        
    else {
      // TODO: generate dynamic test      
      warn("performance: reduce function " + repr + " operates on zero values of sparse object " + findDefinition(x.asInstanceOf[Sym[Any]]).get.toString) // TODO: context for v
      None
    }      
  }  
  
}