package ppl.delite.framework.ops

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

trait DeliteReductionOpsExp extends BaseFatExp {  

  /**
   * Interface for binary reduction ops that write to the lhs. Instances of this interface
   * will be checked for and safely handled inside DeliteOpForeachReduce.
   */
  abstract class DeliteReduction[L:Manifest, R:Manifest] extends Def[Unit] {
    /* variable being reduced - this must be mutable! */
    val lhs: Exp[L]    
    /* zero value of the reduction */
    def zero: Exp[R]        
    /* expression that produces the next value to reduce */
    def rhs: Exp[R]
    /* reduction function */
    def reduce: (Exp[R], Exp[R]) => Exp[R]
    /* updates lhs with result rhs */    
    // TODO: this becomes redundant in practice - something like: a.setValue(reduce(a.value), b). 
    // can we rework this API to better focus on the necessary info? maybe:
    //   set(x: Exp[L]): Exp[Unit]
    //   get(x: Exp[L]): Exp[R]          
    // updateValue = set(x, reduce(get(x), rhs))
    def updateValue: (Exp[L], Exp[R]) => Exp[Unit]
    
    val mL = manifest[L]
    val mR = manifest[R]
  }

}
