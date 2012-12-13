package ppl.apps.interop

import ppl.dsl.optiml.{OptiML,OptiML_,DeliteInteroperability}
import ppl.delite.framework.datastructures.DeliteArray

/**
 * What we can certainly do: light-weight composition of DSLs into the same object
 * (Similar to OptiLA - OptiML, except defined by application)
 * 
 * What is less clear: independently staged and recombined DSLs for safety
 */
 
// object ExampleInteropRunner extends ExampleInterop

/* 
 * Light-weight DSL composition: 
 *  combine the DSL blocks into a single object with narrowed visibility per block
 */
/*
trait ExampleInterop extends DeliteInteroperability {
  
  def main(args: Array[String]) {
    println("scala 1")
    
    OptiML {
      println("optiml 1")
    }
    
    // ideally: OptiML compose OptiQL {  }
    // DOWNSIDE: not independently staged -- not particularly interesting from a research perspective??
     
    // single scope, but no way to isolate different code blocks
    
    // OptiML compose (OptiQLInteractive,OptiQLInteractiveRunner) {      
    //   println("optiml+optiql")
    // }

    // in order to use imports to limit visibility, it has to be in the same place as the code block (i.e. the application)
    // also no way to use scopes with the import trick, because the code is executed in a different object where everything is in scope, as designed
    
    // val global = new OptiMLInteractiveRunner with OptiQLInteractiveRunner     
    // new Scope[OptiMLInteractive, OptiMLInteractiveRunner, R](b)
  
    // surrounding scala code will run immediately ...
    
    {
      val OptiMLFacade = global: OptiMLApplication
      import OptiMLFacade._            
      
      // optiml code
    }
    {
      val OptiQLFacade = global: OptiQLApplication
      import OptiQLFacade._                
      
      // optiql code
    }
    
  
    println("scala 2")
  }
}
*/

/**
 * Independently staged and recombined scopes by adding an additional compilation stage
 */
// trait ExampleInterop extends DeliteInteroperability {
object ExampleInterop {
  
  def main(args: Array[String]) {
    println("scala 1")
    
    // staged blocks generate new re-stageable code
    // -- do we need an outer enclosing scope to execute the composition?
    OptiML_ {
      println("optiml")
      DeliteArray[Int](10)                             
    }
    
    OptiML_ {
      println("got input from previous stage: " + lastScopeResult.AsInstanceOf[DeliteArray[Int]]) 
      println("optiml 2")
    }
    
    // unstaged blocks get executed immediately like normal
    println("scala 2")
    
    // a is of type Scope[OptiMLApplication,OptiMLApplicationRunner,DeliteArray[Int]]
    // a.length is meaningless.. how do we unpack this future data?
    
    // we want a to be:
    // Rep[DeliteArray[Int]], but "whose" Rep? each Scope has a different IR instance, and the outer scope (currently) has none.
  
    // 1. stage a, return symbolic result
    // 2. stage b, using symbolic result of a
    // 3. compose(a,b) transforms a by lowering, transforms b by lowering, creates a new scope, and executes it?
  
    /*
    val a = 
      $OptiML$ {
        println("optiml")
        DeliteArray(10) // returns an OptiML Rep[DeliteArray[Int]], not an outer Rep[DeliteArray[Int]] -- what to do?
                        // how can any staged data escape the scope it is staged in? what happens if we try to return a Rep[DenseVector] (should be an error, but how?)

        // DeliteArrayFacade(DeliteArray(10)) 
        
        facade(DeliteArray(10))
      }
  
    val b =
      $OptiML$ {
        val aa = materialize(a)
        
        // val aa: Rep[DeliteArray[Int]] = materialize(a)
        
        println("optiml 2")
        // println("input length: " + aa.length)
      }
    
    // creates a third scope that stages and executes a,b
    val c = compose(a, b)

    println("scala 2")
    */ 
    
    /*
    compose[OptiMLApplication,OptiMLApplication] {
      // scope 1: only OptiML #1 
      println("optiml")
      DeliteArray(10)
    }
    {
      // scope 2: only OptiML #2
      val a = args(0) // DeliteArray(10) ??
      println("optiml 2")
    }
    */
    
  }
}
