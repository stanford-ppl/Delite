package ppl.delite.framework.analysis

import java.io._
import scala.collection.mutable.HashMap
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{AbstractSubstTransformer,Transforming,FatBlockTraversal}
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollection}
import ppl.delite.framework.datastructures.{DeliteArray,DeliteArrayOpsExp}
import ppl.delite.framework.Config

trait StencilExp extends LoopsExp {  
  abstract class AccessPattern
  object all extends AccessPattern { override def toString = "all" }
  object one extends AccessPattern { override def toString = "one" }
  case class Interval(offsetMultiplier: Exp[Int], stride: Exp[Int], length: Exp[Int]) extends AccessPattern
  case class Constant(index: Exp[Int]) extends AccessPattern
  
  // from a data symbol (e.g. DeliteArray) to its access pattern
  type Stencil = HashMap[Exp[Any],AccessPattern]      
}

trait StencilAnalysis extends FatBlockTraversal {
  val IR: DeliteOpsExp
  import IR._
  
  // from a loop to its stencil
  var loopStencils = new HashMap[Exp[Any],Stencil]()
  
  def verbose = false //Config.debug  
  def log(x: String) = if (verbose) Predef.println(x) else ()
  def result(x: String) = Predef.println(x)
  def strDef(x: Exp[Any]) = x match {
    case Const(z) => z.toString
    case _ => 
      val z = findDefinition(x.asInstanceOf[Sym[Any]])
      if (!z.isDefined) {
        "(bound " + x.toString + ")"        
      }
      else 
       z.get
  }
  
  def addStencil(s: Exp[Any], stencil: Stencil) {
    if (loopStencils.contains(s)) {
      loopStencils(s) = loopStencils(s) ++ stencil
    }
    else {
      loopStencils += s -> stencil
    }
  }
    
  def getLoopStencils = loopStencils
  def resetLoopStencils() { loopStencils = new HashMap[Exp[Any],Stencil]() }
    
  def run[A](b: Block[A]) = {
    log("[stencil analysis]: Starting")
          
    // run traversal
    traverseBlock(b)    
    
    result("")
    result("[stencil analysis]: Finished. Found stencils (in no particular order): ")
    result("")
    for ((k,v) <- loopStencils) {
      result("loop: " + strDef(k))
      result("stencil: " + v)
      result("")
    }
    
    loopStencils
    // println(loopStencils)    
    // sys.exit(0) // temporary: debugging stencil analysis    
  }
    
  /**
   * Determine if the index x corresponds to a chunk of the input, starting from ref
   * Currently, only stride == 1 is allowed (i.e., we assume stride = 1).
   */
   def chunked(ref: Sym[Int], x: Exp[Int], context: List[Stm]): Option[(Exp[Int],Exp[Int],Exp[Int])] = {     
     // checks if the given loop index corresponds to the bound variable of any loop
     // in the current context, and if it does, returns the loop size     
     def loopSize(i: Exp[Int]) = {
       val z = context.map(_.rhs) collect { 
         case l:AbstractLoop[_] if l.v == i => (l,l.size) 
         case Reflect(l:AbstractLoop[_],u,es) if l.v == i => (l,l.size) 
         case l:AbstractFatLoop if l.v == i => (l,l.size)
         case Reflect(l:AbstractFatLoop,u,es) if l.v == i => (l,l.size) 
       }       
       if (z == Nil) {
         log("    found no loops in the current context with bound var equal to " + strDef(i))
         None
       }
       else {
         if (z.length > 1) {
           log("    !! found multiple loops corresponding to loop index:")         
           log(z.toString)
         }
         log("    found loopSize: " + strDef(z(0)._2) + " from loop " + z(0)._1.toString)
         // log("      i was: " + i.toString + ", bound sym of loop is: " + z(0)._1.v.toString)
         Some(z(0)._2)
       }
     }

     // we expect the offset (start) to be in the form index*multiplier or multiplier*index
     // can we have anything else?
     //   stuff like start = (v+2)*numCols is not allowed (along with other arbitrary index arithmetic)
     def startMultiplier(i: Exp[Int]): Option[Exp[Int]] = i match {
       case Def(IntTimes(a,b)) if a == ref => Some(b)
       case Def(IntTimes(a,b)) if b == ref => Some(a)
       case _ => 
        log("    !! could not find startMultiplier for " + i.toString)
        None
     }
          
     x match {
       // we find the size of the chunk by matching the index in the apply against an outer loop in the
       // current context. if the index is bound to the outer loop, the chunk size is equal that loop's length.
            
       // index*stride + start
       case Def(IntPlus(Def(IntTimes(a,b)),c)) if a != ref && loopSize(a).isDefined && startMultiplier(c).isDefined => 
         log("    found index*stride + start")
         log("    IntPlus(IntTimes(" + strDef(a) + ", " + strDef(b) + "), " + strDef(c) + ")")
         Some(startMultiplier(c).get,b,loopSize(a).get)
      
       // stride*index + start 
       case Def(IntPlus(Def(IntTimes(a,b)),c)) if b != ref && loopSize(b).isDefined && startMultiplier(c).isDefined =>
         log("    found stride*index + start")
         log("    IntPlus(IntTimes(" + strDef(a) + ", " + strDef(b) + "), " + strDef(c) + ")")
         Some(startMultiplier(c).get,a,loopSize(b).get)
       
       // start + index*stride 
       case Def(IntPlus(a,Def(IntTimes(b,c)))) if b != ref && loopSize(b).isDefined && startMultiplier(a).isDefined => 
         log("    found start + index*stride")
         log("    found IntPlus(" + strDef(a) + ", IntTimes(" + strDef(b) + ", " + strDef(c) + "))")
         Some(startMultiplier(a).get,c,loopSize(b).get)
        
       // start + stride*index
       case Def(IntPlus(a,Def(IntTimes(b,c)))) if c != ref && loopSize(c).isDefined && startMultiplier(a).isDefined => 
         log("    found start + stride*index")
         log("    found IntPlus(" + strDef(a) + ", IntTimes(" + strDef(b) + ", " + strDef(c) + "))")
         Some(startMultiplier(a).get,b,loopSize(c).get)       
      
       // index + start (i.e. stride == 1)
       case Def(IntPlus(a,b)) if a != ref && loopSize(a).isDefined && startMultiplier(b).isDefined =>
         log("    found index + start")
         log("    found IntPlus(" + strDef(a) + ", " + strDef(b) + ")")
         Some(startMultiplier(b).get,Const(1),loopSize(a).get)       

       // start + index (i.e. stride == 1)
       case Def(IntPlus(a,b)) if b != ref && loopSize(b).isDefined && startMultiplier(a).isDefined =>
         log("    found start + index")
         log("    found IntPlus(" + strDef(a) + ", " + strDef(b) + ")")
         Some(startMultiplier(a).get,Const(1),loopSize(b).get)       
       
       case Def(IntPlus(Def(IntTimes(a,b)),c)) => 
         log("    xx IntPlus(IntTimes(" + strDef(a) + ", " + strDef(b) + "), " + strDef(c) + ")")
         None
         
       case Def(IntPlus(a,Def(IntTimes(b,c)))) => 
         log("    xx found IntPlus(" + strDef(a) + ", IntTimes(" + strDef(b) + ", " + strDef(c) + "))")
         None
      
       // index
       // case a if a != ref && loopSize(a).isDefined =>
       //   // how do we know how a relates to ref? this says that it touches elements starting at 'ref', but it really touches elements starting at 'a'..
       //   Some(Const(1),Const(1),loopSize(a).get)
         
       case _ => None
     }
   }
  
  
  /**
   * For a given IR node, determine any constraints it imposes with respect to
   * the loop variable v.
   */
  def examine(x: Stm, v: Sym[Int], stencil: Stencil, context: List[Stm]) {

    def processArrayAccess(a: Exp[DeliteArray[Any]], i: Exp[Int]) {
      // TODO: if we already have an entry for 'a' in the stencil, what should we do?
      // take the most conservative value?
      
      lazy val interval = chunked(v,i,context)
      
      log("      found DeliteArrayApply of array " + strDef(a) + " and index " + strDef(i))
      if (v == i) {
        log("    determining that array " + a.toString + " accesses elements at loop index " + v.toString)                
        stencil += a -> one
      }
      else if (interval.isDefined) {
        val (start,stride,size) = interval.get
        log("    determining that array " + a.toString + " accesses elements at loop index v*" + strDef(start) + " with stride " + strDef(stride) + " and chunk size " + strDef(size))                                            
        stencil += a -> Interval(start,stride,size)
      }
      else {          
        i match {
          case c@Const(n) => 
            log("    determining that array " + a.toString + " accesses the element at constant index " + n.toString)                
            stencil += a -> Constant(c)
          case _ =>
            log("    could not determine how array " + a.toString + " is accessed as a function of loop index " + v.toString + ". assuming all elements are accessed.")
            stencil += a -> all            
        }
      }                
    }
    
    x.rhs match {
      case DeliteArrayApply(a,i) => processArrayAccess(a,i)
      case Reflect(DeliteArrayApply(a,i), u, es) => processArrayAccess(a,i)        
      case _ => // log("examining " + x.rhs.toString)
    }
  }
  
  /**
   * For the given loop element, determine all index constraints for all
   * DeliteCollections used in its body. 
   */
  def process[A](s: Sym[A], v: Sym[Int], body: Def[_]) { 
    body match {
        
      case DeliteForeachElem(func,numDynamicChunks) =>
        val stencil = new Stencil()
        
        log("  ++ found foreach elem")
        log("    func is: " + strDef(func.res))
        val schedule = buildScheduleForResult(func)        
        log("    schedule for func is: " + schedule)
        log("    loop bound sym is: " + v.toString)
        
        schedule.foreach(examine(_,v,stencil,schedule))           
        
        addStencil(s, stencil)
        
      case DeliteCollectElem(func,cond,par,buf,iFunc,iF,sF,eF,numDynamicChunks) =>
        val stencil = new Stencil()
        
        log("  ++ found collect elem")
        log("    func is: " + strDef(func.res))        
        val schedule = buildScheduleForResult(func)        
        log("    schedule for func is: " + schedule)
        log("    loop bound sym is: " + v.toString)
        
        schedule.foreach(examine(_,v,stencil,schedule))   
        
        addStencil(s, stencil)
      
      case DeliteReduceElem(func,cond,zero,accInit,rV,rFunc,stripFirst,numDynamicChunks) =>
        val stencil = new Stencil()
        
        log("  ++ found reduce elem")
        log("    func is: " + strDef(func.res))
        log("    cond is: " + cond.map(b => strDef(b.res)).mkString(", "))
        log("    loop bound sym is: " + v.toString)        
        val funcSchedule = buildScheduleForResult(func)        
        log("    schedule for func is: " + funcSchedule)        
        funcSchedule.foreach(examine(_,v,stencil,funcSchedule))   
        
        val condSchedule = buildScheduleForResult(cond)        
        log("    schedule for cond is: " + condSchedule)                
        condSchedule.foreach(examine(_,v,stencil,condSchedule))   
        
        val redSchedule = buildScheduleForResult(rFunc)        
        log("    schedule for red is: " + redSchedule)                
        redSchedule.foreach(examine(_,v,stencil,redSchedule))   
        
        addStencil(s, stencil)
              
      case DeliteReduceTupleElem(func,cond,zero,rVPar,rVSeq,rFuncPar,rFuncSeq,stripFirst,numDynamicChunks) =>
        val stencil = new Stencil
        
        log("  ++ found reduce tuple elem")
        log("    func 1 is: " + strDef(func._1.res))
        log("    func 2 is: " + strDef(func._2.res))
        log("    cond is: " + cond.map(b => strDef(b.res)).mkString(", "))
        log("    loop bound sym is: " + v.toString)        
        
        val func1Schedule = buildScheduleForResult(func._1)        
        log("    schedule for func1 is: " + func1Schedule)        
        func1Schedule.foreach(examine(_,v,stencil,func1Schedule))   

        val func2Schedule = buildScheduleForResult(func._2)        
        log("    schedule for func2 is: " + func2Schedule)        
        func2Schedule.foreach(examine(_,v,stencil,func2Schedule))   
        
        val condSchedule = buildScheduleForResult(cond)        
        log("    schedule for cond is: " + condSchedule)                
        condSchedule.foreach(examine(_,v,stencil,condSchedule))
        
        val redSchedule = buildScheduleForResult(rFuncSeq)        
        log("    schedule for red is: " + redSchedule)                
        redSchedule.foreach(examine(_,v,stencil,redSchedule))              
        
        addStencil(s, stencil)
       
      case DeliteHashReduceElem(keyFunc,valFunc,cond,zero,rV,rFunc,buf,numDynamicChunks) =>
        val stencil = new Stencil
        
        log("  ++ found hash reduce elem")
        log("    key func is: " + strDef(keyFunc.res))
        log("    val func is: " + strDef(valFunc.res))
        log("    reduce func is: " + strDef(rFunc.res))
        log("    cond is: " + cond.map(b => strDef(b.res)).mkString(", "))
        log("    loop bound sym is: " + v.toString)        
        
        val keyFuncSchedule = buildScheduleForResult(keyFunc)        
        log("    schedule for keyFunc is: " + keyFuncSchedule)        
        keyFuncSchedule.foreach(examine(_,v,stencil,keyFuncSchedule))   
        
        val valFuncSchedule = buildScheduleForResult(valFunc)        
        log("    schedule for valFunc is: " + valFuncSchedule)        
        valFuncSchedule.foreach(examine(_,v,stencil,valFuncSchedule))   
        
        val rFuncSchedule = buildScheduleForResult(rFunc)        
        log("    schedule for rFunc is: " + rFuncSchedule)        
        rFuncSchedule.foreach(examine(_,v,stencil,rFuncSchedule))   
        
        val condSchedule = buildScheduleForResult(cond)        
        log("    schedule for cond is: " + condSchedule)                
        condSchedule.foreach(examine(_,v,stencil,condSchedule))        
        
        addStencil(s, stencil)
        
      case _ => 
    }
  }  
  
  
  // our atoms are top-level loops (currently after lowerings, in order to associate the analysis information directly with transformed Delite array symbols)
   
  // if we do the traversal on SoA'd loops (pre-fusion) we end up inspecting each split loop independently
  // doing the traversal on SoA'd loops post-fusion is tricky due to the mechanics of the implementation (we need to integrate fusion into the traversal)
  // if we do the traversal pre-SoA, the analysis may be easier, but we need to merge the information together in the DEG as necessary (this doesn't sound too bad)
  override def traverseStm(stm: Stm): Unit = stm match {    
    case TP(s,l:DeliteOpLoop[_]) =>             
      log("-- found op " + l.toString)
      log("  body is: " + l.body.toString)
      // process(s,l)
      process(s,l.v,l.body)
    case TP(s,Reflect(l:DeliteOpLoop[_], u, es)) =>      
      log("-- found effectful op " + l.toString)
      log("  body is: " + l.body.toString)    
      // process(s,l)
      process(s,l.v,l.body)
      
    // for lowered loops:    
    case TP(s,rhs@SimpleLoop(sz,v,body)) =>
      log("-- found simple loop " + rhs.toString)
      log("-- body is: ")
      log("     " + body.toString)    
      process(s,v,body)
    case TTP(lhs,mhs,rhs@SimpleFatLoop(sz,v,body)) =>
      log("-- found abstract fat loop " + rhs.toString)
      log("-- body is: ")
      for ((s,d) <- lhs.zip(body)) {
        log("     " + d.toString)
        process(s,v,d)
      }
    /*
    case op:AbstractFatLoopIf =>
    */
    case _ => 
      // log(" xx " + stm.rhs.toString)
      super.traverseStm(stm)
  }  
}
