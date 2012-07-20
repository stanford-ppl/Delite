package ppl.dsl.optiml.transform

import java.io._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{AbstractSubstTransformer,Transforming}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, BaseDeliteOpsTraversalFat}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.Config
import ppl.delite.framework.transform.{DeliteTransform,ForwardPassTransformer}
import ppl.dsl.optila.vector.DenseVectorOpsExp

/**
 * Transform nested multiloops by selecting a dimension to parallelize for the GPU.
 */

trait MultiloopTransformExp extends DeliteTransform 
  with DeliteApplication with DeliteOpsExp 
  with BooleanOpsExp with MiscOpsExp with StringOpsExp with ObjectOpsExp with PrimitiveOpsExp
  with LiftString with LiftBoolean with LiftPrimitives { // may want to use self-types instead of mix-in to decrease risk of accidental inclusion
  
  self: DenseVectorOpsExp =>
  
  val cudaGen = getCodeGenPkg(cudaTarget) // use a fresh CUDA generator to avoid any interference

  /*
   * This selects which multiloop transformer (policy) to use. TODO: make configurable
   */
  // private val t = deviceDependentLowering  
  private val t = new MultiloopTransformOuter { val IR: self.type = self }
  if (Config.generateCUDA && Config.optimize > 0) {
    appendTransformer(t)    
  }
  
  def isGPUable(block: Block[Any]): Boolean = {
    try {
      cudaGen.innerScope = t.innerScope
      cudaGen.withStream(new PrintWriter(new ByteArrayOutputStream()))(cudaGen.emitBlock(block))
    }
    catch {
      case _ => return false
    }
    // Predef.println("found GPUable collect elem: " + block)
    true
  }
  
  def blockContainsGPUableCollect(block: Block[Any]) = {
    var foundCollect = false
    // Predef.println("unwrapping block: " + block)
    t.focusBlock(block) {
      t.focusExactScope(block) { levelScope => 
        // for (e <- levelScope) Predef.println("  examining stm: " + e)
        foundCollect = levelScope.filter(_.rhs.isInstanceOf[DeliteOpLoop[Any]])
                                 .filter(_.rhs.asInstanceOf[DeliteOpLoop[Any]].body.isInstanceOf[DeliteCollectElem[_,_,_]])
                                 .exists(e=>isGPUable(e.rhs.asInstanceOf[DeliteOpLoop[Any]].body.asInstanceOf[DeliteCollectElem[_,_,_]].func))
      }
    }    
    foundCollect
  }
  
  def loopContainsGPUableCollect(l: DeliteOpLoop[_]) = {
    // Predef.println("examining loop: " + l); 
    l.body match {
      case e:DeliteCollectElem[_,_,_] => blockContainsGPUableCollect(e.func)
      case e:DeliteForeachElem[_] => blockContainsGPUableCollect(e.func)
      case e:DeliteReduceElem[_] => blockContainsGPUableCollect(e.func) || blockContainsGPUableCollect(e.rFunc)
      case _ => false
    }
  }
  
  def shouldTransformLoop(l: DeliteOpLoop[_]) = {    
    // an old, but flawed criteria
    // loopContainsGPUableCollect(l)
    
    // an equally flawed criteria, but hey
    l.size match {
      case Const(x) if x < 1024 => 
        // Predef.println("transforming loop due to size (" + x + "): " + l.toString)
        true
      case _ => 
        // Predef.println("not transforming loop of size (" + l.size.toString + "): " + l.toString)
        false
    }
  }
    
  def transformCollectToWhile[A:Manifest,I<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest](s: Sym[_], loop: DeliteOpLoop[_], e: DeliteCollectElem[A,I,CA]): Exp[CA] = {
    // inlined to expose interior parallelism    
    def collectToWhile(size: Rep[Int],
                       alloc: Rep[Int] => Rep[I],
                       f: Rep[Int] => Rep[A],                        
                       upd: (Rep[I],Rep[Int],Rep[A]) => Rep[Unit], 
                       cond: List[Rep[Int] => Rep[Boolean]], 
                       append: (Rep[I],Rep[Int],Rep[A]) => Rep[Boolean], 
                       setSize: (Rep[I],Rep[Int]) => Rep[Unit],
                       finalizer: Rep[I] => Rep[CA]) = {      
                         
      val i = var_new(0) 
      val res = if (cond.nonEmpty) alloc(0) else alloc(size) 
      val appended = var_new(0)
      while (i < size) {
        // mixed staged and interpreted code follows (operations on cond are unlifted)
        if (cond.nonEmpty) {
          if (cond.map(c=>c(i)).reduce((a,b) => a && b)) 
             if (append(res,i,f(i))) appended += 1 
        }
        else 
          upd(res,i,f(i))
        
        i += 1
      }
      if (cond.nonEmpty) setSize(res,appended)
      
      finalizer(res).unsafeImmutable 
    }       
    
    // s.asInstanceOf[Sym[CA]].atPhase(t) {
        collectToWhile(      
          t(loop.size),
          {sz => transformBlockWithBound(t, e.allocN, List(e.sV -> sz))},
          {n => transformBlockWithBound(t, e.func, List(loop.v -> n))},                          
          {(col,n,x) => transformBlockWithBound(t, e.update, List(e.allocVal -> col, loop.v -> n, e.eV -> x))},                
          e.cond.map{c => { n: Rep[Int] => transformBlockWithBound(t, c, List(loop.v -> n)) }},
          {(col,n,x) => transformBlockWithBound(t, e.buf.append, List(e.allocVal -> col, loop.v -> n, e.eV -> x))},                
          {(col,sz) => transformBlockWithBound(t, e.buf.setSize, List(e.allocVal -> col, e.sV -> sz))},                
          {col => transformBlockWithBound(t, e.finalizer, List(e.allocVal -> col))})
    // }
  }  
  
  def transformForeachToWhile[A:Manifest](s: Sym[_], loop: DeliteOpLoop[_], e: DeliteForeachElem[A]): Exp[Unit] = {
    // inlined to expose interior parallelism    
    def foreachToWhile(size: Rep[Int],
                       f: Rep[Int] => Rep[A]) = {
                         
      val i = var_new(0) 
      while (i < size) {
        f(i)
        i += 1
      }
    }       
    
    // s.asInstanceOf[Sym[Unit]].atPhase(t) {
        foreachToWhile(      
          t(loop.size),
          {n => transformBlockWithBound(t, e.func, List(loop.v -> n))})
    // }
  }  
  
  def transformReduceToWhile[A:Manifest](s: Sym[_], loop: DeliteOpLoop[_], e: DeliteReduceElem[A]): Exp[A] = {
    // inlined to expose interior parallelism    
    def reduceToWhile(size: Rep[Int],
                      f: Rep[Int] => Rep[A],                        
                      cond: List[Rep[Int] => Rep[Boolean]], 
                      zero: Rep[A],
                      rFunc: (Rep[A],Rep[A]) => Rep[A],
                      stripFirst: Boolean,
                      mutable: Boolean) = {
      
      // mixed staged and interpreted code follows (operations on cond, stripFirst and mutable unlifted)      
      // zero can be primitive even when mutable == true, so we need to save the var regardless        
      val i = if (stripFirst) var_new(1) else var_new(0) 
      val acc = if (stripFirst) var_new(f(0)) else var_new(zero.unsafeImmutable) 
      while (i < size) {
        if (cond.nonEmpty) {
          if (cond.map(c=>c(i)).reduce((a,b) => a && b)) {
            if (mutable) acc = (rFunc(acc.unsafeMutable,f(i))).unsafeImmutable 
            else acc = rFunc(acc, f(i))
           }
        }
        else {
          if (mutable) acc = (rFunc(acc.unsafeMutable,f(i))).unsafeImmutable
          else acc = rFunc(acc, f(i))
        }
        i += 1
      }
      acc.unsafeImmutable      
    }       
    
    // s.asInstanceOf[Sym[A]].atPhase(t) {
        reduceToWhile(      
          t(loop.size),
          {n => transformBlockWithBound(t, e.func, List(loop.v -> n))},                          
          e.cond.map{c => { n: Rep[Int] => transformBlockWithBound(t, c, List(loop.v -> n)) }},
          t.reflectBlock(e.zero),
          {(a,b) => transformBlockWithBound(t, e.rFunc, List(e.rV._1 -> a, e.rV._2 -> b))},                
          e.stripFirst,
          loop.asInstanceOf[DeliteOpReduceLike[_]].mutable)
    // }
  }  
  
}

/* Always parallelize inner multiloop, if GPUable */ 
trait MultiloopTransformOuter extends ForwardPassTransformer with BaseDeliteOpsTraversalFat with LoopFusionOpt {  
  val IR: LoopsFatExp with IfThenElseFatExp with MultiloopTransformExp with DenseVectorOpsExp
  import IR._
  
  var inMultiloop = 0

  override def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    val save = Config.opfusionEnabled
    // disabling fusion for now because still not sure how to make
    // fusion+transformer combo work correctly in general
    Config.opfusionEnabled = false
    val z = super.runOnce(s)
    Config.opfusionEnabled = save
    // 
    // Predef.println("global syms: ")
    // for (s<-IR.globalDefs) Predef.println("  "+s)
    z
  }  
  
  override def unapplySimpleIndex(e: Def[Any]) = e match { // TODO: what to do about this? should inherit from BaseDenseVectorTraversal or something? 
    case DenseVectorApply(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }  
      
  override def traverseStm(stm: Stm) = stm match {        
    case TTP(lhs, mhs, rhs) => 
      // Predef.println("unwrapping TTP: " + stm)
      lhs.map(e => findDefinition(e).get).foreach(traverseStm) // ??
     
    case TP(s,l:DeliteOpLoop[_]) =>
      inMultiloop += 1
      super.traverseStm(stm)
      inMultiloop -= 1
      
    case TP(s,Reflect(l:DeliteOpLoop[_], u, es))  =>
      inMultiloop += 1
      super.traverseStm(stm)
      inMultiloop -= 1
      
    case _ => super.traverseStm(stm)
  }        
  
  override def transformStm(stm: Stm): Exp[Any] = stm match {
    case TP(s,l:DeliteOpLoop[_]) if (inMultiloop < 2 && shouldTransformLoop(l)) => 
      val newSym = l.body match {
        case e:DeliteCollectElem[_,_,_] => (transformCollectToWhile(s,l,e)(e.mA,e.mI,e.mCA))
        case e:DeliteForeachElem[_] => (transformForeachToWhile(s,l,e)(e.mA))
        case e:DeliteReduceElem[_] => (transformReduceToWhile(s,l,e)(e.mA))
        case _ => super.transformStm(stm)
      }      
      newSym
      
    case TP(s,Reflect(l:DeliteOpLoop[_], u, es)) if (inMultiloop < 2 && shouldTransformLoop(l)) =>
      val newSym = l.body match {
        case e:DeliteCollectElem[a,i,c] => reflectTransformed(this.asInstanceOf[IR.Transformer], (transformCollectToWhile(s,l,e)(e.mA,e.mI,e.mCA)), u, es)(e.mCA) // cast needed why?
        case e:DeliteForeachElem[_] => reflectTransformed(this.asInstanceOf[IR.Transformer], (transformForeachToWhile(s,l,e)(e.mA)), u, es) 
        case e:DeliteReduceElem[a] => reflectTransformed(this.asInstanceOf[IR.Transformer], (transformReduceToWhile(s,l,e)(e.mA)), u, es)(e.mA)       
        case _ => super.transformStm(stm)        
      }
      newSym
      
    case _ => 
      // Predef.println("=== FOUND NO TRANFORM MATCH FOR: " + stm.toString)
      super.transformStm(stm)
  }
}