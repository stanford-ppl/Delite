package ppl.dsl.deliszt.analysis

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen

import ppl.delite.framework.DeliteApplication

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.meshset.MeshSetOpsExp

import scala.collection.mutable.{Map => MMap}

trait LoopColoringOpt extends GenericFatCodegen with SimplifyTransform {
  val IR: DeliteApplication with LoopsFatExp with DeLisztExp
  import IR._
  
  import StencilCollector.StencilMap
  
  val blockSize = 1

  object LCSimpleIndex {
    def unapply(a: Def[Any]): Option[(Exp[Any], Exp[Int])] = unapplySimpleIndex(a)
  }

  object LCSimpleDomain {
    def unapply(a: Def[Int]): Option[Exp[Any]] = unapplySimpleDomain(a)
  }

  object LCSimpleCollect {
    def unapply(a: Def[Any]): Option[Exp[Any]] = unapplySimpleCollect(a)
  }

  object LCSimpleCollectIf {
    def unapply(a: Def[Any]): Option[(Exp[Any],List[Exp[Boolean]])] = unapplySimpleCollectIf(a)
  }
  
  var firstRun = true

  override def focusExactScopeFat[A](currentScope0: List[TTP])(result0: List[Exp[Any]])(body: List[TTP] => A): A = {
    var result: List[Exp[Any]] = result0
    var currentScope = currentScope0
    
    System.out.println("focusing scopes")
    
    // Get the map of for loops to Stencil
    val forMap = analysisResults("StencilCollectorStencils").asInstanceOf[MMap[Int,StencilMap]]
    val msMap = analysisResults("StencilCollectorMeshsets").asInstanceOf[MMap[Int,MeshSet[MeshObj]]]
       
    if(firstRun) {
      System.out.println("Top level loops")
      System.out.println(forMap.keys)
      firstRun = false
    }
    
    // Find loops at current top level that exist in the stencil
    var foreachs = super.focusExactScopeFat(currentScope)(result) { levelScope =>   
      System.out.println(levelScope)
      // Collect various types of loops. I believe #2 is the current style but it might change...
      levelScope collect { case e @ TTP(syms, SimpleFatLoop(_,_,_)) if syms exists { s => forMap.contains(s.id) } => e
                           case e @ TTP(syms, ThinDef(Reflect(MeshSetForeach(_,_),_,_))) if syms exists { s => forMap.contains(s.id) } => e
                           case e @ TTP(syms, ThinDef(MeshSetForeach(_,_))) if syms exists { s => forMap.contains(s.id) } => e
      }
    }
    
    // Color each loop!
    for(loop <- foreachs) {
      // Grab the MeshSet
      val id = (loop.lhs.collectFirst {
        case Sym(id) => id
      }).get
      
      val ms = msMap(id)
      val stencil = forMap(id)
      
      // Initialize colorers!
      val colorer = new RegisterColorer()
      val interferenceBuilder = new InterferenceBuilder(colorer, blockSize)
      
      for((mo, rwset) <- stencil) {
        System.out.println(mo)
        for(FieldAccess(i, mo) <- rwset.read) {
          System.out.println("read field " + i)
          System.out.println(mo)
        }
        for(FieldAccess(i, mo) <- rwset.write) {
          System.out.println("write field " + i)
          System.out.println(mo)
        }
      }
      
      // And color!
      val coloring = interferenceBuilder.buildAndColor(ms, stencil)
      
      // Output coloring for debugging
      var i = 0
      System.out.print("Loop id: " + id)
      System.out.println(" Num colors: " + coloring.numColors)
      while(i < ms.size) {
        System.out.println("Node: " + coloring.nodes(i) + " color: " + coloring.colors(i))
        i += 1
      }
      
      // var UloopSyms = currentScope collect { case e @ TTP(lhs, SimpleFatLoop(_,_,_)) if !Wloops.contains(e) => lhs }
      
      // utils
      def loopParams(e: TTP): (Exp[Int], Exp[Int]) = e.rhs match {
        case SimpleFatLoop(s,x,rhs) => (s, x)
        case ThinDef(d) => d match {
          case Reflect(msf @ MeshSetForeach(_,_),_,_) => (msf.size, msf.v)
          case msf @ MeshSetForeach(_,_) => (msf.size, msf.v)
        }
      }
      
      //def WgetLoopRes(e: TTP): List[Def[Any]] = e.rhs match { case SimpleFatLoop(s,x,rhs) => rhs }

      // val loopCollectSyms = (loop.lhs zip WgetLoopRes(loop)) collect { case (s, LCSimpleCollectIf(_,_)) => s }
      
      val (shape, loopVar) = loopParams(loop)
      
      def extendLoopWithCondition(e: TTP, shape: Exp[Int], targetVar: Sym[Int], c: List[Exp[Boolean]]): List[Exp[Any]] = e.rhs match { 
        case SimpleFatLoop(s,x,rhs) => rhs.map { r => findOrCreateDefinition(SimpleLoop(shape,targetVar,applyAddCondition(r,c))).sym }
      }
             
      val t = new SubstTransformer
    }
    
    /*  // actually do the fusion: now transform the loops bodies
      // within fused loops, remove accesses to outcomes of the fusion
      currentScope.foreach {
        case e@TTP(List(s), ThinDef(LCSimpleIndex(a, i))) =>
          printlog("considering " + e)
          Wloops.find(_.lhs contains a) match {
            case Some(fused) if WgetLoopVar(fused) contains t(i) => 
              val index = fused.lhs.indexOf(a)
              
              printlog("replace " + e + " at " + index + " within " + fused)

              val rhs = WgetLoopRes(fused)(index) match { case LCSimpleCollectIf(y,c) => y }
              
              t.subst(s) = rhs
            case _ => //e
          }
        case _ => //e
      }
      
      
      currentScope = getFatSchedule(currentScope)(currentScope) // clean things up!

      // SIMPLIFY! <--- multiple steps necessary???
      
      def withEffectContext(body: =>List[TTP]): List[TTP] = {
        val save = context
        context = Nil
        val scope = body
        val leftovereffects = context.filterNot((scope.flatMap(_.lhs)) contains _)
        if (leftovereffects.nonEmpty) 
          printlog("warning: transformation left effect context (will be discarded): "+leftovereffects)
        context = save
        scope
      }
      
      currentScope = withEffectContext { transformAll(currentScope, t) }
      result = t(result)
      currentScope = getFatSchedule(currentScope)(currentScope) // clean things up!

      currentScope = withEffectContext { transformAll(currentScope, t) }
      result = t(result)
      currentScope = getFatSchedule(currentScope)(currentScope) // clean things up!

      currentScope = withEffectContext { transformAll(currentScope, t) }
      result = t(result)
      currentScope = getFatSchedule(currentScope)(result) // clean things up!


      // once more to see if we are converged
      val previousScope = currentScope
      
      currentScope = withEffectContext { transformAll(currentScope, t) }
      result = t(result)
      currentScope = getFatSchedule(currentScope)(result) // clean things up!
      
      if (currentScope != previousScope) { // check convergence
        printerr("error: transformation of scope contents has not converged")
        printdbg(previousScope + "-->" + currentScope)
      }
      
      //Wloops = currentScope collect { case e @ TTP(_, FatLoop(_,_,_)) => e }

      Wloops = transformAll(Wloops, t)
      
      UloopSyms = UloopSyms map (t onlySyms _) // just lookup the symbols
    }
    
    // PREVIOUS PROBLEM: don't throw out all loops, might have some that are *not* in levelScope
    // note: if we don't do it here, we will likely see a problem going back to innerScope in 
    // FatCodegen.focusExactScopeFat below. --> how to go back from SimpleFatLoop to VectorPlus??
    // UPDATE: UloopSyms puts a tentative fix in place. check if it is sufficient!!
    // what is the reason we cannot just look at Wloops??
    currentScope = currentScope.filter { case e@TTP(lhs, _: AbstractFatLoop) => 
      val keep = UloopSyms contains lhs
      //if (!keep) println("dropping: " + e + ", not int UloopSyms: " + UloopSyms)
      keep case _ => true } ::: Wloops

    // schedule (and emit)
    currentScope = getFatSchedule(currentScope)(result) // clean things up!

    // the caller of emitBlock will quite likely call getBlockResult afterwards,
    // and if we change the result here, the caller will emit a reference to a sym
    // that doesn't exist (because it was replaced)

    if (result0 != result) {
      printlog("super.focusExactScopeFat with result changed from " + result0 + " to " + result)
      
      (result0 zip result) foreach {
        case (r0 @ Def(Reify(x, _, _)),Def(Reify(y, u, es))) => 
          if (!x.isInstanceOf[Sym[Any]])
            printlog("non-sym block result: " + x + " to " + y)
          else
            currentScope = currentScope :+ TTP(List(x.asInstanceOf[Sym[Any]]), ThinDef(Forward(y)))
          currentScope = currentScope :+ TTP(List(r0.asInstanceOf[Sym[Any]]), ThinDef(Reify(x,u,es)))
          // should rewire result so that x->y assignment is inserted
        case (r0,r) => 
          if (r0 != r) currentScope = currentScope :+ TTP(List(r0.asInstanceOf[Sym[Any]]), ThinDef(Forward(r)))
      }
      
    }
    */
    
    // do what super does ...
    super.focusExactScopeFat(currentScope)(result0)(body)
  }
}
