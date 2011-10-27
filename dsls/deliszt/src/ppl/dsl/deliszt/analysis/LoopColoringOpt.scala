package ppl.dsl.deliszt.analysis

import java.io.PrintWriter
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen

import ppl.delite.framework.{Config, DeliteApplication}

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.meshset.MeshSetOpsExp

import scala.collection.mutable.{Map => MMap}

trait LoopColoringOpsExp extends BaseFatExp with EffectExp { this: DeLisztExp =>
  case class ColorIndexSetNew(indices: Exp[Array[Int]], start: Exp[Int], end: Exp[Int], mo: Manifest[MeshSet[MeshObj]]) extends Def[MeshSet[MeshObj]]    
  
  def color_index_set_new(indices: Array[Int], start: Int, end: Int)(implicit mo: Manifest[MeshSet[MeshObj]]): Exp[MeshSet[MeshObj]] = {
    //reifyEffects(ColorIndexSetNew(Array(indices: _*),unit(start),unit(end),mo))
    ColorIndexSetNew(Array(indices: _*),unit(start),unit(end),mo)
  }
  def color_lift(x: Int) = Const(x)
}

trait ScalaGenLoopColoringOps extends ScalaGenBase {
  val IR: LoopColoringOpsExp
  import IR._

  // clean this up when deliszt front-end does not depend on deliszt back-end
  def meshObjConstructor(mo: Manifest[MeshSet[MeshObj]]) = {
    val tp = mo.typeArguments(0)
    val (cellM,edgeM,faceM,vertexM) = (manifest[Cell],manifest[Edge],manifest[Face],manifest[Vertex])

    tp match {
      case `cellM` => "generated.scala.MeshObjConstruct.CellConstruct"
      case `edgeM` => "generated.scala.MeshObjConstruct.EdgeConstruct"
      case `faceM` => "generated.scala.MeshObjConstruct.FaceConstruct"
      case `vertexM` => "generated.scala.MeshObjConstruct.VertexConstruct"
    }
  }
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case e@ColorIndexSetNew(indices,start,end,mo) => 
        val moc = meshObjConstructor(mo)
        emitValDef(sym, "new generated.scala.IndexSetImpl(" + quote(indices) + ", " + quote(start) + "," + quote(end) +")(" + moc + ")")        
      case _ => super.emitNode(sym,rhs)
    }
  }
}


trait LoopColoringOpt extends GenericFatCodegen with SimplifyTransform {
  val IR: DeliteApplication with LoopsFatExp with LoopColoringOpsExp with MeshSetOpsExp
  import IR._
  import scala.collection.mutable.ArrayBuffer
  import scala.collection.immutable.List  
  import StencilCollector.StencilMap
  
  val blockSize = 1

  var firstRun = true

  def colog(s: String) = printlog("[liszt coloring] " + s)
  def codbg(s: String) = printdbg("[liszt coloring] " + s)
  
  // the issue here is that to transform the MeshSetForeach, we need more information than SimpleFatLoop keeps around.
  // by not turning into a SimpleFatLoop, we preclude the ability to fuse MeshSetForeachs. If fusing is important in Liszt,
  // we should figure out how to get both.
  override def fatten(e: TP[Any]): TTP = e.rhs match {
    case Reflect(MeshSetForeach(_,_),_,_) => TTP(List(e.sym), ThinDef(e.rhs)) // don't turn MeshSetForeach into a SimpleFatLoop!
    case _ => super.fatten(e)
  }
    
  override def focusExactScopeFat[A](currentScope0: List[TTP])(result0: List[Exp[Any]])(body: List[TTP] => A): A = {
    var result: List[Exp[Any]] = result0
    var currentScope = currentScope0
    
    if(Config.collectStencil) {
      // Get the map of for loops to Stencil
      val forMap = analysisResults("StencilCollectorStencils").asInstanceOf[MMap[Int,StencilMap]]
      val msMap = analysisResults("StencilCollectorMeshsets").asInstanceOf[MMap[Int,MeshSet[MeshObj]]]
         
      if(firstRun) {
        println("Top level loops")
        
        for((id, stencil) <- forMap) {
          println(id)
        }
        
        firstRun = false
      }
      
      // Find loops at current top level that exist in the stencil
      var Wloops = super.focusExactScopeFat(currentScope)(result) { levelScope =>   
        levelScope collect { case e @ TTP(syms, ThinDef(Reflect(MeshSetForeach(_,_),_,_))) if syms exists { s => forMap.contains(s.id) } => e }
      }
      
      // keep track of loops in inner scopes
      // var UloopSyms = currentScope collect {case e @ TTP(lhs, ThinDef(Reflect(MeshSetForeach(_,_),_,_))) if !Wloops.contains(e) => lhs }
      
      // Color each loop!
      for(loop <- Wloops) { 
        assert(loop.lhs.length == 1)
        
        // Grab the MeshSet
        val id = (loop.lhs.collectFirst {
          case Sym(id) => id
        }).get
        
        val ms = msMap(id)
        val stencil = forMap(id)
        
        // Initialize colorers!
        val colorer = new RegisterColorer()
        val interferenceBuilder = new InterferenceBuilder(colorer, blockSize)
        
        // println("Read write sets!")
        // for((mo, rwset) <- stencil) {
        //   println("Element: " + mo)
        //   for(FieldAccess(i, mo) <- rwset.write) {
        //     println("write field " + i + " mo: " + mo)
        //   }
        // }
        
        // And color!
        val coloring = interferenceBuilder.buildAndColor(ms, stencil)
        val (color_idx, color_values) = coloring.collect()
        
        // Output coloring for debugging
        print("Loop id: " + id)
        println(" num elements: " + ms.size)
        println(" num colors: " + coloring.numColors)
        
        var i = 0
        // while(i <= coloring.numColors) {
        //   println("color_idx: " + i + " " + color_idx(i))
        //   i += 1
        // }
        // 
        // i = 0
        // while(i < coloring.numColors) {
        //   var j = color_idx(i)
        //   while (j < color_idx(i+1)) {
        //     println("color_values: " + i + " " + color_values(j))
        //     j += 1
        //   }
        //   i += 1
        // }
        
        /* transform loop into multiple loops, one per color */
        
        // utils
        def WgetLoopParams(e: TTP): (Exp[MeshSet[MeshObj]], Exp[Int], Exp[Int], Exp[MeshObj] => Exp[Unit], Exp[Unit]) = e.rhs match {
          case ThinDef(Reflect(f@MeshSetForeach(in,func),_,_)) => (in, f.size, f.v, func, f.body.asInstanceOf[DeliteForeachElem[Unit]].func)
          // case SimpleFatLoop(s,v,body) => body match {
          //   case List(DeliteForeachElem(func, sync)) => (v, func)
          // }
        }
        
        def fission[A](in: List[A], old: A, replace: List[A]) = {
          val pos = in.indexOf(old)
          pos match {
            case -1 => in
            case _ => 
              val left = in.slice(0,pos)
              val right = in.slice(pos+1, in.length)
              left ::: replace ::: right
          }
        }
        
        def buildInScope[A](func: => Exp[A]): Exp[A] = {
          val save = globalDefs
          val out = func
          val addedDefs = globalDefs diff save
          innerScope = (innerScope ::: addedDefs).distinct
          currentScope = (currentScope ::: fattenAll(addedDefs)).distinct
          out
        }
        
        // build colored loops
        i = 0        
        var deps = List[TTP]()
        var colorLoops = List[TTP]()         
        var prevLoop: Option[TTP] = None
        val loopRefTransformer = new SubstTransformer                      
        
        println("<SCOPE BEFORE COLORING ---"+result0+"/"+result)
        currentScope.foreach(println)
        println("--->")          
        
        while (i < coloring.numColors) {
          var j = color_idx(i)
          val indices = ArrayBuffer[Int]()
          while (j < color_idx(i+1)) {
            indices += color_values(j)
            j += 1
          }

          // new loop dependencies
          val (in,size,v,f,body) = WgetLoopParams(loop)          
          val colorLoopVar = fresh(v.Type) // shouldn't need to transform the loop var, but what the hey...
          val colorSet = buildInScope(reifyEffects(color_index_set_new(indices.toArray, 0, indices.length)(in.Type)))
          val colorSize = buildInScope(color_lift(indices.length))
          val colorFunc = buildInScope(reifyEffects(f(dc_apply(colorSet,colorLoopVar))))
          
          colog("old func was: " + body.toString)
          colog("old func def: " + findDefinition(body.asInstanceOf[Sym[Unit]]).get.toString)
          colog("colorFunc is: " + colorFunc.toString)
          colog("colorFunc def is: " + findDefinition(colorFunc.asInstanceOf[Sym[Unit]]).get.toString)
          //innerScope :+= findDefinition(colorSet.asInstanceOf[Sym[MeshSet[MeshObj]]]).get 
          //innerScope :+= findDefinition(colorFunc.asInstanceOf[Sym[Unit]]).get 
          // currentScope :+= fatten(findDefinition(colorSet.asInstanceOf[Sym[MeshSet[MeshObj]]]).get)
          // currentScope :+= fatten(findDefinition(colorFunc.asInstanceOf[Sym[Unit]]).get)
          //innerScope :::= buildScheduleForResult(colorFunc)
          //deps = deps union fattenAll(buildScheduleForResult(colorFunc))          
          
          // add dependencies to scope
          // currentScope = currentScope union fattenAll(buildScheduleForResult(colorFunc))
            
          // transform loop
          val t = new SubstTransformer
          t.subst(v) = colorLoopVar
          t.subst(in) = colorSet 
          t.subst(size) = colorSize          
          t.subst(body) = colorFunc
          // val transformedLoop = (withEffectContext { transformAll(List(loop), t) })(0)                          
          val (newScope, transformedLoopSyms) = transformAllFully(currentScope, loop.lhs, t) 
          // val (newScope, transformedLoopSyms) = transformAllFully(getFatSchedule(currentScope)(loop.lhs), loop.lhs, t)
          // currentScope = currentScope union newScope //(newScope diff colorLoops)
          val transformedLoop = newScope.find(_.lhs == transformedLoopSyms).get          
          //colog("TransformedLoopExp: " + transformedLoopExp.toString)
          // add previous transformedLoop to new one as a dep
          val transformedLoop2 = transformedLoop match { 
            case TTP(lhs, ThinDef(Reflect(a,u,d))) if prevLoop.isDefined => TTP(lhs, ThinDef(Reflect(a,u,d ::: prevLoop.get.lhs)))
            case _ => transformedLoop
          }
          
          prevLoop = Some(transformedLoop2)
          colorLoops :+= transformedLoop2          
          i += 1
        }                
        colog("colored loop " + id + " into " + colorLoops.length + " loops using " + coloring.numColors + " colors")
        colog("original loop: " + loop.toString)
        colorLoops foreach { l => colog("colored loop: " + l.toString) }
      
        // replace old loop with new loops
        println("<SCOPE BEFORE FISSION ---"+result0+"/"+result)
        currentScope.foreach(println)
        println("--->")          

        println("=== DEPS: " + deps)
        
        //currentScope = fission(currentScope, loop, deps union colorLoops).distinct        
        currentScope = fission(currentScope, loop, colorLoops).distinct        
        
        println("<SCOPE AFTER FISSION ---"+result0+"/"+result)
        currentScope.foreach(println)
        println("--->")          
        
        //currentScope = fission(currentScope, loop, getFatDependentStuff(currentScope)(colorLoops.flatMap(e => syms(e.rhs))) ::: colorLoops)
        
        // replace references to old loop (since we are replacing effectful loops of type Unit, should only be effectful dependencies...)
        currentScope = currentScope map (e => e match {
          case TTP(lhs, ThinDef(Reify(x,u,es))) if (es contains loop.lhs(0)) =>           
            val cleanEs = fission(es,loop.lhs(0),colorLoops flatMap { _.lhs })
            val o = lhs(0)
            val n = fresh(x.Type)            
            loopRefTransformer.subst(o) = n
            TTP(List(n), ThinDef(Reify(x,u,cleanEs))) 
          case _ => e
        })
        
        println("<SCOPE AFTER COLORING ---"+result0+"/"+result)
        currentScope.foreach(println)
        println("--->")          
        
        // how do we get the new dependencies into our new schedule? --> should be from getFatSchedule, but the possible deps need to be added to currentScope!
                
        result = loopRefTransformer(result) 
        currentScope = getFatSchedule(currentScope)(result) // clean things up!  
        //currentScope = getFatSchedule(currentScope)(currentScope) // clean things up!  
        
        // println("<BEFORE ITERATING ---"+result0+"/"+result)
        // currentScope.foreach(println)
        // println("--->")      

        // result = loopRefTransformer(result) 
        // transformAllFully(currentScope, result, loopRefTransformer) match { case (a,b) => // ENDS UP EMPTY - try when transforming result first
        // // transformAllFully(getFatSchedule(currentScope)(result), result, loopRefTransformer) match { case (a,b) => // too bad we can't use pair assigment
        //   currentScope = a
        //   result = b
        // }

        println("<AFTER ITERATING ---"+result0+"/"+result)
        currentScope.foreach(println)
        println("--->")              
        
      } // end loop foreach
              
      // replace old loop with new loops
      // currentScope = currentScope.filter {
      //   case e@TTP(lhs, ThinDef(Reflect(f@MeshSetForeach(in,_),_,_))) => 
      //     val keep = UloopSyms contains lhs
      //     if (!keep) println("dropping: " + e + ", not in UloopSyms: " + UloopSyms)
      //     keep         
      //   case _ => true
      // } ::: colorLoops
      
              
      // schedule (and emit)
      // currentScope = getFatSchedule(currentScope)(result) // clean things up!  
      // println("<x---"+result0+"/"+result)
      // currentScope.foreach(println)
      // println("---x>")      
      
      // if (result0 != result) {
      //   throw new RuntimeException("todo: handle different results after loop fission")
      //   //printlog("super.focusExactScopeFat with result changed from " + result0 + " to " + result)
      // }
          
    } // end if Config.collectStencil
    
    // println("<!!!!---"+result0+"/"+result)
    // currentScope.foreach(println)
    // println("---!!!!>")      
    
    // result0 does not depend on any of our newly introduced loops...
    
    // do what super does ...
    // super.focusExactScopeFat(currentScope)(result0)(body)
    super.focusExactScopeFat(currentScope)(result)(body)
  }
}
