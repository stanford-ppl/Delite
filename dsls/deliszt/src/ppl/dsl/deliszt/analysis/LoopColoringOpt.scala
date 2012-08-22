package ppl.dsl.deliszt.analysis

import java.io.PrintWriter
import reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen

import ppl.delite.framework.{Config, DeliteApplication}

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.meshset.MeshSetOpsExp

import scala.collection.mutable.{Map => MMap}

trait LoopColoringOpsExp extends BaseFatExp with EffectExp { this: DeLisztExp =>
  case class ColorIndexSetNew(indices: Exp[Array[Int]], start: Exp[Int], end: Exp[Int], mo: Manifest[MeshSet[MeshObj]]) extends Def[MeshSet[MeshObj]]    
  
  def color_index_set_new(indices: Array[Int], start: Int, end: Int)(implicit mo: Manifest[MeshSet[MeshObj]]): Exp[MeshSet[MeshObj]] = {
    ColorIndexSetNew(Array(indices: _*),unit(start),unit(end),mo)
  }
  def color_lift(x: Int) = Const(x)

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ColorIndexSetNew(_,_,_,_) => Nil
    case _ => super.boundSyms(e)
  }

  override def effectSyms(e: Any): List[Sym[Any]] = e match {
    case ColorIndexSetNew(_,_,_,_) => Nil
    case _ => super.effectSyms(e)
  }
}

trait ScalaGenLoopColoringOps extends ScalaGenBase {
  val IR: LoopColoringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case e@ColorIndexSetNew(indices,start,end,mo) => 
        emitValDef(sym, "new generated.scala.IndexSetImpl(" + quote(indices) + "," + quote(indices) + ".length" + "," + quote(start) + "," + quote(end) +", 1)")
      case _ => super.emitNode(sym,rhs)
    }
  }
}


trait LoopColoringOpt extends GenericFatCodegen with SimplifyTransform {
  val IR: DeliteApplication with LoopsFatExp with LoopColoringOpsExp with MeshSetOpsExp
  import IR._
  import scala.collection.mutable.ArrayBuffer
  import scala.collection.immutable.List  
  import Stencil._
  
  val blockSize = 1

  var firstRun = true

  def colog(s: String) = printlog("[deliszt coloring log] " + s)
  def codbg(s: String) = printdbg("[deliszt coloring debug] " + s)
  
  // the issue here is that to transform the MeshSetForeach, we need more information than SimpleFatLoop keeps around.
  // by not turning into a SimpleFatLoop, we preclude the ability to fuse MeshSetForeachs. If fusing is important in Liszt,
  // we should figure out how to get both.
  
  override def fatten(e: Stm): Stm = e.rhs match {
    case Reflect(MeshSetForeach(_,_),_,_) => e //TTP(List(e.sym), List(e.rhs), ThinDef(e.rhs)) // don't turn MeshSetForeach into a SimpleFatLoop!
    case _ => super.fatten(e)
  }
 
  override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any],Any)] = {
    val x = fresh[A]
    val y = reifyEffects(f(x))

    val sA = mA.toString
    val sB = mB.toString

    printlog("-- emitSource")
    availableDefs.foreach(printlog(_))
    
    stream.println("{\"DEG\":{\n"+
                   "\"version\" : 0.1,\n"+
                   "\"kernelpath\" : \"" + Config.buildDir  + "\",\n"+
                   "\"targets\": [" + generators.map("\""+_+"\"").mkString(",")  + "],\n"+
                   "\"ops\": [")

    stream.println("{\"type\" : \"Arguments\" , \"kernelId\" : \"x0\"},")

    // first run coloring 
    /*val (newScope, newResult) =*/ val newResult = focusBlock(y) {
      // from emitBlockFocused -- TODO: clean up
      var currentScope = fattenAll(innerScope)
      val fatRes = y match {
        case Block(Combine(rs)) => rs 
        case Block(a) => List(a) 
      }
      val (newScope, newResult) = colorLoops(currentScope)(fatRes)
      codbg("<current scope after coloring ---"+"/"+newResult); newScope.foreach(e=>codbg(e.toString)); codbg("--->")          
      codbg("<inner scope after coloring ---"+"/"+newResult); innerScope.foreach(e=>codbg(e.toString)); codbg("--->")          
      //emitFatBlockFocused(newScope)(newResult)(stream)
      newResult
    }

    // then do scheduling (code motion)

    //emitFatBlockFocused(newScope)(newResult)(stream)
    //currentScope = newScope
    withStream(stream)(emitFatBlock(newResult.map(e=>reifyEffects(e))))
    //stream.println(quote(getBlockResult(y)))
    stream.println("{\"type\":\"EOP\"}\n]}}")

    stream.flush
    Nil
  }

  def colorLoops(currentScope0: List[Stm])(result0: List[Exp[Any]]): (List[Stm], List[Exp[Any]]) = {
    var result: List[Exp[Any]] = result0
    var currentScope = currentScope0
    
    if(Config.collectStencil) {
      // Get the map of for loops to Stencil
      val forMap = analysisResults("StencilCollectorStencils").asInstanceOf[ForMap]
      val msMap = analysisResults("StencilCollectorMeshsets").asInstanceOf[MeshSetMap]
         
      if(firstRun) {
        colog("Top level loops")
        
        for((id, stencil) <- forMap) {
          colog(id.toString)
        }
        
        firstRun = false
      }
      
      // Find loops at current top level that exist in the stencil
      var Wloops = //super.focusExactScopeFat(currentScope)(result) { levelScope =>   
        /*level*/currentScope collect { case e @ TP(sym, Reflect(MeshSetForeach(_,_),_,_)) if forMap.contains(sym.id) => e }
      //}

      codbg("========== COLORING SCOPE =============")
      currentScope.foreach(e=>codbg(e.toString))
       
      codbg("========== FOUND LOOPS: ===============")
      Wloops.foreach(e=>codbg(e.toString))

      
      // Color each loop!
      for(loop <- Wloops) { 
        assert(loop.lhs.length == 1)
        
        // Grab the MeshSet
        val id = (loop.lhs.collectFirst {
          case Sym(id) => id
        }).get
        
        val ms = msMap(id)
        
        val coloring = if(forMap.contains(id)) {
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
          interferenceBuilder.buildAndColor(ms, stencil)
        }
        else {
          val interferenceBuilder = new InterferenceBuilder(null, blockSize)
          interferenceBuilder.trivialColoring(ms)
        }
        
        if(coloring.numColors <= 1) {
          colog("Found one color for loop " + id)
        }
        else if(coloring.numColors > 1) {
          colog("Coloring loop " + id + " num colors: " + coloring.numColors)
          val (color_idx, color_values) = coloring.collect()
              
          colog("Loop id: " + id)                              
          if(forMap.contains(id)) {
            // Output coloring for debugging
            colog(" num elements: " + ms.size)
            colog(" num colors: " + coloring.numColors)
                    
            var i = 0
            while(i <= coloring.numColors) {
              colog("color_idx: " + i + " " + color_idx(i))
              i += 1
            }
            
            /* i = 0
            while(i < coloring.numColors) {
              var j = color_idx(i)
              while (j < color_idx(i+1)) {
                colog("color_values: " + i + " " + color_values(j))
                j += 1
              }
              i += 1
            } */
          }
          else {
            colog(" trivial coloring")
          }
     
          /* transform loop into multiple loops, one per color */
          
          // utils
          def WgetLoopParams(e: Stm): (Exp[MeshSet[MeshObj]], Exp[Int], Exp[Int], Exp[MeshObj] => Exp[Unit], Exp[Unit]) = e.rhs match {
            case Reflect(f@MeshSetForeach(in,func),_,_) => (in, f.size, f.v, func, getBlockResult(f.body.asInstanceOf[DeliteForeachElem[Unit]].func))
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
            innerScope = innerScope union addedDefs 
            currentScope = currentScope union fattenAll(addedDefs) 
            out
          }
          
          // build colored loops
          var i = 0        
          var colorLoops = List[Stm]()         
          var prevLoop: Option[Stm] = None
          var depChain = List[Sym[Any]]()
          //val loopRefTransformer = new SubstTransformer                      
          
          codbg("<loop " + loop.toString + " scope before coloring ---"+result0+"/"+result); currentScope.foreach(e=>codbg(e.toString)); codbg("--->")                  
                             
          while (i < coloring.numColors) {
            var j = color_idx(i)
            val indices = ArrayBuffer[Int]()
            while (j < color_idx(i+1)) {
              indices += color_values(j)
              j += 1
            }

            // new loop dependencies            
            val (in,size,v,f,body) = WgetLoopParams(loop)          
            val colorLoopVar = fresh(v.tp) 
            val colorSet = buildInScope(color_index_set_new(indices.toArray, 0, indices.length)(in.tp))
            val colorSize = buildInScope(color_lift(indices.length))
            val colorFunc = buildInScope(reifyEffects(f(dc_apply(colorSet,colorLoopVar))).res)
                        
            // transform loop
            val t = new SubstTransformer
            t.subst(v) = colorLoopVar
            t.subst(in) = colorSet 
            t.subst(size) = colorSize          
            t.subst(body) = colorFunc
            val (newScope, transformedLoopSyms) = transformAllFully(currentScope, loop.lhs, t) 
            val transformedLoop = newScope.find(_.lhs == transformedLoopSyms).get          
            assert(transformedLoop.lhs.size == 1)
            
            // add previous transformedLoop to new one as a dependency
            val transformedLoop2 = transformedLoop match { 
              case TP(lhs, Reflect(a,u,d)) if prevLoop.isDefined => 
                depChain :+= infix_lhs(prevLoop.get)(0) //infix doesn't kick in :(  //prevLoop.get.lhs(0)
                fatten(findOrCreateDefinition(Reflect(a,u,d ::: depChain),infix_lhs(prevLoop.get)(0).pos)(infix_lhs(prevLoop.get)(0).tp))
                //TTP(lhs, ThinDef(Reflect(a,u,d ::: prevLoop.get.lhs)))
              case _ => transformedLoop
            }          
            prevLoop = Some(transformedLoop2)
            colorLoops :+= transformedLoop2          
            i += 1
          }                
          colog("colored loop " + id + " into " + colorLoops.length + " loops using " + coloring.numColors + " colors")
          colog("original loop: " + loop.toString + ", type: " + infix_lhs(loop)(0).tp.toString)
          colorLoops foreach { l => colog("colored loop: " + l.toString + ", type: " + infix_lhs(l)(0).tp.toString) }
        
          // replace old loop with new loops
          currentScope = fission(currentScope, loop, colorLoops)  
          val thinLoop = loop match {
            case TP(lhs, r@Reflect(a,u,d)) => findDefinition(r).get
          }
          val thinColors = colorLoops map (e => e match {
            case TP(lhs, r@Reflect(a,u,d)) => findDefinition(r).get
          }) 
          innerScope = fission(innerScope, thinLoop, thinColors)

          val loopRefTransformer = new SubstTransformer

          // replace references to old loop (since we are replacing effectful loops of type Unit, should only be effectful dependencies...)
         
          // NOTE: we get (so far) harmless violated ordering of effects errors due to the fact
          // that we are transforming the currentScope below using transformAllFully, but simply
          // modifying effectful nodes directly here. if the schedule changes, the spliced in order
          // may be wrong.

          innerScope foreach {
            case TP(lhs, Reify(x,u,es)) if (es contains infix_lhs(loop)(0)) =>
              val cleanEs = fission(es,infix_lhs(loop)(0),colorLoops flatMap { _.lhs })
              val o = lhs
              val n = infix_lhs(findOrCreateDefinition(Reify(x,u,cleanEs),lhs.pos)(lhs.tp))(0)
              loopRefTransformer.subst(o) = n
            case TP(lhs, Reflect(x,u,es)) if (es contains infix_lhs(loop)(0)) =>
              val cleanEs = fission(es,infix_lhs(loop)(0),colorLoops flatMap { _.lhs })
              val o = lhs
              val n = infix_lhs(findOrCreateDefinition(Reflect(x,u,cleanEs),lhs.pos)(lhs.tp))(0)
              loopRefTransformer.subst(o) = n
            case _ =>
          }
          
          
          codbg("<loop A " + loop.toString + " scope after coloring ---"+result0+"/"+result); currentScope.foreach(e=>codbg(e.toString)); codbg("--->")          
          codbg(loopRefTransformer.subst.toString)

          // update the schedule
          transformAllFully(currentScope, result, loopRefTransformer) match { case (a,b) => // too bad we can't use pair assigment
            currentScope = a
            result = b
          }

          codbg("<loop B " + loop.toString + " scope after coloring ---"+result0+"/"+result); currentScope.foreach(e=>codbg(e.toString)); codbg("--->")          
          codbg(loopRefTransformer.subst.toString)
	}
      } // end loop foreach
      
    } // end if Config.collectStencil
        
    // do what super does ...
    // result0 does not depend on any of our newly introduced loops...    
    // super.focusExactScopeFat(currentScope)(result0)(body)
    //super.focusExactScopeFat(currentScope)(result)(body)  
    (currentScope, result)
  }
  
}
