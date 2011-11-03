package ppl.dsl.deliszt.analysis

import java.io.PrintWriter
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen

import ppl.delite.framework.{Config, DeliteApplication}

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.meshset.MeshSetOpsExp

import scala.collection.mutable.{Map => MMap}

trait LoopColoringOpsExp extends BaseFatExp with EffectExp { this: DeLisztExp =>
  case class ColorIndexSetNew(indices: Exp[Array[Int]], start: Exp[Int], end: Exp[Int], mo: Manifest[MeshSet[MeshObj]]) extends Def[MeshSet[MeshObj]]    
  
  private def baseManifest[A<:MeshObj:Manifest] = manifest[MeshSet[A]]
  
  def color_index_set_new(indices: Array[Int], start: Int, end: Int)(implicit mo: Manifest[MeshSet[MeshObj]]): Exp[MeshSet[MeshObj]] = {
    // this shouldn't need to be effectful, but this is a workaround for the issue that if this node is promoted to
    // an effect in a more specific scope, it gains the more specific Type manifest, which we don't want in this case.
    
    // we could also probably use remap to fix this, but I didn't want to disturb anything else.
    reflectEffect(ColorIndexSetNew(Array(indices: _*),unit(start),unit(end),mo))(baseManifest(mo.typeArguments(0).asInstanceOf[Manifest[MeshObj]]))
  }
  def color_lift(x: Int) = Const(x)
}

trait ScalaGenLoopColoringOps extends ScalaGenBase {
  val IR: LoopColoringOpsExp
  import IR._

  // clean this up when deliszt front-end does not depend on deliszt back-end
  // def meshObjConstructor(mo: Manifest[MeshSet[MeshObj]]) = {
  //   val tp = mo.typeArguments(0)
  //   val (cellM,edgeM,faceM,vertexM) = (manifest[Cell],manifest[Edge],manifest[Face],manifest[Vertex])
  // 
  //   tp match {
  //     case `cellM` => "generated.scala.MeshObjConstruct.CellConstruct"
  //     case `edgeM` => "generated.scala.MeshObjConstruct.EdgeConstruct"
  //     case `faceM` => "generated.scala.MeshObjConstruct.FaceConstruct"
  //     case `vertexM` => "generated.scala.MeshObjConstruct.VertexConstruct"
  //   }
  // }
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case e@ColorIndexSetNew(indices,start,end,mo) => 
        //val moc = meshObjConstructor(mo)
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

  def colog(s: String) = printlog("[deliszt coloring] " + s)
  def codbg(s: String) = printdbg("[deliszt coloring] " + s)
  
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
      val forMap = analysisResults("StencilCollectorStencils").asInstanceOf[ForMap]
      val msMap = analysisResults("StencilCollectorMeshsets").asInstanceOf[MeshSetMap]
         
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
        
        val (color_idx, color_values) = coloring.collect()
       
/*       
        print("Loop id: " + id)
        if(forMap.contains(id)) {
          // Output coloring for debugging
          println(" num elements: " + ms.size)
          println(" num colors: " + coloring.numColors)
                  
          var i = 0
          while(i <= coloring.numColors) {
            println("color_idx: " + i + " " + color_idx(i))
            i += 1
          }
          
          i = 0
          while(i < coloring.numColors) {
            var j = color_idx(i)
            while (j < color_idx(i+1)) {
              println("color_values: " + i + " " + color_values(j))
              j += 1
            }
            i += 1
          }
        }
        else {
          println(" trivial coloring")
        }
*/        
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
          innerScope = innerScope union addedDefs 
          currentScope = currentScope union fattenAll(addedDefs) 
          out
        }
        
        // build colored loops
        var i = 0        
        var colorLoops = List[TTP]()         
        var prevLoop: Option[TTP] = None
        val loopRefTransformer = new SubstTransformer                      
        
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
          val colorLoopVar = fresh(v.Type) 
          val colorSet = buildInScope(reifyEffects(color_index_set_new(indices.toArray, 0, indices.length)(in.Type)))
          val colorSize = buildInScope(color_lift(indices.length))
          val colorFunc = buildInScope(reifyEffects(f(dc_apply(colorSet,colorLoopVar))))
                      
          // transform loop
          val t = new SubstTransformer
          t.subst(v) = colorLoopVar
          t.subst(in) = colorSet 
          t.subst(size) = colorSize          
          t.subst(body) = colorFunc
          val (newScope, transformedLoopSyms) = transformAllFully(currentScope, loop.lhs, t) 
          val transformedLoop = newScope.find(_.lhs == transformedLoopSyms).get          
                    
          // add previous transformedLoop to new one as a dependency
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
        currentScope = fission(currentScope, loop, colorLoops) 
        
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
        
        codbg("<loop " + loop.toString + " scope after coloring ---"+result0+"/"+result); currentScope.foreach(e=>codbg(e.toString)); codbg("--->")          
        
        // update the schedule
        result = loopRefTransformer(result) 
        currentScope = getFatSchedule(currentScope)(result) // clean things up!  
        
      } // end loop foreach
              
    } // end if Config.collectStencil
        
    // do what super does ...
    // result0 does not depend on any of our newly introduced loops...    
    // super.focusExactScopeFat(currentScope)(result0)(body)
    super.focusExactScopeFat(currentScope)(result)(body)
  }
  
}
