package ppl.dsl.deliszt.analysis

import java.io.{PrintWriter}

import scala.collection.immutable.{Set => ISet}
import scala.collection.mutable.{Set => MSet, Map => MMap, HashMap, ArrayBuilder}

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.analysis.TraversalAnalysis

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt._

import ppl.delite.framework.datastruct.scala._

class MultipleMeshSetImpl[MO<:MeshObj:MeshObjConstruct](objs: IndexedSeq[Int]) extends MeshSet[MO] {
  def apply(i : Int) = {
    MeshObjImpl[MO](objs(i))
  }
  
  override val size = objs.size
}

class MultipleMeshSetBuilder {
  val builder = ArrayBuilder.make[Int]()
  val members = MSet[Int]()
  
  def addSet[MO<:MeshObj](ms: MeshSet[MO]) {
    for(mo <- ms) {
      if(!members.contains(mo.internalId)) {
        builder += mo.internalId
      }
    }
  }
  
  def +=[MO<:MeshObj](ms: MeshSet[MO]) = addSet(ms)
  
  def result[MO<:MeshObj:MeshObjConstruct] = new MultipleMeshSetImpl(builder.result)
}

trait MultipleMeshObj[MO<:MeshObj] {
  def +(b: MO) : MultipleMeshObj[MO] = new MultipleMeshObjImpl(objs + b)
  def ++(b: MultipleMeshObj[MO]) : MultipleMeshObj[MO] = new MultipleMeshObjImpl(objs ++ b.objs)
  val objs : Set[MO]
}

class MultipleMeshObjImpl[MO<:MeshObj](val objs : Set[MO] = Set[MO]()) extends MultipleMeshObj[MO] {
}

case class NoObjs[MO<:MeshObj]() extends MultipleMeshObj[MO] {
  val objs = Set[MO]()
  
  override def ++(b: MultipleMeshObj[MO]) = b
}

case class OneObj[MO<:MeshObj](mo: MO) extends MultipleMeshObj[MO] {
  val objs = Set[MO](mo)
}

case class MultipleMeshObjSet[MO<:MeshObj](val ms : MeshSet[MO]) extends MultipleMeshObj[MO] {
  val objs = ms.toSet
} 

case class FieldAccess(field: Int, mo: MeshObj)

class ReadWriteSet {
  val read = MSet[FieldAccess]()
  val write = MSet[FieldAccess]()
}

object StencilCollector {
  type StencilMap = MMap[MeshObj,ReadWriteSet]
}

trait DeLisztCodeGenAnalysis extends TraversalAnalysis {
  val IR: DeliteApplication with DeLisztExp
  import IR._
  
  object MultipleMeshSet {
    def apply[A<:MeshObj:Manifest,B<:MeshObj:Manifest:MeshObjConstruct](in: MultipleMeshObj[A], f: A => MeshSet[B]) : MeshSet[B] = {
      val builder = new MultipleMeshSetBuilder()
      
      for(mo <- in.objs) {
        builder += f(mo)
      }
      
      builder.result[B]
    }
  
    def apply[A<:MeshObj:Manifest,B<:MeshObj:Manifest:MeshObjConstruct](e: Exp[A], f: A => MeshSet[B]) : MeshSet[B] = {
      apply(moValue[A](e), f)
    }
  }
  
  object MultipleMeshObj {
    def apply[MO<:MeshObj:Manifest](mo: MO) = new MultipleMeshObjImpl(ISet(mo))
    def apply[MO<:MeshObj:Manifest](ms: MeshSet[MO]) = new MultipleMeshObjImpl(ms.toSet)
    
    def apply(e: Any) = {
      e match {
        case mo: MeshObj => OneObj(mo)
        case _ => e
      }
    }

    def multi[A<:MeshObj:Manifest,B<:MeshObj:Manifest](in: MultipleMeshObj[A], f: A => MultipleMeshObj[B]) : MultipleMeshObj[B] = {
      var out: MultipleMeshObj[B] = NoObjs[B]()
      
      for(mo <- in.objs) {
        out = out ++ f(mo)
      }
      
      out
    }
    
    def multi[A<:MeshObj:Manifest,B<:MeshObj:Manifest](e: Exp[_], f: A => MultipleMeshObj[B]) : MultipleMeshObj[B] = {
      multi(moValue[A](e), f)
    }
    
    def multi[A<:MeshObj:Manifest,B<:MeshObj:Manifest](e: A, f: A => MultipleMeshObj[B]) : MultipleMeshObj[B] = {
      multi(OneObj(e), f)
    }
    
    def apply[A<:MeshObj:Manifest,B<:MeshObj:Manifest](in: MultipleMeshObj[A], f: A => B) : MultipleMeshObj[B] = {
      var out: MultipleMeshObj[B] = NoObjs[B]()
      
      for(mo <- in.objs) {
        out = out + f(mo)
      }
      
      out
    }
    
    def apply[A<:MeshObj:Manifest,B<:MeshObj:Manifest](e: Exp[_], f: A => B) : MultipleMeshObj[B] = {
      apply(moValue[A](e), f)
    }
    
    def apply[A<:MeshObj:Manifest,B<:MeshObj:Manifest](e: A, f: A => B) : MultipleMeshObj[B] = {
      apply(OneObj(e), f)
    }
  }
  
  import StencilCollector.StencilMap

  val forMap = new HashMap[Int,StencilMap]()
  val msMap = MMap[Int,MeshSet[_]]()
  
  // Store the current top level for loop
  var currentFor : Option[Int] = None
  
  // And the current mesh object in the top level for loop
  var currentMo : Option[MeshObj] = None
  
  val className = "StencilCollector"
  val on = true
  
  override def initializeGenerator(buildDir:String, args: Array[String], _analysisResults: MMap[String,Any]): Unit = {
    super.initializeGenerator(buildDir, args, _analysisResults)
    
    analysisResults("StencilCollectorStencils") = forMap
    analysisResults("StencilCollectorMeshsets") = msMap
  
    MeshLoader.init(if(args.length > 0) args(0) else "liszt.cfg")
  }
  
  // Mark accesses
  def markRead[MO<:MeshObj,VT](f: Exp[Field[MO,VT]], i: Exp[MeshObj]) {
    val sym = f.asInstanceOf[Sym[Field[MO,VT]]]
    val mos = value[MultipleMeshObj[MeshObj]](i)
    
    currentFor match {
	    case Some(x) => {
	      for(mo <- mos.objs) {
  	      forMap(x)(currentMo.get).read += FieldAccess(sym.id, mo)
  	    }
	    }
	    case None => System.out.println("No top level for")
    }
  }
  
  def markWrite[MO<:MeshObj,VT](f: Exp[Field[MO,VT]], i: Exp[MeshObj]) {
    val sym = f.asInstanceOf[Sym[Field[MO,VT]]]
    val mos = value[MultipleMeshObj[MeshObj]](i)
    
    currentFor match {
	    case Some(x) => {
	      for(mo <- mos.objs) {
          mo match {
            case Cell(0) => // Ignore
            case _ => forMap(x)(currentMo.get).write += FieldAccess(sym.id, mo)
          }
	      }
	    }
	    case None => System.out.println("No top level for")
    }    
  }
  
  def matchFor(i: Int) = {
    currentFor match {
      case Some(x) => i == x
      case _ => false
    }
  }
  
  def setFor(i: Int, ms: MeshSet[_]) {
    currentFor = Some(i)
    forMap(i) = new HashMap[MeshObj,ReadWriteSet]() { override def default(key: MeshObj) = { val rwset = new ReadWriteSet(); this(key) = rwset; rwset }  }
    
    msMap(i) = ms
  }
  
  val values = MMap[Int,Any]()
  
  def store(sym: Sym[_], x: Any) {
    values(sym.id) = x
  }
    
  def combine(a: Any, b: Any) = {
    (a, b) match {
      case (x: MultipleMeshObj[MeshObj], y: MultipleMeshObj[MeshObj]) => x ++ y
      case (x: MultipleMeshObj[MeshObj], _) => x
      case (_, y: MultipleMeshObj[MeshObj]) => y
      
      // Just pick one, doesn't really matter
      case _ => a
    }
  }
  
  def combine(a: Exp[Any], b: Exp[Any]) : Any = {
    val resultA = value(a)
    val resultB = value(b)
    
    // Combine the two! Only care really if it is a mesh element though
    combine(resultA,resultB)
  }
  
  def rawValue(x: Exp[Any]) : Option[Any] = x match {
    case Const(null) => None
    case Const(s: String) => Some(s)
    case null => None
    case Const(f: Float) => Some(f)
    case Const(z) => Some(z)
    case Sym(n) => values.get(n)
    // Doesn't exist anymore?
    // case External(s, args) => null
    case _ => { throw new RuntimeException("Could not get value"); None }
  }
  
  def value[T:Manifest](x: Exp[Any]) : T = {
    (rawValue(x) match {
    case Some(o) => o
    case None => null
    case _ => null
  }).asInstanceOf[T]}
  
  def value[T:Manifest](x: Def[Any]) : T = (maybeValue(x) match {
    case Some(o) => o
    case None => null
    case _ => null
  }).asInstanceOf[T]
  
  def moValue[MO<:MeshObj:Manifest](x: Exp[Any]) = value[MultipleMeshObj[MO]](x)
  
  def maybeValue(x: Def[Any])(implicit stream: PrintWriter) : Option[Any] = {
    val o = x match {
      // or could also lookup the exp
      case b@DeLisztBoundarySet(name) => Mesh.boundarySet(value[String](name))(b.moc)

      case DeLisztMesh() => OneObj(Mesh.mesh)

      case DeLisztVerticesCell(e) => MultipleMeshSet(e, (mo:Cell) => Mesh.vertices(mo))
      case DeLisztVerticesEdge(e) => MultipleMeshSet(e, (mo:Edge) => Mesh.vertices(mo))
      case DeLisztVerticesFace(e) => MultipleMeshSet(e, (mo:Face) => Mesh.vertices(mo))
      case DeLisztVerticesVertex(e) => MultipleMeshSet(e, (mo:Vertex) => Mesh.vertices(mo))
      case DeLisztVerticesMesh(e) => MultipleMeshSet(e, (mo:Mesh) => Mesh.vertices(mo))

      case DeLisztVertex(e, i) => MultipleMeshObj(e, (mo: Cell) => Mesh.vertex(mo, value(i)))

      case DeLisztFaceVerticesCCW(e) => MultipleMeshSet(e, (mo:Face) => Mesh.verticesCCW(mo))
      case DeLisztFaceVerticesCW(e) => MultipleMeshSet(e, (mo:Face) => Mesh.verticesCW(mo))

      case DeLisztCellsCell(e) => MultipleMeshSet(e, (mo:Cell) => Mesh.cells(mo))
      case DeLisztCellsEdge(e) => MultipleMeshSet(e, (mo:Edge) => Mesh.cells(mo))
      case DeLisztCellsFace(e) => MultipleMeshSet(e, (mo:Face) => Mesh.cells(mo))
      case DeLisztCellsVertex(e) => MultipleMeshSet(e, (mo:Vertex) => Mesh.cells(mo))
      case DeLisztCellsMesh(e) => MultipleMeshSet(e, (mo:Mesh) => Mesh.cells(mo))

      case DeLisztEdgeCellsCCW(e) => MultipleMeshSet(e, (mo:Edge) => Mesh.cellsCCW(mo))
      case DeLisztEdgeCellsCW(e) => MultipleMeshSet(e, (mo:Edge) => Mesh.cellsCW(mo))

      case DeLisztEdgesCell(e) => MultipleMeshSet(e, (mo:Cell) => Mesh.edges(mo))
      case DeLisztEdgesFace(e) => MultipleMeshSet(e, (mo:Face) => Mesh.edges(mo))
      case DeLisztEdgesVertex(e) => MultipleMeshSet(e, (mo:Vertex) => Mesh.edges(mo))
      case DeLisztEdgesMesh(e) => MultipleMeshSet(e, (mo:Mesh) => Mesh.edges(mo))

      case DeLisztFacesCell(e) => MultipleMeshSet(e, (mo:Cell) => Mesh.faces(mo))
      case DeLisztFacesEdge(e) => MultipleMeshSet(e, (mo:Edge) => Mesh.faces(mo))
      case DeLisztFacesVertex(e) => MultipleMeshSet(e, (mo:Vertex) => Mesh.faces(mo))
      case DeLisztFacesMesh(e) => MultipleMeshSet(e, (mo:Mesh) => Mesh.faces(mo))

      case DeLisztEdgeFacesCCW(e) => MultipleMeshSet(e, (mo:Edge) => Mesh.facesCCW(mo))
      case DeLisztEdgeFacesCW(e) => MultipleMeshSet(e, (mo:Edge) => Mesh.facesCW(mo))
  
      case DeLisztFaceEdgesCCW(e) => MultipleMeshSet(e, (mo:Face) => Mesh.edgesCCW(mo))
      case DeLisztFaceEdgesCW(e) => MultipleMeshSet(e, (mo:Face) => Mesh.edgesCW(mo))

      case DeLisztEdgeHead(e) => MultipleMeshObj[Edge,Vertex](e, (mo:Edge) => Mesh.head(mo))
      case DeLisztEdgeTail(e) => MultipleMeshObj[Edge,Vertex](e, (mo:Edge) => Mesh.tail(mo))

      case DeLisztFaceInside(e) => MultipleMeshObj[Face,Cell](e, (mo:Face) => Mesh.inside(mo))
      case DeLisztFaceOutside(e) => MultipleMeshObj[Face,Cell](e, (mo:Face) => Mesh.outside(mo))
  
      case DeLisztFace(e, i) => MultipleMeshObj[Edge,Face](e, (mo: Edge) => Mesh.face(mo, value(i)))

      case DeLisztFlipEdge(e) => MultipleMeshObj[Edge,Edge](e, (mo:Edge) => Mesh.flip(mo))
      case DeLisztFlipFace(e) => MultipleMeshObj[Face,Face](e, (mo:Face) => Mesh.flip(mo))

      case DeLisztTowardsEdgeVertex(e, v) => MultipleMeshObj.multi(e, (e1: Edge) => MultipleMeshObj(v, (e2: Vertex) => Mesh.towards(e1, e2)))
      case DeLisztTowardsFaceCell(e, c) => MultipleMeshObj.multi(e, (e1: Face) => MultipleMeshObj(c, (e2: Cell) => Mesh.towards(e1, e2)))
      
      case DeliteCollectionApply(e, i) => {
        val obj = (rawValue(e), rawValue(i)) match {
          case (Some(c), Some(idx)) => (c.asInstanceOf[DeliteCollection[Any]]).dcApply(idx.asInstanceOf[Int])
          case _ => None
        }
        
        MultipleMeshObj(obj)
      }
      
      // While loops. Execute once. Store results if results are a mesh element
      case While(c,b) => {
        blockValue(b)
      }
      
      case DeliteWhile(c,b) => {
        blockValue(b)
      }
      
      // Execute both branches. Store results if results are a mesh element
      case IfThenElse(c,a,b) => {        
        val resultA = blockValue[Any](a)
        val resultB = blockValue[Any](b)
        
        // Combine the two! Only care really if it is a mesh element though
        combine(resultA,resultB)
      }
      
      case DeliteIfThenElse(c,a,b,f) => {        
        val resultA = blockValue[Any](a)
        val resultB = blockValue[Any](b)
                
        // Combine the two! Only care really if it is a mesh element though
        combine(resultA,resultB)
      }

      case _ => None
    }
    
    o match {
      case None => None
      case _ => Some(o)
    }
  }
  
  def blockValue[T:Manifest](b: Exp[Any]) = value[T](getBlockResult(b))
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    // System.out.println("EMITTING NODE")
    // System.out.println(rhs)
  
    if(Config.collectStencil) {
      rhs match {
        // Foreach, only apply to top level foreach though...
        case f@MeshSetForeach(m, b) => {
          // Get value for the mesh set
          val ms = value[MeshSet[_]](m)
      
          // if not current top, mark current top....
          if(currentFor.isEmpty) {
            setFor(sym.id, ms)
            System.out.println("Found a top level foreach sym " + sym.id)
          }
            
          val mssize: Int = ms.size
          val stuff: Int = 0
          
          var i: Int = 0
          // Run foreach over mesh set
          for(mo <- ms) {
            // If top level foreach, mark the current element as one we are collecting stencil for
            if(matchFor(sym.id)) {
              currentMo = Some(mo)
            }
          
            // Store loop index in loop index symbol
            store(f.i, i)
            store(f.v, i)
            
            // Re "emit" block
            f.body match {
              case DeliteForeachElem(func, sync) => emitBlock(func)
            }
            
            i += 1
          }
          
          // Clear out the current for loop if we are the top
          if(matchFor(sym.id)) {
            currentFor = None
          }
        }
        
        // While loops. Execute once. Store results if results are a mesh element
        case DeliteWhile(c,b) => {
          emitBlock(c)
          
          emitBlock(b)
        }
        
        // While loops. Execute once. Store results if results are a mesh element
        case While(c,b) => {
          emitBlock(c)
          
          emitBlock(b)
        }
        
        // Execute both branches. Store results if results are a mesh element
        case IfThenElse(c,a,b) => {
          emitBlock(c)
          
          emitBlock(a)
          emitBlock(b)
        }
        
              
        // Execute both branches. Store results if results are a mesh element
        case DeliteIfThenElse(c,a,b,h) => {
          emitBlock(c)
          
          emitBlock(a)
          emitBlock(b)
        }
          
        // Mark 
        case FieldApply(f,i) => {
          // Mark a read on the field for the current element... for i
          markRead(f, i)
        }
        
        case FieldUpdate(f,i,v) => {
          // Mark a write on the field for the current element... for i
          markWrite(f, i)
        }
          
        case FieldPlusUpdate(f,i,v) => {
          // Mark a write on the field for the current element... for i
          markWrite(f, i)
        }
        
        case FieldTimesUpdate(f,i,v) => {
          // Mark a write on the field for the current element... for i
          markWrite(f, i)
        }
          
        case FieldMinusUpdate(f,i,v) => {
          // Mark a write on the field for the current element... for i
          markWrite(f, i)
        }
        
        case FieldDivideUpdate(f,i,v) => {
          // Mark a write on the field for the current element... for i
          markWrite(f, i)
        }
        
        case _ => None
      }
      
      maybeValue(rhs) match {
        case Some(o) => {
          store(sym, o)
        }
        case None => {}
      }
    }
  }
}
