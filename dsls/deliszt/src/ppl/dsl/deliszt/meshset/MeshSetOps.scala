package ppl.dsl.deliszt.meshset

import java.io.PrintWriter
import ppl.dsl.deliszt._

import reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

import ppl.dsl.deliszt.datastruct.scala.Mesh

trait MeshSetOps extends Variables {
  this: DeLiszt =>

  implicit def repMeshSetToMeshSetOps[MO <: MeshObj:Manifest](x: Rep[MeshSet[MO]]) = new meshSetOpsCls[MO](x)
  implicit def varToMeshSetOps[MO <: MeshObj:Manifest](x: Var[MeshSet[MO]]) = new meshSetOpsCls[MO](readVar(x))

  /**
   * This class defines the public interface for the Field[T] class.
   */
  class meshSetOpsCls[MO<:MeshObj:Manifest](x: Rep[MeshSet[MO]]) {
    def foreach(block: Rep[MO] => Rep[Unit]) = meshset_foreach(x, block)
  }

  def meshset_foreach[MO<:MeshObj:Manifest](x: Rep[MeshSet[MO]], block: Rep[MO] => Rep[Unit]) : Rep[Unit]
}

trait MeshSetOpsExp extends MeshSetOps with VariablesExp with BaseFatExp {
  this: DeLisztExp =>

  ////////////////////////////////
  // implemented via delite ops

  case class MeshSetForeach[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], func: Exp[MO] => Exp[Unit])
    extends DeliteOpForeach[MO] {

    def sync = n => List()
    val in = copyTransformedOrElse(_.in)(x)
    val size = copyTransformedOrElse(_.size)(x.size)
  }
  
  // e comes in as an internal id of a face
  case class NestedMeshSetForeach[MO<:MeshObj:Manifest](in: Exp[MeshSet[MO]], crs: Exp[CRS], e: Exp[Int], block: Exp[MO] => Exp[Unit]) extends Def[Unit] {
    def mm = manifest[MO]
    val mo = fresh[MO]
    
    val body = reifyEffects(block(mo))
  }
  
  // e comes in as an internal id of a face
  case class DirectedNestedMeshSetForeach[E<:MeshObj:Manifest,MO<:MeshObj:Manifest](in: Exp[MeshSet[MO]], crs: Exp[CRS], dir: Int, e: Exp[E], block: Exp[MO] => Exp[Unit]) extends Def[Unit] {
    def mm = manifest[MO]
    val mo = fresh[MO]
    
    val eid = ID(e)
    val body = reifyEffects(block(mo))
  }
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    // DOES NOT USE IN, only the index
    case f@NestedMeshSetForeach(m, crs, e, fn) => f.mo :: syms(crs):::syms(e):::syms(f.body)
    case f@DirectedNestedMeshSetForeach(m, crs, dir, e, fn) => f.mo :: syms(crs):::syms(e):::syms(f.eid):::syms(f.body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case f@NestedMeshSetForeach(m, crs, e, fn) => f.mo :: effectSyms(crs):::effectSyms(e):::effectSyms(f.body)
    case f@DirectedNestedMeshSetForeach(m, crs, dir, e, fn) => f.mo :: effectSyms(crs):::effectSyms(e):::effectSyms(f.eid):::effectSyms(f.body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case f@NestedMeshSetForeach(m, crs, e, fn) => (f.mo,1.0) :: freqNormal(crs) ++ freqNormal(e) ++ freqHot(f.body)
    case f@DirectedNestedMeshSetForeach(m, crs, dir, e, fn) => (f.mo,1.0) :: freqNormal(crs) ++ freqNormal(e) ++ freqNormal(f.eid) ++ freqHot(f.body)
    case _ => super.symsFreq(e)
  }
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    // Doesn't work wtf
    // case f@NestedMeshSetForeach(crs,e,func) => reflectPure(NestedMeshSetForeach(f(crs),f(e),f(func))(f.m))(mtype(manifest[A]))
    case Reflect(e@NestedMeshSetForeach(m,crs,i,func), u, es) => reflectMirrored(Reflect(NestedMeshSetForeach(f(m),f(crs),f(i),f(func)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MeshSetForeach(a,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MeshSetForeach(f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
  
  /////////////////////
  // object interface

  /////////////////////
  // class interface
  
  def meshset_foreach[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], block: Exp[MO] => Exp[Unit]) : Exp[Unit] = {
    val t = MeshSetForeach(x,block)
  
    x match {
      // Why do I have to cast? I hate you
      /*
      case Def(DeLisztCellsCell(e,m)) => nms_foreach(x, crs_ctoc(m), e, block)
      case Def(DeLisztCellsEdge(e,m)) => nms_foreach(x, crs_etoc(m), e, block)
      case Def(DeLisztCellsFace(e,m)) => nms_foreach(x, crs_ftoc(m), e, block)
      case Def(DeLisztCellsVertex(e,m)) => nms_foreach(x, crs_vtoc(m), e, block)
      case Def(DeLisztVerticesCell(e,m)) => nms_foreach(x, crs_ctov(m), e, block)
      case Def(DeLisztVerticesEdge(e,m)) => nms_foreach(x, crs_etov(m), e, block)
      case Def(DeLisztVerticesFace(e,m)) => nms_foreach(x, crs_ftov(m), e, block)
      case Def(DeLisztVerticesVertex(e,m)) => nms_foreach(x, crs_vtov(m), e, block)
      case Def(DeLisztEdgesCell(e,m)) => nms_foreach(x, crs_ctoe(m), e, block)
      case Def(DeLisztEdgesFace(e,m)) => nms_foreach(x, crs_ftoe(m), e, block)
      case Def(DeLisztEdgesVertex(e,m)) => nms_foreach(x, crs_vtoe(m), e, block)
      case Def(DeLisztFacesEdge(e,m)) => nms_foreach(x, crs_etof(m), e, block)
      case Def(DeLisztFacesCell(e,m)) => nms_foreach(x, crs_ctof(m), e, block)
      case Def(DeLisztFacesVertex(e,m)) => nms_foreach(x, crs_vtof(m), e, block)
      case Def(DeLisztEdgeFacesCCW(e,m)) => dnms_foreach(x, crs_etof(m), Mesh.HEAD, e, block)
      case Def(DeLisztEdgeFacesCW(e,m)) => dnms_foreach(x, crs_etof(m), Mesh.TAIL, e, block)
      case Def(DeLisztFaceEdgesCCW(e,m)) => dnms_foreach(x, crs_ftoe(m), Mesh.OUTSIDE, e, block)
      case Def(DeLisztFaceEdgesCW(e,m)) => dnms_foreach(x, crs_ftoe(m), Mesh.INSIDE, e, block)
      case Def(DeLisztEdgeCellsCCW(e,m)) => dnms_foreach(x, crs_etof(m), Mesh.HEAD, e, block)
      case Def(DeLisztEdgeCellsCW(e,m)) => dnms_foreach(x, crs_etof(m), Mesh.TAIL, e, block)
      case Def(DeLisztFaceVerticesCCW(e,m)) => dnms_foreach(x, crs_ftov(m), Mesh.OUTSIDE, e, block)
      case Def(DeLisztFaceVerticesCW(e,m)) => dnms_foreach(x, crs_ftov(m), Mesh.INSIDE, e, block)
      */
      case _ => reflectEffect(t, summarizeEffects(t.body.asInstanceOf[DeliteForeachElem[MO]].func).star)
    }
  }
  
  def nms_foreach[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], crs: Exp[CRS], e: Exp[Int], block: Exp[MO] => Exp[Unit]) : Exp[Unit] = {
    val t = NestedMeshSetForeach(x, crs, e, block)
    
    reflectEffect(t, summarizeEffects(t.body).star)
  }
  
  def dnms_foreach[E<:MeshObj:Manifest,MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], crs: Exp[CRS], dir: Int, e: Exp[E], block: Exp[MO] => Exp[Unit]) : Exp[Unit] = {
    val t = DirectedNestedMeshSetForeach(x, crs, dir, e, block)
    
    reflectEffect(t, summarizeEffects(t.body).star)
  }
}

trait MeshSetOpsExpOpt extends MeshSetOpsExp {
  this: DeLisztExp =>
}

trait ScalaGenMeshSetOps extends ScalaGenFat {
  val IR: MeshSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case f@NestedMeshSetForeach(m,crs,i,body) => {
        stream.println("val " + quote(sym) + " = { // Begin nested foreach " + sym.id)
          stream.println("var i = " + quote(crs) + ".row(" + quote(i) + ")")
          stream.println("val end = " + quote(crs) + ".row(" + quote(i) + "+1)")
          stream.println("while (i < end) {")
            stream.println("val " + quote(f.mo) + " = " + quote(crs) + ".values(i)") 
            emitBlock(f.body)
            stream.println("i += 1")
            stream.print(quote(getBlockResult(f.body)))
        stream.println("}} // End nested foreach " + sym.id)
      }
      
      case f@DirectedNestedMeshSetForeach(m,crs,dir,i,body) => {
        stream.println("val " + quote(sym) + " = { // Begin directed foreach " + sym.id)
          stream.println("if((" + quote(i) + " >>> generated.scala.Mesh.SHIFT) == " + dir + ") {")
            stream.println("var i = " + quote(crs) + ".row(" + quote(f.eid) + ")")
            stream.println("val end = " + quote(crs) + ".row(" + quote(f.eid) + "+1)")
            stream.println("while (i < end) {")
              stream.println("val " + quote(f.mo) + " = " + quote(crs) + ".values(i)") 
              emitBlock(f.body)
              stream.println("i += 1")
              stream.print(quote(getBlockResult(f.body)))
          stream.println("}} else {")
            stream.println("var i = " + quote(crs) + ".row(" + quote(f.eid) + "+1)-1")
            stream.println("val end = " + quote(crs) + ".row(" + quote(f.eid) + ")-1")
            stream.println("while (i > end) {")
              stream.println("val " + quote(f.mo) + " = " + quote(crs) + ".values(i) ^ generated.scala.Mesh.DMASK") 
              emitBlock(f.body)
              stream.println("i -= 1")
              stream.print(quote(getBlockResult(f.body)))
        stream.println("}}} // End directed foreach " + sym.id)
      }
    
      // these are the ops that call through to the underlying real data structure
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenMeshSetOps extends CudaGenFat {
  val IR: MeshSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMeshSetOps extends CGenFat {
  val IR: MeshSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
