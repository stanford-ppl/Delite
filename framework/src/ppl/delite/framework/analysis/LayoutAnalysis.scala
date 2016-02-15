/*package ppl.delite.framework.analysis

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.reflect.SourceContext

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

import ppl.delite.framework.Util._

// Quick approximation of how layout analysis will work
// Currently just sets all MultiArrays to be flat

// TODO: Not sure about this abstraction yet, except for type signature [T,R]
// Seq[Seq[Int]] intended to represent various dimension layering and order... potentially could work
trait LayoutMetadata extends RankMetadata {
  sealed abstract class MASubtype { def toString: String }
  case object Plain extends MASubtype { override def toString = "Plain" }
  case object View extends MASubtype { override def toString = "View" }
  case object Buffer extends MASubtype { override def toString = "Buffer" }
  case object BufferView extends MASubtype { override def toString = "BufferView" }
  object MASubtype {
    def apply(view: Boolean, buff: Boolean) = (view, buff) match {
      case (false,false) => Plain
      case (false,true) => Buffer
      case (true,false) => View
      case (true,true) => BufferView
    }
  }

  sealed abstract class Layout[T:Manifest,R:Manifest](val subtype: MASubtype) extends Metadata {
    override def key = classOf[Layout[_,_]]
    val dims: Seq[Seq[Int]]
    val mT = manifest[T]
    val mR = manifest[R]
  }

  // Assume row major for now - can potentially make other special case classes later
  case class FlatLayout[T:Manifest](rank: Int, override val subtype: MASubtype) extends Layout[T,T](subtype) { self =>
    val dims = Seq(Seq.tabulate(rank){i=>i+1})
    override def makeString(prefix: String) = s"${rank}D, Flat, $subtype"

    def _matches(that: self.type) = this.rank == that.rank && this.subtype == that.subtype
    def _incompatibilities(that: self.type)(implicit t: MeetFunc) = {
      (if (this.rank != that.rank) List(s"Differing ranks (${this.rank} != ${that.rank})") else Nil) ++
      (if (this.subtype != that.subtype) List(s"Differing array subtypes (${this.subtype} != ${that.subtype})") else Nil)
    }
    def _meet(that: self.type)(implicit t: MeetFunc) = that
  }

  /*case class SinglyNested[T:Manifest](rank: Int, inner: Int, override val data: MADataType) extends Layout[T,DeliteArray[T]](data) {
    val dims = Seq(Seq.tabulate(inner-1){i=>i+1},Seq.tabulate(rank-inner+1){i=>i+inner})
  }*/
}

trait LayoutMetadataOps extends RankMetadataOps with LayoutMetadata {
  def getLayout(p: SymbolProperties) = p[Layout[_,_]]   //p("layout").map{_.asInstanceOf[Layout[_,_]]}
  def getLayout(e: Exp[Any]) = meta[Layout[_,_]](e)     //getMetadata(e, "layout").map{_.asInstanceOf[Layout[_,_]]}
  def layout(p: SymbolProperties) = getLayout(p).get
  def layout(e: Exp[Any]) = getLayout(e).get

  override def rank(e: Exp[Any]): Int = getLayout(e) match {
    case Some(FlatLayout(r, _)) => r
    case _ => super.rank(e)
  }
  override def rank(p: SymbolProperties): Int = getLayout(p) match {
    case Some(FlatLayout(r, _)) => r
    case _ => super.rank(p)
  }
}

// TODO: Not a full analysis stage right now - just fills in layouts as flat
trait LayoutAnalyzer extends Traversal {
  val IR: DeliteOpsExp with DeliteMultiArrayOpsExp with LayoutMetadataOps
  import IR._
  override val name = "Layout Analyzer"

  // Recursively sets layouts of all arrays to be flat
  // TODO: Currently all layouts are of type "Nothing"..
  def setLayout(p: Option[SymbolProperties])(implicit ctx: SourceContext): Option[SymbolProperties] = p match {
    case Some(a: ArrayProperties) =>
      val subtype = MASubtype(isPhysView(a), isPhysBuffer(a))
      Some(ArrayProperties(setLayout(a.child), PropMap(classOf[Layout[_,_]], FlatLayout(rank(a),subtype) )))
    case Some(s: StructProperties) =>
      Some(StructProperties(s.children.map{child => setLayout(Some(child)).get}, s.data))
    case _ => p
  }

  // Super simple analysis - make everything flat, row-major
  override def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    for ((e,m) <- metadata) {
      implicit val ctx: SourceContext = mpos(e.pos)

      if (hasMultiArrayType(e.tp)) {
        val origProps = getProps(e)
        val newProps = setLayout(origProps)
        setProps(e, meet(MetaOverwrite, origProps, newProps))
      }
    }
    (s)
  }
}*/

