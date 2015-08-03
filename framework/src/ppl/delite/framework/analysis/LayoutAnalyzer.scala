package ppl.delite.framework.analysis

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.internal.Meetable._
import scala.reflect.SourceContext

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

import ppl.delite.framework.Util._

// Quick approximation of how layout analysis will work
// Currently just sets all MultiArrays to be flat

// TODO: Not sure about this abstraction yet, except for type signature [T,R]
// Seq[Seq[Int]] intended to represent various dimension layering and order... potentially could work
trait LayoutMetadata extends RankMetadata {
  sealed abstract class MADataType { def toString: String }
  case object Plain extends MADataType { override def toString = "Plain" }
  case object View extends MADataType { override def toString = "View" }
  case object Buffer extends MADataType { override def toString = "Buffer" }
  case object BufferView extends MADataType { override def toString = "BufferView" }
  object MADataType {
    def apply(view: Boolean, buff: Boolean) = (view, buff) match {
      case (false,false) => Plain
      case (false,true) => Buffer
      case (true,false) => View
      case (true,true) => BufferView
    }
  }

  sealed abstract class Layout[T:Manifest,R:Manifest](val dtype: MADataType) extends Metadata {
    val name = "layout"
    val dims: Seq[Seq[Int]]
    val mT = manifest[T]
    val mR = manifest[R]
  }

  // Assume in order for now - can potentially make other special case classes later
  case class FlatLayout[T:Manifest](rank: Int, override val dtype: MADataType) extends Layout[T,T](dtype) { 
    val dims = Seq(Seq.tabulate(rank){i=>i+1})
    override def makeString(prefix: String) = rank + "D, Flat, " + dtype
  }

  /*case class SinglyNested[T:Manifest](rank: Int, inner: Int, override val data: MADataType) extends Layout[T,DeliteArray[T]](data) { 
    val dims = Seq(Seq.tabulate(inner-1){i=>i+1},Seq.tabulate(rank-inner+1){i=>i+inner}) 
  }*/

  // TODO: Fill more of this in with more layouts / more advanced analysis
  override def metadataMatches(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case (a: FlatLayout[_], b: FlatLayout[_]) => 
      a.dims.zip(b.dims).map{d => d._1 == d._2}.reduce{_&&_}
    case _ => super.metadataMatches(a,b)
  }
  override def canMeetMetadata(a: Metadata, b: Metadata, t: MeetFunc): Boolean = (a,b,t) match {
    case (a: Layout[_,_], b: Layout[_,_], _) => true
    case _ => super.canMeetMetadata(a,b,t)
  }
  override def meetMetadata(a: Metadata, b: Metadata, t: MeetFunc): Metadata = (a,b,t) match {
    case (a: Layout[_,_], b: Layout[_,_], _) => b
    case _ => super.meetMetadata(a,b,t)
  }
}

trait LayoutMetadataOps extends RankMetadataOps with LayoutMetadata {
  def getLayout(p: SymbolProperties) = p("layout").map{_.asInstanceOf[Layout[_,_]]}
  def getLayout(e: Exp[Any]) = getMetadata(e, "layout").map{_.asInstanceOf[Layout[_,_]]}
  def layout(e: Exp[Any]) = extractMetadata(e, "layout").asInstanceOf[Layout[_,_]]
  def layout(p: SymbolProperties) = extractMetadata(p, "layout").asInstanceOf[Layout[_,_]]

  override def rank(e: Exp[Any]): Int = getLayout(e) match {
    case Some(FlatLayout(n,_)) => n
    case _ => super.rank(e)
  }
  override def rank(p: SymbolProperties): Int = getLayout(p) match {
    case Some(FlatLayout(n,_)) => n
    case _ => super.rank(p)
  }
}

// TODO: Not a full analysis stage right now - just fills in layouts as flat
trait LayoutAnalyzer extends IRVisitor {
  val IR: DeliteOpsExp with DeliteMultiArrayOpsExp with LayoutMetadataOps
  import IR._
  override lazy val name = "Layout Analyzer"

  // Recursively sets layouts of all arrays to be flat
  // TODO: Currently all layouts are of type "Nothing"
  def setLayout(p: Option[SymbolProperties])(implicit ctx: SourceContext): Option[SymbolProperties] = p match {
    case Some(a: ArrayProperties) => 
      val datType = MADataType(isPhysView(a), isPhysBuffer(a))
      Some(ArrayProperties(setLayout(a.child), PropertyMap("layout", FlatLayout(rank(a),datType) )))
    case Some(s: StructProperties) => 
      Some(StructProperties(s.children.map{(f,v) => f -> setLayout(v)}, s.data))
    case _ => p
  }

  // Super simple analysis - make everything flat, row-major 
  override def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    for ((e,m) <- symbolData) {
      implicit val ctx: SourceContext = mpos(e.pos)
 
      if (hasMultiArrayTpe(e.tp)) {
        val origProps = getProps(e)
        val newProps = setLayout(origProps)
        setProps(e, attemptMeet(origProps, newProps, func = MetaOverwrite))
      }
    }
    (s)
  }
}


