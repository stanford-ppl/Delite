package ppl.delite.framework.analysis

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures.DeliteMultiArrayOpsExp

/*trait DeliteMutationMetadata extends DeliteMetadata {
  case class Mutable(mutable: Boolean) {
    val name = "mutable"
    override def makeString(prefix: String) = mutable.toString
    def isMutable: Boolean = mutable
  }

  override def metadataMatches(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case (a: Mutable, b: Mutable) => (a.mutable == b.mutable)
    case _ => super.metadataMatches(a,b)
  }
  override def canMeetMetadata(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case (a: Mutable, b: Mutable) => (a.mutable == b.mutable)
    case _ => super.canMeetMetadata(a,b)
  }
  override def meetMetadata(a: Metadata, b: Metadata): Metadata = (a,b) match {
    case (a: Mutable, b: Mutable) if canMeetMetadata(a,b) => Mutable(a.mutable)
    case _ => super.meetMetadata(a,b)
  }

  def dataComplete(a: SymbolProperties): Boolean = true
}

trait MutationAnalysis extends AnalysisBase {
  val IR: DeliteOpsExp
  import IR._

  override val IRMetadata = new DeliteMutationMetadata {}
  import IRMetadata._

  def hasReflectMutable(e: Exp[Any]): Boolean = e match {
    case s: Sym[_] => isWritableSym(s)
    case _ => false
  }

  def getMutable(p: SymbolProperties): Option[Mutable] = p("mutable").map{_.asInstanceOf[Mutable]}
  def getMutable(e: Exp[Any]): Option[Mutable] = metadata.get(e) match {case Some(p) => getMutable(p); case None => None}

  def isMutable(e: Exp[Any]): Boolean = getMutable(e).getOrElse(Mutable(false)).isMutable

  override def analyzeStm(stm: Stm): Unit = stm match {
    case TP(s, Reflect(d,_,_)) => processOp(s,d)
    case TP(s, d) => processOp(s,d)
    case _ => // Nothing
  }

  def processOp[A](e: Exp[A], d: Def[_]): Unit = {
    if (hasReflectMutable(e))
      setMetadata(e, Mutable(true))
  }
}*/



/*
    case DeliteMultiArrayUpdate(ma,_,_) => 
      check(isMutable(ma), "Cannot update immutable array", e)
    case DeliteMultiArrayMutableMap(ma,_) =>
      check(isMutable(ma), "Cannot mutable-map immutable array", e)
    case DeliteMultiArrayMutableZipWith(ma,mb,_) => 
      check(isMutable(ma), "Cannot mutable-zipwith immutable array", e)
      

    case DeliteMultiArrayNDInsert(ma,rhs,axis,i) =>
      check(isMutable(ma), "Cannot insert into immutable array", e)
    case DeliteMultiArrayNDAppend(ma,rhs,axis) =>
      check(isMutable(ma), "Cannot append to immutable array", e)
    case DeliteMultiArrayInsertAll(ma,rhs,axis,i) =>
      check(isMutable(ma), "Cannot insert into immutable array", e)
    case DeliteMultiArrayAppendAll(ma,rhs,axis) =>
      check(isMutable(ma), "Cannot append to immutable array", e)
    case DeliteMultiArrayInsert(ma,x,i) => 
      check(isMutable(ma), "Cannot insert into immutable array", e)
    case DeliteMultiArrayAppend(ma,x) =>
      check(isMutable(ma), "Cannot append to immutable array", e)
    case DeliteMultiArrayRemove(ma,axis,start,end) =>
      check(isMutable(ma), "Cannot remove elements from an immutable array", e)
    
}
*/