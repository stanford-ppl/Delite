package ppl.delite.framework.analysis

import java.io._
import scala.collection.mutable.{HashMap,HashSet}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.FatBlockTraversal
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollection}
import ppl.delite.framework.datastructures.{DeliteMultiArray,DeliteArrayOpsExp}
import ppl.delite.framework.Config

import ppl.delite.framework.analysis.Slot // change later..

trait MultiArrayAnalysis extends AnalysisBase {
  override val IR: DeliteOpsExp with DeliteMultiArrayLayouts

  // TODO: these shortcuts are all really ugly
  def rank(e: Exp[Any]) = metadata.get(e) match {
    case Some(m: MultiArrayMetadata) => m.rank
    case _ => throw new NoSuchElementException("Can't get rank of " + strDef(e))
  }
  def child(e: Exp[Any]) = metadata.get(e) match {
    case Some(m: MultiArrayMetadata) => m.child
    case _ => throw new NoSuchElementException("Can't get child of " + strDef(e))
  }
  def target(e: Exp[Any]) = metadata.get(e) match {
    case Some(m: MultiArrayMetadata) => m.target
    case _ => throw new NoSuchElementException("Can't get target of " + strDef(e))
  }

  def isView(e: Exp[Any]) = metadata.get(e) match {
    case Some(m: MultiArrayMetadata) => m.isView
    case _ => throw new NoSuchElementException("Can't get isView of " + strDef(e))
  }

  def isDataStructure[A](mA: Manifest[A]) = mA match {
    case StructType(_,_) => true
    case _ => isSubType(mA.erasure,classOf[DeliteMultiArray[_]])
  }
}

trait RankAnalysis extends MultiArrayAnalysis {
  val name = "Rank Analysis"

  // TODO: Assuming for now that we can have aliased views with different
  // target ranks.
  // for example: 
  //    val x = MultiArray[Int](1, 2, 3)
  //    val y = MultiArray[Int](3, 3)          // will be padded between here and assign
  //    val z = MultiArray[MultiArray[Int]](2)
  //    z(0) = y
  //    z(1) = x.slice2D(1,0::3,0::3) 


  // Note that views adopt child of target
  // e.g. val x = MultiArray[MultiArray[Int]](...)   // child is MultiArray
  //      val y = x.slice(...)                       // child is MultiArray

  override def equivalent(a: Metadata, b: Metadata) = (a,b) match {
    case (a: MultiArrayMetadata, b: MultiArrayMetadata) =>
      val rankEquiv = (a.rank == b.rank)
      val childEquiv = childEquivalent(a.child, b.child)
      val targEquiv = (a.target, b.target) match {
        case (Some(at), Some(bt)) => (at.rank == bt.rank)
        case (None, None) => true
        case _ => false
      }
      (rankEquiv && childEquiv && targEquiv)

    case _ => super.equivalent(a,b)
  }

  override def compatible(a: Metadata, b: Metadata) = (a,b) match {
    case (a: MultiArrayMetadata, b: MultiArrayMetadata) =>
      (a.rank == b.rank) && childMatch(a.child, b.child)

    case _ => super.compatible(a,b)
  }

  override def meet(a: Metadata, b: Metadata) = (a,b) match {
    case (a: MultiArrayMetadata, b: MultiArrayMetadata) if compatible(a,b) =>
      val child = childMeet(a.child, b.child)
      val target = (a.target, b.target) match {
        case (Some(at),Some(bt)) => 
          val rank = Math.max(at.rank, bt.rank)

        case (Some(at),None) => Some(at.copy)
        case (None,Some(bt)) => Some(bt.copy)
        case (None,None) => None
      }

    case _ => super.meet(a,b)
  }

  /**
   * Test if child field has been set in MultiArray metadata
  */
  override def isComplete[A](m: Metadata) = m match {
    case MultiArrayMetadata(_,child,target,_,_) => 
      val childDone = child match {
        case Known(meta) => isComplete(meta)
        case Unknown => false
        case Nix => true
      }
      val targDone = target match {
        case Some(meta) => isComplete(meta)
        case None => true
      }
      (childDone && targDone)

    case _ => super.isComplete(m)
  }

  /**
   * Test if child field has not yet been set in MultiArray
   * TBD: Do we need this as a separate def? seems to at least nearly be 
   * the inverse of isComplete()
  */
  override def isIncomplete[A](m: Metadata) = m match {
    case MultiArrayMetadata(_,child,target,_,_) => 
      val childIncom = child match {
        case Known(meta) => isIncomplete(meta)
        case Unknown => true
        case Nix => false
      }
      val targIncom = target match {
        case Some(meta) => isIncomplete(meta)
        case None => false
      }
      (childIncom || targIncom)

    case _ => super.isIncomplete(m)
  }

  // Use copy of child instead of child directly
  override def child(e: Exp[Any]) = metadata.get(e) match {
    case Some(m: MultiArrayMetadata) => m.child match {
      case Known(c) => Known(c.copy)
      case Unknown => Unknown
      case Nix => Nix
    }
    case _ => throw new NoSuchElementException("Can't get child of " + strDef(e))
  }

  /**
   * Propagate rank and child info through IR
  */
  override def processOp[A](e: Exp[A], d: Def[_]): Unit = d match {
    case DeliteMultiArraySortIndices(_,_) => add(e, MultiArrayMetadata(1))
    case DeliteStringSplit(_,_,_) => add(e, MultiArrayMetadata(1))

    case op @ DeliteMultiArrayNew(dims) => 
      if (isDataStructure(op.mA)) add(s, MultiArrayMetadata(dims.length, Unknown))
      else add(s, MultiArrayMetadata(dims.length))
    case DeliteMultiArrayView(t,o,s,dims) => 
      if (isComplete(t)) {
        if (isView(t)) add(s, MultiArrayMetadata(dims.length, child(t), target(t)))
        else           add(s, MultiArrayMetadata(dims.length, child(t), metadata.get(t)))
      }
    case DeliteMultiArrayPermute(ma,c) =>
      if (isComplete(ma)) add(s, MultiArrayMetadata(rank(ma), child(ma)))
      else wait(s)
    case DeliteMultiArrayReshape(ma,dims) => 
      if (isComplete(ma)) add(s, MultiArrayMetadata(dims.length, child(ma)))
      else wait(s)
    case op @ DeliteMultiArrayApply(ma,i) if isDataStructure(op.mA) => 
      if (isComplete(ma)) add(s, child(ma).get)
      else wait(s)

    case op @ DeliteMultiArrayFromFunction(dims,f) =>
      if (isDataStructure(op.mA)) {
        partial(s, MultiArrayMetadata(dims.length, Unknown))
        if (isComplete(op.body.res)) updateChild(s, metadata(op.body.res).copy)
      }
      else add(s, MultiArrayMetadata(dims.length))

    case op @ DeliteMultiArrayMap(ma,f) =>
      if (isComplete(ma)) {
        if (isDataStructure(op.mA)) add(op.a, child(ma).get)

        if (isDataStructure(op.mB)) {   // output type is nested structure
          partial(s, MultiArrayMetadata(rank(ma), Unknown))
          if (isComplete(op.body.res)) updateChild(s, metadata(op.body.res).copy)
        }
        else add(s, MultiArrayMetadata(rank(ma)))
      }
      else wait(s)

    case op @ DeliteMultiArrayZipWith(ma,mb,f) =>
      if (isComplete(ma) && isComplete(mb)) {
        if (isDataStructure(op.mA)) add(op.a, child(ma).get)
        if (isDataStructure(op.mB)) add(op.b, child(mb).get)
        
        if (isDataStructure(op.mT)) {
          partial(s, MultiArrayMetadata(rank(ma), Unknown))
          if (isComplete(op.body.res)) updateChild(s, metadata(op.body.res).copy)
        }
        else add(s, MultiArrayMetadata(rank(ma)))
      }
      else wait(s)

    case op @ DeliteMultiArrayReduce(ma,f,z) if isDataStructure(op.mA) =>
      if (isComplete(ma)) {
        add(op.a1, child(ma).get)
        add(op.a2, child(ma).get)

        if (isComplete(op.body.res)) add(s, metadata(op.body.res).copy)
        else wait(s)
      }
      else wait(s)

    case op @ DeliteMultiArrayForeach(ma,f) if isDataStructure(op.mA) =>
      if (isComplete(ma)) add(op.a, child(ma).get)

    /* case op: DeliteMultiArrayForIndices(ma,f) --- don't need to analyze this */

    case op @ DeliteMultiArrayNDMap(in,mdims,f) => 
      if (isComplete(in)) {
        add(op.ma, MultiArrayMetadata(mdims.length, child(in)))

        if (isDataStructure(op.mB)) {
          partial(s, MultiArrayMetadata(rank(in), Unknown))
          if (isComplete(op.body.res)) updateChild(s, child(op.body.res).get)
        }
        else add(s, MultiArrayMetadata(rank(in)))
      }
      else wait(s)

    // rank(ma) should always be 1 here
    case op @ DeliteMultiArrayMapFilter(ma,map,cond) =>
      if (isComplete(ma)) {
        if (isDataStructure(op.mA)) add(op.a, child(ma).get)

        if (isDataStructure(op.mB)) {
          partial(s, MultiArrayMetadata(rank(ma), Unknown))
          if (isComplete(op.mapFunc.res)) updateChild(s, metadata(op.mapFunc.res).copy)
        }
        else add(s, MultiArrayMetadata(rank(ma)))
      }
      else wait(s)

    case op @ DeliteMultiArrayFlatMap(ma,f) => 
      if (isComplete(ma)) {
        if (isDataStructure(op.mA)) add(op.a, child(ma).get)

        if (isDataStructure(op.mB)) {
          partial(s, MultiArrayMetadata(rank(ma), Unknown))
          if (isComplete(op.body.res)) updateChild(s, child(op.body.res).get)
        }
        else add(s, MultiArrayMetadata(rank(ma)))
      }
      else wait(s)

    // Possible to have initialization be something like
    // val x = MultiArray[MultiArray](...)
    // x.forIndices{i => x(i) = f(i)}
    // - OR -
    // x.mmap{e => MultiArray[Int](...)}  
    // - OR -
    // x.mzip(y){(a,b) => f(b)}

    // Without these mutable updates, no way to know what concrete inner type is
    // (This should probably result in an error)

    case op @ DeliteMultiArrayUpdate(ma,i,x) if isDataStructure(op.mA) =>
      if (isComplete(x)) updateChild(ma, metadata(x).copy)

    case op @ DeliteMultiArrayMutableMap(ma,f) if isDataStructure(op.mA) =>
      if (isComplete(ma)) add(op.a, child(ma).get)
      if (isComplete(op.body.res)) updateChild(ma, metadata(op.body.res).copy)

    case op @ DeliteMultiArrayMutableZipWith(ma,mb,f) =>
      if (isComplete(ma) && isDataStructure(op.mA)) add(op.a, child(ma).get)
      if (isComplete(mb) && isDataStructure(op.mB)) add(op.a, child(ma).get)
      if (isDataStructure(op.mA) && isComplete(op.body.res)) updateChild(ma, metadata(op.body.res).copy)

    case _ =>
      if (isDataStructure(manifest[A])) {

      }
  }

  override def run[A](b: Block[A]) = {
    super.run(b)

    if (iter > MAX_ITERS && incomplete.nonEmpty) {
      result("[rank analysis] Maximum iterations exceeded before all ranks were fully known")
      result("[rank analysis] Try increasing the maximum number of iterations")
      sys.exit(0)
    }
    else if (incomplete.isEmpty) {
      result("[rank analysis] Unable to statically determine the ranks of some arrays: ")
      // print arrays

      sys.exit(0)
    }
    else {
      log("")
      log("[rank analysis] Completed. Found ranks: ")
      log("")
      for ((k,v) <- multiaRanks) {
        log("")
        log("sym: " + strDef(k))
        log("\trank: " + v.rank + ", view: " + v.view + ", mutable: " + v.mutable)
      }
    }
  }



}


trait RankChecking extends RankAnalysisBase {

  def check(x: Boolean, msg: String, ctx: SourceContext) {

  }

  def checkOp[A](s: Exp[A], d: Def[_]): Unit = d match {
    // Mutations --- may not need these if this is already being checked in staging
    case op @ DeliteMultiArrayUpdate(ma,i,x) => 
      check(isMutable(ma), "Cannot update immutable data structure", op.ctx)
    case op @ DeliteMultiArrayMutableMap(ma,f) =>
      check(isMutable(ma), "Cannot mutable-map immutable data structure", op.ctx)
    case op @ DeliteMultiArrayMutableZipWith(ma,mb,f) => 
      check(isMutable(ma), "Cannot mutable-zipwith immutable data structure", op.ctx)
      check(rank(ma) == rank(mb), "Ranks in inputs to mutable-zipwith must match", op.ctx)
    
    case op @ DeliteMultiArrayZipWith(ma,mb,f) =>
      check(rank(ma) == rank(mb), "Ranks in inputs to zipwith must match", op.ctx)

    case op @ DeliteMultiArrayReduce(ma,f,z) if =>
      check(rank(z) == metadata(ma).child.get.rank)

    case op @ DeliteMultiArrayNDMap(in,mdims,f) =>
      check(rank(op.ma) == rank(op.body.res), "Input and output rank must match in ND-Map", op.ctx)
    case op @ DeliteMultiArrayNDInsert(ma,rhs,axis,i) =>
      check(rank(ma) == rank(rhs) + 1, "Right hand side must have rank one less than left hand side in ND-Insert", op.ctx)
    case op @ DeliteMultiArrayNDAppend(ma,rhs,axis) =>
      check(rank(ma) == rank(rhs) + 1, "Right hand side must have rank one less than left hand side in ND-Append", op.ctx)
    case op @ DeliteMultiArrayInsertAll(ma,rhs,axis,i) =>
      check(rank(ma) == rank(rhs), "Ranks of left and right hand side must match in Insert All", op.ctx)
    case op @ DeliteMultiArrayAppendAll(ma,rhs,axis) =>
      check(rank(ma) == rank(rhs), "Ranks of left and right hand side must match in Append All", op.ctx)
    case op @ DeliteMultiArrayMkString(ma,dels) =>
      check(rank(ma) == dels.length, "Number of delimeters given in MkString must match input structure's rank", op.ctx)

    case op @ DeliteMultiArrayMapFilter(ma,f,c) => 
      check(rank(ma) == 1, "MapFilter is only defined on 1D arrays", op.ctx)
    case op @ DeliteMultiArrayFlatMap(ma,f) => 
      check(rank(ma) == 1, "FlatMap is only defined on 1D arrays", op.ctx)     
    case op @ DeliteMultiArrayInsert(ma,x,i) => 
      check(rank(ma) == 1, "Element Insert is only defined on 1D arrays", op.ctx)
    case op @ DeliteMultiArrayAppend(ma,x) =>
      check(rank(ma) == 1, "Element Append is only defined on 1D arrays", op.ctx)

    case op @ DeliteMatrixMultiply(lhs,rhs) =>
      check(rank(lhs) == 2, "Left hand side of matrix multiply must be a 2D array", op.ctx)
      check(rank(rhs) == 2, "Right hand side of matrix multiply must be a 2D array", op.ctx)
    case op @ DeliteMatrixVectorMultiply(mat,vec) =>
      check(rank(mat) == 2, "Matrix argument to matrix-vector multiply must be a 2D array", op.ctx)
      check(rank(vec) == 2, "Vector argument to matrix-vector multiply must be a 1D array", op.ctx)
  }

  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(s, Reflect(d,_,_)) => checkOp(s,d)
    case TP(s, d) => checkOp(s,d)
    case _ => super.traverseStm(stm)
  }

}