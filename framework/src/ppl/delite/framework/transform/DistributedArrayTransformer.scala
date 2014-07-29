package ppl.delite.framework.transform

import scala.virtualization.lms.internal.FatBlockTraversal
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteFileReaderOpsExp, DeliteCollection, DeliteFileStream}
import ppl.delite.framework.datastructures.{DeliteArrayOpsExp, DeliteStructsExp, DeliteArray}


trait DistributedArrayTransformer extends ForwardPassTransformer { 
  val IR: DeliteOpsExp with DeliteArrayOpsExp with DeliteStructsExp
  import IR._

  abstract class Mode
  object Distributed extends Mode
  object Numa extends Mode
  object Local extends Mode

  val distributedMode: Mode = Local //TODO: Config flag

  //checks for allowed IR nodes on distributed collections and tags them as distributed (forwards tags through IR)
  //any other IR node that consumes a distributed collection is considered illegal; throws error to user
  //also forwards tags for numa partitioning, but in this case nothing is considered illegal
  override def transformStm(stm: Stm) = stm match {   
    //only multiloops can consume and produce distributed collections
    case TP(sym, d@Loop(s,v,b)) => markPartitioned(sym,v,d); super.transformStm(stm)
    //whitelist: the following ops are always allowed because necessary metadata is available on master
    case TP(sym, DeliteArrayLength(_)) => super.transformStm(stm)
    case TP(sym, Field(_,_)) => super.transformStm(stm)
    case TP(sym, Struct(_,_)) => super.transformStm(stm)
    //disallow everything else
    case TP(sym, d) => checkIllegalUsage(sym,d); super.transformStm(stm)
    case _ => Console.println("ERROR: unhandled stm in distributed transformer: " + stm); super.transformStm(stm)
  }

  def markPartitioned[T](sym: Sym[T], v: Sym[Int], d: Def[T]) {
    def symIsPartitioned(e: Exp[Any]) = e match {
      case Partitionable(t) => t.partition
      case e if e.tp == manifest[DeliteFileStream] => true //TODO: should be configurable
      case _ => false
    }

    //this only checks for array accesses dependent on the loop index... is this sufficient/correct?
    def inputIsPartitioned = getFatDependentStuff(initialDefs)(List(v)).exists(stm => stm match {
      case TP(s, DeliteArrayApply(arr,idx)) => symIsPartitioned(arr)
      //case TP(s, DeliteFileStreamReadLine(stream,idx)) => symIsPartitioned(stream)
      //do we want to mach on specific nodes (above) or all nodes that consume a sym of the right type (below)?
      case TP(s,d) if syms(d).exists(_.tp == manifest[DeliteFileStream]) => syms(d).filter(_.tp == manifest[DeliteFileStream]).exists(symIsPartitioned)
      case _ => false
    })

    def setPartitioned(e: Exp[Any]): Unit = e match {
      case Def(Struct(_,elems)) => elems.foreach(e => setPartitioned(e._2)) //partition every array in struct (?)
      case Partitionable(t) => t.partition = true
      case _ => Console.println("WARNING: tried to partition " + e.toString)
    }

    if (inputIsPartitioned) {
      checkAccessStencil(sym,d)
      d match {
        case Loop(_,_,body:DeliteCollectElem[_,_,_]) if body.par == ParFlat => Console.println("partitoning " + d.toString); setPartitioned(getBlockResult(body.buf.alloc))
        case Loop(_,_,body:DeliteCollectElem[_,_,_]) => Console.println("partitoning " + d.toString); setPartitioned(getBlockResult(body.buf.allocRaw))
        case _ => //other loop types (Reduce) will produce result on master
      }
    } else {
      Console.println("input not partitioned!")
    }
  }

  def checkAccessStencil[T](sym: Sym[T], d: Def[T]) {
    //TODO: integrate stencil analysis here (and again during codegen)
    //phase ordering issues? fusion + other optimizations could change stencil
    //TODO: need to special case the stencil for FileReader nodes + special logic in runtime scheduler... tag and/or look for Stream type?
  }

  def checkIllegalUsage[T](sym: Sym[T], d: Def[T]) {
    if (distributedMode == Distributed && deliteKernel) { //allow arbitrary consumers for numa or if already inside a parallel op
      syms(d).foreach(s => s match {
        case Partitionable(t) if t.partition => throw new RuntimeException("Illegal Operation at " + sym.pos.head + ": " + d.toString + " cannot be applied to distributed collection at " + s.pos.head)
        case _ => //ignore
      })
    }
  }

  object Partitionable {
    def unapply[T](e: Exp[T]): Option[PartitionTag[T]] = unapplyPartionable(e).map(_.asInstanceOf[PartitionTag[T]])
  }

  def unapplyPartionable[T](e: Exp[_]): Option[PartitionTag[_]] = e match {
    case Def(DeliteArrayNew(_,_,tag)) => Some(tag)
    case Def(Reflect(DeliteArrayNew(_,_,tag),_,_)) => Some(tag)
    case _ => None
  }

}
