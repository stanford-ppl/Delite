package ppl.delite.framework.ops

import java.io.{FileWriter, File, PrintWriter}
import scala.virtualization.lms.common.{BaseFatExp, TupleOpsExp, VariablesExp, EffectExp, CudaGenEffect, ScalaGenEffect}
import scala.virtualization.lms.internal.{GenericCodegen, GenericFatCodegen}
import ppl.delite.framework.DeliteCollection

trait DeliteOpsExp extends BaseFatExp with EffectExp with VariablesExp {
  /**
   * The base type of the DeliteOp hierarchy.
   */
  sealed abstract class DeliteOp[A]() extends Def[A]

  /**
   * A sequential task - will execute block in a single thread and respect any free variable dependencies inside it.
   *
   * @param  block   the task to execute; must be reified if it contains effectful operations!
   */
  class DeliteOpSingleTask[A](val block: Exp[A]) extends DeliteOp[A]

  
  abstract class ThinLoop[A] extends DeliteOp[A] { // as opposed to FatLoop ...TODO: make DeliteLoop extending ThinLoop
    val size: Exp[Int]
    val v: Sym[Int]
    val body: Def[A]
  }


  abstract class DeliteCollectElem[A, C[X] <: DeliteCollection[X]] extends Def[C[A]] {
    val func: Exp[A]
    val alloc: Exp[C[A]]
  }
  
  abstract class DeliteReduceElem[A] extends Def[A] {
    val func: Exp[A]
    val rV: (Sym[A], Sym[A])
    val rFunc: Exp[A]
  }
  
  
  
/*
  abstract class DeliteOpMapNew[A,C[X] <: DeliteCollection[X]]() extends DeliteOp[C[A]] {
    val size: Exp[Int]
    val v: Sym[Int]
    val func: Exp[A]
    val alloc: Exp[C[A]]
  }

  abstract class DeliteOpReduceNew[A]() extends DeliteOp[A] {
    val size: Exp[Int]
    val v: Sym[Int]
    val func: Exp[A]
    val rV: (Sym[A], Sym[A])
    val rFunc: Exp[A]
  }
*/



  /**
   * Parallel map from DeliteCollection[A] => DeliteCollection[B]. Input functions can depend on free
   * variables, but they cannot depend on other elements of the input or output collection (disjoint access).
   *
   * @param  in    the input collection
   * @param  v     the bound symbol that the mapping function operates over
   * @param  func  the mapping function; reified version of Exp[A] => Exp[B]
   * @param  alloc function returning the output collection. if it is the same as the input collection,
   *               the operation is mutable; reified version of Unit => DeliteCollection[B].
   */
  abstract class DeliteOpMap[A,B,C[X] <: DeliteCollection[X]]() extends DeliteOp[C[B]] {
    val in: Exp[C[A]]
    val v: Sym[A]
    val func: Exp[B]
    val alloc: Exp[C[B]]
  }

  /**
   * Parallel 2 element zipWith from (DeliteCollection[A],DeliteCollection[B]) => DeliteCollection[R].
   * Input functions can depend on free variables, but they cannot depend on other elements of the input or
   * output collection (disjoint access).
   *
   * @param  inA   the first input collection
   * @param  inB   the second input collection
   * @param  v     the bound symbol that the zipWith function operates over
   * @param  func  the zipWith function; reified version of ([Exp[A],Exp[B]) => Exp[R]
   * @param  alloc function returning the output collection. if it is the same as the input collection,
   *               the operation is mutable; reified version of Unit => DeliteCollection[B].
   */
  abstract class DeliteOpZipWith[A,B,R,C[X] <: DeliteCollection[X]]() extends DeliteOp[C[B]] {
    val inA: Exp[C[A]]
    val inB: Exp[C[B]]
    val v: (Sym[A],Sym[B])
    val func: Exp[R]
    val alloc: Exp[C[R]]
  }

  /**
   * Parallel reduction of a DeliteCollection[A]. Reducing function must be associative.
   *
   * @param  in    the input collection
   * @param  v     the bound symbol that the reducing function operates over
   * @param  func  the reduction function; reified version of ([Exp[A],Exp[A]) => Exp[A]. Must be associative.
   */
  abstract class DeliteOpReduce[A]() extends DeliteOp[A] {
    val in: Exp[DeliteCollection[A]]
    val v: (Sym[A],Sym[A])
    val func: Exp[A]
  }


  /**
   * Parallel map-reduction from a DeliteCollection[A] => R. The map-reduce is composed, so no temporary collection
   * is instantiated to hold the result of the map.
   *
   * @param  in      the input collection
   * @param  mV      the bound symbol that the mapping function operates over
   * @param  map     the mapping function; reified version of Exp[A] => Exp[R]
   * @param  rV      the bound symbol that the reducing function operates over
   * @param  reduce  the reduction function; reified version of ([Exp[R],Exp[R]) => Exp[R]. Must be associative.
   */
  abstract class DeliteOpMapReduce[A,R,C[X] <: DeliteCollection[X]]() extends DeliteOp[R] {
    val in: Exp[C[A]]
    //val acc: Exp[R]
    val mV: Sym[A]

    // for accumulating each partial sum
    //val mapreduce: Exp[R] // reified of Exp[(R,A)] => Exp[R] composition of map and reduce
    val map: Exp[R]

    // for reducing remaining partial sums
    val rV: (Sym[R],Sym[R])
    val reduce: Exp[R]
  }


  /**
   * Parallel foreach from DeliteCollection[A] => Unit. Input functions must specify any free variables that it
   * requires are protected (e.g. locked before chunk execution) using the sync list.
   *
   * @param  in     the input collection
   * @param  v      the bound symbol that the foreach function operates over
   * @param  func   the foreach function; reified version of Exp[A] => Exp[Unit]
   * @param  i      the bound symbol that the sync function operates over
   * @param  sync   a function from an index to a list of objects that should be locked, in a total ordering,
   *                prior to chunk execution, and unlocked after; reified version of Exp[Int] => Exp[List[_]]
   */
  abstract class DeliteOpForeach[A,C[X] <: DeliteCollection[X]]() extends DeliteOp[Unit] {
    val in: Exp[C[A]]
    val v: Sym[A]
    val func: Exp[Unit]
    val i: Sym[Int]
    val sync: Exp[List[Any]]
  }

  var deliteKernel : Boolean = _ // used by code generators to handle nested delite ops

  def getReifiedOutput(out: Exp[_]) = out match {
    case Def(Reify(x, effects)) => x
    case x => x
  }
}

trait BaseGenDeliteOps extends GenericFatCodegen {
  val IR: DeliteOpsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match { //TR TODO: question -- is alloc a dependency (should be part of result) or a definition (should not)???
    case s: DeliteOpSingleTask[_] => syms(s.block)
    case op: ThinLoop[_] => syms(op.size) ++ syms(op.body)
    case op: DeliteCollectElem[_,_] => syms(op.func) ++ syms(op.alloc)
    case op: DeliteReduceElem[_] => syms(op.func) ++ syms(op.rFunc)
    case map: DeliteOpMap[_,_,_] => /*if (shallow) syms(map.in) else */ syms(map.in) ++ syms(map.alloc) ++ syms(map.func)
    case zip: DeliteOpZipWith[_,_,_,_] => /*if (shallow) syms(zip.inA) ++ syms(zip.inB) else*/ syms(zip.inA) ++ syms(zip.inB) ++ syms(zip.alloc) ++ syms(zip.func)
    case red: DeliteOpReduce[_] => /*if (shallow) syms(red.in) else*/ syms(red.in) ++ syms(red.func)
    case mapR: DeliteOpMapReduce[_,_,_] => /*if (shallow) syms(mapR.in) else*/ syms(mapR.in) ++ syms(mapR.map) ++ syms(mapR.reduce)
    case foreach: DeliteOpForeach[_,_] => /*if (shallow) syms(foreach.in) else*/ syms(foreach.in) ++ syms(foreach.func)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match { //TR TODO
    case s: DeliteOpSingleTask[_] => effectSyms(s.block)
    case op: ThinLoop[_] => op.v::boundSyms(op.body)
    case op: DeliteCollectElem[_,_] => effectSyms(op.func) ++ effectSyms(op.alloc)
    case op: DeliteReduceElem[_] => effectSyms(op.func) ++ effectSyms(op.rFunc) ++ List(op.rV._1, op.rV._2)
    case zip: DeliteOpZipWith[_,_,_,_] => zip.v._1::zip.v._2::effectSyms(zip.alloc):::effectSyms(zip.func)
    case map: DeliteOpMap[_,_,_] => map.v::effectSyms(map.alloc):::effectSyms(map.func)
    case mapR: DeliteOpMapReduce[_,_,_] => mapR.mV::mapR.rV._1::mapR.rV._2::effectSyms(mapR.map):::effectSyms(mapR.reduce)
    case red: DeliteOpReduce[_] => red.v._1::red.v._2::effectSyms(red.func)
    case foreach: DeliteOpForeach[_,_] => foreach.v::foreach.i::effectSyms(foreach.func):::effectSyms(foreach.sync)
    case _ => super.boundSyms(e)
  }
    
    
  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case s: DeliteOpSingleTask[_] => getFreeVarBlock(s.block,Nil)
    case map: DeliteOpMap[_,_,_] => getFreeVarBlock(List(map.func,map.alloc),List(map.v.asInstanceOf[Sym[_]]))
    case zip: DeliteOpZipWith[_,_,_,_] => 
      println("FREEVAR "+zip + "/" + List(zip.func,zip.alloc))
      val z1 = getFreeVarBlock(zip.func, List(zip.v._1.asInstanceOf[Sym[_]], zip.v._2.asInstanceOf[Sym[_]]))
      val z2 = getFreeVarBlock(zip.alloc, Nil)
      println(z1 + "/" + z2)
      z1:::z2
    case red: DeliteOpReduce[_] => getFreeVarBlock(red.func,List(red.v._1.asInstanceOf[Sym[_]], red.v._2.asInstanceOf[Sym[_]]))
    case mapR: DeliteOpMapReduce[_,_,_] => getFreeVarBlock(mapR.map, List(mapR.mV.asInstanceOf[Sym[_]])) ++ getFreeVarBlock(mapR.reduce, List(mapR.rV._1.asInstanceOf[Sym[_]], mapR.rV._2.asInstanceOf[Sym[_]]))
    case foreach: DeliteOpForeach[_,_] => getFreeVarBlock(foreach.func,List(foreach.v.asInstanceOf[Sym[_]]))
    case _ => super.getFreeVarNode(rhs)
  }

}

trait ScalaGenDeliteOps extends ScalaGenEffect with BaseGenDeliteOps {
  import IR._

  def quotearg(x: Sym[Any]) = quote(x) + ": " + quotetp(x)
  def quotetp(x: Sym[Any]) = remap(x.Type)

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case s:DeliteOpSingleTask[_] => {
      println("EMIT single "+s)
      val save = deliteKernel
      deliteKernel = false
      val b = s.block
      stream.println("val " + quote(sym) + " = { ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
      deliteKernel = save
    }
    case op:ThinLoop[_] =>
        if (!deliteKernel) {
          // wide zip, no reduce yet
          op.body match {
            case elem: DeliteCollectElem[_,_] =>
              stream.println("val " + quote(sym) + " = {")
              emitBlock(elem.alloc)
              stream.println(quote(getBlockResult(elem.alloc)))
              stream.println("}")
            //case DeliteReduceElem(func, rV, rFunc) =>
          }
          stream.println("var " + quote(op.v) + " = 0")
          stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {")
          op.body match {
            case elem: DeliteCollectElem[_,_] =>
              emitBlock(elem.func)
              stream.println(quote(sym) + ".dcUpdate(" + quote(op.v) + ", " + quote(getBlockResult(elem.func)) + ")")
            //case DeliteReduceElem(func, rV, rFunc) =>
          }
          stream.println(quote(op.v) + " += 1")
          stream.println("}")
/*        // red
          stream.println("val " + quote(sym) + " = {")
          stream.println("var " + quote(red.v._1) + " = " + quote(red.in) + "(0)")
          stream.println("var reduceIdx = 1")
          stream.println("while (reduceIdx < " + quote(red.in) + ".size) {")
          stream.println("val " + quote(red.v._2) + " = " + quote(red.in) + ".dcApply(reduceIdx)")
          stream.println(quote(red.v._1) + " = {")
          emitBlock(red.func)
          stream.println(quote(getBlockResult(red.func)))
          stream.println("}")
          stream.println("reduceIdx += 1")
          stream.println("} // end while")
          stream.println(quote(red.v._1))
          stream.println("}")
*/
        } else {
          // wide zip, no reduce yet
          val kernelName = List(sym).map(quote).mkString("")
          val actType = "activation_"+kernelName
          deliteKernel = false
          stream.println("val " + kernelName + " = new generated.scala.DeliteOpMultiLoop[" + actType + "] {")
          stream.println("def size = " + quote(op.size))
          op.body match {
            case elem: DeliteCollectElem[_,_] =>
              stream.println("def alloc: " + actType + " = {")
              stream.println("val __act = new " + actType)
              emitBlock(elem.alloc)
              stream.println("__act." + quote(sym) + " = " + quote(getBlockResult(elem.alloc)))
              stream.println("__act")
              stream.println("}")
              stream.println("def split(__act: " + actType + "): " + actType + " = {")
              stream.println("__act")
              stream.println("}")
              stream.println("def process(__act: " + actType + ", " + quotearg(op.v) + "): Unit = {")
              emitBlock(elem.func)
              stream.println("__act." + quote(sym) + ".dcUpdate(" + quote(op.v) + ", " + quote(getBlockResult(elem.func)) + ")")
              stream.println("}")
              stream.println("def combine(__act: " + actType + ", rhs: " + actType + "): Unit = {")
              stream.println("}")
          }
          stream.println("}")
          deliteKernel = true
        }
/*
// sample zip/map/reduce kernel: simultaneously compute x900 = sumOfSquares(b) and x11 = vectorPlus(a,b)
package generated.scala
class activation_x900x11{
  var x900: Int = _
  var x11: generated.scala.IntVectorImpl = _
}

object kernel_x900x11{
def apply(x1:generated.scala.Vector[Int],x2:generated.scala.Vector[Int],x4:Int,x5:Boolean): generated.scala.DeliteOpMultiLoop[activation_x900x11] = {
val x11 = new generated.scala.DeliteOpMultiLoop[activation_x900x11] {
//def inA = x1
//def inB = x2
def size = {
val a1 = x1.size
a1
}
def alloc = {
val __act = new activation_x900x11
__act.x900 = 0
__act.x11 = new generated.scala.IntVectorImpl(x4,x5)
__act
}
def alloc(rhs: activation_x900x11) = {
val __act = new activation_x900x11
__act.x900 = 0
__act.x11 = rhs.x11
__act
}
def mapreduce(__act: activation_x900x11, idx: Int): Unit = {
val a1 = x1.apply(idx)
val a2 = x2.apply(idx)
val x10 = a1 + a2
__act.x900 += a2*a2
__act.x11.update(idx, x10)
}
def reduce(__act: activation_x900x11, rhs: activation_x900x11): Unit = {
__act.x900 += rhs.x900
}
}

x11
}}
*/
    
    case map:DeliteOpMap[_,_,_] => {
      if (deliteKernel == false){
        stream.println("val " + quote(sym) + " = {")
        emitBlock(map.alloc)
        stream.println("var mapIdx = 0")
        stream.println("while (mapIdx < " + quote(map.in) + ".size) {")
        stream.println("val " + quote(map.v) + " = " + quote(map.in) + ".dcApply(mapIdx)")
        stream.println(quote(getBlockResult(map.alloc)) + ".dcUpdate(mapIdx, " + " {")
        emitBlock(map.func)
        stream.println(quote(getBlockResult(map.func)))
        stream.println("})")
        stream.println("mapIdx += 1")
        stream.println("} // end while")
        stream.println(quote(getBlockResult(map.alloc)))
        stream.println("}")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpMap[" + remap(map.v.Type) + "," + remap(map.func.Type) + "," + remap(map.alloc.Type) + "] {")
        stream.println("def in = " + quote(map.in))
        stream.println("def alloc = {")
        emitBlock(map.alloc)
        stream.println(quote(getBlockResult(map.alloc)))
        stream.println("}")
        stream.println("def map(" + quote(map.v) + ": " + remap(map.v.Type) + ") = {")
        emitBlock(map.func)
        stream.println(quote(getBlockResult(map.func)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case zip: DeliteOpZipWith[_,_,_,_] => {
      if (deliteKernel == false){
        stream.println("val " + quote(sym) + " = {")
        emitBlock(zip.alloc)
        stream.println("var zipIdx = 0")
        stream.println("while (zipIdx < " + quote(zip.inA) + ".size) {")
        stream.println("val " + quote(zip.v._1) + " = " + quote(zip.inA) + ".dcApply(zipIdx)")
        stream.println("val " + quote(zip.v._2) + " = " + quote(zip.inB) + ".dcApply(zipIdx)")
        stream.println(quote(getBlockResult(zip.alloc)) + ".dcUpdate(zipIdx, " + " {")
        emitBlock(zip.func)
        stream.println(quote(getBlockResult(zip.func)))
        stream.println("})")
        stream.println("zipIdx += 1")
        stream.println("} // end while")
        stream.println(quote(getBlockResult(zip.alloc)))
        stream.println("}")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpZipWith[" + remap(zip.v._1.Type) + "," + remap(zip.v._2.Type) + "," + remap(zip.func.Type) + "," + remap(zip.alloc.Type) +"] {")
        stream.println("def inA = " + quote(zip.inA))
        stream.println("def inB = " + quote(zip.inB))
        stream.println("def alloc = {")
        emitBlock(zip.alloc)
        stream.println(quote(getBlockResult(zip.alloc)))
        stream.println("}")
        stream.println("def zip(" + quote(zip.v._1) + ": " + remap(zip.v._1.Type) + ", " + quote(zip.v._2) + ": " + remap(zip.v._2.Type) + ") = {")
        emitBlock(zip.func)
        stream.println(quote(getBlockResult(zip.func)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case red: DeliteOpReduce[_] => {
      if (deliteKernel == false){
        stream.println("val " + quote(sym) + " = {")
        stream.println("var " + quote(red.v._1) + " = " + quote(red.in) + "(0)")
        stream.println("var reduceIdx = 1")
        stream.println("while (reduceIdx < " + quote(red.in) + ".size) {")
        stream.println("val " + quote(red.v._2) + " = " + quote(red.in) + ".dcApply(reduceIdx)")
        stream.println(quote(red.v._1) + " = {")
        emitBlock(red.func)
        stream.println(quote(getBlockResult(red.func)))
        stream.println("}")
        stream.println("reduceIdx += 1")
        stream.println("} // end while")
        stream.println(quote(red.v._1))
        stream.println("}")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpReduce[" + remap(red.func.Type) + "] {")
        stream.println("def in = " + quote(red.in))
        stream.println("def reduce(" + quote(red.v._1) + ": " + remap(red.v._1.Type) + "," + quote(red.v._2) + ": " + remap(red.v._2.Type) + ") = {")
        emitBlock(red.func)
        stream.println(quote(getBlockResult(red.func)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case mapR:DeliteOpMapReduce[_,_,_] => {
      if (deliteKernel == false){
        stream.println("val " + quote(sym) + " = {")
        stream.println("val " + quote(mapR.mV) + " = " + quote(mapR.in) + ".dcApply(0)")
        stream.println("var " + quote(mapR.rV._1) + " = {")
        emitBlock(mapR.map)
        stream.println(quote(getBlockResult(mapR.map)))
        stream.println("}")
        stream.println("var mapReduceIdx = 1")
        stream.println("while (mapReduceIdx < " + quote(mapR.in) + ".size) {")
        stream.println("val " + quote(mapR.mV) + " = " + quote(mapR.in) + ".dcApply(mapReduceIdx)")
        stream.println("val " + quote(mapR.rV._2) + " = {")
        emitBlock(mapR.map)
        stream.println(quote(getBlockResult(mapR.map)))
        stream.println("}")
        stream.println(quote(mapR.rV._1) + " = {")
        emitBlock(mapR.reduce)
        stream.println(quote(getBlockResult(mapR.reduce)))
        stream.println("}")
        stream.println("mapReduceIdx += 1")
        stream.println("} // end while")
        stream.println(quote(mapR.rV._1))
        stream.println("}")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpMapReduce[" + remap(mapR.mV.Type) + "," + remap(mapR.reduce.Type) + "] {")
        stream.println("def in = " + quote(mapR.in))
        stream.println("def map(" + quote(mapR.mV) + ": " + remap(mapR.mV.Type) + ") = {")
        emitBlock(mapR.map)
        stream.println(quote(getBlockResult(mapR.map)))
        stream.println("}")
        stream.println("")
        stream.println("def reduce(" + quote(mapR.rV._1) + ": " + remap(mapR.rV._1.Type) + "," + quote(mapR.rV._2) + ": " + remap(mapR.rV._2.Type) + ") = {")
        emitBlock(mapR.reduce)
        stream.println(quote(getBlockResult(mapR.reduce)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case foreach:DeliteOpForeach[_,_] => {
      if (deliteKernel == false){
        stream.println("val " + quote(sym) + " = {")
        stream.println("var forIdx = 0")
        stream.println("while (forIdx < " + quote(foreach.in) + ".size) {")
        stream.println("val " + quote(foreach.v) + " = " + quote(foreach.in) + ".dcApply(forIdx)")
        emitBlock(foreach.func)
        stream.println(quote(getBlockResult(foreach.func)))
        stream.println("forIdx += 1")
        stream.println("} // end while")
        stream.println("}")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpForeach[" + remap(foreach.v.Type) + "] {")
        stream.println("def in = " + quote(foreach.in))
        stream.println("def sync(" + quote(foreach.i) + ": " + remap(foreach.i.Type) + ") = {")
        emitBlock(foreach.sync)
        stream.println(quote(getBlockResult(foreach.sync)))
        stream.println("}")
        stream.println("def foreach(" + quote(foreach.v) + ": " + remap(foreach.v.Type) + ") = {")
        emitBlock(foreach.func)
        stream.println(quote(getBlockResult(foreach.func)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case _ => super.emitNode(sym,rhs)
  }
}

trait CudaGenDeliteOps extends CudaGenEffect with BaseGenDeliteOps {
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
      //case s:DeliteOpSingleTask[_] =>
      //case map:DeliteOpMap[_,_,_] =>
      case mapR:DeliteOpMapReduce[_,_,_] => {
        emitValDef(mapR.rV._1.asInstanceOf[Sym[_]],quote(sym))
        emitValDef(mapR.rV._2.asInstanceOf[Sym[_]],quote(getBlockResult(mapR.map)))
        stream.println(addTab()+"int %s = %s.apply(0);".format(quote(mapR.mV),quote(mapR.in)))
        addVarLink(getBlockResult(mapR.map).asInstanceOf[Sym[_]],sym)
        emitBlock(mapR.map)
        removeVarLink(getBlockResult(mapR.map).asInstanceOf[Sym[_]],sym)
        stream.println(addTab()+"for(int cnt=1; cnt<%s.length; cnt++) {".format(quote(mapR.in)))
        tabWidth += 1
        stream.println(addTab()+"%s = %s.apply(cnt);".format(quote(mapR.mV),quote(mapR.in)))
        emitBlock(mapR.map)
        addVarLink(getBlockResult(mapR.reduce).asInstanceOf[Sym[_]],sym)
        emitBlock(mapR.reduce)
        removeVarLink(getBlockResult(mapR.reduce).asInstanceOf[Sym[_]],sym)
        tabWidth -= 1
        stream.println(addTab()+"}")
        allocOutput(sym,getBlockResult(mapR.map).asInstanceOf[Sym[_]])
      }
      case _ => super.emitNode(sym,rhs)
  }
}
