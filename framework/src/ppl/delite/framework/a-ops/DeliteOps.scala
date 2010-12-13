package ppl.delite.framework.ops

import java.io.{FileWriter, File, PrintWriter}
import scala.virtualization.lms.common.{TupleOpsExp, VariablesExp, EffectExp}
import scala.virtualization.lms.internal.{GenericCodegen, CudaGenEffect, GenericNestedCodegen, ScalaGenEffect}
import ppl.delite.framework.DeliteCollection

trait DeliteOpsExp extends EffectExp with VariablesExp {
  // representation must be reified! this places the burden on the caller, but allows the caller to avoid the
  // use of function values (which can be uglier).

  sealed abstract class DeliteOp[A]() extends Def[A]

  case class DeliteOpSingleTask[A](block: Exp[A]) extends DeliteOp[A]

  /**
   * @param  out   defined as a def to avoid being picked up as a control dependency; however,
   *               should be implemented as a val, or spurious references will be introduced!
   */
  abstract case class DeliteOpMap[A,B,C[X] <: DeliteCollection[X]]() extends DeliteOp[C[B]] {
    val in: Exp[C[A]]
    val v: Exp[A]    // input symbol for func
    val func: Exp[B] // reified of Exp[A] => Exp[R]
    def out: Exp[C[B]]
  }

  /**
   * @param  out   defined as a def to avoid being picked up as a control dependency; however,
   *               should be implemented as a val, or spurious references will be introduced!
   */
  abstract case class DeliteOpZipWith[A,B,R,C[X] <: DeliteCollection[X]]() extends DeliteOp[C[B]] {
    val inA: Exp[C[A]]
    val inB: Exp[C[B]]
    val v: (Exp[A],Exp[B])
    val func: Exp[R]
    def out: Exp[C[R]]
  }

  abstract case class DeliteOpReduce[A]() extends DeliteOp[A] {
    val in: Exp[DeliteCollection[A]]
    val v: (Exp[A],Exp[A])
    val func: Exp[A]
  }

  abstract case class DeliteOpMapReduce[A,R,C[X] <: DeliteCollection[X]]() extends DeliteOp[R] {
    val in: Exp[C[A]]
    //val acc: Exp[R]
    val mV: Exp[A]

    // for accumulating each partial sum
    //val mapreduce: Exp[R] // reified of Exp[(R,A)] => Exp[R] composition of map and reduce
    val map: Exp[R] // reified of Exp[A] => Exp[R]

    // for reducing remaining partial sums
    val rV: (Exp[R],Exp[R])
    val reduce: Exp[R]
  }
}

trait BaseGenDeliteOps extends GenericNestedCodegen {
  val IR: DeliteOpsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => if (shallow) Nil else syms(s.block)
    case map: DeliteOpMap[_,_,_] => if (shallow) syms(map.in) ++ syms(map.out) else syms(map.in) ++ syms(map.out) ++ syms(map.func)
    case zip: DeliteOpZipWith[_,_,_,_] => if (shallow) syms(zip.inA) ++ syms(zip.inB) else syms(zip.inA) ++ syms(zip.inB) ++ syms(zip.func)
    case red: DeliteOpReduce[_] => if (shallow) syms(red.in) else syms(red.in) ++ syms(red.func)
    case mapR: DeliteOpMapReduce[_,_,_] => if (shallow) syms(mapR.in) else syms(mapR.in) ++ syms(mapR.map) ++ syms(mapR.reduce)
    case _ => super.syms(e)
  }

  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case s: DeliteOpSingleTask[_] => getFreeVarBlock(s.block,Nil)
    case map: DeliteOpMap[_,_,_] => getFreeVarBlock(map.func,List(map.v.asInstanceOf[Sym[_]]))
    case zip: DeliteOpZipWith[_,_,_,_] => getFreeVarBlock(zip.func,List(zip.v._1.asInstanceOf[Sym[_]], zip.v._2.asInstanceOf[Sym[_]]))
    case red: DeliteOpReduce[_] => getFreeVarBlock(red.func,List(red.v._1.asInstanceOf[Sym[_]], red.v._2.asInstanceOf[Sym[_]]))
    case mapR: DeliteOpMapReduce[_,_,_] => getFreeVarBlock(mapR.map, List(mapR.mV.asInstanceOf[Sym[_]])) ++ getFreeVarBlock(mapR.reduce, List(mapR.rV._1.asInstanceOf[Sym[_]], mapR.rV._2.asInstanceOf[Sym[_]]))
    case _ => super.getFreeVarNode(rhs)
  }

}

trait ScalaGenDeliteOps extends ScalaGenEffect with BaseGenDeliteOps {
  import IR._

  private var nested = false

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case s:DeliteOpSingleTask[_] => {
      val b = s.block
      stream.println("val " + quote(sym) + " = { ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    }
    case map:DeliteOpMap[_,_,_] => {
      if (nested == true){
        stream.println("val " + quote(sym) + " = {")
        stream.println("var mapIdx = 0")
        stream.println("while (mapIdx < " + quote(map.in) + ".length) {")
        stream.println("val " + quote(map.v) + " = " + quote(map.in) + "(mapIdx)")
        stream.println(quote(map.out) + "(mapIdx)" + "= {")
        emitBlock(map.func)
        stream.println(quote(getBlockResult(map.func)))
        stream.println("}")
        stream.println("mapIdx += 1")
        stream.println("} // end while")
        stream.println(quote(map.out))
        stream.println("}")
      }
      else {
        nested = true
        stream.println("val " + quote(sym) + "= new generated.scala.DeliteOpMap[" + remap(map.v.Type) + "," + remap(map.func.Type) + "," + remap(map.out.Type) + "] {")
        stream.println("def in = " + quote(map.in))
        stream.println("def out = " + quote(map.out))
        stream.println("def map(" + quote(map.v) + ": " + remap(map.v.Type) + ") = {")
        emitBlock(map.func)
        stream.println(quote(getBlockResult(map.func)))
        stream.println("}}")
        nested = false
      }

    }
    case zip: DeliteOpZipWith[_,_,_,_] => {
      if (nested == true){
        stream.println("val " + quote(sym) + " = {")
        stream.println("val " + quote(zip.out) + " = {")
        emitBlock(zip.out)
        stream.println(quote(getBlockResult(zip.out)))
        stream.println("}")
        stream.println("var zipIdx = 0")
        stream.println("while (zipIdx < " + quote(zip.inA) + ".length) {")
        stream.println("val " + quote(zip.v._1) + " = " + quote(zip.inA) + "(zipIdx)")
        stream.println("val " + quote(zip.v._2) + " = " + quote(zip.inB) + "(zipIdx)")
        stream.println(quote(zip.out) + "(zipIdx)" + " = {")
        emitBlock(zip.func)
        stream.println(quote(getBlockResult(zip.func)))
        stream.println("}")
        stream.println("zipIdx += 1")
        stream.println("} // end while")
        stream.println(quote(zip.out))
        stream.println("}")
      }
      else {
        nested = true
        stream.println("val " + quote(sym) + "= new generated.scala.DeliteOpZipWith[" + remap(zip.v._1.Type) + "," + remap(zip.v._2.Type) + "," + remap(zip.func.Type) + "," + remap(zip.out.Type) +"] {")
        stream.println("val " + quote(zip.out) + " = {")
        emitBlock(zip.out)
        stream.println(quote(getBlockResult(zip.out)))
        stream.println("}")
        stream.println("def inA = " + quote(zip.inA))
        stream.println("def inB = " + quote(zip.inB))
        stream.println("def out = " + quote(zip.out))
        stream.println("def zip(" + quote(zip.v._1) + ": " + remap(zip.v._1.Type) + ", " + quote(zip.v._2) + ": " + remap(zip.v._2.Type) + ") = {")
        emitBlock(zip.func)
        stream.println(quote(getBlockResult(zip.func)))
        stream.println("}}")
        nested = false
      }
    }
    case red: DeliteOpReduce[_] => {
      if (nested == true){
        stream.println("val " + quote(sym) + " = {")
        stream.println("var " + quote(red.v._1) + " = " + quote(red.in) + "(0)")
        stream.println("var reduceIdx = 1")
        stream.println("while (reduceIdx < " + quote(red.in) + ".length) {")
        stream.println("val " + quote(red.v._2) + " = " + quote(red.in) + "(reduceIdx)")
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
        nested = true
        stream.println("val " + quote(sym) + "= new generated.scala.DeliteOpReduce[" + remap(red.func.Type) + "] {")
        stream.println("def in = " + quote(red.in))
        stream.println("def reduce(" + quote(red.v._1) + ": " + remap(red.v._1.Type) + "," + quote(red.v._2) + ": " + remap(red.v._2.Type) + ") = {")
        emitBlock(red.func)
        stream.println(quote(getBlockResult(red.func)))
        stream.println("}}")
        nested = false
      }
    }
    case mapR:DeliteOpMapReduce[_,_,_] => {
      if (nested == true){
        // straight line code
        stream.println("val " + quote(sym) + " = {")
        stream.println("val " + quote(mapR.mV) + " = " + quote(mapR.in) + "(0)")
        stream.println("var " + quote(mapR.rV._1) + " = {")
        emitBlock(mapR.map)
        stream.println(quote(getBlockResult(mapR.map)))
        stream.println("}")
        stream.println("var mapReduceIdx = 1")
        stream.println("while (mapReduceIdx < " + quote(mapR.in) + ".length) {")
        stream.println("val " + quote(mapR.mV) + " = " + quote(mapR.in) + "(mapReduceIdx)")
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
        // kernel
        nested = true
        stream.println("val " + quote(sym) + "= new generated.scala.DeliteOpMapReduce[" + remap(mapR.mV.Type) + "," + remap(mapR.reduce.Type) + "] {")
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
        nested = false
      }
    }
    case _ => super.emitNode(sym,rhs)
  }
}

trait CudaGenDeliteOps extends CudaGenEffect with BaseGenDeliteOps {
  import IR._
}
