package ppl.delite.framework.ops

import java.io.{FileWriter, File, PrintWriter}
import ppl.delite.framework.DeliteCollection
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common.{TupleOpsExp, VariablesExp, EffectExp}

trait DeliteOpsExp extends EffectExp with VariablesExp {
  // representation must be reified! this places the burden on the caller, but allows the caller to avoid the
  // use of function values (which can be uglier).

  sealed abstract class DeliteOp[A]() extends Def[A]

  case class DeliteOpSingleTask[A](block: Exp[A]) extends DeliteOp[A]
  
  abstract case class DeliteOpMap[A,B,C[X] <: DeliteCollection[X]] extends DeliteOp[C[B]] {
    val in: Exp[C[A]]
    val out: Exp[C[B]]
    val v: Exp[A]    // input symbol for func
    val func: Exp[B] // reified of Exp[A] => Exp[R]
  }
  
  abstract case class DeliteOpZipWith[A,B,R,C[X] <: DeliteCollection[X]] extends DeliteOp[C[B]] {
    val inA: Exp[C[A]]
    val inB: Exp[C[B]]
    val out: Exp[C[R]]
    val v: Exp[(A,B)]
    val func: Exp[R]
  }

  abstract case class DeliteOpReduce[A] extends DeliteOp[A] {
    val in: Exp[DeliteCollection[A]]
    val v: Exp[(A,A)]
    val func: Exp[A] // reified of Exp[(A,A)] = Exp[A]
  }

  abstract case class DeliteOpMapReduce[A,R,C[X] <: DeliteCollection[X]] extends DeliteOp[R] {
    val in: Exp[C[A]]
    //val acc: Exp[R]
    val mV: Exp[A]

    // for accumulating each partial sum
    //val mapreduce: Exp[R] // reified of Exp[(R,A)] => Exp[R] composition of map and reduce
    val map: Exp[R] // reified of Exp[A] => Exp[R]

    // for reducing remaining partial sums
    val rV: Exp[(R,R)]
    val reduce: Exp[R] // reified of Exp[(R,R)] => Exp[R]
  }
}

trait BaseGenDeliteOps extends GenericNestedCodegen {
  val IR: DeliteOpsExp with TupleOpsExp
   import IR._

   override def syms(e: Any): List[Sym[Any]] = e match {
     // TODO: test -- else clause may not be necessary if productIterator still works on abstract case class
     case map:DeliteOpMap[_,_,_] if shallow => syms(map.in) ++ syms(map.out) // in shallow mode, don't count deps from nested blocks
     case map:DeliteOpMap[_,_,_] => syms(map.in) ++ syms(map.out) ++ syms(map.func)
     case mapR: DeliteOpMapReduce[_,_,_] if shallow => syms(mapR.in)
     case mapR: DeliteOpMapReduce[_,_,_] => syms(mapR.in) ++ syms(mapR.map) ++ syms(mapR.reduce)
     case _ => super.syms(e)
   }
}

trait ScalaGenDeliteOps extends ScalaGenEffect with BaseGenDeliteOps {
  import IR._

  /*
   override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case s:DeliteOpSingleTask[_] => getFreeVarBlock(s.block,Nil)
    case map:DeliteOpMap[_,_,_] => getFreeVarBlock(map.func,List(map.v.asInstanceOf[Sym[_]]))
    case mapR:DeliteOpMapReduce[_,_,_,_] => getFreeVarBlock(mapR.map, List(mapR.mV.asInstanceOf[Sym[_]])) ++ getFreeVarBlock(mapR.reduce, List(mapR.rV.asInstanceOf[Sym[_]]))
    case _ => super.getFreeVarNode(rhs)
  }
  */

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case s:DeliteOpSingleTask[_] => {
      val b = s.block
      stream.println("val " + quote(sym) + " = { ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    }
    case map:DeliteOpMap[_,_,_] =>
      /*
      stream.println("def map(" + quote(mapR.mV) + ": " + mapR.mV.Type + ") = {")
      emitBlock(mapR.map)
      stream.println(quote(getBlockResult(mapR.map)))
      stream.println("}")
      stream.println(""); stream.println("")
      */
    case mapR:DeliteOpMapReduce[_,_,_] => {
      // straight line code
      /*
      stream.println("val " + quote(sym) + " = {")
      stream.println("//map")
      stream.println("for (i <- 0 until " + quote(mapR.in) + ".length) {")
      stream.println(quote(mapR.out) + "(i) = {" )
      stream.println("val " + quote(mapR.mV) + " = " + quote(mapR.in) + "(i)")
      emitBlock(mapR.map)
      stream.println(quote(getBlockResult(mapR.map)))
      stream.println("}}")
      stream.println("//end map")

      stream.println("//reduce")
      stream.println("var acc = " + quote(mapR.out) + "(0)")
      stream.println("for (i <- 1 until " + quote(mapR.out) + ".length) {")
      stream.println("val " + quote(mapR.rV) + " = (acc," + quote(mapR.out) + "(i))")
      stream.println("acc = {")
      emitBlock(mapR.reduce)
      stream.println(quote(getBlockResult(mapR.reduce)))
      stream.println("}}}")
      stream.println("//end reduce")
      */

      // kernel
      stream.println("val " + quote(sym) + "= new generated.DeliteOpMapReduce[" + remap(mapR.mV.Type) + "," + remap(mapR.reduce.Type) + "] {")
      stream.println("def in = " + quote(mapR.in))
      stream.println("")
      //stream.println("def mapreduce(" + quote(mapR.acc) + ": " + mapR.acc.Type + ", " + quote(mapR.mV) + ": " + mapR.mV.Type + ") = {")
      //emitBlock(mapR.mapreduce)
      //stream.println(quote(getBlockResult(mapR.mapreduce)))
      stream.println("def map(" + quote(mapR.mV) + ": " + remap(mapR.mV.Type) + ") = {")
      emitBlock(mapR.map)
      stream.println(quote(getBlockResult(mapR.map)))
      stream.println("}")
      stream.println("")
      //stream.println("def reduce(" + quote(t2(mapR.rV)._1) + ": " + remap(t2(mapR.rV)._1.Type) + "," + quote(t2(mapR.rV)._2) + ": " + remap(t2(mapR.rV)._2.Type) + ") = {")
      stream.println("def reduce(" + quote(mapR.rV) + ": " + remap(mapR.rV.Type) + ") = {")
      emitBlock(mapR.reduce)
      stream.println(quote(getBlockResult(mapR.reduce)))
      stream.println("}")      
      stream.println("}")
    }
    case _ => super.emitNode(sym,rhs)
  }
}

trait CudaGenDeliteOps extends CudaGenEffect with BaseGenDeliteOps {
  import IR._
}