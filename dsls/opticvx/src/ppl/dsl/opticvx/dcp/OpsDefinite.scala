package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

import ppl.dsl.opticvx.solvers._
import ppl.dsl.opticvx.solverir._


case class MatrixDefinite(m: Int, n: Int, data: Seq[Double]) {
  if(data.length != m*n) throw new IRValidationException()
  def getat(i: Int, j: Int): Double = {
    if((i < 0)||(i >= m)||(j < 0)||(j >= n)) throw new IRValidationException()
    data(i + m*j)
  }
  def mmpy(x: Seq[Double]) = {
    for (i <- 0 until m) yield {
      (0 until m).foldLeft(0.0)((a, j) => a + getat(i, j)*x(j))
    }
  }
  def mmpyT(x: Seq[Double]) = {
    for (i <- 0 until m) yield {
      (0 until m).foldLeft(0.0)((a, j) => a + getat(j, i)*x(j))
    }
  }
}

object SolverRuntimeDefinite extends SolverRuntime[Int, MatrixDefinite, MultiSeq[MatrixDefinite], Seq[Double], MultiSeq[Seq[Double]]] {
  //INTEGER OPERATIONS
  def intlikei: IntLike[Int] = IntLikeInt

  //VECTOR OPERATIONS
  //base objects
  def size(arg: Seq[Double]): Int = arg.length
  def zero(size: Int): Seq[Double] = for(i <- 0 until size) yield 0.0
  def one: Seq[Double] = Seq(1.0)
  //linear operators
  def sum(arg1: Seq[Double], arg2: Seq[Double]): Seq[Double] = {
    if(arg1.length != arg2.length) throw new IRValidationException()
    for(i <- 0 until arg1.length) yield arg1(i) + arg2(i)
  }
  def sumfor(len: Int, size: Int, arg: (Int => Seq[Double])): Seq[Double] = {
    var rv: Seq[Double] = for (i <- 0 until size) yield 0.0
    for(j <- 0 until len) {
      val aj = arg(j)
      rv = for (i <- 0 until size) yield rv(i) + aj(i)
    }
    rv
  }
  def neg(arg: Seq[Double]): Seq[Double] = {
    for(a <- arg) yield -a
  }
  def scaleconstant(arg: Seq[Double], scale: Double): Seq[Double] = {
    for(a <- arg) yield a*scale
  }
  def cat(arg1: Seq[Double], arg2: Seq[Double]): Seq[Double] = {
    arg1 ++ arg2
  }
  def catfor(len: Int, arg: (Int => Seq[Double])): Seq[Double] = {
    var rv: Seq[Double] = Seq()
    for(j <- 0 until len) {
      rv = rv ++ arg(j)
    }
    rv
  }
  def slice(arg: Seq[Double], at: Int, size: Int): Seq[Double] = {
    arg.slice(at, at + size)
  }
  //nonlinear operators
  def dot(arg1: Seq[Double], arg2: Seq[Double]): Seq[Double] = {
    if(arg1.length != arg2.length) throw new IRValidationException()
    Seq((0 until arg1.length).foldLeft(0.0)((a, i) => a + arg1(i)*arg2(i)))
  }
  def mpy(arg: Seq[Double], scale: Seq[Double]): Seq[Double] = {
    if(scale.length != 1) throw new IRValidationException()
    for (a <- arg) yield a * scale(0)
  }
  def div(arg: Seq[Double], scale: Seq[Double]): Seq[Double] = {
    if(scale.length != 1) throw new IRValidationException()
    for (a <- arg) yield a / scale(0)
  }
  def norm2(arg: Seq[Double]): Seq[Double] = {
    Seq(arg.foldLeft(0.0)((a, x) => a + x*x))
  }
  def sqrt(arg: Seq[Double]): Seq[Double] = {
    if(arg.length != 1) throw new IRValidationException()
    Seq(scala.math.sqrt(arg(0)))
  }
  def max(arg1: Seq[Double], arg2: Seq[Double]): Seq[Double] = {
    if(arg1.length != arg2.length) throw new IRValidationException()
    for (i <- 0 until arg1.length) yield scala.math.max(arg1(i), arg2(i))
  }
  def min(arg1: Seq[Double], arg2: Seq[Double]): Seq[Double] = {
    if(arg1.length != arg2.length) throw new IRValidationException()
    for (i <- 0 until arg1.length) yield scala.math.min(arg1(i), arg2(i))
  }


  def matrixmpy(m: MatrixDefinite, x: Seq[Double]): Seq[Double] = m.mmpy(x)
  def matrixmpytranspose(m: MatrixDefinite, x: Seq[Double]): Seq[Double] = m.mmpyT(x)

  def matrixget(mats: MultiSeq[MatrixDefinite], at: Seq[Int]): MatrixDefinite = mats(at)

  def vectorget(vecs: MultiSeq[Seq[Double]], at: Seq[Int]): Seq[Double] = vecs(at)
  def vectorset(src: Seq[Double], vecs: MultiSeq[Seq[Double]], at: Seq[Int]): MultiSeq[Seq[Double]] = vecs.updated(at, src)

  def converge(memory: Seq[MultiSeq[Seq[Double]]], body: (Seq[MultiSeq[Seq[Double]]]) => (Seq[MultiSeq[Seq[Double]]], Seq[Double])): Seq[MultiSeq[Seq[Double]]] = {
    var m = memory
    var cond: Boolean = true
    while(cond) {
      val (nm, v) = body(m)
      cond = (v.foldLeft(0.0)((a, x) => a + x*x) >= 1e-12)
      m = nm
    }
    m
  }
  def runfor(len: Int, memory: Seq[MultiSeq[Seq[Double]]], body: (Int, Seq[MultiSeq[Seq[Double]]]) => Seq[MultiSeq[Seq[Double]]]): Seq[MultiSeq[Seq[Double]]] = {
    var m = memory
    for(i <- 0 until len) {
      m = body(i, m)
    }
    m
  }
}

// trait DCPOpsDefinite extends DCPOps {

 
//   type ParamDesc = Int
//   type InputDesc = InputDescDefinite
//   type ExprRT = Seq[Double]

//   case class InputDescDefinite(val size: IRPoly, val data: (Int)=>Double) extends HasSize

//   implicit def double2inputdesc(x: Double) = InputDescDefinite(IRPoly.const(1, globalArity),
//     (i: Int) => {
//       if(i != 0) throw new IRValidationException()
//       x
//       })

//   def vector_input(size: Int)(data: (Int)=>Double) = InputDescDefinite(IRPoly.const(size, globalArity), data)
//   def vector_input(size: IRPoly)(data: (Int)=>Double) = InputDescDefinite(size, data)

//   override def postsolve(problem: Problem, params: Seq[Int], inputs: Seq[InputDescDefinite], syms: Seq[Symbol[Expr, ExprRT]]) {
//     val tt = PrimalDualSubgradient.Gen(problem).solver

//     var vvinputs: Seq[Double] = Seq()
//     for (i <- inputs) {
//       vvinputs = vvinputs ++ ((0 until i.size.eval(params)(IntLikeInt)) map i.data)
//     }

//     val vv = tt.run(params, vvinputs)

//     for(s <- syms) {
//       val x = s.binding
//       val scontext = SolverContext(tt.input, Seq(tt.variables(0)))
//       val avlsv = AVectorLikeSVector(scontext)
//       val sv = SVectorAdd(
//         x.offset.translate(avlsv),
//         x.almap.mmpy(SVectorRead(scontext, 0): SVector)(avlsv))
//       s.rset(sv.eval(params, vvinputs, Seq(vv(0))))
//     }
//   }

// }
