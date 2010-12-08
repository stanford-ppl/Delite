package ppl.dsl.optiml

import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.common.{TupleOps, NumericOps, DSLOpsExp, Base}
import scala.virtualization.lms.internal.ScalaGenEffect
import java.io.PrintWriter
import reflect.Manifest

/* Machinery provided by OptiML itself (language features and control structures).
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * created: Nov 29, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait LanguageOps extends Base { this: ArithImplicits =>
  type Zero[A]

  // TODO: type class should probable be Zeroable[A] or something
  def <>[A:Manifest:ArithOps] = optiml_zero
  
  def sum[A:Manifest:ArithOps](start: Rep[Int], end: Rep[Int])(block: Rep[Int] => Rep[A]) = optiml_sum(start, end, block)

  def optiml_sum[A:Manifest:ArithOps](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A]) : Rep[A]
  def optiml_zero[A:Manifest:ArithOps] : Rep[A]
}

trait LanguageOpsExp extends LanguageOps with TupleOps with NumericOps with VectorOps with DSLOpsExp with DeliteOpsExp
  { this: LanguageImplOps with ArithImplicits =>

  case class ZeroExp[A:Manifest:ArithOps]() extends Def[A]
  type Zero[A] = ZeroExp[A]

  case class Sum[A:Manifest:ArithOps](in: Exp[Vector[Int]], acc: Exp[A], mV: Exp[Int],
                                      mapreduce: Exp[A], rV: Exp[(A,A)], reduce: Exp[A])
    extends DeliteOpMapReduce[Int,A,Vector]

  // implemented via kernel embedding
  //case class Sum[A:Manifest:ArithOps](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[A])
    //extends DSLOp(reifyEffects(optiml_sum_impl(start, end, block)))

  // reflectEffect shouldn't be necessary; somehow CSE is kicking in across different type parameters
  def optiml_zero[A:Manifest:ArithOps] = ZeroExp[A]()//reflectEffect(ZeroExp[A]())
  
  def optiml_sum[A](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[A])
                   (implicit mA: Manifest[A], ops: ArithOps[A]) = {
    //Sum(start, end, block)
    // TODO: how does the map know the Vector.zeros is not needed, after it is optimized away in the reducing function?
    // each call to map is going to instantiate a Vector.zeros that is never used (maybe we can get around this by being lazy..)
    // actually does not get optimized out, because at this point we run the function on rV, not on its real inputs
    // unless we use mapreduce composition
    // -- and even then i think the if statement in GDA is screwing us.. the resulting vector might or might be  a zero vector in the +=
    val in = Vector.range(start, end)
    val acc = fresh[A]
    val mV = fresh[Int]
    val mapreduce = reifyEffects(ops.+=(acc, reifyEffects(block(mV))))
    val rV = fresh[(A,A)]
    val reduce = reifyEffects(ops.+=(rV._1,rV._2))

    //val map = reifyEffects(block(mV))
    //val mapreduce = reifyEffects(ops.+=(reifyEffects(block(mV)), reifyEffects(block(mV2))))

    // TODO: how to do n partial sums?
    // var acc = mapreduce(x1,x2); acc += mapreduce(x2,x3); ... acc += mapreduce(x(n-1), x(n))
    // this version allocates a new result for every pair of elements
    // need a mapreduce function that takes a list of elements? -- seems reasonable
    // or pass the accumulator into the mapreduce function -- in this case we need to be able to instantiate multiple output accs dynamically (cannot just pass one in)
    // var acc = in(0) 
    // mapreduce(acc, x1); mapreduce(acc, x2); ... mapreduce(acc, xn) for nth partial sum

    Sum(in, acc, mV, mapreduce, rV, reduce)
  }
}

trait ScalaGenLanguageOps extends ScalaGenEffect {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    val IntVec = manifest[Vector[Int]]
    val DblVec = manifest[Vector[Double]]

    rhs match {
      // TODO: this is not working right
      case z@ZeroExp() => toAtom(z).Type match {
                          case IntVec => emitValDef(sym, "IntZeroVector")
                          case DblVec => emitValDef(sym, "DoubleZeroVector")
                          case Manifest.Int => emitValDef(sym, "0")
                          case Manifest.Double => emitValDef(sym, "0.0")
                          case _ => throw new UnsupportedOperationException("ZeroVector")
                        }
      case _ => super.emitNode(sym, rhs)
    }
  }
}
