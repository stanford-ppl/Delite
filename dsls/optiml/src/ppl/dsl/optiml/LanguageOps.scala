package ppl.dsl.optiml

import datastruct.scala.{Vector,Matrix}
import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import reflect.Manifest
import scala.virtualization.lms.internal.{GenericNestedCodegen, CudaGenBase, ScalaGenEffect}
import scala.virtualization.lms.common._

/* Machinery provided by OptiML itself (language features and control structures).
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * created: Nov 29, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait LanguageOps extends Base { this: OptiML =>

  /**
   * random
   */

  private val INITIAL_SEED = 100

  // TODO: all of this needs to be lifted.. but how (and where) to generate the random vars?
  // maybe we keep them in global pre-generated datastructure (optiml's predef?)

  // these are thread-safe
  private var _randRef = new scala.util.Random(INITIAL_SEED)
  private var _intRandRef = new scala.util.Random(INITIAL_SEED)

  // this version is for optiml's use exclusively, so it does not interfere with application behavior
  private def _random[A](implicit mA: Manifest[A]): Rep[A] =
    mA match {
      case Manifest.Double => unit(_intRandRef.nextDouble().asInstanceOf[A])
      case Manifest.Float => unit(_intRandRef.nextFloat().asInstanceOf[A])
      case Manifest.Int => unit(_intRandRef.nextInt().asInstanceOf[A])
      case Manifest.Long => unit(_intRandRef.nextLong().asInstanceOf[A])
      case Manifest.Boolean => unit(_intRandRef.nextBoolean().asInstanceOf[A])
      case _ => throw new UnsupportedOperationException()
  }

  // public version for application use
  def random[A](implicit mA: Manifest[A]): Rep[A] =
    mA match {
      case Manifest.Double => unit(_randRef.nextDouble().asInstanceOf[A])
      case Manifest.Float => unit(_randRef.nextFloat().asInstanceOf[A])
      case Manifest.Int => unit(_randRef.nextInt().asInstanceOf[A])
      case Manifest.Long => unit(_randRef.nextLong().asInstanceOf[A])
      case Manifest.Boolean => unit(_randRef.nextBoolean().asInstanceOf[A])
      case _ => throw new UnsupportedOperationException()
  }

  def randomInt(end: Int) = unit(_randRef.nextInt(end))
  def randomGaussian = unit(_randRef.nextGaussian())

  def reseed {
    // reseeds for all threads
    //_randRef = new _randCls
    _randRef.setSeed(INITIAL_SEED)
    _intRandRef.setSeed(INITIAL_SEED)
  }

  /**
   * Zero
   */
  // TODO: type class should probable be Zeroable[A] or something
  //def <>[A:Manifest:Arith] = optiml_zero

  /**
   * Sum
   */
  def sum[A:Manifest:Arith](start: Rep[Int], end: Rep[Int])(block: Rep[Int] => Rep[A]) = optiml_sum(start, end, block)

  def optiml_sum[A:Manifest:Arith](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A]) : Rep[A]


  /**
   * IndexVector construction
   */
  implicit def intToIndexOp(i : Int) = new IndexOp(i)
  implicit def repIntToIndexOp(i : Rep[Int]) = new IndexOp(i)

  class IndexOp(val _end : Rep[Int]) {
    def ::(_start : Int) = indexvector_range(_start, _end)
    def ::(_start : Rep[Int]) = indexvector_range(_start, _end)
  }


  /**
   *  Profiling
   */
  // lightweight profiling, matlab style
  def tic() = profile_start()
  def toc() = profile_stop()

  def profile_start() : Rep[Unit]
  def profile_stop() : Rep[Unit]
}

trait LanguageOpsExp extends LanguageOps with EffectExp {
  this: OptiMLExp with LanguageImplOps =>

  case class ProfileStart() extends Def[Unit]
  case class ProfileStop() extends Def[Unit]
  
  def profile_start() = reflectEffect(ProfileStart())
  def profile_stop() = reflectEffect(ProfileStop())

  case class Sum[A](start: Exp[Int], end: Exp[Int], mV: Exp[Int], map: Exp[A])(implicit mA: Manifest[A], ops: Arith[A])
    extends DeliteOpMapReduce[Int,A,Vector] {

    val in = Vector.range(start, end)
    val rV = (fresh[A],fresh[A])
    val reduce = reifyEffects(ops.+=(rV._1,rV._2))
    //val mapreduce = reifyEffects(ops.+=(acc, reifyEffects(block(mV))))
  }

  def optiml_sum[A](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[A])
                   (implicit mA: Manifest[A], ops: Arith[A]) = {

    val mV = fresh[Int]
    val map = reifyEffects(block(mV))
    Sum(start, end, mV, map)
  }
}

trait BaseGenLanguageOps extends GenericNestedCodegen {
  val IR: LanguageOpsExp
  import IR._

  /*
  override def syms(e: Any): List[Sym[Any]] = e match {
    case _ => super.syms(e)
  }

  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case _ => super.getFreeVarNode(rhs)
  }
  */
}

trait ScalaGenLanguageOps extends ScalaGenEffect with BaseGenLanguageOps {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case ProfileStart() => emitValDef(sym, "ppl.delite.runtime.profiler.Stopwatch.start()")
      case ProfileStop() => emitValDef(sym, "ppl.delite.runtime.profiler.Stopwatch.stop()")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

/*
trait CudaGenLanguageOps extends CudaGenBase with BaseGenLanguageOps {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
     }
  }
}
*/