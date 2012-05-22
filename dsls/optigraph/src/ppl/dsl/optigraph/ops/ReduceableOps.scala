package ppl.dsl.optigraph.ops

import java.io.{PrintWriter}
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen}
import ppl.dsl.optigraph._

trait ReduceableOps extends Variables {
  this: OptiGraph => 
  
  implicit def repReduceableToReduceableOps[T:Manifest](r: Rep[Reduceable[T]]) = new ReduceableOpsCls(r)
  implicit def repReduceableToVal[T:Manifest](r: Rep[Reduceable[T]]):Rep[T] = r.value
  //implicit def varReduceableToVal[T:Manifest](r: Var[Reduceable[T]]):Rep[T] = r.value
  implicit def varToReduceableOps[T:Manifest](r: Var[Reduceable[T]]) = new ReduceableOpsCls(readVar(r))
  //implicit def varToReduceableOps[T:Manifest](r: Ref[Reduceable[T]]) = new ReduceableOpsCls(readVar(r))
  
  /** Reduceable constructors */
  object Reduceable {
    /** Creates a new Reduceable with initial value init */
    def apply[T:Manifest](init:Rep[T]) = red_new(init)
  }
  
  /** Operations on Reduceables */
  class ReduceableOpsCls[T:Manifest](r: Rep[Reduceable[T]]) {
    /** Returns the current value of r*/
    def value: Rep[T] = red_getvalue(r)
    /** Sets the current value of r to v */
    def setValue(v: Rep[T]): Rep[Unit] = red_setvalue(r,v)
    
    /** Reduction assignments */
    // sum
    def +=(v: Rep[T])(implicit a: Numeric[T]): Rep[Unit] = red_sum(r,v)
    // product
    def *=(v: Rep[T])(implicit a: Numeric[T]): Rep[Unit] = red_product(r,v)
    // min
    def <=(v: Rep[T])(implicit a: Ordering[T]):Rep[Unit] = red_min(r,v)
    // max
    def >=(v: Rep[T])(implicit a: Ordering[T]): Rep[Unit] = red_max(r,v)
    // count (TODO: how to constrain method to work on Reduceable[Int] only?)
    def ++=(v: Rep[Boolean]) = red_count(r.asInstanceOf[Rep[Reduceable[Int]]],v)
    // all (boolean AND)
    def &&=(v: Rep[Boolean]): Rep[Unit] = red_all(r.asInstanceOf[Rep[Reduceable[Boolean]]],v)
    // any (boolean OR)
    def ||=(v: Rep[Boolean]): Rep[Unit] = red_any(r.asInstanceOf[Rep[Reduceable[Boolean]]],v)
  }
  
  def red_new[T:Manifest](init: Rep[T]): Rep[Reduceable[T]]
  def red_getvalue[T:Manifest](r: Rep[Reduceable[T]]): Rep[T]
  def red_setvalue[T:Manifest](r: Rep[Reduceable[T]], v: Rep[T]): Rep[Unit]
  def red_sum[T:Manifest:Numeric](r: Rep[Reduceable[T]], v: Rep[T]): Rep[Unit]
  def red_product[T:Manifest:Numeric](r: Rep[Reduceable[T]], v: Rep[T]): Rep[Unit]
  def red_count(r: Rep[Reduceable[Int]], v: Rep[Boolean]): Rep[Unit]
  def red_min[T:Manifest:Ordering](r: Rep[Reduceable[T]], v: Rep[T]): Rep[Unit]
  def red_max[T:Manifest:Ordering](r: Rep[Reduceable[T]], v: Rep[T]): Rep[Unit]
  def red_all(r: Rep[Reduceable[Boolean]], v: Rep[Boolean]): Rep[Unit]
  def red_any(r: Rep[Reduceable[Boolean]], v: Rep[Boolean]): Rep[Unit]
}

trait ReduceableOpsExp extends ReduceableOps with EffectExp {
  this: OptiGraphExp =>
 
  case class RedObjectNew[T](init: Rep[T])(val mR: Manifest[Reduceable[T]]) extends Def[Reduceable[T]]
  case class RedGetValue[T:Manifest](r: Exp[Reduceable[T]]) extends Def[T]
  case class RedSetValue[T:Manifest](r: Exp[Reduceable[T]], x: Rep[T]) extends Def[Unit]
  case class RedSetOutput[T:Manifest](r:Exp[Reduceable[T]], out: Exp[T], reduceOpString: String, dep: Exp[Unit]) extends Def[Unit]
  
  abstract class ReductionOp[T:Manifest]() extends Def[Unit] {
    val m = manifest[T]
    val rv: Exp[Reduceable[T]]
    val rhs: Exp[T]
    val zero: Exp[T]
    def reduce: (Exp[T], Exp[T]) => Exp[T]
    // reduceStr stores the String representation of the reduction operation
    // in order to combine the result of the parallel block with the initial value
    // TODO: there must be a better way to do this
    def reduceStr: String
  }
  
  case class RedSum[T:Manifest:Numeric](rv: Exp[Reduceable[T]], v: Exp[T]) extends ReductionOp[T] {
    val n = implicitly[Numeric[T]]   
    val rhs = v
    val zero = unit(0.asInstanceOf[T])
    def reduce: (Exp[T],Exp[T])=>Exp[T] = (a,b) => a + b
    def reduceStr: String = "(a,b) => a + b"
  }
  
  case class RedProd[T:Manifest:Numeric](rv: Exp[Reduceable[T]], v: Exp[T]) extends ReductionOp[T] {
    val n = implicitly[Numeric[T]]
    val rhs = v
    val zero = unit(1.asInstanceOf[T])
    def reduce: (Exp[T],Exp[T])=>Exp[T] = (a,b) => a * b
    def reduceStr: String = "(a,b) => a * b"
  }
  
  case class RedCount(rv: Exp[Reduceable[Int]], v: Exp[Boolean]) extends ReductionOp[Int] {
    val rhs = if(v) { unit(1) } else { unit(0) }
    val zero = unit(0)
    def reduce = (a,b) => a + b
    def reduceStr: String = "(a,b) => a + b"
  }
  
  case class RedMax[T:Manifest:Ordering](rv: Exp[Reduceable[T]], v: Exp[T]) extends ReductionOp[T] {
    val n = implicitly[Ordering[T]]
    val rhs = v
    val mT: Manifest[T] = manifest[T]
    val zero = (mT match {
      case Manifest.Double => MIN_DOUBLE
      case Manifest.Float => MIN_FLOAT
      case Manifest.Int =>  MIN_INT
      case _ => throw new RuntimeException()
    }).asInstanceOf[Exp[T]]
    def reduce = (a,b) => if (a > b) a else b
    def reduceStr: String = "(a,b) => if (a > b) a else b"
  }
  
  case class RedMin[T:Manifest:Ordering](rv: Exp[Reduceable[T]], v: Exp[T]) extends ReductionOp[T] {
    val n = implicitly[Ordering[T]]
    val rhs = v
    val mT: Manifest[T] = manifest[T]
    val zero = (mT match {
      case Manifest.Double => MAX_DOUBLE
      case Manifest.Float => MAX_FLOAT
      case Manifest.Int =>  MAX_INT
      case _ => throw new RuntimeException()
    }).asInstanceOf[Exp[T]]
    def reduce = (a,b) => if (a < b) a else b
    def reduceStr: String = "(a,b) => if (a < b) a else b"
  }
  
  case class RedAll(rv: Exp[Reduceable[Boolean]], v: Exp[Boolean]) extends ReductionOp[Boolean] {
    val rhs = v
    val zero = unit(true)
    def reduce = (a,b) => a && b
    def reduceStr: String = "(a,b) => a && b"
  }
  
  case class RedAny(rv: Exp[Reduceable[Boolean]], v: Exp[Boolean]) extends ReductionOp[Boolean] {
    val rhs = v
    val zero = unit(false)
    def reduce = (a,b) => a || b
    def reduceStr: String = "(a,b) => a || b"
  }
  
  def red_new[T:Manifest](init: Exp[T]) = reflectMutable(RedObjectNew(init)(manifest[Reduceable[T]]))
  def red_getvalue[T:Manifest](r: Exp[Reduceable[T]]) = reflectPure(RedGetValue(r))
  def red_setvalue[T:Manifest](r: Exp[Reduceable[T]], x: Exp[T]) = reflectWrite(r)(RedSetValue(r,x))
   
  def red_sum[T:Manifest:Numeric](r: Exp[Reduceable[T]], v: Exp[T]) = reflectEffect(RedSum(r,v))
  def red_product[T:Manifest:Numeric](r: Exp[Reduceable[T]], v: Exp[T]) = reflectWrite(r)(RedProd(r,v))
  def red_count(r: Exp[Reduceable[Int]], v: Exp[Boolean]) = reflectWrite(r)(RedCount(r,v))
  def red_min[T:Manifest:Ordering](r: Exp[Reduceable[T]], v: Exp[T]) = reflectWrite(r)(RedMin(r,v))
  def red_max[T:Manifest:Ordering](r: Exp[Reduceable[T]], v: Exp[T]) = reflectWrite(r)(RedMax(r,v))
  def red_all(r: Exp[Reduceable[Boolean]], v: Exp[Boolean]) = reflectWrite(r)(RedAll(r,v))
  def red_any(r: Exp[Reduceable[Boolean]], v: Exp[Boolean]) = reflectWrite(r)(RedAny(r,v))
  
  /*override def syms(e: Any): List[Sym[Any]] = e match {
    case RedSetOutput(r, out, dep) => syms(r)// ::: deps.flatMap(syms) 
    case _ => super.syms(e)
  }
  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
   
    case RedSetOutput(r, out, dep) => freqNormal(r) //::: freqNormal(out) //::: body.flatMap(freqHot)
    case _ => super.symsFreq(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case RedSetOutput(r, out, dep) => readSyms(r) ::: readSyms(out)///::: body.flatMap(readSyms)
    case _ => super.readSyms(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case RedSetOutput(r, out, dep) => Nil//out.asInstanceOf[Sym[Any]] :: scala.List() //v :: body.flatMap(boundSyms)
    case _ => super.boundSyms(e)
  }*/
  
}

trait BaseGenReduceableOps extends GenericNestedCodegen {
  val IR: ReduceableOpsExp
  import IR._

}

trait ScalaGenReduceableOps extends BaseGenReduceableOps with ScalaGenBase {
  val IR: ReduceableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case r@RedObjectNew(init) => emitValDef(sym, "new " + remap(r.mR) + "(" + quote(init) +")")
      case RedSetValue(r,x) => emitValDef(sym, quote(r) + ".setValue("+ quote(x) + ")")
      case RedGetValue(r) => emitValDef(sym, quote(r) + ".getValue()")
      //case RedSetOutput(r, out, reduceOp, dep) => emitValDef(sym, quote(r) + ".reduce("+ quote(reduceOp) + "," + quote(out) + ")")
      case RedSetOutput(r, out, reduceOp, dep) => emitValDef(sym, quote(r) + ".combine( " + reduceOp + "," + quote(out) + ")") 
      case _ => super.emitNode(sym, rhs)
    }
  }
}

