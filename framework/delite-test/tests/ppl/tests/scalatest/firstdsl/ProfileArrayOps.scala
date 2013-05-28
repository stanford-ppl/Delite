package ppl.tests.scalatest.firstdsl

import reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common.{NumericOpsExp, FractionalOpsExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteCollectionOpsExp,DeliteOpsExp}
import ppl.delite.framework.ops.DeliteCollection

trait ProfileArrayOps extends Base {
  // a simple way of enumerating choices in our syntax
  class Reporter
  object average extends Reporter
  object median extends Reporter

  // add report and length methods to Rep[ProfileArray]
  def infix_report(x: Rep[ProfileArray], y: Reporter) = profile_report(x, y)
  def infix_length(x: Rep[ProfileArray]) = profile_length(x)

  // implementation
  def profile_report(x: Rep[ProfileArray], y: Reporter): Rep[Double]
  def profile_length(x: Rep[ProfileArray]): Rep[Int]
}

trait ProfileArrayOpsExp extends ProfileArrayOps with NumericOpsExp
  with FractionalOpsExp with DeliteCollectionOpsExp with DeliteOpsExp {

  // a Delite parallel operation! was it really that easy?
  case class ReportSum(in: Exp[ProfileArray]) 
    extends DeliteOpReduce[Double] {
      val zero = unit(0.0)
      val size = copyTransformedOrElse(_.size)(in.length)
      def func = (a,b) => a + b
  }

  // median is a little trickier, let's just be sequential
  case class ReportMedian(in: Exp[ProfileArray]) extends Def[Double]

  // length, apply, update need to reference the underlying data structure
  case class ProfileLength(in: Exp[ProfileArray]) extends Def[Int]
  case class ProfileApply(in: Exp[ProfileArray], n: Exp[Int]) extends Def[Double]
  case class ProfileUpdate(in: Exp[ProfileArray], n: Exp[Int], y: Exp[Double]) 
    extends Def[Unit]

  /////////////////////
  // delite collection

  def isProfileArray[A](x: Exp[DeliteCollection[A]]) = x.isInstanceOf[Exp[ProfileArray]]
  def asProfileArray[A](x: Exp[DeliteCollection[A]]) = x.asInstanceOf[Exp[ProfileArray]]

  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])
    (implicit ctx: SourceContext) = {

    if (isProfileArray(x)) asProfileArray(x).length
    else super.dc_size(x)
  }

  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])
    (implicit ctx: SourceContext) = {

    if (isProfileArray(x)) (profile_apply(asProfileArray(x),n)).asInstanceOf[Exp[A]]
    else super.dc_apply(x,n)
  }

  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])
    (implicit ctx: SourceContext) = {

    if (isProfileArray(x)) profile_update(asProfileArray(x),n,y.asInstanceOf[Exp[Double]])
    else super.dc_update(x,n,y)
  }

  def profile_report(x: Exp[ProfileArray], y: Reporter) = y match {
    case this.average => ReportSum(x) / x.length   // inline
    case this.median => ReportMedian(x)
    case _ => throw new IllegalArgumentException("unknown report type")
  }
  def profile_length(x: Exp[ProfileArray]) = ProfileLength(x)
  def profile_apply(x: Exp[ProfileArray], n: Exp[Int]): Exp[Double]
    = ProfileApply(x,n)
  def profile_update(x: Exp[ProfileArray], n: Exp[Int], y: Exp[Double]) 
    = ProfileUpdate(x,n,y)
}

trait ScalaGenProfileArrayOps extends ScalaGenBase {
  val IR: ProfileArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) =
    rhs match {
      case ReportMedian(x) =>
        val a = quote(x)
        val size = a + "._numMeasurements"
        stream.println("val " + quote(sym) + " = {")
        stream.println("val d = new Array[Double]("+size+")")
        stream.println("System.arraycopy("+a+"._data, 0, d, 0, "+size+")")
        stream.println("scala.util.Sorting.quickSort(d)")
        stream.println("d(Math.ceil("+size+"/2).asInstanceOf[Int])")
        stream.println("}")
      case ProfileLength(x) => emitValDef(sym, quote(x) + "._numMeasurements")
      case ProfileApply(x,n) => emitValDef(sym, quote(x) + "._data(" + quote(n) + ")")
      case ProfileUpdate(x,n,y) => 
         emitValDef(sym, quote(x) + "._data(" + quote(n) + ") = " + quote(y))
      case _ => super.emitNode(sym, rhs)
    }
}