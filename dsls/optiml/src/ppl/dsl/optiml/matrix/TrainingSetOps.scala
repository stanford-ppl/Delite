package ppl.dsl.optiml.matrix

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{Variables, Base, BaseExp, CGenBase, CudaGenBase, OpenCLGenBase, ScalaGenBase}
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optiml._

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optiml._

trait TrainingSetOps extends Variables with OverloadHack {
  this: OptiML =>

  object SupervisedTrainingSet {
    def apply[A:Manifest,B:Manifest](xs: Rep[DenseMatrix[A]], labels: Rep[DenseVector[B]])(implicit ctx: SourceContext) = supervised_trainingset_obj_fromMat(xs, labels)
  }
  
  object UnsupervisedTrainingSet {
    def apply[A:Manifest](xs: Rep[DenseMatrix[A]])(implicit ctx: SourceContext) = unsupervised_trainingset_obj_fromMat(xs)
  }

  implicit def reptoTrainingSetOps[A:Manifest](x: Rep[TrainingSet[A]]) = new TrainingSetOpsCls(x)
  implicit def varToTrainingSetOps[A:Manifest](x: Var[TrainingSet[A]]) = new TrainingSetOpsCls(readVar(x))
  

  /* Interface is not needed here because the implementation and return signature of these methods is identical for all TrainingSets. */
  class TrainingSetOpsCls[A:Manifest](x: Rep[TrainingSet[A]]) {
    def numSamples(implicit ctx: SourceContext) = trainingset_get_data(x).numRows
    def numFeatures(implicit ctx: SourceContext) = trainingset_get_data(x).numCols    
    def apply(i: Rep[Int], j: Rep[Int]) = trainingset_get_data(x).apply(i,j)
    def apply(i: Rep[Int]) = trainingset_get_data(x).getRow(i)
    def data = trainingset_get_data(x)
  }

  implicit def reptoUnsupervisedTrainingSetOps[A:Manifest](x: Rep[UnsupervisedTrainingSet[A]]) = new UnsupervisedTrainingSetOpsCls(x)
  implicit def varToUnsupervisedTrainingSetOps[A:Manifest](x: Var[UnsupervisedTrainingSet[A]]) = new UnsupervisedTrainingSetOpsCls(readVar(x))
  
  class UnsupervisedTrainingSetOpsCls[A:Manifest](x: Rep[UnsupervisedTrainingSet[A]]) {
    def t = UnsupervisedTrainingSet(trainingset_get_data(x).t)
  }

  implicit def reptoSupervisedTrainingSetOps[A:Manifest,B:Manifest](x: Rep[SupervisedTrainingSet[A,B]]) = new SupervisedTrainingSetOpsCls(x)
  implicit def varToSupervisedTrainingSetOps[A:Manifest,B:Manifest](x: Var[SupervisedTrainingSet[A,B]]) = new SupervisedTrainingSetOpsCls(readVar(x))
  
  class SupervisedTrainingSetOpsCls[A:Manifest,B:Manifest](x: Rep[SupervisedTrainingSet[A,B]]) {
    def t = SupervisedTrainingSet(trainingset_get_data(x).t, labels)
    def labels = supervised_trainingset_get_labels(x)        
  }
      
  // object defs
  def supervised_trainingset_obj_fromMat[A:Manifest,B:Manifest](xs: Rep[DenseMatrix[A]], labels: Rep[DenseVector[B]])(implicit ctx: SourceContext): Rep[SupervisedTrainingSet[A,B]]
  def unsupervised_trainingset_obj_fromMat[A:Manifest](xs: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[UnsupervisedTrainingSet[A]]
  
  // class defs
  def trainingset_get_data[A:Manifest](x: Rep[TrainingSet[A]])(implicit ctx: SourceContext): Rep[DenseMatrix[A]]
  def supervised_trainingset_get_labels[A:Manifest,B:Manifest](x: Rep[SupervisedTrainingSet[A,B]])(implicit ctx: SourceContext): Rep[DenseVector[B]]
}

trait TrainingSetOpsExp extends TrainingSetOps with BaseExp { this: DeliteOpsExp with OptiMLExp =>  
  // implemented via method on real data structure
  case class SupervisedTrainingSetObjectFromMat[A:Manifest,B:Manifest](xs: Exp[DenseMatrix[A]], labels: Exp[DenseVector[B]]) extends Def[SupervisedTrainingSet[A,B]] {
     val mA = manifest[A]
     val mB = manifest[B]
  }
  
  case class UnsupervisedTrainingSetObjectFromMat[A:Manifest](xs: Exp[DenseMatrix[A]]) extends Def[UnsupervisedTrainingSet[A]] {
     val mA = manifest[A]
  }
  
  case class TrainingSetGetData[A:Manifest](x: Exp[TrainingSet[A]]) extends Def[DenseMatrix[A]] {
    val mA = manifest[A]
  }
    
  case class SupervisedTrainingSetGetLabels[A:Manifest,B:Manifest](x: Exp[SupervisedTrainingSet[A,B]]) extends Def[DenseVector[B]] {
    val mA = manifest[A]
    val mB = manifest[B]
  }

  def supervised_trainingset_obj_fromMat[A:Manifest,B:Manifest](xs: Exp[DenseMatrix[A]], labels: Exp[DenseVector[B]])(implicit ctx: SourceContext) = SupervisedTrainingSetObjectFromMat(xs, labels)
  def unsupervised_trainingset_obj_fromMat[A:Manifest](xs: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = UnsupervisedTrainingSetObjectFromMat(xs)
  
  def trainingset_get_data[A:Manifest](x: Exp[TrainingSet[A]])(implicit ctx: SourceContext) = TrainingSetGetData(x)
  def supervised_trainingset_get_labels[A:Manifest,B:Manifest](x: Exp[SupervisedTrainingSet[A,B]])(implicit ctx: SourceContext) = SupervisedTrainingSetGetLabels(x)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@SupervisedTrainingSetGetLabels(x) => supervised_trainingset_get_labels(f(x))(e.mA,e.mB,implicitly[SourceContext])
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}


trait ScalaGenTrainingSetOps extends ScalaGenBase {
  val IR: TrainingSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case t@SupervisedTrainingSetObjectFromMat(xs, labels) => emitValDef(sym, "new " + remap("generated.scala.SupervisedTrainingSet[" + remap(t.mA) + "," + remap(t.mB) + "]") + "(" + quote(xs) + "," + quote(labels) + ")")
    case t@UnsupervisedTrainingSetObjectFromMat(xs) => emitValDef(sym, "new " + remap("generated.scala.UnsupervisedTrainingSet[" + remap(t.mA) + "]") + "(" + quote(xs) + ")")
    case TrainingSetGetData(x) => emitValDef(sym, quote(x) + "._data")
    case SupervisedTrainingSetGetLabels(x) => emitValDef(sym, quote(x) + "._labels")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenTrainingSetOps extends CudaGenBase {
  val IR: TrainingSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    // case TrainingSetObjectFromMat(xs, labels) => throw new GenerationFailedException("CudaGen: TrainingSet Cannot be generated from GPU")
    // case TrainingSetTransposed(x) => emitValDef(sym, "(*"+quote(x) + ".transposed)")
    //case TrainingSetLabels(x) => emitValDef(sym, quote(x) + ".labels")
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenTrainingSetOps extends OpenCLGenBase {
  val IR: TrainingSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // case TrainingSetObjectFromMat(xs, labels) => throw new GenerationFailedException("OpenCLGen: TrainingSet Cannot be generated from GPU")
    // //case TrainingSetTransposed(x) => emitValDef(sym, "(*"+quote(x) + ".transposed)")
    // case TrainingSetTransposed(x) => emitValDef(sym, "%s_transposed(%s)".format(remap(sym.Type),quote(x)))
    // case TrainingSetLabels(x) => emitValDef(sym, "%s_labels(%s)".format(remap(sym.Type),quote(x)))
    case _ => super.emitNode(sym,rhs)
  }
}

trait CGenTrainingSetOps extends CGenBase {
  val IR: TrainingSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    //case t@TrainingSetObjectFromMat(xs, labels) => emitValDef(sym, "new " + remap(t.mM) + "(" + quote(xs) + "," + quote(labels) + ")")
    // case TrainingSetTransposed(x) => emitValDef(sym, quote(x) + ".transposed")
    // case TrainingSetLabels(x) => emitValDef(sym, quote(x) + ".labels")
    case _ => super.emitNode(sym, rhs)
  }
}
