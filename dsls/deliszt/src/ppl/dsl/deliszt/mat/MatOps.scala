package ppl.dsl.deliszt.mat

import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication,DSLType}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException}

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.Config

import ppl.dsl.deliszt.{DeLisztExp,DeLiszt}
import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import ppl.dsl.deliszt.datastruct.scala._

trait MatOps extends DSLType with Variables {
  this: DeLiszt with MetaInteger =>

  object Mat {
    def apply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest]() = mat_obj_n_new[R,C,A](MIntDepth[R], MIntDepth[C])
    def apply[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](r: Rep[Int], c: Rep[Int]) = mat_obj_n_new[R,C,A](r,c)
  }

  implicit def repMatToMatOps[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x:Rep[Mat[R,C,A]]) = new matOpsCls(x)
  implicit def varToMatOps[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x:Var[Mat[R,C,A]]) = new matOpsCls(readVar(x))

  // could convert to infix, but apply doesn't work with it anyways yet
  class matOpsCls[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x:Rep[Mat[R,C,A]]) {
    type Self = Mat[R,C,A]

    // accessors
    def apply(r:Rep[Int],c:Rep[Int]) = mat_apply(x,r,c)
    def update(r:Rep[Int],c:Rep[Int],v:Rep[A]) = mat_update(x,r,c,v)

    def apply[RR <:IntM:Manifest:MVal ,CC <:IntM:Manifest:MVal](r:RR,c:CC) = mat_apply(x,MIntDepth[RR],MIntDepth[CC])
    def update[RR <:IntM:Manifest:MVal ,CC <:IntM:Manifest:MVal](r:RR,c:CC,v:Rep[A]) = mat_update(x,MIntDepth[RR],MIntDepth[CC],v)

    // Dims
    def numRows(implicit r: MVal[R] = null) : Rep[Int] = { if(r != null) MIntDepth[R] else mat_num_rows(x) }
    def numCols(implicit c: MVal[C] = null) : Rep[Int] = { if(c != null) MIntDepth[C] else mat_num_cols(x) }

    def t = mat_transpose(x)

    // arithmetic operations
    def +(y:Rep[Self])(implicit a:Arith[A]) = mat_plus(x,y)
    def -(y:Rep[Self])(implicit a:Arith[A]) = mat_minus(x,y)
    def unary_-(implicit a:Arith[A]) = mat_unary_minus(x)

    def *:*(y: Rep[Self])(implicit a: Arith[A]) = mat_times(x,y)
    def *[RR<:IntM:Manifest,CC<:IntM:Manifest](y:Rep[Mat[RR,CC,A]])(implicit a:Arith[A]) = mat_multiply(x,y)
    def *(y:Rep[Vec[C,A]])(implicit a:Arith[A],o:Overloaded1) = mat_times_vector(x,y)
    def *(y:Rep[A])(implicit a:Arith[A],o:Overloaded2) = mat_times_scalar(x,y)

    def /(y:Rep[A])(implicit a:Arith[A],o:Overloaded1) = mat_divide_scalar(x,y)
  }

  def mat_obj_new[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest]:Rep[Mat[R,C,A]]
  def mat_obj_n_new[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](r: Rep[Int], c: Rep[Int]): Rep[Mat[R,C,A]]

  // class defs
  def mat_apply[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x:Rep[Mat[R,C,A]],i:Rep[Int],j:Rep[Int]):Rep[A]
  def mat_update[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x:Rep[Mat[R,C,A]],i:Rep[Int],j:Rep[Int],y:Rep[A]):Rep[Unit]

  def mat_num_rows(m:Rep[Mat[_,_,_]]):Rep[Int]
  def mat_num_cols(m:Rep[Mat[_,_,_]]):Rep[Int]

  def row[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](m:Rep[Mat[R,C,A]],a:Rep[Int]):Rep[Vec[C,A]]
  def col[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](m:Rep[Mat[R,C,A]],a:Rep[Int]):Rep[Vec[R,A]]

  def mat_transpose[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x:Rep[Mat[R,C,A]]):Rep[Mat[R,C,A]]

  def mat_plus[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[Mat[R,C,A]]):Rep[Mat[R,C,A]]
  def mat_plus_scalar[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[A]):Rep[Mat[R,C,A]]
  def mat_minus[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[Mat[R,C,A]]):Rep[Mat[R,C,A]]
  def mat_times[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[Mat[R,C,A]]):Rep[Mat[R,C,A]]
  def mat_multiply[R<:IntM:Manifest,C<:IntM:Manifest,RR<:IntM:Manifest,CC<:IntM:Manifest,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[Mat[RR,CC,A]]):Rep[Mat[R,CC,A]]
  def mat_times_vector[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[Vec[C,A]]):Rep[Vec[C,A]]
  def mat_times_scalar[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[A]):Rep[Mat[R,C,A]]
  def mat_divide_scalar[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Rep[Mat[R,C,A]],y:Rep[A]):Rep[Mat[R,C,A]]
  def mat_unary_minus[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Rep[Mat[R,C,A]]):Rep[Mat[R,C,A]]
}


trait MatOpsExp extends MatOps with VariablesExp {
  this:MatImplOps with DeLisztExp =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure
  
  case class MatObjectNNew[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](numRows:Exp[Int],numCols:Exp[Int]) extends Def[Mat[R,C,A]] {
    val mM = manifest[MatImpl[R,C,A]]
  }

  case class MatApply[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x:Exp[Mat[R,C,A]],i:Exp[Int],j:Exp[Int]) extends Def[A]

  case class MatDCApply[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x:Exp[Mat[R,C,A]],i:Exp[Int]) extends Def[A]

  case class MatUpdate[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x:Exp[Mat[R,C,A]],i:Exp[Int],j:Exp[Int],y:Exp[A]) extends Def[Unit]

  case class MatTranspose[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x: Exp[Mat[R,C,A]])
    extends DeliteOpSingleTask(reifyEffectsHere(mat_transpose_impl(x)))
  
  case class MatNumRows(x:Exp[Mat[_,_,_]]) extends Def[Int]

  case class MatNumCols(x:Exp[Mat[_,_,_]]) extends Def[Int]

  ///////////////////////////////////////////////////////////////////
  // BLAS enabled routines (currently these must all be singletasks)

  // TODO: generalize this so that we can generate fused, delite parallel op, or BLAS variants

  case class MatTimesVec[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Vec[C,A]])
    extends DeliteOpSingleTask(reifyEffectsHere(mat_times_vector_impl(x,y)),true) {

    val mV = manifest[VecImpl[C,A]]
    def mev = manifest[A]
    def aev = implicitly[Arith[A]]
  }

  case class MatMultiply[R<:IntM:Manifest,C<:IntM:Manifest,RR<:IntM:Manifest,CC<:IntM:Manifest,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Mat[RR,CC,A]])
    extends DeliteOpSingleTask(reifyEffectsHere(mat_multiply_impl(x,y)),true) {

    val mM = manifest[MatImpl[R,CC,A]]
  }

  ////////////////////////////////
  // implemented via delite ops 
  abstract class MatArithmeticMap[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](in:Exp[Mat[R,C,A]]) extends DeliteOpMap[A,A,Mat[R,C,A]] {
    def alloc = Mat[R,C,A](in.numRows, in.numCols)
    val size = in.numRows * in.numCols

    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  abstract class MatArithmeticZipWith[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](inA:Exp[Mat[R,C,A]],inB:Exp[Mat[R,C,A]]) extends DeliteOpZipWith[A,A,A,Mat[R,C,A]] {
    def alloc = Mat[R,C,A](inA.numRows, inA.numCols)
    val size = inA.numRows * inA.numCols

    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  case class MatPlus[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](inA:Exp[Mat[R,C,A]],inB:Exp[Mat[R,C,A]])
    extends MatArithmeticZipWith(inA, inB) {

    def func = (a,b) => a + b
  }

  case class MatMinus[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](inA:Exp[Mat[R,C,A]],inB:Exp[Mat[R,C,A]])
    extends MatArithmeticZipWith(inA, inB) {

    def func = (a,b) => a - b
  }

  case class MatUnaryMinus[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](in:Exp[Mat[R,C,A]])
    extends MatArithmeticMap(in) {

    def func = e => -e
  }

  case class MatTimes[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](inA:Exp[Mat[R,C,A]],inB:Exp[Mat[R,C,A]])
    extends MatArithmeticZipWith(inA, inB) {

    def func = (a,b) => a * b
  }
  
  case class MatTimesScalar[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](in:Exp[Mat[R,C,A]],y:Exp[A])
    extends MatArithmeticMap(in) {

    def func = e => e * y
  }

  case class MatDivide[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](inA:Exp[Mat[R,C,A]],inB:Exp[Mat[R,C,A]])
    extends MatArithmeticZipWith(inA, inB) {

    def func = (a,b) => a / b
  }
  
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case MatMultiply(a,b) => Nil
    case MatTimes(a,b) => Nil
    case MatTimesVec(a,v) => Nil
    case MatTimesScalar(a,x) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case MatMultiply(a,b) => Nil
    case MatTimes(a,b) => Nil
    case MatTimesVec(a,v) => Nil
    case MatTimesScalar(a,x) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case MatMultiply(a,b) => Nil
    case MatTimes(a,b) => Nil
    case MatTimesVec(a,v) => Nil
    case MatTimesScalar(a,x) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case MatMultiply(a,b) => Nil
    case MatTimes(a,b) => Nil
    case MatTimesVec(a,v) => Nil
    case MatTimesScalar(a,x) => Nil
    case _ => super.copySyms(e)
  } 

  ////////////////////
  // object interface
  def mat_obj_n_new[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](r: Exp[Int], c: Exp[Int]) = reflectMutable(MatObjectNNew[R,C,A](r,c))

  ///////////////////
  // class interface

  def mat_num_rows(x:Exp[Mat[_,_,_]]) = reflectPure(MatNumRows(x))
  def mat_num_cols(x:Exp[Mat[_,_,_]]) = reflectPure(MatNumCols(x))

  def mat_apply[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x:Exp[Mat[R,C,A]],i:Exp[Int],j:Exp[Int]) = reflectPure(MatApply(x,i,j))
  def mat_update[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x:Exp[Mat[R,C,A]],i:Exp[Int],j:Exp[Int],y:Exp[A]) = reflectWrite(x)(MatUpdate(x,i,j,y))

  def mat_transpose[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x:Rep[Mat[R,C,A]]) = reflectPure(MatTranspose(x))

  def mat_plus[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Mat[R,C,A]]) = reflectPure(MatPlus(x,y))
  def mat_minus[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Mat[R,C,A]]) = reflectPure(MatMinus(x,y))
  def mat_unary_minus[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Exp[Mat[R,C,A]]) = MatUnaryMinus(x)

  def mat_times[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Mat[R,C,A]]) = reflectPure(MatMultiply(x,y))
  def mat_multiply[R<:IntM:Manifest,C<:IntM:Manifest,RR<:IntM:Manifest,CC<:IntM:Manifest,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Mat[RR,CC,A]]) = reflectPure(MatMultiply(x,y))
  def mat_times_vector[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Vec[C,A]]) = reflectPure(MatTimesVec(x,y))
  def mat_times_scalar[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[A]) = reflectPure(MatTimesScalar(x,y))

  def mat_divide[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest:Arith](x:Exp[Mat[R,C,A]],y:Exp[Mat[R,C,A]]) = reflectPure(MatDivide(x,y))

  //////////////////
  // internal

  def mat_dcapply[R<:IntM:Manifest,C<:IntM:Manifest,A:Manifest](x:Exp[Mat[R,C,A]],i:Exp[Int]) = reflectPure(MatDCApply(x,i))
}

/**
 *  Optimizations for composite MatOps operations.
 */

trait MatOpsExpOpt extends MatOpsExp {
  this:MatImplOps with DeLisztExp =>
}


trait ScalaGenMatOps extends ScalaGenBase {
  val IR:MatOpsExp

  import IR._

  override def emitNode(sym:Sym[Any],rhs:Def[Any])(implicit stream:PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case m@MatObjectNNew(numRows,numCols) => emitValDef(sym,"new " + remap(m.mM) + "(" + quote(numRows) + "," + quote(numCols) + ")")
    //case MatApply(x,i,j) => emitValDef(sym, quote(x) + "(" + quote(i) + ", " + quote(j) + ")")
    case MatDCApply(x,i) => emitValDef(sym,quote(x) + ".dcApply(" + quote(i) + ")")
    case MatUpdate(x,i,j,y) => emitValDef(sym,quote(x) + "(" + quote(i) + ", " + quote(j) + ") = " + quote(y))

    case MatNumRows(x) => emitValDef(sym,quote(x) + ".numRows")
    case MatNumCols(x) => emitValDef(sym,quote(x) + ".numCols")

    // BLAS calls
    // all corresponding nodes should have their DeliteOpSingleTask second argument set to "true" (require inputs)
    case m@MatMultiply(x,y) if (Config.useBlas) =>
      emitValDef(sym,"new " + remap(m.mM) + "(" + quote(x) + ".numRows," + quote(y) + ".numCols)")
      stream.println("scalaBLAS.matMult(%s.data,%s.data,%s.data,%s.numRows,%s.numCols,%s.numCols)".format(quote(x),quote(y),quote(sym),quote(x),quote(x),quote(y)))
    case m@MatTimesVec(x,y) if (Config.useBlas) =>
      emitValDef(sym,"new " + remap(m.mV) + "(" + quote(x) + ".numRows, false)")
      stream.println("scalaBLAS.matVMult(%s.data,%s.data,%s.data,%s.numRows,%s.numCols,0,1)".format(quote(x),quote(y),quote(sym),quote(x),quote(x)))
      stream.println("scalaBLAS.sigmoid(%s.data,%s.data,0,%s.numRows*%s.numCols)".format(quote(x),quote(sym),quote(x),quote(x)))
    case _ => super.emitNode(sym,rhs)
  }
}

trait CudaGenMatOps extends CudaGenBase with CudaGenDataStruct {
  val IR:MatOpsExp

  import IR._

  override def emitNode(sym:Sym[Any],rhs:Def[Any])(implicit stream:PrintWriter) = rhs match {
    /* CUBLAS calls */
    case MatMultiply(x,y) => {
      val callStream = "cublasSetKernelStream(stream);"
      val callKernel = if (remap(x.Type.typeArguments(0)) == "double")
        "cublasDgemm('n','n',%s.numCols,%s.numRows,%s.numRows,1.0,%s.data,%s.numCols,%s.data,%s.numCols,0.0,%s.data,%s.numCols);".format(quote(y),quote(x),quote(y),quote(y),quote(y),quote(x),quote(x),quote(sym),quote(sym))
      else if (remap(x.Type.typeArguments(0)) == "float")
        "cublasSgemm('n','n',%s.numCols,%s.numRows,%s.numRows,1.0,%s.data,%s.numCols,%s.data,%s.numCols,0.0,%s.data,%s.numCols);".format(quote(y),quote(x),quote(y),quote(y),quote(y),quote(x),quote(x),quote(sym),quote(sym))
      else
        throw new RuntimeException("CudaGen: Not GPUable (Type %s is not supported for MatMulitply CUBLAS library)".format(remap(x.Type.typeArguments(0))))
      emitMatAlloc(sym,"%s->numRows".format(quote(x)),"%s->numCols".format(quote(y)),false)
      emitLibCall(sym,List(callStream,callKernel))
    }
    case MatTimesVec(x,y) => {
      val callStream = "cublasSetKernelStream(stream);"
      val callKernel = if (remap(x.Type.typeArguments(0)) == "double")
        "cublasDgemv('t', %s.numCols, %s.numRows, 1.0, %s.data, %s.numCols, %s.data, 1, 0.0, %s.data, 1);".format(quote(x),quote(x),quote(x),quote(x),quote(y),quote(sym))
      else if (remap(x.Type.typeArguments(0)) == "float")
        "cublasSgemv('t', %s.numCols, %s.numRows, 1.0, %s.data, %s.numCols, %s.data, 1, 0.0, %s.data, 1);".format(quote(x),quote(x),quote(x),quote(x),quote(y),quote(sym))
      else
        throw new RuntimeException("CudaGen: Not GPUable (Type %s is not supported for Mat*Vec CUBLAS library)".format(remap(x.Type.typeArguments(0))))
      emitVecAlloc(sym,"%s->numRows".format(quote(x)),"false",false)
      emitLibCall(sym,List(callStream,callKernel))
    }
    /* The ops that call through to the underlying data structure */
    case MatDCApply(x,i) =>
      emitValDef(sym,"%s.dcApply(%s)".format(quote(x),quote(i)))
    case MatApply(x,i,j) =>
      emitValDef(sym,"%s.apply(%s,%s)".format(quote(x),quote(i),quote(j)))
    case MatUpdate(x,i,j,y) =>
      stream.println(addTab() + "%s.update(%s,%s,%s);".format(quote(x),quote(i),quote(j),quote(y)))
    case MatNumRows(x) =>
      emitValDef(sym,quote(x) + ".numRows")
    case MatNumCols(x) =>
      emitValDef(sym,quote(x) + ".numCols")

    case _ => super.emitNode(sym,rhs)
  }
}

trait CGenMatOps extends CGenBase {
  val IR:MatOpsExp

  import IR._

  override def emitNode(sym:Sym[Any],rhs:Def[Any])(implicit stream:PrintWriter) = rhs match {

    case MatObjectNNew(numRows,numCols) =>
      stream.println("%s *%s_data = malloc(sizeof(%s)*%s*%s);".format(remap(sym.Type.typeArguments(0)),quote(sym),remap(sym.Type.typeArguments(0)),quote(numRows),quote(numCols)))
      stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
      stream.println("%s.numRows = %s;".format(quote(sym),quote(numRows)))
      stream.println("%s.numCols = %s;".format(quote(sym),quote(numCols)))
      stream.println("%s.data = %s_data;".format(quote(sym),quote(sym)))
    case MatDCApply(x,i) =>
      emitValDef(sym,"%s.apply(%s)".format(quote(x),quote(i)))
    //case MatApply(x,i,j) =>
    //  emitValDef(sym, "%s.apply(%s,%s)".format(quote(x),quote(i),quote(j)))
    case MatUpdate(x,i,j,y) =>
      stream.println("%s.update(%s,%s,%s);".format(quote(x),quote(i),quote(j),quote(y)))
    case MatNumRows(x) =>
      emitValDef(sym,quote(x) + ".numRows")
    case MatNumCols(x) =>
      emitValDef(sym,quote(x) + ".numCols")
    case _ => super.emitNode(sym,rhs)
  }
}
