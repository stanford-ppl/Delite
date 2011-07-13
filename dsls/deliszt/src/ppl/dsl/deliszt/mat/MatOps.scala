package ppl.dsl.deliszt.mat

import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException}

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.Config

import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}
import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import ppl.dsl.deliszt.datastruct.scala._

trait MatOps extends DSLType with Variables {
  this: DeLiszt =>

  object Mat {
    def apply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,VT:Manifest] = mat_obj_new[R,C,VT]
  }

  implicit def repMatToMatOps[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT: Manifest](x: Rep[Mat[R,C,VT]]) = new matOpsCls(x)
  implicit def varToMatOps[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT: Manifest](x: Var[Mat[R,C,VT]]) = new matOpsCls(readVar(x))

  // could convert to infix, but apply doesn't work with it anyways yet
  class matOpsCls[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest](x: Rep[Mat[R,C,VT]]) {
    type Self = Mat[R,C,VT]

    // accessors
    def apply(r: Rep[Int], c: Rep[Int]) = mat_apply(x,r,c)
    def update(r: Rep[Int], c: Rep[Int], v: Rep[VT]) = mat_update(x,r,c,v)
    
    def apply[RR<:IntM:MVal, CC<:IntM:MVal](r: RR, c: CC) = mat_apply(x,MIntDepth[RR],MIntDepth[CC])
    def update[RR<:IntM:MVal, CC<:IntM:MVal, Rep[VT]](r: RR, c: CC, v: Rep[VT]) = mat_update(x,MIntDepth[RR],MIntDepth[CC],v)

    // Dims
    def numRows = mat_num_rows(x)
    def numCols = mat_num_cols(x)

    // arithmetic operations
    def +(y: Rep[Self])(implicit a: Arith[VT]) = mat_plus(x,y)
    def -(y: Rep[Self])(implicit a: Arith[VT]) = mat_minus(x,y)
    def unary_-(implicit a: Arith[VT]) = mat_unary_minus(x)

    def *(y: Rep[Self])(implicit a: Arith[VT]) = mat_multiply(x,y)
    def *(y: Rep[Vec[C,VT]])(implicit a: Arith[VT], o: Overloaded1) = mat_times_vector(x,y)
    def *(y: Rep[VT])(implicit a: Arith[VT], o: Overloaded2) = mat_times_scalar(x,y)

    def /(y: Rep[VT])(implicit a: Arith[VT], o: Overloaded1) = mat_divide_scalar(x,y)
  }

  def mat_obj_new[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,VT:Manifest] : Rep[Mat[R,C,VT]]
  
  // class defs
  def mat_apply[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest](x: Rep[Mat[R,C,VT]], i: Rep[Int], j: Rep[Int]): Rep[VT]
  def mat_update[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest](x: Rep[Mat[R,C,VT]], i: Rep[Int], j: Rep[Int], y: Rep[VT]): Rep[Unit]

  def mat_num_rows(m: Rep[Mat[_,_,_]]): Rep[Int]
  def mat_num_cols(m: Rep[Mat[_,_,_]]): Rep[Int]

  def row[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest](m: Rep[Mat[R,C,VT]], a: Rep[Int]): Rep[Vec[C,VT]]
	def col[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest](m: Rep[Mat[R,C,VT]], a: Rep[Int]): Rep[Vec[R,VT]]

  def mat_plus[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Mat[R,C,VT]], y: Rep[Mat[R,C,VT]]): Rep[Mat[R,C,VT]]
  def mat_plus_scalar[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Mat[R,C,VT]], y: Rep[VT]): Rep[Mat[R,C,VT]]
  def mat_minus[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Mat[R,C,VT]], y: Rep[Mat[R,C,VT]]): Rep[Mat[R,C,VT]]
  def mat_times[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Mat[R,C,VT]], y: Rep[Mat[R,C,VT]]): Rep[Mat[R,C,VT]]
  def mat_multiply[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Mat[R,C,VT]], y: Rep[Mat[R,C,VT]]): Rep[Mat[R,C,VT]]
  def mat_times_vector[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Mat[R,C,VT]], y: Rep[Vec[C,VT]]): Rep[Vector[VT]]
  def mat_times_scalar[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Mat[R,C,VT]], y: Rep[VT]): Rep[Mat[R,C,VT]]
  def mat_divide_scalar[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Mat[R,C,VT]], y: Rep[VT]): Rep[Mat[R,C,VT]]
  def mat_unary_minus[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Mat[R,C,VT]]): Rep[Mat[R,C,VT]]
}


trait MatOpsExp extends MatOps with VariablesExp {
  this: MatImplOps with DeLisztExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class MatObjectNew[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,VT:Manifest](numRows: Exp[Int], numCols: Exp[Int]) extends Def[Mat[R,C,VT]] {
     val mM = manifest[MatImpl[R,C,VT]]
  }
  case class MatApply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,VT:Manifest](x: Exp[Mat[R,C,VT]], i: Exp[Int], j: Exp[Int]) extends Def[VT]
  case class MatDCApply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,VT:Manifest](x: Exp[Mat[R,C,VT]], i: Exp[Int]) extends Def[VT]
  case class MatUpdate[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,VT:Manifest](x: Exp[Mat[R,C,VT]], i: Exp[Int], j: Exp[Int], y: Exp[VT]) extends Def[Unit]

  case class MatNumRows(x: Exp[Mat[_,_,_]]) extends Def[Int]
  case class MatNumCols(x: Exp[Mat[_,_,_]]) extends Def[Int]

  ///////////////////////////////////////////////////////////////////
  // BLAS enabled routines (currently these must all be singletasks)

  // TODO: generalize this so that we can generate fused, delite parallel op, or BLAS variants

  case class MatTimesVector[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Mat[R,C,VT]], y: Exp[Vec[C,VT]])
    extends DeliteOpSingleTask(reifyEffectsHere(mat_times_vector_impl(x,y)), true) {

    val mV = manifest[VecImpl[C,VT]]
    def mev = manifest[VT]
    def aev = implicitly[Arith[VT]]
  }

  case class MatMultiply[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Mat[R,C,VT]], y: Exp[Mat[R,C,VT]])
    extends DeliteOpSingleTask(reifyEffectsHere(mat_multiply_impl(x,y)), true) {

    val mM = manifest[MatImpl[R,C,VT]]
  }

  ////////////////////////////////
  // implemented via delite ops
  
  abstract class MatArithmeticMap[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](in: Exp[Mat[R,C,VT]]) extends DeliteOpMap[VT,VT,Mat[R,C,VT]] {
    def alloc = Mat[R,C,VT](in.numRows, in.numCols)
    val size = in.numRows*in.numCols
    
    def m = manifest[VT]
    def a = implicitly[Arith[VT]]
  }
  
  abstract class MatArithmeticZipWith[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](inA: Exp[Mat[R,C,VT]], inB: Exp[Mat[R,C,VT]]) extends DeliteOpZipWith[VT,VT,VT,Mat[R,C,VT]] {
    def alloc = Mat[R,C,VT](inA.numRows, inA.numCols)
    val size = inA.numRows*inA.numCols
    
    def m = manifest[VT]
    def a = implicitly[Arith[VT]]
  }

  case class MatPlus[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](inA: Exp[Mat[R,C,VT]], inB: Exp[Mat[R,C,VT]])
    extends DeliteOpZipWith[VT,VT,VT,Mat[R,C,VT]] {

    val alloc = reifyEffects(Mat[R,C,VT]())
    val v = (fresh[VT],fresh[VT])
    val func = v._1 + v._2
  }

  case class MatMinus[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](inA: Exp[Mat[R,C,VT]], inB: Exp[Mat[R,C,VT]])
    extends DeliteOpZipWith[VT,VT,VT,Mat[R,C,VT]] {

    val alloc = reifyEffects(Mat[R,C,VT]())
    val v = (fresh[VT],fresh[VT])
    val func = v._1 - v._2
  }

  case class MatTimesScalar[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](in: Exp[Mat[R,C,VT]], y: Exp[VT])
    extends DeliteOpMap[VT,VT,Mat[R,C,VT]] {

    val alloc = reifyEffects(Mat[R,C,VT]())
    val v = fresh[VT]
    val func = v * y
  }

  case class MatDivide[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](inA: Exp[Mat[R,C,VT]], inB: Exp[Mat[R,C,VT]])
    extends DeliteOpZipWith[VT,VT,VT,Mat[R,C,VT]] {

    val alloc = reifyEffects(Mat[R,C,VT]())
    val v = (fresh[VT],fresh[VT])
    val func = v._1 / v._2
  }

  ////////////////////
  // object interface
  def mat_obj_new[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest]() = reflectMutable(MatObjectNew[R,C,VT](MIntDepth[R],MIntDepth[C]))
  
  ///////////////////
  // class interface

  def mat_num_rows(x: Exp[Mat[_,_,_]]) = reflectPure(MatNumRows(x))
  def mat_num_cols(x: Exp[Mat[_,_,_]]) = reflectPure(MatNumCols(x))

  def mat_apply[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest](x: Exp[Mat[R,C,VT]], i: Exp[Int], j: Exp[Int]) = reflectPure(MatApply[R,C,VT](x,i,j))  
  def mat_update[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest](x: Exp[Mat[R,C,VT]], i: Exp[Int], j: Exp[Int], y: Exp[VT]) = reflectWrite(x)(MatUpdate[R,C,VT](x,i,j,y))

  def mat_plus[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Mat[R,C,VT]], y: Exp[Mat[R,C,VT]]) = reflectPure(MatPlus(x, y))
  def mat_minus[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Mat[R,C,VT]], y: Exp[Mat[R,C,VT]]) = reflectPure(MatMinus(x,y))
  def mat_unary_minus[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Mat[R,C,VT]]) = MatUnaryMinus(x)

  def mat_multiply[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Mat[R,C,VT]], y: Exp[Mat[R,C,VT]]) = reflectPure(MatMultiply(x,y))
  def mat_times_vector[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Mat[R,C,VT]], y: Exp[Vector[VT]]) = reflectPure(MatTimesVector(x,y))
  def mat_times_scalar[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,VT:Manifest:Arith](x: Exp[Mat[R,C,VT]], y: Exp[VT]) = reflectPure(MatTimesScalar(x,y))

  def mat_divide[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,VT:Manifest:Arith](x: Exp[Mat[R,C,VT]], y: Exp[Mat[R,C,VT]]) = reflectPure(MatDivide(x,y))

  //////////////////
  // internal

  def mat_dcapply[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,VT:Manifest](x: Exp[Mat[R,C,VT]], i: Exp[Int]) = reflectPure(MatDCApply(x,i))
}

/**
 *  Optimizations for composite MatOps operations.
 */

trait MatOpsExpOpt extends MatOpsExp {
  this: MatImplOps with DeLisztExp =>
}


trait ScalaGenMatOps extends ScalaGenBase {
  val IR: MatOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case m@MatObjectNew(numRows, numCols) => emitValDef(sym, "new " + remap(m.mM) + "(" + quote(numRows) + "," + quote(numCols) + ")")
    //case MatApply(x,i,j) => emitValDef(sym, quote(x) + "(" + quote(i) + ", " + quote(j) + ")")
    case MatDCApply(x,i) => emitValDef(sym, quote(x) + ".dcApply(" + quote(i) + ")")
    case MatUpdate(x,i,j,y)  => emitValDef(sym, quote(x) + "(" + quote(i) + ", " + quote(j) + ") = " + quote(y))

    case MatNumRows(x)  => emitValDef(sym, quote(x) + ".numRows")
    case MatNumRows(x)  => emitValDef(sym, quote(x) + ".numCols")

    // BLAS calls
    // all corresponding nodes should have their DeliteOpSingleTask second argument set to "true" (require inputs)
    case m@MatMultiply(x,y) if (Config.useBlas) =>
      emitValDef(sym, "new " + remap(m.mM) + "(" + quote(x) + ".numRows," + quote(y) + ".numCols)")
      stream.println("scalaBLAS.matMult(%s.data,%s.data,%s.data,%s.numRows,%s.numCols,%s.numCols)".format(quote(x),quote(y),quote(sym),quote(x),quote(x),quote(y)))
    case m@MatTimesVector(x,y) if (Config.useBlas) =>
      emitValDef(sym, "new " + remap(m.mV) + "(" + quote(x) + ".numRows, false)")
      stream.println("scalaBLAS.matVMult(%s.data,%s.data,%s.data,%s.numRows,%s.numCols,0,1)".format(quote(x),quote(y),quote(sym),quote(x),quote(x)))
    case m@MatSigmoid(x) if (Config.useBlas) =>
      emitValDef(sym, "new " + remap(m.mM) + "(" + quote(x) + ".numRows," + quote(x) + ".numCols)")
      stream.println("scalaBLAS.sigmoid(%s.data,%s.data,0,%s.numRows*%s.numCols)".format(quote(x),quote(sym),quote(x),quote(x)))
    case m@MatSigmoidF(x) if (Config.useBlas) =>
      emitValDef(sym, "new " + remap(m.mM) + "(" + quote(x) + ".numRows," + quote(x) + ".numCols)")
      stream.println("scalaBLAS.sigmoid(%s.data,%s.data,0,%s.numRows*%s.numCols)".format(quote(x),quote(sym),quote(x),quote(x)))

    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMatOps extends CudaGenBase with CudaGenDataStruct {
  val IR: MatOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    /* CUBLAS calls */
    case MatMultiply(x,y) => {
      val callStream = "cublasSetKernelStream(stream);"
      val callKernel = if(remap(x.Type.typeArguments(0)) == "double")
        "cublasDgemm('n','n',%s.numCols,%s.numRows,%s.numRows,1.0,%s.data,%s.numCols,%s.data,%s.numCols,0.0,%s.data,%s.numCols);".format(quote(y),quote(x),quote(y),quote(y),quote(y),quote(x),quote(x),quote(sym),quote(sym))
      else if(remap(x.Type.typeArguments(0)) == "float")
        "cublasSgemm('n','n',%s.numCols,%s.numRows,%s.numRows,1.0,%s.data,%s.numCols,%s.data,%s.numCols,0.0,%s.data,%s.numCols);".format(quote(y),quote(x),quote(y),quote(y),quote(y),quote(x),quote(x),quote(sym),quote(sym))
      else
        throw new RuntimeException("CudaGen: Not GPUable (Type %s is not supported for MatMulitply CUBLAS library)".format(remap(x.Type.typeArguments(0))))
      emitMatAlloc(sym,"%s->numRows".format(quote(x)),"%s->numCols".format(quote(y)),false)
      emitLibCall(sym,List(callStream,callKernel))
    }
    case MatTimesVector(x,y) => {
      val callStream = "cublasSetKernelStream(stream);"
      val callKernel = if(remap(x.Type.typeArguments(0)) == "double")
        "cublasDgemv('t', %s.numCols, %s.numRows, 1.0, %s.data, %s.numCols, %s.data, 1, 0.0, %s.data, 1);".format(quote(x),quote(x),quote(x),quote(x),quote(y),quote(sym))
      else if(remap(x.Type.typeArguments(0)) == "float")
        "cublasSgemv('t', %s.numCols, %s.numRows, 1.0, %s.data, %s.numCols, %s.data, 1, 0.0, %s.data, 1);".format(quote(x),quote(x),quote(x),quote(x),quote(y),quote(sym))
      else
        throw new RuntimeException("CudaGen: Not GPUable (Type %s is not supported for Mat*Vector CUBLAS library)".format(remap(x.Type.typeArguments(0))))
      emitVectorAlloc(sym,"%s->numRows".format(quote(x)),"false",false)
      emitLibCall(sym,List(callStream,callKernel))
    }
    /* The ops that call through to the underlying data structure */
    case MatDCApply(x,i) =>
      emitValDef(sym, "%s.dcApply(%s)".format(quote(x),quote(i)))
    case MatApply(x,i,j) =>
      emitValDef(sym, "%s.apply(%s,%s)".format(quote(x),quote(i),quote(j)))
    case MatUpdate(x,i,j,y)  =>
      stream.println(addTab() + "%s.update(%s,%s,%s);".format(quote(x),quote(i),quote(j),quote(y)))
    case MatNumRows(x)  =>
      emitValDef(sym, quote(x) + ".numRows")
    case MatNumCols(x)  =>
      emitValDef(sym, quote(x) + ".numCols")

    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMatOps extends CGenBase {
  val IR: MatOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    case MatObjectNew(numRows,numCols) =>
      stream.println("%s *%s_data = malloc(sizeof(%s)*%s*%s);".format(remap(sym.Type.typeArguments(0)),quote(sym),remap(sym.Type.typeArguments(0)),quote(numRows),quote(numCols)))
      stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
      stream.println("%s.numRows = %s;".format(quote(sym),quote(numRows)))
      stream.println("%s.numCols = %s;".format(quote(sym),quote(numCols)))
      stream.println("%s.data = %s_data;".format(quote(sym),quote(sym)))
    case MatDCApply(x,i) =>
      emitValDef(sym, "%s.apply(%s)".format(quote(x),quote(i)))
    //case MatApply(x,i,j) =>
    //  emitValDef(sym, "%s.apply(%s,%s)".format(quote(x),quote(i),quote(j)))
    case MatUpdate(x,i,j,y)  =>
      stream.println("%s.update(%s,%s,%s);".format(quote(x),quote(i),quote(j),quote(y)))
    case MatNumRows(x)  =>
      emitValDef(sym, quote(x) + ".numRows")
    case MatNumCols(x)  =>
      emitValDef(sym, quote(x) + ".numCols")
    case _ => super.emitNode(sym, rhs)
  }
}
