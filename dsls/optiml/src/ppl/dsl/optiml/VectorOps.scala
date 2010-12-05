package ppl.dsl.optiml

import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.internal.{CudaGenBase, ScalaGenBase}
import scala.virtualization.lms.common._
import java.io.{StringWriter, PrintWriter}

trait Vector[T] {
  // fields required on real underlying data structure impl
  def length : Int
  def is_row : Boolean
  def apply(n: Int) : T
  def update(index: Int, x: T)
}

trait VectorOps extends DSLType with Variables { this: ArithImplicits =>

  object Vector {
    def apply[A : Manifest](len: Rep[Int], is_row : Rep[Boolean] = true) : Rep[Vector[A]] = vector_new(len, is_row)
    def zeros(len: Rep[Int]) : Rep[Vector[Double]] = vector_obj_zeros(len)
    def range(start: Rep[Int], end: Rep[Int], stride: Rep[Int] = 1, is_row: Rep[Boolean] = true) =
      vector_obj_range(start, end, stride, is_row)
  }

  implicit def repVecToRepVecOps[A:Manifest](x: Rep[Vector[A]]) = new vecRepCls(x)
  implicit def vecToRepVecOps[A:Manifest](x: Vector[A]) = new vecRepCls(x)
  implicit def varToRepVecOps[A:Manifest](x: Var[Vector[A]]) : vecRepCls[A]

  // could convert to infix, but apply doesn't work with it anyways yet
  class vecRepCls[A:Manifest](x: Rep[Vector[A]]) {

    def apply(n: Rep[Int]) = vector_apply(x, n)
    def update(n: Rep[Int], y: Rep[A]) = vector_update(x,n,y)
    def length = vector_length(x)
    def toBoolean(implicit conv: Rep[A] => Rep[Boolean]) = vector_toboolean(x)
    def +(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_plus(x,y)
    def -(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_minus(x,y)
    def *(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_times(x,y)
    def /(y: Rep[A])(implicit f: Fractional[A]) = vector_divide(x,y)
    def **(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_outer(x,y)
    def ~ = vector_trans(x)
    def pprint = vector_pprint(x)
    def is_row = vector_is_row(x)

    def +=(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_plusequals(x,y)
    def +=(y: Rep[A]) = vector_insert(x,x.length,y)

    def map[B:Manifest](f: Rep[A] => Rep[B]) = vector_map(x,f)
    def sum(implicit ops: ArithOps[A]) : Rep[A] = vector_sum(x)
  }

  // object defs
  def vector_obj_zeros(len: Rep[Int]): Rep[Vector[Double]]
  def vector_obj_range(start: Rep[Int], end: Rep[Int], stride: Rep[Int], is_row: Rep[Boolean]): Rep[Vector[Int]]  

  // class defs
  def vector_apply[A:Manifest](x: Rep[Vector[A]], n: Rep[Int]): Rep[A]
  def vector_update[A:Manifest](x: Rep[Vector[A]], n: Rep[Int], y: Rep[A]): Rep[Unit]
  def vector_length[A:Manifest](x: Rep[Vector[A]]): Rep[Int]
  def vector_insert[A:Manifest](x: Rep[Vector[A]], pos: Rep[Int], y: Rep[A]): Rep[Vector[A]]
  def vector_is_row[A:Manifest](x: Rep[Vector[A]]): Rep[Boolean]

  def vector_toboolean[A](x: Rep[Vector[A]])(implicit conv: Rep[A] => Rep[Boolean], mA: Manifest[A]): Rep[Vector[Boolean]]
  def vector_plus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_plusequals[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_minus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_times[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_divide[A:Manifest:Fractional](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vector_trans[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_outer[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Matrix[A]]
  def vector_pprint[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
  def vector_map[A:Manifest,B:Manifest](x: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[B]]
  def vector_sum[A:Manifest:ArithOps](x: Rep[Vector[A]]) : Rep[A]

  // impl defs
  def vector_new[A:Manifest](len: Rep[Int], is_row: Rep[Boolean]) : Rep[Vector[A]]
}

trait VectorOpsExp extends VectorOps with VariablesExp with DSLOpsExp with RangeOpsExp with FunctionsExp with FractionalOpsExp with NumericOpsExp { this: VectorImplOps with ArithImplicits =>

  implicit def varToRepVecOps[A:Manifest](x: Var[Vector[A]]) = new vecRepCls(readVar(x))

  // implemented via method on real data structure
  case class VectorApply[A](x: Exp[Vector[A]], n: Exp[Int]) extends Def[A]
  case class VectorUpdate[A:Manifest](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) extends Def[Unit]
  case class VectorLength[A:Manifest](x: Exp[Vector[A]]) extends Def[Int]
  case class VectorInsert[A:Manifest](x: Exp[Vector[A]], pos: Rep[Int], y: Exp[A]) extends Def[Vector[A]]
  case class VectorIsRow[A:Manifest](x: Exp[Vector[A]]) extends Def[Boolean]

  // implemented via kernel embedding
  case class VectorObjectZeros(len: Exp[Int])
    extends DSLOp(reifyEffects(vector_obj_zeros_impl(len)))

  case class VectorToBoolean[A](x: Exp[Vector[A]])(implicit conv: Exp[A] => Exp[Boolean], mA: Manifest[A])
    extends DSLOp(reifyEffects(vector_toboolean_impl[A](x,conv)))

  case class VectorPlus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_plus_impl[A](x,y)))

  case class VectorPlusEquals[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_plusequals_impl[A](x,y)))

  //case class VectorMinus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
  //  extends DSLZipwith[A,A,A,Vector](x,y,reifyEffects(vector_new[A](x.length,x.is_row)), reifyEffects(range_until(0,x.length)), doLambda2[A,A,A]((a1,a2) => a1-a2))
  case class VectorMinus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_minus_impl[A](x,y)))

  //case class VectorDivide[A:Manifest:Fractional](x: Exp[Vector[A]], y: Exp[A])
  //  extends DSLMap[A,A,Vector](x, reifyEffects(vector_new[A](x.length,x.is_row)), reifyEffects(range_until(0,x.length)), doLambda[A,A](a=>a/y))
  case class VectorDivide[A:Manifest:Fractional](x: Exp[Vector[A]], y: Exp[A])
    extends DSLOp(reifyEffects(vector_divide_impl[A](x,y)))

  case class VectorOuter[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_outer_impl[A](x,y)))
  
  case class VectorPPrint[A:Manifest](x: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_pprint_impl[A](x)))

  case class VectorTrans[A:Manifest](x: Exp[Vector[A]])
      extends DSLOp(reifyEffects(vector_trans_impl[A](x)))

  case class VectorTimes[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
      extends Def[Vector[A]]

  case class VectorNew[A:Manifest](len: Exp[Int], is_row: Exp[Boolean])
    extends DSLOp(reifyEffects(vector_new_impl[A](len, is_row)))

  case class VectorObjectRange(start: Exp[Int], end: Exp[Int], stride: Exp[Int], is_row: Exp[Boolean])
    extends DSLOp(reifyEffects(vector_obj_range_impl(start,end,stride,is_row)))

  case class VectorMap[A:Manifest,B:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[B])
    extends DSLOp(reifyEffects(vector_map_impl(x, f)))

  case class VectorSum[A:Manifest:ArithOps](x: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_sum_impl(x)))

  def vector_apply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) = VectorApply(x, n)
  def vector_update[A:Manifest](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) = reflectMutation(VectorUpdate(x,n,y))
  def vector_length[A:Manifest](x: Exp[Vector[A]]) = VectorLength(x)
  def vector_insert[A:Manifest](x: Exp[Vector[A]], pos: Rep[Int], y: Exp[A]) = reflectMutation(VectorInsert(x, pos, y))
  def vector_is_row[A:Manifest](x: Exp[Vector[A]]) = VectorIsRow(x)

  def vector_obj_zeros(len: Exp[Int]) = reflectEffect(VectorObjectZeros(len))
  def vector_toboolean[A](x: Exp[Vector[A]])(implicit conv: Exp[A] => Exp[Boolean], mA: Manifest[A]) = VectorToBoolean(x)
  def vector_plus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(x, y)
  def vector_plusequals[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlusEquals(x, y)
  def vector_minus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorMinus(x, y)
  def vector_times[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorTimes(x, y)
  def vector_divide[A:Manifest:Fractional](x: Exp[Vector[A]], y: Exp[A]) = VectorDivide(x, y)
  def vector_trans[A:Manifest](x: Exp[Vector[A]]) = VectorTrans(x)
  def vector_outer[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorOuter(x, y)
  def vector_pprint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(VectorPPrint(x))

  def vector_new[A:Manifest](len: Exp[Int], is_row: Exp[Boolean]) = reflectEffect(VectorNew[A](len, is_row))

  def vector_obj_range(start: Exp[Int], end: Exp[Int], stride: Exp[Int], is_row: Exp[Boolean]) = reflectEffect(VectorObjectRange(start, end, stride, is_row))
  def vector_map[A:Manifest,B:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[B]) = VectorMap(x, f)
  def vector_sum[A:Manifest:ArithOps](x: Exp[Vector[A]]) = VectorSum(x)
}

/**
 * Optimizations for composite VectorOps operations.
 */

trait VectorOpsExpOpt extends VectorOpsExp { this: VectorImplOps with ArithImplicits =>
  override def vector_plus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    // (TB + TD) == T(B + D)
    case (Def(VectorTimes(a, b)), Def(VectorTimes(c, d))) if (a == c) => VectorTimes[A](a.asInstanceOf[Exp[Vector[A]]], VectorPlus[A](b.asInstanceOf[Exp[Vector[A]]],d.asInstanceOf[Exp[Vector[A]]]))
    // ...
    case _ => super.vector_plus(x, y)
  }

  // allows sum to be expressive without actually doing pointless accumulations
  override def vector_plusequals[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    // remove runtime check on zero vector being same length as argument
    case (a, Def(VectorObjectZeros(len))) => a
    case (Def(VectorObjectZeros(len)), b) => b
    case _ => super.vector_plusequals(x,y)
  }

  override def vector_times[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    case _ => super.vector_times(x, y)
  }
}

trait ScalaGenVectorOps extends ScalaGenBase {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case VectorApply(x, n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
    case VectorUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
    case VectorLength(x)    => emitValDef(sym, quote(x) + ".length")
    case VectorIsRow(x)     => emitValDef(sym, quote(x) + ".is_row")
    case VectorInsert(x,pos,y) => emitValDef(sym, quote(x) + ".insert(" + quote(pos) + ", " + quote(y) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}


trait CudaGenVectorOps extends CudaGenBase {
  val IR: VectorOpsExp
  import IR._

  // The statements that will be included in the gpu memory allocation helper function
  def allocStmts(sym:Sym[_], length: String, isRow:String): String = {
    val str = new StringWriter()
    val stream = new PrintWriter(str)
    val typeStr = CudaType(sym.Type.toString)
    val targTypeStr = CudaType(sym.Type.typeArguments(0).toString)

    stream.println("\t%s *devPtr;".format(targTypeStr))
    stream.println("\tDeliteCudaMalloc((void**)%s,%s*sizeof(%s));".format("&devPtr",length,targTypeStr))
    stream.println("\t%s %s;".format(typeStr,quote(sym)))
    stream.println("\t%s.length = %s;".format(quote(sym),length))
    stream.println("\t%s.is_row = %s;".format(quote(sym),isRow))
    stream.println("\t%s.data = %s;".format(quote(sym),"devPtr"))
    stream.println("\treturn %s;".format(quote(sym)))
    stream.flush
    str.toString
  }

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure

    case VectorDivide(x,y) =>
      gpuBlockSizeX = quote(x)+".length"
      stream.println(addTab()+"if( %s < %s ) {".format("idxX",quote(x)+".length"))
      tabWidth += 1
      stream.println(addTab()+"%s.update(%s, (%s.apply(%s))/%s);".format(quote(sym),"idxX",quote(x),"idxX",quote(y)))
      tabWidth -= 1
      stream.println(addTab()+"}")
      // Add allocation helper function
      emitAlloc(sym,allocStmts(sym,quote(x)+".length",quote(x)+".is_row"))

    case VectorSum(x) =>
      gpuBlockSizeX = quote(x)+".length"
      stream.println(addTab()+"if( %s < %s ) {".format("idxX",quote(x)+".length"))
      tabWidth += 1
      stream.println("Function Body")
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitAlloc(sym,allocStmts(sym,quote(x)+".length",quote(x)+".is_row"))
    
    case VectorObjectZeros(len) =>
      throw new RuntimeException("CudaGen: Not GPUable")
    case VectorNew(len,is_row) =>
      throw new RuntimeException("CudaGen: Not GPUable")
    case VectorApply(x, n) =>
      if(!isGPUable) throw new RuntimeException("CudaGen: Not GPUable")
      else emitValDef(CudaType(sym.Type.typeArguments(0).toString), sym, quote(x) + ".apply(" + quote(n) + ")")
    case VectorUpdate(x,n,y) =>
      if(!isGPUable) throw new RuntimeException("CudaGen: Not GPUable")
      else stream.println(addTab() + "%s.update(%s,%s);".format(quote(x),quote(n),quote(y)))
    case VectorLength(x)    =>
      if(!isGPUable) throw new RuntimeException("CudaGen: Not GPUable")
      else emitValDef("int", sym, quote(x) + ".length")
    case VectorIsRow(x)     =>
      if(!isGPUable) throw new RuntimeException("CudaGen: Not GPUable")
      else emitValDef("bool", sym, quote(x) + ".is_row")
    case VectorPlusEquals(x,y) =>
      if(!isGPUable) throw new RuntimeException("CudaGen: Not GPUable")
      else emitValDef(sym, quote(x) + " += " + quote(y))

    case _ => super.emitNode(sym, rhs)
  }
}

