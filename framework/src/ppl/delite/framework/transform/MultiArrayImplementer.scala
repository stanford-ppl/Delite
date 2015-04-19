package ppl.delite.framework.transform

import scala.virtualization.lms.common._
import scala.reflect.SourceContext

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.analysis._
import ppl.delite.framework.visit._
import ppl.delite.framework.Util._

trait MultiArrayImpl[T]
// TBD: Bit annoying to have four of these for each layout type - some way to avoid this?
trait MultiArrayPlainImpl[T] extends MultiArrayImpl[T]
trait MultiArrayViewImpl[T] extends MultiArrayImpl[T]
trait MultiArrayBuffImpl[T] extends MultiArrayImpl[T]
trait MultiArrayBuffViewImpl[T] extends MultiArrayImpl[T]

trait MultiArrayImplExp extends MultiArrayWrapExp with DeliteInternalOpsExp with DeliteSimpleOpsExp

trait MultiArrayImplementer extends TransformerBase with MultiArrayHelperStageThree {
  val IR: MultiArrayImplExp with LayoutMetadata
  import IR._

  // --- Helper functions
  def reductionTree(x: Seq[Exp[Int]])(f: (Exp[Int],Exp[Int]) => Exp[Int]): Seq[Exp[Int]] = {
    if (x.length == 1)
      x
    else if (x.length % 2 == 0)
      reductionTree(Seq.tabulate(x.length / 2){i => f(x(2*i), x(2*i + 1)) })(f)
    else
      reductionTree(Seq.tabulate(x.length / 2){i => f(x(2*i), x(2*i + 1)) } :+ x.last)(f)
  }
  def sumTree(x: Seq[Exp[Int]]): Exp[Int] = reductionTree(x){(a,b) => delite_int_plus(a,b)}.apply(0)
  def productTree(x: Seq[Exp[Int]]): Exp[Int] = reductionTree(x){(a,b) => delite_int_times(a,b)}.apply(0)

  def isLoopIndices[T](tp: Manifest[T]) = isSubtype(tp.erasure, classOf[LoopIndices])
  def isIndices[T](tp: Manifest[T]) = isSubtype(tp.erasure, classOf[Indices])

  def isMultiArrayImplType[T](tp: Manifest[T]) = isSubtype(tp.erasure, classOf[MultiArrayImpl[T]])
  def isMultiArrayViewImplType[T](tp: Manifest[T]) = isSubtype(tp.erasure, classOf[MultiArrayViewImpl[T]])
  def isMultiArrayBuffImplType[T](tp: Manifest[T]) = isSubtype(tp.erasure, classOf[MultiArrayViewImpl[T]])
  def isMultiArrayBuffViewImplType[T](tp: Manifest[T]) = isSubtype(tp.erasure, classOf[MultiArrayBuffViewImpl[T]])

  implicit def multiarrayToCastOps[T:Manifest](x: Exp[DeliteMultiArray[T]]) = new MultiArrayCastOpsCls[T](x)
  class MultiArrayCastOpsCls[T:Manifest](x: Exp[DeliteMultiArray[T]]) { 
    def asDeliteArray = {
      assert(isDeliteArrayType(x.tp), "Cannot cast " + x.tp + " to DeliteArray!")
      x.asInstanceOf[Exp[DeliteArray[T]]]
    }
    def asDeliteArrayBuffer = {
      assert(isDeliteArrayBufferType(x.tp), "Cannot cast " + x.tp + " to DeliteArrayBuffer")
      x.asInstanceOf[Exp[DeliteArrayBuffer[T]]]
    }
    def asMultiArrayImpl = {
      assert(isMultiArrayImplType(x.tp), "Cannot cast " + x.tp + " to MultiArrayImpl")
      x.asInstanceOf[Exp[MultiArrayImpl[T]]]
    }
    def asMultiArrayViewImpl = {
      assert(isMultiArrayViewImplType(x.tp), "Cannot cast " + x.tp + " to MultiArrayViewImpl")
      x.asInstanceOf[Exp[MultiArrayViewImpl[T]]]
    }
    def asMultiArrayBuffImpl = {
      assert(isMultiArrayBuffImplType(x.tp), "Cannot cast " + x.tp + " to MultiArrayBuffImpl")
      x.asInstanceOf[Exp[MultiArrayBuffImpl[T]]]
    }
    def asMultiArrayBuffViewImpl = {
      assert(isMultiArrayBuffViewImplType(x.tp), "Cannot cast " + x.tp + " to MultiArrayBuffViewImpl")
      x.asInstanceOf[Exp[MultiArrayBuffViewImpl[T]]]
    }
  }

  implicit def indicesImplOps(x: Exp[AbstractIndices]) = new AbstractIndicesImplOpsCls(x)
  class AbstractIndicesImplOpsCls(x: Exp[AbstractIndices]) {
    def toSeq(n: Int): Seq[Exp[Int]] = Seq.tabulate(n){d => x(d)}
    def size(n: Int): Exp[Int] = productTree(toSeq(n))
  }

  implicit def multiarrayimplToOpsCls(ma: Exp[MultiArrayImpl[_]]) = new MultiArrayImplOpsCls(ma)
  class MultiArrayImplOpsCls(x: Exp[MultiArrayImpl[_]]) {
    def dims = Seq.tabulate(rank(x)){d => field[Int](x, "dim" + d)}
    def dim(d: Int) = field[Int](x, "dim" + d)
  }
  implicit def multiarrayviewimplToOpsCls(ma: Exp[MultiArrayViewImpl[_]]) = new MultiArrayViewImplOpsCls(ma)
  class MultiArrayViewImplOpsCls(x: Exp[MultiArrayViewImpl[_]]) {
    def start = Seq.tabulate(rank(x)){d => field[Int](x, "ofs" + d)}
    def stride = Seq.tabulate(rank(x)){d => field[Int](x, "stride" + d)}
  }
  implicit def multiarraybuffviewimplToOpsCls(ma: Exp[MultiArrayBuffViewImpl[_]]) = new MultiArrayBuffViewImplOpsCls(ma)
  class MultiArrayBuffViewImplOpsCls(x: Exp[MultiArrayBuffViewImpl[_]]) {
    def start = Seq.tabulate(rank(x)){d => field[Int](x, "ofs" + d)}
    def stride = Seq.tabulate(rank(x)){d => field[Int](x, "stride" + d)}
  }

  def transformManifest[T](t: Manifest[T], a: ArrayProperties, inner: Manifest[_]): Manifest[_] = {
    sys.error("Don't know how to transform type " + t + " with layout " + layout(a))
  }

  // --- Implement helpers
  // TODO: Is mod actually defined on Exp[Int]? Accessible here?
  def calcIndices(v: Exp[Int], dims: Seq[Exp[Int]]): Seq[Exp[Int]] = {
    Seq.tabulate(dims.length) { d => 
      if (d == dims.length - 1) { delite_int_mod(v, dims(d)) }
      else { delite_int_mod( delite_int_divide(v, productTree(dims.drop(d + 1))), dims(d)) }
    }
  }

  def createDims[T:Manifest](ma: Exp[DeliteMultiArray[T]]): Seq[Exp[Int]] = {
    ma.asMultiArrayImpl.dims
  }

  // --- Nested atomic updates
  // Requires that trace has already been transformed (has no MultiArrayTracers)
  def reconstructTrace(sym: Exp[Any], trace: List[AtomicTracer], p: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
    if (trace.isEmpty) {sym}
    else (sym.tp, trace.head, p) match {
      // TODO: How to reconstruct variable trace?
      //case (_, VarTracer, _) =>    
      //  val childSym = readVar(sym)

      case (StructType(_,elems), StructTracer(index), symProps: StructProperties) =>
        val fType = elems.find(_._1 == index).getOrElse{throw new RuntimeException(index + " is not a field of type " + sym.tp)}._2
        val childSym = field(sym,index)(mtype(fType), ctx)
        setProps(childSym, symProps.child(index))(ctx)
        reconstructTrace(childSym, trace.tail, symProps.child(index).get)

      case (tp, ArrayTracer(i), symProps: ArrayProperties) if isDeliteArrayType(tp) => 
        val childSym = darray_apply(sym.asInstanceOf[Exp[DeliteArray[Any]]], i)
        setProps(childSym, symProps.child)(ctx)
        reconstructTrace(childSym, trace.tail, symProps.child.get) 

      case _ => sys.error("Error while reconstructing nested apply trace")
    }
  }

  def transformTracer[T:Manifest](ma: Exp[DeliteMultiArray[T]], t: MultiArrayTracer): List[AtomicTracer] = {
    sys.error("Don't know how to implement tracer for layout " + layout(ma))
  }

  // NOTE: This is a bit counterintuitive. For something like:
  // val x = Struct(data: Array2D[Int], ...)
  // x.data(1, 2) = 3
  // we actually now output something of the form:
  // val index = 1 * x.data.cols + 2   // Index pre-calculation
  // x.data.data(index) = 3            // Atomic write

  // Comment: In transforming nested atomic writes, we want to get the symbol representing the inner
  // data structure since we potentially need to pull dimensions/offsets/etc. from that. However, 
  // we also want to keep the metadata for all intermediate lookups around
  // Steps should be:
  //  1. Transform trace
  //    - requires reconstructing intermediate symbols for some MultiArray index calculations
  //  2. Reconstruct inner symbol 
  //  3. Transform inner def with reconstructed symbol
  //  4. reflect atomic write on transformed inner def
  def transformNestedAtomicWrite[A:Manifest](sym: Exp[Any], trace: List[AtomicTracer], d: AtomicWrite[A])(implicit ctx: SourceContext): Exp[Unit] = {
    
    /** 
     * @param sym     - most recently available/reconstructed symbol
     * @param done    - transformed part of trace
     * @param current - subset of done, trace from 'sym' to current tracer
     * @param remain  - untransformed section of trace
     */
    def transformTrace(sym: Exp[Any], done: List[AtomicTracer], current: List[AtomicTracer], remain: List[AtomicTracer]): (Exp[Any], List[AtomicTracer], List[AtomicTracer]) = {
      if (remain.isEmpty)
        (sym, done, current)
      else if (remain.indexWhere(t => t.isInstanceOf[MultiArrayTracer]) == -1)
        (sym, done ++ remain, current ++ remain)
      else (remain.head) match {
        case VarTracer => transformTrace(sym, done :+ VarTracer, current :+ VarTracer, remain.tail)
        case t@ArrayTracer(i) => transformTrace(sym, done :+ t, current :+ t, remain.tail)
        case t@StructTracer(f) => transformTrace(sym, done :+ t, current :+ t, remain.tail)
        case t: MultiArrayTracer => 
          val cur = reconstructTrace(sym, current, props(sym)).asInstanceOf[Exp[DeliteMultiArray[Any]]]
          val t2 = transformTracer(cur, t)
          transformTrace(cur, done ++ t2, t2, remain.tail)
        case x => sys.error("Error while transforming write tracer " + x)
      }
    }

    val (cur, full, partial) = transformTrace(sym, Nil, Nil, trace)
    val inner = reconstructTrace(cur,partial,props(cur))

    d match {
      case DeliteMultiArrayUpdate(_,i,x) => implementUpdate(inner.asInstanceOf[Exp[DeliteMultiArray[A]]], i, x)
      case DeliteMultiArrayInsert(_,x,i) => implementInsert(inner.asInstanceOf[Exp[DeliteArray1D[A]]], i, x)
      case DeliteMultiArrayInsertAll(_,x,a,i) => implementInsertAll(inner.asInstanceOf[Exp[DeliteMultiArray[A]]], a, i, x)
      case DeliteMultiArrayRemove(_,a,s,l) => implementRemove(inner.asInstanceOf[Exp[DeliteMultiArray[A]]], a, s, l)
      case _ => reflectWrite(sym)(NestedAtomicWrite(sym, full, d))
    }
  }

  // --- Properties
  def implementDim[T:Manifest](ma: Exp[DeliteMultiArray[T]], i: Int)(implicit ctx: SourceContext): Exp[Int] = {
    ma.asMultiArrayImpl.dim(i)
  }
  def implementSize[T:Manifest](ma: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Exp[Int] = {
    productTree(ma.asMultiArrayImpl.dims)
  }
  /*def implementStart[T:Manifest](ma: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Exp[Indices] = layout(ma).dtype match {
    case Plain => indices_new(Seq.fill(rank(ma)){Const(0)})
    case Buffer => indices_new(Seq.fill(rank(ma)){Const(0)})
    case View => indices_new(ma.asMultiArrayViewImpl.start)
    case BufferView => indices_new(ma.asMultiArrayBuffViewImpl.start)
  }
  def implementStride[T:Manifest](ma: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Exp[Indices] = layout(ma).dtype match {
    case Plain => indices_new(createDims(ma))
    case Buffer => indices_new(createDims(ma))
    case View => indices_new(ma.asMultiArrayViewImpl.stride)
    case BufferView => indices_new(ma.asMultiArrayBuffViewImpl.stride)
  }*/

  // --- Array constructors 
  def implementNew[A:Manifest](dims: Seq[Exp[Int]], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement new multiarray for target layout " + layout(out))
  }
  def implementView[A:Manifest](ma: Exp[DeliteMultiArray[A]], start: Seq[Exp[Int]], stride: Seq[Exp[Int]], dims: Seq[Rep[Int]], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement view creation for layout " + layout(ma) + " and output layout " + layout(out))
  }

  // --- Single element operations
  def implementApply[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[AbstractIndices])(implicit ctx: SourceContext): Exp[A] = {
    sys.error("Don't know how to implement apply node for layout " + layout(ma)) 
  }
  def implementUpdate[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[AbstractIndices], x: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
    sys.error("Don't know how to implement update node for layout " + layout(ma))
  }

  // --- Array permute / reshaping
  def implementPermute[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement permute operation for input layout " + layout(ma) + " and output layout " + layout(out))
  }
  def implementReshape[A:Manifest](ma: Exp[DeliteMultiArray[A]], dims: Seq[Exp[Int]], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement reshape operation for input layout " + layout(ma) + " and output layout " + layout(out))
  }

  // --- Parallel ops
  def implementFromFunction[A:Manifest](dims: Seq[Exp[Int]], body: Block[A], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement map indices for output layout " + layout(out))
  }
  def implementMap[A:Manifest,R:Manifest](in: Exp[DeliteMultiArray[A]], body: Block[R], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement map for input layout " + layout(in) + " with output layout " + layout(out))
  }
  def implementZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DeliteMultiArray[A]], inB: Exp[DeliteMultiArray[B]], body: Block[R], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement map for layouts " + layout(inA) + " and " + layout(inB) + " for output layout " + layout(out))
  }
  def implementReduce[A:Manifest](in: Exp[DeliteMultiArray[A]], lookup: Block[A], body: Block[A], zero: Exp[A], v: Sym[Int], i: Exp[LoopIndices], rV: (Sym[A], Sym[A]), out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
    val dims = createDims(in)
    val size = implementSize(in)
    val inds = calcIndices(v, dims)
    val i2 = loopindices_new(v, inds, dims)

    // Probably don't need subst. for i for lookup and zero..
    val (mirroredLookup, mirroredBody, mirroredZero) = withSubstScope(i -> i2) { (f(lookup), f(body), f(zero)) }
    reflectPure(ReduceFactory(v, rV, size, mirroredLookup, mirroredBody, mirroredZero))
  }
  def implementForeach[A:Manifest](in: Exp[DeliteMultiArray[A]], body: Block[Unit], v: Sym[Int], i: Exp[LoopIndices])(implicit ctx: SourceContext): Exp[Unit] = {
    val dims = createDims(in)
    val size = implementSize(in)
    val inds = calcIndices(v, dims)
    val i2 = loopindices_new(v, inds, dims)

    val mirroredBody = withSubstScope(i -> i2) { f(body) }
    reflectEffect(ForeachFactory(v, size, mirroredBody), summarizeEffects(mirroredBody).star andAlso Simple())
  }
  def implementForIndices[A:Manifest](in: Exp[DeliteMultiArray[A]], body: Block[Unit], v: Sym[Int], i: Exp[LoopIndices])(implicit ctx: SourceContext): Exp[Unit] = {
    val dims = createDims(in)
    val size = implementSize(in)
    val inds = calcIndices(v, dims)
    val i2 = loopindices_new(v, inds, dims)

    val mirroredBody = withSubstScope(i -> i2) { f(body) }
    reflectEffect(ForeachFactory(v, size, mirroredBody), summarizeEffects(body).star andAlso Simple())
  }
  def implementNDMap[A:Manifest,B:Manifest](op: DeliteMultiArrayNDMap[_,_], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement ND map for input " + layout(op.in) + " and output " + layout(out))
  }
  def implementGroupBy[A:Manifest,K:Manifest](op: DeliteMultiArrayGroupBy[_,_])(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement groupby for input " + layout(op.in))
  }
  def implementGroupByReduce[A:Manifest,K:Manifest,V:Manifest](op: DeliteMultiArrayGroupByReduce[_,_,_])(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement groupby-reduce for input " + layout(op.in))
  }

  // --- 1D Parallel Ops
  def implementMapFilter[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], mapFunc: Block[B], filtFunc: Block[Boolean], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement map-filter for input " + layout(in) + " and output " + layout(out))
  }
  def implementFlatMap[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], body: Block[DeliteArray1D[B]], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement flatmap for input " + layout(in) + " and output " + layout(out))
  }

  // --- Buffer ops
  def implementInsert[A:Manifest](ma: Exp[DeliteArray1D[A]], i: Exp[Int], x: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
    sys.error("Don't know how to implement insert for layout " + layout(ma))
  }
  def implementAppend[A:Manifest](ma: Exp[DeliteArray1D[A]], x: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
    sys.error("Don't know how to implement append for layout " + layout(ma))
  }
  def implementInsertAll[A:Manifest](ma: Exp[DeliteMultiArray[A]], axis: Int, i: Exp[Int], x: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext): Exp[Unit] = {
    sys.error("Don't know how to implement insert all for lhs " + layout(ma) + " and rhs " + layout(x))
  }
  def implementRemove[A:Manifest](ma: Exp[DeliteMultiArray[A]], axis: Int, start: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    sys.error("Don't know how to implement remove for layout " + layout(ma))
  }

  // --- Misc.
  def implementMkString[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]]): Exp[String] = {
    sys.error("Don't know how to implement mkString for layout " + layout(ma))
  }

  // --- 1D Ops
  def implementSortIndices(len: Exp[Int], i: (Sym[Int], Sym[Int]), body: Block[Int]): Exp[DeliteArray[Int]] = {
    reflectPure(DeliteArraySortIndices(len, i, body))
  }
  def implementStringSplit(str: Exp[String], split: Exp[String], lim: Exp[Int])(implicit ctx: SourceContext): Exp[DeliteArray[String]] = { 
    darray_split_string(str, split, lim)
  }

  // --- 2D Ops
  def implementMatrixMultiply[A:Manifest:Numeric](lhs: Exp[DeliteArray2D[A]], rhs: Exp[DeliteArray2D[A]])(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement matrix multiply for layouts " + layout(lhs) + ", " + layout(rhs))
  }
  def implementMatrixVectorMultiply[A:Manifest:Numeric](mat: Exp[DeliteArray2D[A]], vec: Exp[DeliteArray1D[A]])(implicit ctx: SourceContext): Exp[Any] = {
    sys.error("Don't know how to implement matrix-vector multiply for layotus " + layout(mat) + ", " + layout(vec))
  }

  // --- Wrap
  def implementBuffify[A:Manifest](ma: Exp[DeliteMultiArray[A]], out: SymbolProperties): Exp[Any] = {
    sys.error("Don't know how to implement buffify for input layout " + layout(ma) + " and output layout " + layout(out))
  }
  def implementViewify[A:Manifest](ma: Exp[DeliteMultiArray[A]], out: SymbolProperties): Exp[Any] = {
    sys.error("Don't know how to implement viewify for input layout " + layout(ma) + " and output layout " + layout(out))
  }
}
