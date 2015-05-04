package ppl.delite.framework.transform

import scala.virtualization.lms.common._
import scala.reflect.{SourceContext,RefinedManifest}

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.analysis._
import ppl.delite.framework.visit._
import ppl.delite.framework.Util._

trait MultiArrayImpl[T] extends DeliteCollection[T]
// TBD: Bit annoying to have four of these for each layout type - some way to avoid this?
trait MultiArrayPlainImpl[T] extends MultiArrayImpl[T]
trait MultiArrayViewImpl[T] extends MultiArrayImpl[T]
trait MultiArrayBuffImpl[T] extends MultiArrayImpl[T]
trait MultiArrayBuffViewImpl[T] extends MultiArrayImpl[T]

trait MultiArrayImplExp extends MultiArrayWrapExp with DeliteInternalOpsExp with DeliteSimpleOpsExp with LayoutMetadata { self => 

  val implementer: MultiArrayImplementer  

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

  def calcIndices(v: Exp[Int], dims: Seq[Exp[Int]]): Seq[Exp[Int]] = {
    if (dims.length == 1) { Seq(v) }
    else {
      Seq.tabulate(dims.length) { d => 
        if (d == dims.length - 1) { delite_int_mod(v, dims(d)) }
        else { delite_int_mod( delite_int_divide(v, productTree(dims.drop(d + 1))), dims(d)) }
      }
    }
  }

  // --- Convenience infix operations for DeliteMultiArray
  implicit def multiarrayToCastOps[T:Manifest](x: Exp[DeliteMultiArray[T]]) = new MultiArrayCastOpsCls[T](x)
  class MultiArrayCastOpsCls[T:Manifest](x: Exp[DeliteMultiArray[T]]) { 
    def asDeliteArray = {
      assert(isDeliteArrayTpe(x.tp), "Cannot cast " + x.tp + " to DeliteArray!")
      x.asInstanceOf[Exp[DeliteArray[T]]]
    }
    def asDeliteArrayBuffer = {
      assert(isDeliteArrayBufferTpe(x.tp), "Cannot cast " + x.tp + " to DeliteArrayBuffer")
      x.asInstanceOf[Exp[DeliteArrayBuffer[T]]]
    }
    def asMultiArrayImpl = {
      assert(isMultiArrayImpl(x), "Cannot cast " + x.tp + " to MultiArrayImpl")
      x.asInstanceOf[Exp[MultiArrayImpl[T]]]
    }
    def asMultiArrayViewImpl = {
      assert(isMultiArrayViewImpl(x), "Cannot cast " + x.tp + " to MultiArrayViewImpl")
      x.asInstanceOf[Exp[MultiArrayViewImpl[T]]]
    }
    def asMultiArrayBuffImpl = {
      assert(isMultiArrayBuffImpl(x), "Cannot cast " + x.tp + " to MultiArrayBuffImpl")
      x.asInstanceOf[Exp[MultiArrayBuffImpl[T]]]
    }
    def asMultiArrayBuffViewImpl = {
      assert(isMultiArrayBuffViewImpl(x), "Cannot cast " + x.tp + " to MultiArrayBuffViewImpl")
      x.asInstanceOf[Exp[MultiArrayBuffViewImpl[T]]]
    }
  }

  // ---- Convenience infix operations for MultiArrayImpl and descendants
  implicit def multiarrayimplToOpsCls(ma: Exp[MultiArrayImpl[_]]) = new MultiArrayImplOpsCls(ma)
  class MultiArrayImplOpsCls(x: Exp[MultiArrayImpl[_]]) {
    def dims = Seq.tabulate(implementer.rank(x)){d => field[Int](x, "dim" + d)}
    def dim(d: Int) = field[Int](x, "dim" + d)
    def size = productTree(this.dims)
  }
  implicit def multiarrayviewimplToOpsCls(ma: Exp[MultiArrayViewImpl[_]]) = new MultiArrayViewImplOpsCls(ma)
  class MultiArrayViewImplOpsCls(x: Exp[MultiArrayViewImpl[_]]) {
    def start = Seq.tabulate(implementer.rank(x)){d => field[Int](x, "ofs" + d)}
    def stride = Seq.tabulate(implementer.rank(x)){d => field[Int](x, "stride" + d)}
  }
  implicit def multiarraybuffviewimplToOpsCls(ma: Exp[MultiArrayBuffViewImpl[_]]) = new MultiArrayBuffViewImplOpsCls(ma)
  class MultiArrayBuffViewImplOpsCls(x: Exp[MultiArrayBuffViewImpl[_]]) {
    def start = Seq.tabulate(implementer.rank(x)){d => field[Int](x, "ofs" + d)}
    def stride = Seq.tabulate(implementer.rank(x)){d => field[Int](x, "stride" + d)}
  }

  // --- Convenience infix operations for AbstractIndices
  implicit def indicesImplOps(x: Exp[AbstractIndices]) = new AbstractIndicesImplOpsCls(x)
  class AbstractIndicesImplOpsCls(x: Exp[AbstractIndices]) {
    def toSeq(n: Int): Seq[Exp[Int]] = Seq.tabulate(n){d => x(d)}
    def size(n: Int): Exp[Int] = productTree(toSeq(n))
  }

  def isLoopIndices[T](x: Exp[T]) = isSubtype(x.tp.erasure, classOf[LoopIndices])
  def isIndices[T](x: Exp[T]) = isSubtype(x.tp.erasure, classOf[Indices])

  def isMultiArrayImpl[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[MultiArrayImpl[_]])
  def isMultiArrayViewImpl[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[MultiArrayViewImpl[_]])
  def isMultiArrayBuffImpl[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[MultiArrayViewImpl[_]])
  def isMultiArrayBuffViewImpl[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[MultiArrayBuffViewImpl[_]])

  def asMultiArrayImpl[T](x: Exp[DeliteCollection[T]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[MultiArrayImpl[T]]]

  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isMultiArrayImpl(x)) implementer.implementSize(x.asInstanceOf[Exp[DeliteMultiArray[A]]])
    else super.dc_size(x)
  }

  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isMultiArrayImpl(x)) implementer.implementApply(x.asInstanceOf[Exp[DeliteMultiArray[A]]], loopindices_new(n, calcIndices(n, asMultiArrayImpl(x).dims)))
    else if (isDeliteArray(x) || isDeliteArrayBuffer(x)) {
      if (implementer.getLayout(x).nonEmpty)
        implementer.implementApply(x.asInstanceOf[Exp[DeliteMultiArray[A]]], loopindices_new(n, Seq(n)))
      else
        super.dc_apply(x,n)   // bit kludgey - this may be called before implementer is run, in which case use super
    }
    else super.dc_apply(x,n)
  }

  trait MultiArrayImplementer extends TransformerBase with MultiArrayHelperStageThree {
    val IR: self.type

    // Get inner element type for given data structure type
    def dataTp[A,B:Manifest](x: Exp[A]): Manifest[B] = dataTp(x.tp)
    def dataTp[A,B:Manifest](tp: Manifest[A]): Manifest[B] = tp match {
      case StructType(_,elems) => elems.find(_._1 == "data").getOrElse(
        throw new RuntimeException("Can't find data field for " + tp)
      )._2.typeArguments(0).asInstanceOf[Manifest[B]]
      case t if !t.typeArguments.isEmpty => t.typeArguments(0).asInstanceOf[Manifest[B]]
      case _ => sys.error("Cannot get data type of " + tp + " - type has no type arguments")
    }

    // --- Type transformation
    /**
     * Transform saved manifests to match transformed IR
     * @param tp - element type manifest, to be transformed
     * @param p  - symbol properties matching type 
     */
    def ttype[A,B:Manifest](tp: Manifest[A], p: SymbolProperties): Manifest[B] = (tp,p) match { 
      case (StructType(_,elems), s: StructProperties) => 
        new RefinedManifest[Record] {
          def runtimeClass = classOf[Record]
          val fields = elems.map{f => f._1 -> ttype(f._2, s.child(f._1).get) }
        }.asInstanceOf[Manifest[B]]
      case (tp, a: ArrayProperties) => 
        val inner = ttype(tp.typeArguments(0), a.child.get)
        transformManifest(tp, a, inner).asInstanceOf[Manifest[B]]
      case (tp, a: ScalarProperties) => tp.asInstanceOf[Manifest[B]]
      case _ => sys.error("Don't know how to transform type " + tp + " with associated properties " + p)
    }

    // TODO: This can be generalized as part of an "AbstractNodeTransformer" trait if we ever need one
    override def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = d match {
      case d: AbstractNode[_] if d.family == "multiarray" => Some(transformAbstract(s, d))
      case NestedAtomicWrite(_,trace,f) =>
        if ((f.isInstanceOf[AbstractNode[_]] && f.asInstanceOf[AbstractNode[_]].family == "multiarray") ||
            trace.map{case t: AbstractAtomicTracer => t.family == "multiarray"; case _ => false }.fold(false){_||_})
          Some(transformAbstract(s, d))
        else 
          None  
      case Reflect(f: AbstractNode[_],_,_) if f.family == "multiarray" => Some(transformAbstract(s, d))
      case Struct(tag, elems) if hasMultiArrayChild(s.tp) => Some(transformAbstract(s, d))
      case Reflect(Struct(tag, elems),_,_) if hasMultiArrayChild(s.tp) => Some(transformAbstract(s, d))
      case Reify(x,u,es) => Some(reflectPure(Reify(f(x),mapOver(f,u),f(es)))(f(x).tp,ctx))
      case _ => None
    }

    def transformAbstract[A](s: Exp[A], d: Def[A])(implicit ctx: SourceContext): Exp[Any] = transformMultiArrayDef(s, d)

    /**
     * Transforms abstract MultiArray IR nodes to concrete implementations using types like
     * DeliteArray, DeliteArrayBuffer, and anonymous Structs
     */
    def transformMultiArrayDef(s: Exp[Any], d: Def[Any])(implicit ctx: SourceContext): Exp[Any] = d match {
      // --- Nested Writes
      case op@NestedAtomicWrite(sym, trace, d) => transformNestedAtomicWrite(f(sym), trace.map{r => mirrorTrace(r,f)}, d)(dataTp(f(sym)), ctx)

      // --- Properties
      case DeliteMultiArrayRank(ma) => Const(rank(f(ma)))
      case DeliteMultiArrayDim(ma,i) => implementDim(f(ma),i)(dataTp(f(ma)), ctx)
      case DeliteMultiArraySize(ma) => implementSize(f(ma))(dataTp(f(ma)), ctx)

      // --- Array Constructors
      case op@DeliteMultiArrayNew(dims) => implementNew(f(dims), props(s))(ttype(op.mA, mdat(s)), ctx)
      case DeliteMultiArrayView(ma,o,t,d) => implementView(f(ma),f(o),f(t),f(d),props(s))(dataTp(f(ma)), ctx)

      // --- Single element ops
      case DeliteMultiArrayApply(ma,i) => implementApply(f(ma),f(i))(dataTp(f(ma)), ctx)
      case DeliteMultiArrayUpdate(ma,i,x) => implementUpdate(f(ma),f(i),f(x))(dataTp(f(ma)), ctx)

      // --- Array permute / reshape
      case DeliteMultiArrayPermute(ma,config) => implementPermute(f(ma),config,props(s))(dataTp(f(ma)), ctx)
      case DeliteMultiArrayReshape(ma,dims) => implementReshape(f(ma),f(dims),props(s))(dataTp(f(ma)), ctx)

      // --- Parallel ops
      case op@DeliteMultiArrayFromFunction(dims,_) => 
        val body = f(op.body)
        implementFromFunction(f(dims),body,op.v,op.i,props(s))(mtype(body.tp), ctx)

      case op@DeliteMultiArrayMap(ma,_) => 
        val body = f(op.body)
        implementMap(f(ma),body,op.v,op.i,props(s))(dataTp(f(ma)),mtype(body.tp), ctx)

      case op@DeliteMultiArrayZipWith(ma,mb,_) => 
        val body = f(op.body)
        implementZipWith(f(ma),f(mb),body,op.v,op.i,props(s))(dataTp(f(ma)),dataTp(f(mb)),mtype(body.tp), ctx)

      case op@DeliteMultiArrayReduce(ma,_,_) => implementReduce(f(ma),f(op.lookup),f(op.body),f(op.zero),op.v,op.i,op.rV,props(s))(dataTp(f(ma)), ctx)

      case op@DeliteMultiArrayForeach(ma,_) => implementForeach(f(ma),f(op.body),op.v,op.i)(dataTp(f(ma)), ctx)
      case op@DeliteMultiArrayForIndices(ma,_) => implementForIndices(f(ma),f(op.body),op.v,op.i)(dataTp(f(ma)), ctx)
      
      // TODO
      case op@DeliteMultiArrayNDMap(ma,_,_) => implementNDMap(op,props(s))(dataTp(f(ma)),ttype(op.mB,mdat(s)), ctx)
      case op@DeliteMultiArrayGroupBy(ma,_) => implementGroupBy(op)(dataTp(f(ma)),op.keyFunc.tp, ctx)
      case op@DeliteMultiArrayGroupByReduce(ma,_,_,_) => implementGroupByReduce(op)(dataTp(ma.tp),op.keyFunc.tp,op.valFunc.tp, ctx)

      // --- 1D Parallel ops
      case op@DeliteMultiArrayMapFilter(ma,_,_) => 
        val body = f(op.mapFunc)
        implementMapFilter(f(ma),body,f(op.filtFunc),op.v,op.i,props(s))(dataTp(f(ma)),mtype(body.tp),ctx)

      case op@DeliteMultiArrayFlatMap(ma,_) =>
        val body = f(op.body)
        implementFlatMap(f(ma),body,op.v,op.i,props(s))(dataTp(f(ma)),dataTp(body.tp),ctx)

      // --- Buffer ops
      case DeliteMultiArrayInsert(ma,x,i) => implementInsert(f(ma),f(i),f(x))(dataTp(f(ma)),ctx)
      case DeliteMultiArrayAppend(ma,x) => implementAppend(f(ma),f(x))(dataTp(f(ma)), ctx)
      case DeliteMultiArrayInsertAll(ma,x,a,i) => implementInsertAll(f(ma),a,f(i),f(x))(dataTp(f(ma)),ctx)
      case DeliteMultiArrayRemove(ma,a,s,l) => implementRemove(f(ma),a,f(s),f(l))(dataTp(f(ma)),ctx)

      // --- Misc
      case op@DeliteMultiArrayMkString(ma, dels) => implementMkString(f(ma),f(dels))(dataTp(f(ma)))
      
      // --- 1D Ops
      case DeliteMultiArraySortIndices(len,i,body) => implementSortIndices(f(len),(f(i._1).asInstanceOf[Sym[Int]],f(i._2).asInstanceOf[Sym[Int]]),f(body),props(s))(ctx)
      case DeliteMultiArrayStringSplit(str,split,lim) => implementStringSplit(f(str),f(split),f(lim),props(s))(ctx)

      // --- 2D Ops
      case op@DeliteMatrixMultiply(lhs,rhs) => implementMatrixMultiply(f(lhs),f(rhs))(op.mA,op.nA,ctx)
      case op@DeliteMatrixVectorMultiply(mat,vec) => implementMatrixVectorMultiply(f(mat),f(vec))(op.mA,op.nA,ctx)

      // --- Structs
      case Struct(tag, elems) =>
        val fields = elems map {elem => elem._2 match {
          case Def(ReadVar(v)) => (elem._1, true, (x: Exp[_]) => f(elem._2))
          case _ => (elem._1, false, (x: Exp[_]) => f(elem._2))
        }}
        val out = record_new(fields)(ttype(s.tp, props(s)))
        elems foreach {elem => setField(out, getProps(f(elem._2)), elem._1) }
        copyMetadata(out, props(s))
        (out)

      // --- Wrapping
      case MultiArrayBuffify(ma) => implementBuffify(f(ma),props(s))(dataTp(f(ma)))
      case MultiArrayViewify(ma) => implementViewify(f(ma),props(s))(dataTp(f(ma)))

      case Reflect(d, u, es) => 
        val e = transformAbstract(s, d)
        transferMetadata(e, s, d)

        e match {
          // Exception added for MkString implementation returning a reify node
          // Reflect(Reify) shouldn't happen in general
          case Def(Reify(_,_,_)) => e 

          case Def(Reflect(d2, u2, es2)) => 
            val out = reflectMirrored(Reflect(d2, mapOver(f,u) andAlso u2, (f(es) ++ es2).distinct))(mtype(e.tp), ctx)
            setProps(out, getProps(e))
              
            // Also need to scrub intermediate Reflect node, otherwise its in the context list
            if (out != e) { scrubSym(e.asInstanceOf[Sym[Any]]) }
            (out)
          case Def(d2) =>
            val out = reflectMirrored(Reflect(d2, mapOver(f,u), f(es)))(mtype(e.tp), ctx)
            setProps(out, getProps(e))

            // Scrub intermediate def, otherwise its in the context list
            if (out != e) { scrubSym(e.asInstanceOf[Sym[Any]]) }
            (out)
          case c: Const[_] => (c)   // This should never happen?
        }
    }

    override def transferMetadata(sub: Exp[Any], orig: Exp[Any], d: Def[Any])(implicit ctx: SourceContext) = d match {
      case DeliteMultiArrayNew(_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayView(_,_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayApply(_,_) if getProps(orig).nonEmpty => copyMetadata(sub,props(orig))
      case DeliteMultiArrayPermute(_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayReshape(_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayFromFunction(_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayMap(_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayZipWith(_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayReduce(_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayNDMap(_,_,_) => copyMetadata(sub,props(orig))

      case DeliteMultiArrayMapFilter(_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayFlatMap(_,_) => copyMetadata(sub,props(orig))

      case DeliteMultiArraySortIndices(_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayStringSplit(_,_,_) => copyMetadata(sub,props(orig))

      case DeliteMatrixMultiply(_,_) => copyMetadata(sub,props(orig))
      case DeliteMatrixVectorMultiply(_,_) => copyMetadata(sub,props(orig))

      case MultiArrayBuffify(_) => copyMetadata(sub,props(orig))
      case MultiArrayViewify(_) => copyMetadata(sub,props(orig))

      case _ => // Nothing
    }

    def transformManifest[T](t: Manifest[T], a: ArrayProperties, inner: Manifest[_]): Manifest[_] = {
      sys.error("Don't know how to transform type " + t + " with layout " + layout(a))
    }

    // --- Nested atomic updates
    def transformTracer[T:Manifest](ma: Exp[DeliteMultiArray[T]], t: MultiArrayTracer): List[AtomicTracer] = {
      sys.error("Don't know how to implement tracer for layout " + layout(ma))
    }

    // Requires that trace has already been transformed (has no MultiArrayTracers)
    def reconstructTrace(sym: Exp[Any], trace: List[AtomicTracer], p: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      if (trace.isEmpty) {sym}
      else (sym.tp, trace.head, p) match {
        // TODO: How to reconstruct variable trace? Is this needed, or does it happen implicitly?
        //case (_, VarTracer, _) =>    
        //  val childSym = readVar(sym)

        case (StructType(_,elems), StructTracer(index), symProps: StructProperties) =>
          val fType = elems.find(_._1 == index).getOrElse{throw new RuntimeException(index + " is not a field of type " + sym.tp)}._2
          val childSym = field(sym,index)(mtype(fType), ctx)
          setProps(childSym, symProps.child(index))(ctx)
          reconstructTrace(childSym, trace.tail, symProps.child(index).get)

        case (tp, ArrayTracer(i), symProps: ArrayProperties) if isDeliteArrayTpe(tp) => 
          val childSym = darray_apply(sym.asInstanceOf[Exp[DeliteArray[Any]]], i)
          setProps(childSym, symProps.child)(ctx)
          reconstructTrace(childSym, trace.tail, symProps.child.get) 

        case _ => sys.error("Error while reconstructing nested apply trace")
      }
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
        case DeliteMultiArrayAppend(_,x) => implementAppend(inner.asInstanceOf[Exp[DeliteArray1D[A]]], x)
        case DeliteMultiArrayInsertAll(_,x,a,i) => implementInsertAll(inner.asInstanceOf[Exp[DeliteMultiArray[A]]], a, i, x)
        case DeliteMultiArrayRemove(_,a,s,l) => implementRemove(inner.asInstanceOf[Exp[DeliteMultiArray[A]]], a, s, l)
        case _ => reflectWrite(sym)(NestedAtomicWrite(sym, full, d))
      }
    }

    // --- Properties
    def getDims[T:Manifest](ma: Exp[DeliteMultiArray[T]]): Seq[Exp[Int]] = {
      if (isMultiArrayImpl(ma)) ma.asMultiArrayImpl.dims
      else sys.error("Don't know how to implement dims for type " + ma.tp)
    }

    def implementDim[T:Manifest](ma: Exp[DeliteMultiArray[T]], i: Int)(implicit ctx: SourceContext): Exp[Int] = {
      if (isMultiArrayImpl(ma)) ma.asMultiArrayImpl.dim(i)
      else sys.error("Don't know how to implement dim for type " + ma.tp)
    }
    def implementSize[T:Manifest](ma: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Exp[Int] = {
      if (isMultiArrayImpl(ma)) ma.asMultiArrayImpl.size
      else sys.error("Don't know how to implement size for type " + ma.tp)
    }

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
      val dims = getDims(in)
      val size = implementSize(in)
      val inds = calcIndices(v, dims)
      val i2 = loopindices_new(v, inds)

      // Probably don't need subst. for i for lookup and zero..
      val (mirroredLookup, mirroredBody, mirroredZero) = withSubstScope(i -> i2) { (f(lookup), f(body), f(zero)) }
      reflectPure(ReduceFactory(v, rV, size, mirroredLookup, mirroredBody, mirroredZero))
    }
    def implementForeach[A:Manifest](in: Exp[DeliteMultiArray[A]], body: Block[Unit], v: Sym[Int], i: Exp[LoopIndices])(implicit ctx: SourceContext): Exp[Unit] = {
      val dims = getDims(in)
      val size = implementSize(in)
      val inds = calcIndices(v, dims)
      val i2 = loopindices_new(v, inds)

      val mirroredBody = withSubstScope(i -> i2) { f(body) }
      reflectEffect(ForeachFactory(v, size, mirroredBody), summarizeEffects(mirroredBody).star andAlso Simple())
    }
    def implementForIndices[A:Manifest](in: Exp[DeliteMultiArray[A]], body: Block[Unit], v: Sym[Int], i: Exp[LoopIndices])(implicit ctx: SourceContext): Exp[Unit] = {
      val dims = getDims(in)
      val size = implementSize(in)
      val inds = calcIndices(v, dims)
      val i2 = loopindices_new(v, inds)

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
    def implementSortIndices(len: Exp[Int], i: (Sym[Int], Sym[Int]), body: Block[Int], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement sortIndices for output layout " + layout(out))
    }
    def implementStringSplit(str: Exp[String], split: Exp[String], lim: Exp[Int], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = { 
      sys.error("Don't know how to implement string split for output layout " + layout(out))
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
}
