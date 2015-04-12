package ppl.delite.framework.transform

import scala.virtualization.lms.common._
import scala.reflect.{SourceContext, RefinedManifest}
import ppl.delite.framework.ops._ 
import ppl.delite.framework.datastructures._

import ppl.delite.framework.analysis.LayoutMetadata
import ppl.delite.framework.visit._
import ppl.delite.framework.Util._

/////////////////
// Flat Layout //
/////////////////

trait FlatArrayPlain[T] extends MultiArrayPlainImpl[T] with Struct
trait FlatArrayView[T] extends MultiArrayViewImpl[T] with Struct
trait FlatArrayBuff[T] extends MultiArrayBuffImpl[T] with Struct
trait FlatArrayBuffView[T] extends MultiArrayBuffViewImpl[T] with Struct

trait FlatArrayImplExp extends MultiArrayImplExp with DeliteOpsExp with DeliteArrayBufferOpsExp {
  def darrayBufferManifest(typeArg: Manifest[_]): Manifest[DeliteArrayBuffer[_]] = makeManifest(classOf[DeliteArrayBuffer[_]], List(typeArg))
  
  private def dataField[T](tp: Manifest[T]): List[(String, Manifest[_])] = List("data" -> darrayManifest(tp))
  private def buffField[T](tp: Manifest[T]): List[(String, Manifest[_])] = List("data" -> darrayBufferManifest(tp))
  private def dimFields(n: Int): List[(String, Manifest[_])] = List.tabulate(n){i => "dim" + i} zip List.fill(n){manifest[Int]}
  private def viewFields(n: Int): List[(String, Manifest[_])] = dimFields(n) ++ (List.tabulate(n){i => "ofs" + i} zip List.fill(n){manifest[Int]}) ++ (List.tabulate(n){i => "stride" + i} zip List.fill(n){manifest[Int]})

  def flatplainManifest[T](tp: Manifest[T], rank: Int): Manifest[FlatArrayPlain[T]]
    = new RefinedManifest[FlatArrayPlain[T]] { 
        def runtimeClass = classOf[FlatArrayPlain[T]]
        val fields = dataField(tp) ++ dimFields(rank)
        //override val typeArguments = List(tp)
    }
  def flatviewManifest[T](tp: Manifest[T], rank: Int): Manifest[FlatArrayView[T]] 
    = new RefinedManifest[FlatArrayView[T]] {
        def runtimeClass = classOf[FlatArrayView[T]]
        val fields = dataField(tp) ++ viewFields(rank) 
        //override val typeArguments = List(tp)
    }
  def flatbuffManifest[T](tp: Manifest[T], rank: Int): Manifest[FlatArrayBuff[T]]
    = new RefinedManifest[FlatArrayBuff[T]] {
        def runtimeClass = classOf[FlatArrayBuff[T]]
        val fields = buffField(tp) ++ dimFields(rank)
        //override val typeArguments = List(tp)
    }
  def flatbuffviewManifest[T](tp: Manifest[T], rank: Int): Manifest[FlatArrayBuffView[T]]
    = new RefinedManifest[FlatArrayBuffView[T]] {
        def runtimeClass = classOf[FlatArrayBuffView[T]]
        val fields = buffField(tp) ++ viewFields(rank)
        //override val typeArguments = List(tp)
    }

  case class DeliteArrayBufferWrap[A:Manifest](data: Exp[DeliteArray[A]], length: Exp[Int])(implicit ctx: SourceContext) extends DeliteStruct[DeliteArrayBuffer[A]] {
    val elems = copyTransformedElems(List("data" -> var_new(data).e, "length" -> var_new(length).e))
    val mA = manifest[A]
  }

  def darray_buffer_wrap[A:Manifest](data: Exp[DeliteArray[A]], length: Exp[Int]) = reflectPure(DeliteArrayBufferWrap(data,length))

  def flatarray_plain[T:Manifest](data: Exp[DeliteArray[T]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayPlain[T]] = {    
    def slf(x: Rep[_]) = (y: Rep[FlatArrayPlain[T]]) => x
    val tp = flatplainManifest(manifest[T], dims.length)
    record_new(("data",false,slf(data)) +: dims.zipWithIndex.map{i => ("dim" + i._2, false, slf(i._1))})(tp)
  }

  def flatarray_view[T:Manifest](data: Exp[DeliteArray[T]], dims: Seq[Exp[Int]], ofs: Seq[Exp[Int]], stride: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayView[T]] = {    
    def slf(x: Rep[_]) = (y: Rep[FlatArrayView[T]]) => x
    val tp = flatviewManifest(manifest[T], dims.length)
    record_new( (("data",false,slf(data)) +: dims.zipWithIndex.map{i => ("dim" + i._2, false, slf(i._1))}) ++ 
               ofs.zipWithIndex.map{d => ("ofs" + d._2, false, slf(d._1))} ++ stride.zipWithIndex.map{d => ("stride" + d._2, false, slf(d._1))})(tp)
  }
  def flatarray_buff[T:Manifest](data: Exp[DeliteArrayBuffer[T]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayBuff[T]] = {
    def slf(x: Rep[_]) = (y: Rep[FlatArrayBuff[T]]) => x
    val tp = flatbuffManifest(manifest[T], dims.length)
    record_new(("data", false, slf(data)) +: dims.zipWithIndex.map{i => ("dim" + i._2, true, slf(i._1))})(tp)
  }
  def flatarray_buffview[T:Manifest](data: Exp[DeliteArrayBuffer[T]], dims: Seq[Exp[Int]], ofs: Seq[Exp[Int]], stride: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayBuffView[T]] = {
    def slf(x: Rep[_]) = (y: Rep[FlatArrayBuffView[T]]) => x
    val tp = flatbuffviewManifest(manifest[T], dims.length)
    record_new( (("data", false, slf(data)) +: dims.zipWithIndex.map{i => ("dim" + i._2, false, slf(i._1))}) ++ 
               ofs.zipWithIndex.map{d => ("ofs" + d._2, false, slf(d._1))} ++ stride.zipWithIndex.map{d => ("stride" + d._2, false, slf(d._1))})(tp)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DeliteArrayBufferWrap(d,l) => reflectPure(new {override val original = Some(f,e) } with DeliteArrayBufferWrap(f(d),f(l))(e.mA, ctx))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteArrayBufferWrap(d,l), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayBufferWrap(f(d),f(l))(e.mA, ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
} 

trait FlatArrayImplementer extends MultiArrayImplementer {  
  val IR: FlatArrayImplExp with LayoutMetadata
  import IR._
  def isFlatArrayPlainType[T](tp: Manifest[T]) = isSubtype(tp.erasure, classOf[FlatArrayPlain[T]])
  def isFlatArrayViewType[T](tp: Manifest[T]) = isSubtype(tp.erasure, classOf[FlatArrayView[T]])
  def isFlatArrayBuffType[T](tp: Manifest[T]) = isSubtype(tp.erasure, classOf[FlatArrayView[T]])
  def isFlatArrayBuffViewType[T](tp: Manifest[T]) = isSubtype(tp.erasure, classOf[FlatArrayBuffView[T]])

  implicit def flatArrayPlainToOpsCls[T:Manifest](x: Exp[FlatArrayPlain[T]])(implicit ctx: SourceContext) = new FlatArrayPlainOpsCls(x)(manifest[T],ctx)
  class FlatArrayPlainOpsCls[T:Manifest](x: Exp[FlatArrayPlain[T]])(implicit ctx: SourceContext) {
    def data = {val res = field[DeliteArray[T]](x, "data"); setProps(res, mdat(x))(ctx); (res) }
  }
  implicit def flatArrayViewToOpsCls[T:Manifest](x: Exp[FlatArrayView[T]])(implicit ctx: SourceContext) = new FlatArrayViewOpsCls(x)(manifest[T],ctx)
  class FlatArrayViewOpsCls[T:Manifest](x: Exp[FlatArrayView[T]])(implicit ctx: SourceContext) {
    def data = {val res = field[DeliteArray[T]](x, "data"); setProps(res, mdat(x))(ctx); (res) }
  }
  implicit def flatArrayBuffToOpsCls[T:Manifest](x: Exp[FlatArrayBuff[T]])(implicit ctx: SourceContext) = new FlatArrayBuffOpsCls(x)(manifest[T],ctx)
  class FlatArrayBuffOpsCls[T:Manifest](x: Exp[FlatArrayBuff[T]])(implicit ctx: SourceContext) {
    def data = {val res = field[DeliteArrayBuffer[T]](x, "data"); setProps(res, mdat(x))(ctx); (res) }
  }
  implicit def flatArrayBuffViewToOpsCls[T:Manifest](x: Exp[FlatArrayBuffView[T]])(implicit ctx: SourceContext) = new FlatArrayBuffViewOpsCls(x)(manifest[T],ctx)
  class FlatArrayBuffViewOpsCls[T:Manifest](x: Exp[FlatArrayBuffView[T]])(implicit ctx: SourceContext) {
    def data = {val res = field[DeliteArrayBuffer[T]](x, "data"); setProps(res, mdat(x))(ctx); (res) }
  }

  implicit def multiarrayToFlatCastOps[T:Manifest](x: Exp[DeliteMultiArray[T]]) = new FlatArrayCastOpsCls[T](x)
  class FlatArrayCastOpsCls[T:Manifest](x: Exp[DeliteMultiArray[T]]) { 
    def asFlatArrayPlain = {
      assert(isFlatArrayPlainType(x.tp), "Cannot cast " + x.tp + " to FlatArrayPlain")
      x.asInstanceOf[Exp[FlatArrayPlain[T]]]
    }
    def asFlatArrayView = {
      assert(isFlatArrayViewType(x.tp), "Cannot cast " + x.tp + " to FlatArrayView")
      x.asInstanceOf[Exp[FlatArrayView[T]]]
    }
    def asFlatArrayBuff = {
      assert(isFlatArrayBuffType(x.tp), "Cannot cast " + x.tp + " to FlatArrayBuff")
      x.asInstanceOf[Exp[FlatArrayBuff[T]]]
    }
    def asFlatArrayBuffView = {
      assert(isFlatArrayBuffViewType(x.tp), "Cannot cast " + x.tp + " to FlatArrayBuffView")
      x.asInstanceOf[Exp[FlatArrayBuffView[T]]]
    }
  }

  def dimsToStride(dims: Seq[Exp[Int]]): Seq[Exp[Int]] = {
    Seq.tabulate(dims.length){d => 
      if (d == dims.length - 1) {Const(1)}
      else productTree(dims.drop(d + 1))
    }
  }

  def flatarray_plain_new[A:Manifest](data: Exp[DeliteArray[A]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayPlain[A]] = {
    val out = flatarray_plain[A](data, dims)
    setMetadata(out, FlatLayout(dims.length, Plain))(ctx)
    setField(out, getProps(data), "data")(ctx)
    (out)
  }
  // If this is a view of a view, will already have accounted for dimensions
  // If this is a wrapped plain array, will calculate the stride in the overloaded version of method
  def flatarray_view_new[A:Manifest](data: Exp[DeliteArray[A]], dims: Seq[Exp[Int]], ofs: Seq[Exp[Int]], stride: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayView[A]] = {    
    val out = flatarray_view[A](data, dims, ofs, stride)
    setMetadata(out, FlatLayout(dims.length, View))(ctx)
    setField(out, getProps(data), "data")(ctx)
    (out)
  }
  def flatarray_view_new[A:Manifest](data: Exp[DeliteArray[A]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayView[A]] = {
    val ofs = Seq.fill(dims.length){Const(0)}
    flatarray_view_new(data, dims, ofs, dimsToStride(dims))  
  }
  def flatarray_buff_new[A:Manifest](data: Exp[DeliteArrayBuffer[A]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayBuff[A]] = {
    val out = flatarray_buff[A](data, dims)
    setMetadata(out, FlatLayout(dims.length, Buffer))(ctx)
    setField(out, getProps(data), "data")(ctx)
    (out)
  }
  def flatarray_buffview_new[A:Manifest](data: Exp[DeliteArrayBuffer[A]], dims: Seq[Exp[Int]], ofs: Seq[Exp[Int]], stride: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayBuffView[A]] = {
    val out = flatarray_buffview[A](data, dims, ofs, stride)
    setMetadata(out, FlatLayout(dims.length, BufferView))(ctx)
    setField(out, getProps(data), "data")(ctx)
    (out)
  }
  def flatarray_buffview_new[A:Manifest](data: Exp[DeliteArrayBuffer[A]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayBuffView[A]] = {
    val ofs = Seq.fill(dims.length){Const(0)}
    flatarray_buffview_new(data, dims, ofs, dimsToStride(dims))
  }

  def array_new[A:Manifest](size: Exp[Int], child: SymbolProperties)(implicit ctx: SourceContext): Exp[DeliteArray[A]] = {
    val out = DeliteArray.imm[A](size)
    setMetadata(out, FlatLayout(1, Plain))(ctx)
    setChild(out, Some(child))(ctx)
    (out)
  }
  def buffer_wrap[A:Manifest](data: Exp[DeliteArray[A]], size: Exp[Int])(implicit ctx: SourceContext): Exp[DeliteArrayBuffer[A]] = {
    val out = darray_buffer_wrap(data, size)
    setMetadata(out, FlatLayout(1, Buffer))(ctx)
    setField(out, getProps(data), "data")(ctx)
    (out)
  }
  def buffer_new[A:Manifest](size: Exp[Int], child: SymbolProperties)(implicit ctx: SourceContext): Exp[DeliteArrayBuffer[A]] = {
    val data = array_new(size, child)
    buffer_wrap(data, size)
  }

  def array_apply[A:Manifest](a: Exp[DeliteArray[A]], i: Exp[Int])(implicit ctx: SourceContext): Exp[A] = {
    val out = darray_apply(a, i)
    setProps(out, getChild(a))(ctx)
    (out)
  }
  def buffer_apply[A:Manifest](a: Exp[DeliteArrayBuffer[A]], i: Exp[Int])(implicit ctx: SourceContext): Exp[A] = {
    val out = darray_buffer_apply(a, i)
    setProps(out, getField(a, "data").flatMap{_.asInstanceOf[ArrayProperties].child})(ctx)
    (out)
  }

  private implicit def indicesToFlatIndexOps(x: Exp[AbstractIndices]) = new FlatIndexOpsCls(x)
  private class FlatIndexOpsCls(x: Exp[AbstractIndices]) {
    def firstOrFlat: Exp[Int] = {
      if (isLoopIndices(x.tp))
        x.asInstanceOf[Exp[LoopIndices]].flat
      else 
        x(0)
    }

    def calcIndex(n: Int, dims: Seq[Exp[Int]]): Exp[Int] = {
      if (isLoopIndices(x.tp)) 
        x.asInstanceOf[Exp[LoopIndices]].flat
      else {
        // TODO: Should stride be stored separately for non-views?
        val stride = dimsToStride(dims)
        sumTree(Seq.tabulate(n - 1){d => delite_int_times(x(d), stride(d))} :+ x(n - 1))
      }
    }

    def calcViewIndex(n: Int, ofs: Seq[Exp[Int]], stride: Seq[Exp[Int]]): Exp[Int] = {
      sumTree(Seq.tabulate(n){d => delite_int_times(delite_int_plus(x(d), ofs(d)), stride(d))})
    }
  }

  // --- Manifests
  override def transformManifest[T](t: Manifest[T], a: ArrayProperties, inner: Manifest[_]): Manifest[_] = layout(a) match {
    case FlatLayout(1, Plain) => darrayManifest(inner)
    case FlatLayout(n, Plain) => flatplainManifest(inner, n)
    case FlatLayout(1, Buffer) => darrayBufferManifest(inner)
    case FlatLayout(n, Buffer) => flatbuffManifest(inner, n)
    case FlatLayout(n, View) => flatviewManifest(inner, n)
    case FlatLayout(n, BufferView) => flatbuffviewManifest(inner, n)
    case _ => super.transformManifest(t, a, inner)
  }
  
  // --- Nested updates
  // Nested writes are disallowed on views
  // TODO: Need to add those cases when/if NestedAtomicRead is implemented
  override def transformTracer[T:Manifest](ma: Exp[DeliteMultiArray[T]], t: MultiArrayTracer): List[AtomicTracer] = layout(ma) match {
    case FlatLayout(1, Plain) => List(ArrayTracer(t.i.firstOrFlat))
    case FlatLayout(1, Buffer) => List(StructTracer("data"), ArrayTracer(t.i.firstOrFlat))
    case FlatLayout(n, Plain) => 
      val impl = ma.asFlatArrayPlain
      val index = t.i.calcIndex(n, impl.dims)
      List(StructTracer("data"), ArrayTracer(index))

    case FlatLayout(n, Buffer) => 
      val impl = ma.asFlatArrayBuff
      val index = t.i.calcIndex(n, impl.dims)
      List(StructTracer("data"), StructTracer("data"), ArrayTracer(index))

    case _ => super.transformTracer(ma, t)
  }

  // --- MultiArray constructors
  // TODO: Need to determine partition tags?
  override def implementNew[A:Manifest](dims: Seq[Exp[Int]], out: SymbolProperties)(implicit ctx: SourceContext) = layout(out) match {
    case FlatLayout(1, Plain) => 
      array_new[A](dims(0), mdat(out))
    case FlatLayout(n, Plain) => 
      flatarray_plain_new[A](array_new[A](productTree(dims), mdat(out)), dims)
    case FlatLayout(n, View) =>
      flatarray_view_new[A](array_new[A](productTree(dims), mdat(out)), dims)
    case FlatLayout(1, Buffer) =>
      buffer_new[A](dims(0), mdat(out))
    case FlatLayout(n, Buffer) => 
      flatarray_buff_new[A](buffer_new[A](productTree(dims), mdat(out)), dims)
    case FlatLayout(n, BufferView) =>
      flatarray_buffview_new[A](buffer_new[A](productTree(dims), mdat(out)), dims)
    case _ => super.implementNew(dims, out)
  }

  // FIXME: View of view currently expects start and target's start are the same length. Is this always true?
  override def implementView[A:Manifest](ma: Exp[DeliteMultiArray[A]], start: Seq[Exp[Int]], stride: Seq[Exp[Int]], dims: Seq[Rep[Int]], out: SymbolProperties)(implicit ctx: SourceContext) = (layout(ma), layout(out)) match {
    case (FlatLayout(1, Plain), FlatLayout(n, _)) => 
      flatarray_view_new[A](ma.asDeliteArray, dims, start, stride)
    case (FlatLayout(1, Buffer), FlatLayout(n,_)) => 
      flatarray_buffview_new[A](ma.asDeliteArrayBuffer, dims, start, stride)
    case (FlatLayout(_, Plain), FlatLayout(n, _)) => 
      val str = dimsToStride(dims).zip(stride).map{s => delite_int_times(s._1, s._2)}
      flatarray_view_new[A](ma.asFlatArrayPlain.data, dims, start, str)
    case (FlatLayout(_, View), FlatLayout(n, _)) => 
      val impl = ma.asFlatArrayView
      val prevOfs = impl.start
      val prevStride = impl.stride
      assert(prevOfs.length == start.length, "Multidimensional viewing is not implemented yet!")
      if (prevOfs.length == start.length) {
        val ofs = start.zip(prevOfs).map{o => delite_int_plus(o._1, o._2)}
        val str = stride.zip(prevStride).map{s => delite_int_times(s._1, s._2)}
        flatarray_view_new[A](impl.data, dims, ofs, str)
      }
      else {

      }
    case (FlatLayout(_, Buffer), FlatLayout(n, _)) => 
      flatarray_buffview_new[A](ma.asFlatArrayBuff.data, dims, start, stride)
    case (FlatLayout(_, BufferView), FlatLayout(n, _)) => 
      val impl = ma.asFlatArrayBuffView
      val prevOfs = impl.start
      val prevStride = impl.stride
      assert(prevOfs.length == start.length, "Multidimensional viewing is not implemented yet!")
      if (prevOfs.length == start.length) { 
        val ofs = start.zip(prevOfs).map{o => delite_int_plus(o._1, o._2)}
        val str = stride.zip(prevStride).map{s => delite_int_times(s._1, s._2)}
        flatarray_buffview_new[A](impl.data, dims, ofs, str)
      }
      else {
        
      }
    case _ => super.implementView(ma,start,stride,dims,out)
  }

  // --- Properties
  override def createDims[T:Manifest](ma: Exp[DeliteMultiArray[T]]): Seq[Exp[Int]] = layout(ma) match {
    case FlatLayout(1, Plain) => Seq(darray_length(ma.asDeliteArray))
    case FlatLayout(1, Buffer) => Seq(darray_buffer_length(ma.asDeliteArrayBuffer))
    case _ => super.createDims(ma)
  }

  override def implementSize[T:Manifest](ma: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Exp[Int] = layout(ma) match {
    case FlatLayout(1, Plain) => darray_length(ma.asDeliteArray)
    case FlatLayout(1, Buffer) => darray_buffer_length(ma.asDeliteArrayBuffer)
    case _ => super.implementSize(ma)
  }

  // --- Single element ops
  override def implementApply[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[AbstractIndices])(implicit ctx: SourceContext) = layout(ma) match {
    case FlatLayout(1, Plain) => 
      array_apply(ma.asDeliteArray, i.firstOrFlat)
    case FlatLayout(1, Buffer) => 
      buffer_apply(ma.asDeliteArrayBuffer, i.firstOrFlat)
    case FlatLayout(n, Plain) => 
      val impl = ma.asFlatArrayPlain
      val index = i.calcIndex(n, impl.dims)
      array_apply(impl.data, index)
    case FlatLayout(n, View) => 
      val impl = ma.asFlatArrayView
      val index = i.calcViewIndex(n, impl.start, impl.stride)
      array_apply(impl.data, index)
    case FlatLayout(n, Buffer) => 
      val impl = ma.asFlatArrayBuff
      val index = i.calcIndex(n, impl.dims)
      buffer_apply(impl.data, index)
    case FlatLayout(n, BufferView) => 
      val impl = ma.asFlatArrayBuffView
      val index = i.calcViewIndex(n, impl.start, impl.stride)
      buffer_apply(impl.data, index)
    case _ => super.implementApply(ma,i)
  }

  override def implementUpdate[T:Manifest](ma: Exp[DeliteMultiArray[T]], i: Exp[AbstractIndices], x: Exp[T])(implicit ctx: SourceContext) = layout(ma) match {
    case FlatLayout(1, Plain) => 
      darray_update(ma.asDeliteArray, i.firstOrFlat, x)
    case FlatLayout(1, Buffer) =>
      darray_buffer_update(ma.asDeliteArrayBuffer, i.firstOrFlat, x)
    case FlatLayout(n, Plain) =>
      val impl = ma.asFlatArrayPlain
      val index = i.calcIndex(n, impl.dims)
      darray_update(impl.data, index, x)
    case FlatLayout(n, Buffer) => 
      val impl = ma.asFlatArrayBuff
      val index = i.calcIndex(n, impl.dims)
      darray_buffer_update(impl.data, index, x)
    case _ => super.implementUpdate(ma,i,x)
  }

  // --- Array permute / reshaping
  // TODO: Should permute give a view?
  override def implementPermute[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = (layout(ma), layout(out)) match {
    // Kludged version of FromFunction/MapIndices/Map
    case (FlatLayout(n, _), FlatLayout(_,_)) => 
      val size = implementSize(ma)
      val dims = createDims(ma)
      val v = fresh[Int]
      val inds = calcIndices(v, dims)
      val i = loopindices_new(v, inds, dims)
      val permuteInds = Seq.tabulate(n){d => i(config(d))}
      val body: Block[A] = reifyEffects(implementApply(ma, indices_new(permuteInds)))
      implementCollect(v, i, layout(out), mdat(out), body, dims, size)

    case _ => super.implementPermute(ma,config,out)
  }
 
  override def implementReshape[A:Manifest](ma: Exp[DeliteMultiArray[A]], dims: Seq[Exp[Int]], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
    case FlatLayout(_,_) => 
      val data = implementCopy(ma, FlatLayout(1, Plain), mdat(out)).asInstanceOf[Exp[DeliteArray[A]]]

      layout(out) match {
        case FlatLayout(1, Plain) => data
        case FlatLayout(n, Plain) => flatarray_plain_new(data, dims)
        case FlatLayout(n, View) => flatarray_view_new(data, dims)
        case FlatLayout(1, Buffer) => buffer_wrap(data, implementSize(ma))
        case FlatLayout(n, Buffer) => flatarray_buff(buffer_wrap(data, implementSize(ma)), dims)
        case FlatLayout(n, BufferView) => flatarray_buffview_new(buffer_wrap(data, implementSize(ma)), dims)
      }

    case _ => super.implementReshape(ma,dims,out)
  }

  // --- Parallel ops
  private def implementCollect[A:Manifest](origV: Exp[Int], origI: Exp[LoopIndices], out: Layout[_,_], dat: SymbolProperties, body: Block[A], dims: Seq[Exp[Int]], size: Exp[Int], cond: Option[Block[Boolean]] = None)(implicit ctx: SourceContext): Exp[Any] = out match {
    case FlatLayout(_,_) => 
      val cf = CollectFactory.array[A](size, body, cond)
      val inds = calcIndices(cf.v, dims)

      register(origV)(cf.v)
      register(origI)(loopindices_new(cf.v, inds, dims))

      val data = reflectPure(cf)
      setMetadata(data, FlatLayout(1, Plain))(ctx)
      setChild(data, Some(dat))(ctx) 

      out match {
        case FlatLayout(1, Plain) => data
        case FlatLayout(n, Plain) => flatarray_plain_new(data, dims)
        case FlatLayout(n, View) => flatarray_view_new(data, dims)
        case FlatLayout(1, Buffer) => buffer_wrap(data, size)
        case FlatLayout(n, Buffer) => flatarray_buff_new(buffer_wrap(data, size), dims)
        case FlatLayout(n, BufferView) => flatarray_buffview_new(buffer_wrap(data, size), dims)
      }
  }

  private def implementCopy[A:Manifest](ma: Exp[DeliteMultiArray[A]], out: Layout[_,_], dat: SymbolProperties) = {
    val size = implementSize(ma)
    val dims = createDims(ma)
    val v = fresh[Int]
    val i = loopindices_new(v, Seq(v), Seq(size))
    val body: Block[A] = reifyEffects(implementApply(ma, i))
    implementCollect(v, i, out, dat, body, dims, size)
  }

  override def implementFromFunction[A:Manifest](dims: Seq[Exp[Int]], body: Block[A], v: Exp[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
    case FlatLayout(_,_) => implementCollect[A](v, i, layout(out), mdat(out), body, dims, productTree(dims))
    case _ => super.implementFromFunction(dims,body,v,i,out)
  }
  override def implementMap[A:Manifest,R:Manifest](in: Exp[DeliteMultiArray[A]], body: Block[R], v: Exp[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
    case FlatLayout(_,_) => implementCollect[R](v, i, layout(out), mdat(out), body, createDims(in), implementSize(in))
    case _ => super.implementMap(in,body,v,i,out)
  }
  override def implementZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DeliteMultiArray[A]], inB: Exp[DeliteMultiArray[B]], body: Block[R], v: Exp[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
    case FlatLayout(_,_) => implementCollect[R](v, i, layout(out), mdat(out), body, createDims(inA), implementSize(inA))
    case _ => super.implementZipWith(inA,inB,body,v,i,out)
  }
  override def implementNDMap[A:Manifest,B:Manifest](op: DeliteMultiArrayNDMap[_,_], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = (layout(op.in),out) match {
    case _ => super.implementNDMap(op,out)
  }
  override def implementGroupBy[A:Manifest,K:Manifest](op: DeliteMultiArrayGroupBy[_,_])(implicit ctx: SourceContext): Exp[Any] = layout(op.in) match {
    case _ => super.implementGroupBy(op)
  }
  override def implementGroupByReduce[A:Manifest,K:Manifest,V:Manifest](op: DeliteMultiArrayGroupByReduce[_,_,_])(implicit ctx: SourceContext): Exp[Any] = layout(op.in) match {
    case _ => super.implementGroupByReduce(op)
  }

  // --- 1D Parallel Ops
  override def implementMapFilter[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], mapFunc: Block[B], filtFunc: Block[Boolean], v: Exp[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
    case FlatLayout(_,_) => implementCollect[B](v, i, layout(out), mdat(out), mapFunc, createDims(in), implementSize(in), Some(filtFunc))
    case _ => super.implementMapFilter(in,mapFunc,filtFunc,v,i,out)  
  }
  override def implementFlatMap[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], body: Block[DeliteArray1D[B]], v: Exp[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
    case FlatLayout(_,_) => 
      val size = implementSize(in)
      val cf = FlatMapFactory.array[B](size, body.asInstanceOf[Block[DeliteCollection[B]]])

      register(v)(cf.v)
      register(i)(loopindices_new(cf.v, Seq(cf.v), Seq(size)))

      val data = reflectPure(cf)
      setMetadata(data, FlatLayout(1, Plain))(ctx)
      setChild(data, Some(mdat(out)))(ctx)

      layout(out) match {
        case FlatLayout(_, Plain) => data
        case FlatLayout(_, View) => flatarray_view_new(data, Seq(size))
        case FlatLayout(_, Buffer) => buffer_wrap(data, size)
        case FlatLayout(_, BufferView) => flatarray_buffview_new(buffer_wrap(data,size), Seq(size))
      }

    case _ => super.implementFlatMap(in,body,v,i,out)
  }

  // --- Buffer ops
  override def implementInsert[A:Manifest](ma: Exp[DeliteArray1D[A]], i: Exp[Int], x: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = layout(ma) match {
    case FlatLayout(1, Buffer) => 
      darray_buffer_insert(ma.asDeliteArrayBuffer, i, x)
    case _ => super.implementInsert(ma,i,x)
  }
  override def implementAppend[A:Manifest](ma: Exp[DeliteArray1D[A]], x: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = layout(ma) match {
    case FlatLayout(1, Buffer) =>
      darray_buffer_append(ma.asDeliteArrayBuffer, x)
    case _ => super.implementAppend(ma,x)
  }
  override def implementInsertAll[A:Manifest](ma: Exp[DeliteMultiArray[A]], axis: Int, i: Exp[Int], x: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext): Exp[Unit] = (layout(ma),layout(x)) match {
    case (FlatLayout(1, Buffer), FlatLayout(1, Plain)) =>
      val impl = ma.asDeliteArrayBuffer
      val rhs = x.asDeliteArray
      darray_buffer_insertAll(impl, i, rhs)

    case (FlatLayout(1, Buffer), FlatLayout(1, Buffer)) => 
      val impl = ma.asDeliteArrayBuffer
      val rhs = x.asDeliteArrayBuffer
      darray_buffer_insertBuffer(impl, i, rhs)

    case (FlatLayout(1, Buffer), FlatLayout(1, _)) => 
      val impl = ma.asDeliteArrayBuffer
      val length = implementSize(x)
      darray_buffer_insertspace(impl, i, length)

      val v = fresh[Int]
      val li = loopindices_new(v, Seq(v), Seq(length))
      val elem = implementApply(x, li).asInstanceOf[Exp[A]]
      val body: Block[Unit] = reifyEffects(darray_buffer_update(impl, delite_int_plus(v, i), elem))
      val fe = ForeachFactory(length, body)
      reflectEffect(fe, summarizeEffects(body).star)
      register(v)(fe.v)

    case _ => super.implementInsertAll(ma,axis,i,x)
  }

  override def implementRemove[A:Manifest](ma: Exp[DeliteMultiArray[A]], axis: Int, start: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = layout(ma) match {
    case FlatLayout(1, Buffer) =>
      darray_buffer_remove(ma.asDeliteArrayBuffer, start, len)



    case _ => super.implementRemove(ma,axis,start,len)
  }

  // --- Misc.
  override def implementMkString[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]]): Exp[String] = layout(ma) match {
    case FlatLayout(1, Plain) => 
      darray_mkstring(ma.asDeliteArray, dels(0))

    //case FlatLayout(1, Buffer) =>
      /*
      val impl = ma.asDeliteArrayBuffer
      val size = impl.size
      if (impl == null)
        unit("null")  
      }
      else if (size == unit(0)) {
        unit("[ ]")
      }
      else {
        var i = unit(1)
        var s = unit("")   
        s = s + darray_buffer_apply(impl, unit(0))      
        while (i < impl.size) {
          s = s + dels(0)
          s = s + darray_buffer_apply(impl, i)
          i += unit(1)
        }
        (s)
      }
      */

    case _ => super.implementMkString(ma,dels)
  }

  // --- 2D Ops
  override def implementMatrixMultiply[A:Manifest:Numeric](lhs: Exp[DeliteArray2D[A]], rhs: Exp[DeliteArray2D[A]])(implicit ctx: SourceContext): Exp[Any] = (layout(lhs),layout(rhs)) match {
    //case (FlatLayout(2, Plain), FlatLayout(2, Plain)) =>


    case _ => super.implementMatrixMultiply(lhs,rhs) 
  }
  override def implementMatrixVectorMultiply[A:Manifest:Numeric](mat: Exp[DeliteArray2D[A]], vec: Exp[DeliteArray1D[A]])(implicit ctx: SourceContext): Exp[Any] = (layout(mat), layout(vec)) match {
    case _ => super.implementMatrixVectorMultiply(mat,vec)
  }

  // --- Wrap
  override def implementBuffify[A:Manifest](ma: Exp[DeliteMultiArray[A]], out: SymbolProperties) = (layout(ma), layout(out)) match {
    case (FlatLayout(1, Plain), FlatLayout(1, Buffer)) => buffer_wrap(ma.asDeliteArray, implementSize(ma))
    case (FlatLayout(n, Plain), FlatLayout(_, Buffer)) => buffer_wrap(ma.asFlatArrayPlain.data, implementSize(ma))
    case (FlatLayout(n, View), FlatLayout(_, BufferView)) => sys.error("TODO - buffify of view")
    case _ => super.implementBuffify(ma, out)
  }
  override def implementViewify[A:Manifest](ma: Exp[DeliteMultiArray[A]], out: SymbolProperties) = layout(ma) match {
    case FlatLayout(1, Plain) => flatarray_view_new(ma.asDeliteArray, createDims(ma))
    case FlatLayout(n, Plain) => flatarray_view_new(ma.asFlatArrayPlain.data, createDims(ma))
    case FlatLayout(1, Buffer) => flatarray_buffview_new(ma.asDeliteArrayBuffer, createDims(ma))
    case FlatLayout(n, Buffer) => flatarray_buffview_new(ma.asFlatArrayBuff.data, createDims(ma))
    case _ => super.implementViewify(ma, out)
  }
}