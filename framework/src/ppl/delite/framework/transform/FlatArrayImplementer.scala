package ppl.delite.framework.transform

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.internal.Meetable._
import scala.reflect.{SourceContext, RefinedManifest}

import ppl.delite.framework.ops._ 
import ppl.delite.framework.datastructures._
import ppl.delite.framework.analysis._
import ppl.delite.framework.visit._
import ppl.delite.framework.Util._

/////////////////
// Flat Layout //
/////////////////

trait FlatArrayPlain[T] extends MultiArrayPlainImpl[T] with Struct
trait FlatArrayView[T] extends MultiArrayViewImpl[T] with Struct
trait FlatArrayBuff[T] extends MultiArrayBuffImpl[T] with Struct
trait FlatArrayBuffView[T] extends MultiArrayBuffViewImpl[T] with Struct

trait FlatArrayExp extends MultiArrayExp { self: DeliteOpsExp => 
  // --- Flat Indexing
  def dimsToStrides(dims: Seq[Exp[Int]]): Seq[Exp[Int]] = {
    Seq.tabulate(dims.length){d => 
      if (d == dims.length - 1) {unit(1)}
      else productTree(dims.drop(d + 1))
    }
  }

  // --- Manifest creation for flat MultiArrays
  private def dataField[T](tp: Manifest[T]): List[(String, Manifest[_])] = List("data" -> darrayManifest(tp))
  private def buffField[T](tp: Manifest[T]): List[(String, Manifest[_])] = List("data" -> darrayBufferManifest(tp))
  private def dimFields(n: Int): List[(String, Manifest[_])] = List.tabulate(n){d => s"dim$d" -> manifest[Int]}
  private def viewFields(n: Int): List[(String, Manifest[_])] = dimFields(n) ++ List("ofs" -> manifest[Int]) ++ 
                                                                List.tabulate(n){d => s"stride$d" -> manifest[Int]}

  def flatplainManifest[T](typeArg: Manifest[T], n: Int): Manifest[FlatArrayPlain[T]] //= makeManifest(classOf[FlatArrayPlain[_]], List(typeArg))
    = new RefinedManifest[FlatArrayPlain[T]] { 
        def runtimeClass = classOf[FlatArrayPlain[T]]
        val fields = dataField(typeArg) ++ dimFields(n)
        override val typeArguments = List(typeArg)
    }
  def flatviewManifest[T](typeArg: Manifest[T], n: Int): Manifest[FlatArrayView[T]] // = makeManifest(classOf[FlatArrayView[_]], List(typeArg))
    = new RefinedManifest[FlatArrayView[T]] {
        def runtimeClass = classOf[FlatArrayView[T]]
        val fields = dataField(typeArg) ++ viewFields(n)
        override val typeArguments = List(typeArg)
    }
  def flatbuffManifest[T](typeArg: Manifest[T], n: Int): Manifest[FlatArrayBuff[T]] //= makeManifest(classOf[FlatArrayBuff[_]], List(typeArg))
    = new RefinedManifest[FlatArrayBuff[T]] {
        def runtimeClass = classOf[FlatArrayBuff[T]]
        val fields = buffField(typeArg) ++ dimFields(n)
        override val typeArguments = List(typeArg)
    }
  def flatbuffviewManifest[T](typeArg: Manifest[T], n: Int): Manifest[FlatArrayBuffView[T]] //= makeManifest(classOf[FlatArrayBuffView[_]], List(typeArg))
    = new RefinedManifest[FlatArrayBuffView[T]] {
        def runtimeClass = classOf[FlatArrayBuffView[T]]
        val fields = buffField(typeArg) ++ viewFields(n)
        override val typeArguments = List(typeArg)
    }

  // --- New flat ND array
  def flatarray_plain_new[T:Manifest](data: Exp[DeliteArray[T]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayPlain[T]] = {    
    //val struct = reflectPure( FlatArrayPlainNew(data, dims) )
    def slf(x: Rep[_]) = (y: Rep[FlatArrayPlain[T]]) => x
    val tp = flatplainManifest(manifest[T], dims.length)
    val struct = record_new(("data",false,slf(data)) +: dims.zipWithIndex.map{i => ("dim" + i._2, false, slf(i._1))})(tp)
    struct.withData(FlatLayout(dims.length, Plain)).withField(getProps(data), "data")
  }

  // --- New flat view
  // If this is a view of a view, will already have accounted for dimensions
  // If this is a wrapped plain array, will calculate the strides in the overloaded version of method
  def flatarray_view_new[T:Manifest](data: Exp[DeliteArray[T]], dims: Seq[Exp[Int]], ofs: Exp[Int], strides: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayView[T]] = {    
    //val struct = reflectPure( FlatArrayViewNew(data, dims, ofs, strides) )
    def slf(x: Rep[_]) = (y: Rep[FlatArrayView[T]]) => x
    val tp = flatviewManifest(manifest[T], dims.length)
    val struct = record_new( (("data", false, slf(data)) +: dims.zipWithIndex.map{i => ("dim" + i._2, false, slf(i._1))}) ++ 
                             (("ofs", false, slf(ofs)) +: strides.zipWithIndex.map{d => ("stride" + d._2, false, slf(d._1))}))(tp)
    struct.withData(FlatLayout(dims.length, View)).withField(getProps(data), "data")
  }
  def flatarray_view_new[A:Manifest](data: Exp[DeliteArray[A]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayView[A]] = {
    flatarray_view_new(data, dims, unit(0), dimsToStrides(dims))  
  }

  // --- New flat buffer
  def flatarray_buff_new[T:Manifest](data: Exp[DeliteArrayBuffer[T]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayBuff[T]] = {
    //val struct = reflectPure( FlatArrayBuffNew(data, dims) )
    def slf(x: Rep[_]) = (y: Rep[FlatArrayBuff[T]]) => x
    val tp = flatbuffManifest(manifest[T], dims.length)
    val struct = record_new(("data", false, slf(data)) +: dims.zipWithIndex.map{i => ("dim" + i._2, true, slf(i._1))})(tp)
    struct.withData(FlatLayout(dims.length, Buffer)).withField(getProps(data), "data")
  }
    
  // --- New flat buffer view
  def flatarray_buffview_new[T:Manifest](data: Exp[DeliteArrayBuffer[T]], dims: Seq[Exp[Int]], ofs: Exp[Int], strides: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayBuffView[T]] = {
    //val struct = reflectPure( FlatArrayBuffViewNew(data, dims, ofs, strides) )
    def slf(x: Rep[_]) = (y: Rep[FlatArrayBuffView[T]]) => x
    val tp = flatbuffviewManifest(manifest[T], dims.length)
    val struct = record_new( (("data", false, slf(data)) +: dims.zipWithIndex.map{i => ("dim" + i._2, false, slf(i._1))}) ++ 
                             (("ofs", false, slf(ofs)) +: strides.zipWithIndex.map{d => ("stride" + d._2, false, slf(d._1))}))(tp)
    struct.withData(FlatLayout(dims.length, BufferView)).withField(getProps(data), "data")                       
  }
  def flatarray_buffview_new[A:Manifest](data: Exp[DeliteArrayBuffer[A]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayBuffView[A]] = {
    flatarray_buffview_new(data, dims, unit(0), dimsToStrides(dims))
  }

  // --- Alternatives on DeliteArray, DeliteArrayBuffer for inserting metadata
  // TODO: Need to determine partition tags?
  def flatbuffer_wrap[A:Manifest](data: Exp[DeliteArray[A]], length: Exp[Int], mutable: Boolean)(implicit ctx: SourceContext) = {
    if (mutable) reflectMutable(DeliteArrayBufferWrap(data,length)).withData(FlatLayout(1,Buffer)).withField(getProps(data), "data")
    else reflectPure(DeliteArrayBufferWrap(data,length)).withData(FlatLayout(1, Buffer)).withField(getProps(data), "data")
  }
  def flatarray_new[A:Manifest](size: Exp[Int], child: SymbolProperties, mutable: Boolean)(implicit ctx: SourceContext): Exp[DeliteArray[A]] = {
    if (mutable) DeliteArray[A](size).withData(FlatLayout(1,Plain)).withChild(child)
    else DeliteArray.imm[A](size).withData(FlatLayout(1,Plain)).withChild(child)
  }
  def flatbuffer_new[A:Manifest](size: Exp[Int], child: SymbolProperties, mutable: Boolean)(implicit ctx: SourceContext): Exp[DeliteArrayBuffer[A]] = {
    flatbuffer_wrap(flatarray_new(size, child, mutable), size, mutable)
  }

  def flatarray_apply[A:Manifest](a: Exp[DeliteArray[A]], i: Exp[Int])(implicit ctx: SourceContext): Exp[A] = {
    darray_apply(a, i).withProps(getChild(a))
  }
  def flatbuffer_apply[A:Manifest](a: Exp[DeliteArrayBuffer[A]], i: Exp[Int])(implicit ctx: SourceContext): Exp[A] = {
    // getting child of data field of ArrayBuffer (single element of inner Array)
    darray_buffer_apply(a, i).withProps(getField(a, "data").map{p => mdat(p)}) 
  }

  // --- Delite collection dc_apply (for use as result in flatMap)
  def isFlatArrayPlain[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[FlatArrayPlain[_]])
  def isFlatArrayView[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[FlatArrayView[_]])
  def isFlatArrayBuff[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[FlatArrayView[_]])
  def isFlatArrayBuffView[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[FlatArrayBuffView[_]])

  def asFlatArrayPlain[T](x: Exp[DeliteCollection[T]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[FlatArrayPlain[T]]]
  def asFlatArrayView[T](x: Exp[DeliteCollection[T]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[FlatArrayView[T]]]
  def asFlatArrayBuff[T](x: Exp[DeliteCollection[T]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[FlatArrayBuff[T]]]
  def asFlatArrayBuffView[T](x: Exp[DeliteCollection[T]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[FlatArrayBuffView[T]]]

  // Will be checking these where we've already seen that the layout is some kind of view
  // Need to make the conservative assumption that the view is an actual view of some other data if
  // we don't know the implementation type here for some reason
  override def isTrueView(p: SymbolProperties) = getView(p).map{_.isTrueView}.getOrElse(true)
  override def isTrueView(e: Exp[Any]) = getView(e).map{_.isTrueView}.getOrElse(true)
  override def isTrueBuffer(p: SymbolProperties) = getBuffer(p).map{_.isTrueBuffer}.getOrElse(true)
  override def isTrueBuffer(e: Exp[Any]) = getBuffer(e).map{_.isTrueBuffer}.getOrElse(true)
}

trait FlatArrayImplExp extends MultiArrayImplExp with FlatArrayExp { self: DeliteOpsExp with DeliteFileReaderOpsExp => 
  override val implementer : FlatArrayImplementer

  // --- Flat indexing
  def calcFlatIndex(i: Seq[Exp[Int]], strides: Seq[Exp[Int]]): Exp[Int] = {
    sumTree(Seq.tabulate(i.length - 1){d => delite_int_times(i(d), strides(d))} :+ i.last)
  }

  // flat index = sum(ofs_i + stride_i * index_i)
  def calcFlatViewIndex(i: Seq[Exp[Int]], ofs: Exp[Int], strides: Seq[Exp[Int]]): Exp[Int] = {
    sumTree(Seq.tabulate(i.length){d => delite_int_times(i(d), strides(d)) } :+ ofs)
  }

  // --- Convenience infix ops for flat indexing
  private implicit def indicesToFlatIndexOps(x: Exp[AbstractIndices]) = new FlatIndexOpsCls(x)
  private class FlatIndexOpsCls(x: Exp[AbstractIndices]) {
    // Take the first index (for Indices) or the loop iterator (for LoopIndices)
    def firstOrFlat: Exp[Int] = {
      if (isLoopIndices(x))
        x.asInstanceOf[Exp[LoopIndices]].flat
      else 
        x(0)
    }

    // Small optimization - if we're indexing directly using a loop index, intended behavior is sequential accesses (use flat index)
    // TODO: Should stride be stored separately for non-views?
    def calcIndex(n: Int, dims: Seq[Exp[Int]]): Exp[Int] = {
      if (isLoopIndices(x)) 
        x.asInstanceOf[Exp[LoopIndices]].flat
      else
        calcFlatIndex(x.toSeq(n), dimsToStrides(dims))
    }

    def calcViewIndex(n: Int, ofs: Exp[Int], strides: Seq[Exp[Int]], trueView: Boolean = true): Exp[Int] = {
      if (trueView) 
        calcFlatViewIndex(x.toSeq(n), ofs, strides)
      else if (isLoopIndices(x)) 
        x.asInstanceOf[Exp[LoopIndices]].flat
      else 
        calcFlatIndex(x.toSeq(n), strides)
    }
  }

  // --- Convenience methods for casting DeliteMultiArray to FlatArrays
  // Casts must be guarded by assertions, esp. during compiler debugging. If an assertion here fails, something
  // went wrong with the transformer ordering!
  implicit def multiarrayToFlatCastOps[T:Manifest](x: Exp[DeliteMultiArray[T]]) = new FlatArrayCastOpsCls[T](x)
  class FlatArrayCastOpsCls[T:Manifest](x: Exp[DeliteMultiArray[T]]) { 
    def asDeliteArray = {
      assert(isDeliteArrayTpe(x.tp), "Cannot cast " + x.tp + " to DeliteArray\n" + strDef(x))
      x.asInstanceOf[Exp[DeliteArray[T]]]
    }
    def asDeliteArrayBuffer = {
      assert(isDeliteArrayBufferTpe(x.tp), "Cannot cast " + x.tp + " to DeliteArrayBuffer\n" + strDef(x))
      x.asInstanceOf[Exp[DeliteArrayBuffer[T]]]
    }
    def asFlatArrayPlain = {
      assert(isFlatArrayPlain(x), "Cannot cast " + x.tp + " to FlatArrayPlain\n" + strDef(x))
      x.asInstanceOf[Exp[FlatArrayPlain[T]]]
    }
    def asFlatArrayView = {
      assert(isFlatArrayView(x), "Cannot cast " + x.tp + " to FlatArrayView\n" + strDef(x))
      x.asInstanceOf[Exp[FlatArrayView[T]]]
    }
    def asFlatArrayBuff = {
      assert(isFlatArrayBuff(x), "Cannot cast " + x.tp + " to FlatArrayBuff\n" + strDef(x))
      x.asInstanceOf[Exp[FlatArrayBuff[T]]]
    }
    def asFlatArrayBuffView = {
      assert(isFlatArrayBuffView(x), "Cannot cast " + x.tp + " to FlatArrayBuffView\n" + strDef(x))
      x.asInstanceOf[Exp[FlatArrayBuffView[T]]]
    }
  }

  // --- Convenience methods for data field of FlatArray types
  implicit def flatArrayPlainToOpsCls[T:Manifest](x: Exp[FlatArrayPlain[T]])(implicit ctx: SourceContext) = new FlatArrayPlainOpsCls(x)(manifest[T],ctx)
  class FlatArrayPlainOpsCls[T:Manifest](x: Exp[FlatArrayPlain[T]])(implicit ctx: SourceContext) {
    def data = field[DeliteArray[T]](x, "data").withProps(mdat(x))
  }
  implicit def flatArrayViewToOpsCls[T:Manifest](x: Exp[FlatArrayView[T]])(implicit ctx: SourceContext) = new FlatArrayViewOpsCls(x)(manifest[T],ctx)
  class FlatArrayViewOpsCls[T:Manifest](x: Exp[FlatArrayView[T]])(implicit ctx: SourceContext) {
    def data = field[DeliteArray[T]](x, "data").withProps(mdat(x))
    def start = implementer.getFlatOffset(x.asInstanceOf[Exp[DeliteMultiArray[T]]])
  }
  implicit def flatArrayBuffToOpsCls[T:Manifest](x: Exp[FlatArrayBuff[T]])(implicit ctx: SourceContext) = new FlatArrayBuffOpsCls(x)(manifest[T],ctx)
  class FlatArrayBuffOpsCls[T:Manifest](x: Exp[FlatArrayBuff[T]])(implicit ctx: SourceContext) {
    def data = field[DeliteArrayBuffer[T]](x, "data").withProps(mdat(x))
  }
  implicit def flatArrayBuffViewToOpsCls[T:Manifest](x: Exp[FlatArrayBuffView[T]])(implicit ctx: SourceContext) = new FlatArrayBuffViewOpsCls(x)(manifest[T],ctx)
  class FlatArrayBuffViewOpsCls[T:Manifest](x: Exp[FlatArrayBuffView[T]])(implicit ctx: SourceContext) {
    def data = field[DeliteArrayBuffer[T]](x, "data").withProps(mdat(x))
    def start = implementer.getFlatOffset(x.asInstanceOf[Exp[DeliteMultiArray[T]]])
  }

  trait FlatArrayImplementer extends MultiArrayImplementer {  
    val IR: self.type

    // --- Manifests
    override def transformManifest[T](t: Manifest[T], a: ArrayProperties, typeArg: Manifest[_]): Manifest[_] = layout(a) match {
      case FlatLayout(1, Plain) => darrayManifest(typeArg)
      case FlatLayout(n, Plain) => flatplainManifest(typeArg, n)
      case FlatLayout(1, Buffer) => darrayBufferManifest(typeArg)
      case FlatLayout(n, Buffer) => flatbuffManifest(typeArg, n)
      case FlatLayout(n, View) => flatviewManifest(typeArg, n)
      case FlatLayout(n, BufferView) => flatbuffviewManifest(typeArg, n)
      case _ => super.transformManifest(t, a, typeArg)
    }
    
    // --- Nested updates
    // NOTE: Nested writes are disallowed on views
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

    // --- Properties
    def getFlatOffset[T:Manifest](ma: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Exp[Int] = layout(ma) match {
      case FlatLayout(_, Plain) => cwarn("Getting offset of flat array"); Const(0)
      case FlatLayout(_, Buffer) => cwarn("Getting offset of flat buffer"); Const(0)
      case FlatLayout(_, View) => field[Int](ma, "ofs")
      case FlatLayout(_, BufferView) => field[Int](ma, "ofs")
    }
 
    override def getStrides[T:Manifest](ma: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Seq[Exp[Int]] = layout(ma) match {
      case FlatLayout(m, Plain) => cwarn("Getting strides of flat array"); Seq.fill(m)( Const(1) )
      case FlatLayout(m, Buffer) => cwarn("Getting strides of flat buffer"); Seq.fill(m)( Const(1) )
      case FlatLayout(m, View) => Seq.tabulate(m){d => implementStride(ma, d)}
      case FlatLayout(m, BufferView) => Seq.tabulate(m){d => implementStride(ma, d)}
      case _ => super.getStrides(ma)
     }

    // TODO: stride for non-views should actually be the stride from dimsToStrides?
    override def implementStride[T:Manifest](ma: Exp[DeliteMultiArray[T]], d: Int)(implicit ctx: SourceContext): Exp[Int] = layout(ma) match {
      case FlatLayout(_, Plain) => cwarn("Getting stride of flat array"); Const(1)
      case FlatLayout(_, Buffer) => cwarn("Getting stride of flat buffer"); Const(1)
      case FlatLayout(_, View) => field[Int](ma,"stride"+d) //field[DeliteArray[Int]](ma, "strides").apply(unit(d))

      case FlatLayout(_, BufferView) => field[Int](ma,"stride"+d) //field[DeliteArray[Int]](ma, "strides").apply(unit(d))
      case _ => super.implementStride(ma, d)
    }

    override def getDims[T:Manifest](ma: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Seq[Exp[Int]] = layout(ma) match {
      case FlatLayout(1, Plain) => Seq(darray_length(ma.asDeliteArray))
      case FlatLayout(1, Buffer) => Seq(darray_buffer_length(ma.asDeliteArrayBuffer))
      case FlatLayout(m, _) => Seq.tabulate(m){d => implementDim(ma, d) }
      case _ => super.getDims(ma)
    }

    override def implementDim[T:Manifest](ma: Exp[DeliteMultiArray[T]], i: Int)(implicit ctx: SourceContext): Exp[Int] = layout(ma) match {
      case FlatLayout(1, Plain) => if (i == 0) darray_length(ma.asDeliteArray) else { cwarn("Geting dim " + i + " of 1D array"); Const(1) }
      case FlatLayout(1, Buffer) => if (i == 0) darray_buffer_length(ma.asDeliteArrayBuffer) else { cwarn("Getting dim " + i + " of 1D array"); Const(1) } 
      case FlatLayout(m, _) => 
        if (i < m) field[Int](ma,"dim"+i) //field[DeliteArray[Int]](ma, "dims").apply(unit(i))
        else { cwarn("Getting dim " + i + " of MultiArray of rank " + m); Const(1) }
      case _ => super.implementDim(ma,i)
    }

    override def implementSize[T:Manifest](ma: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Exp[Int] = layout(ma) match {
      case FlatLayout(1, Plain) => darray_length(ma.asDeliteArray)
      case FlatLayout(1, Buffer) => darray_buffer_length(ma.asDeliteArrayBuffer)
      case FlatLayout(_, _) => productTree(getDims(ma))
      case _ => super.implementSize(ma)
    }

    // --- MultiArray constructors
    /**
     * Creates a new MD array node
     * @param dims - M dimensions of output multiarray
     * @param out  - symbol properties of output multiarray
     */ 
    override def implementNew[A:Manifest](dims: Seq[Exp[Int]], out: SymbolProperties)(implicit ctx: SourceContext) = layout(out) match {
      case FlatLayout(1, Plain) => 
        flatarray_new[A](dims(0), mdat(out), mayUpdate(out))
      case FlatLayout(n, Plain) => 
        flatarray_plain_new[A](flatarray_new[A](productTree(dims), mdat(out), mayUpdate(out)), dims)
      case FlatLayout(n, View) =>
        flatarray_view_new[A](flatarray_new[A](productTree(dims), mdat(out), mayUpdate(out)), dims)
      case FlatLayout(1, Buffer) =>
        flatbuffer_new[A](dims(0), mdat(out), mayUpdate(out))
      case FlatLayout(n, Buffer) => 
        flatarray_buff_new[A](flatbuffer_new[A](productTree(dims), mdat(out), mayUpdate(out)), dims)
      case FlatLayout(n, BufferView) =>
        flatarray_buffview_new[A](flatbuffer_new[A](productTree(dims), mdat(out), mayUpdate(out)), dims)
      case _ => super.implementNew(dims, out)
    }

    /**
     * Calculates flat offset and ND stride from a previous offset and MD stride
     * @param m          - rank of input MD array
     * @param prevOfs    - flat offset of input MD array
     * @param prevStride - MD stride of input array
     * @param n          - rank of output ND array
     * @param ofs        - additional MD offset
     * @param stride     - additional ND stride
     * @param unitDims   - dimensions of input MD array which are dropped (for case when N < M, should be Nil otherwise)  
     */
    private def createViewParams(m: Int, prevOfs: Exp[Int], prevStride: Seq[Exp[Int]], n: Int, ofs: Seq[Exp[Int]], stride: Seq[Exp[Int]], unitDims: Seq[Int])(implicit ctx: SourceContext): (Exp[Int], Seq[Exp[Int]]) = {
      val flatOfs = sumTree( ofs.zip(prevStride).map{o => delite_int_times(o._1, o._2)} )
      val newOfs = delite_int_plus(flatOfs, prevOfs)
      val newStride = if (m == n)     stride.zip(prevStride).map{s => delite_int_times(s._1, s._2)}
                      else if (m < n) stride.take(n - m) ++ stride.drop(n - m).zip(prevStride).map{s => delite_int_times(s._1, s._2)}                           // reshape view?
                      else            prevStride.zipWithIndex.filterNot{unitDims contains _._2}.map{_._1}.zip(stride).map{s => delite_int_times(s._1, s._2)}    // sub-dim slice
      (newOfs, newStride)
    }

    /**
     * Creates a flat ND-view of a flat MD array (any config)
     * @param ma       - input MD array
     * @param start    - MD offset (same dimension as input multiarray)
     * @param stride   - ND stride (same dimension as output multiarray)
     * @param dims     - ND dimensions (dimensions of output multiarray)
     * @param unitDims - dimensions of input MD array which are dropped (for case when N < M, should be Nil otherwise)
     * @param out      - symbol properties of output ND array
     */
    override def implementView[A:Manifest](ma: Exp[DeliteMultiArray[A]], start: Seq[Exp[Int]], stride: Seq[Exp[Int]], dims: Seq[Exp[Int]], unitDims: Seq[Int], out: SymbolProperties)(implicit ctx: SourceContext) = (layout(ma), layout(out)) match {
      case (FlatLayout(1, Plain), FlatLayout(n, View)) => 
        val (newOfs, newStride) = createViewParams(1, unit(0), Seq(unit(1)), n, start, stride, unitDims)
        flatarray_view_new[A](ma.asDeliteArray, dims, newOfs, newStride)
      case (FlatLayout(1, Buffer), FlatLayout(n, BufferView)) => 
        val (newOfs, newStride) = createViewParams(1, unit(0), Seq(unit(1)), n, start, stride, unitDims)
        flatarray_buffview_new[A](ma.asDeliteArrayBuffer, dims, newOfs, newStride)
      case (FlatLayout(m, Plain), FlatLayout(n, View)) => 
        val impl = ma.asFlatArrayPlain
        val (newOfs, newStride) = createViewParams(m, unit(0), dimsToStrides(impl.dims), n, start, stride, unitDims)
        flatarray_view_new[A](impl.data, dims, newOfs, newStride)
      case (FlatLayout(m, Buffer), FlatLayout(n, BufferView)) => 
        val impl = ma.asFlatArrayBuff
        val (newOfs, newStride) = createViewParams(m, unit(0), dimsToStrides(impl.dims), n, start, stride, unitDims)
        flatarray_buffview_new[A](impl.data, dims, newOfs, newStride)
      case (FlatLayout(m, View), FlatLayout(n, View)) => 
        val impl = ma.asFlatArrayView
        val (newOfs, newStride) = createViewParams(m, impl.start, impl.strides, n, start, stride, unitDims)
        flatarray_view_new[A](impl.data, dims, newOfs, newStride)
      case (FlatLayout(m, BufferView), FlatLayout(n, BufferView)) => 
        val impl = ma.asFlatArrayBuffView
        val (newOfs, newStride) = createViewParams(m, impl.start, impl.strides, n, start, stride, unitDims)
        flatarray_buffview_new[A](impl.data, dims, newOfs, newStride)
      case _ => super.implementView(ma,start,stride,dims,unitDims,out)
    }

    // --- Single element ops
    /** 
     * Creates an apply node for a flat MD multiarray (any config)
     * @param ma - input MD array
     * @param i  - abstract (MD) index - may either be a loop index (with flat indexing and indices) or a simple index (with just indices)
     */
    override def implementApply[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[AbstractIndices])(implicit ctx: SourceContext) = layout(ma) match {
      case FlatLayout(1, Plain) => 
        flatarray_apply(ma.asDeliteArray, i.firstOrFlat)
      case FlatLayout(1, Buffer) => 
        flatbuffer_apply(ma.asDeliteArrayBuffer, i.firstOrFlat)
      case FlatLayout(n, Plain) => 
        val impl = ma.asFlatArrayPlain
        val index = i.calcIndex(n, impl.dims)
        flatarray_apply(impl.data, index)
      case FlatLayout(n, View) => 
        val impl = ma.asFlatArrayView
        val index = i.calcViewIndex(n, impl.start, impl.strides, isTrueView(ma))
        flatarray_apply(impl.data, index)
      case FlatLayout(n, Buffer) => 
        val impl = ma.asFlatArrayBuff
        val index = i.calcIndex(n, impl.dims)
        flatbuffer_apply(impl.data, index)
      case FlatLayout(n, BufferView) => 
        val impl = ma.asFlatArrayBuffView
        val index = i.calcViewIndex(n, impl.start, impl.strides, isTrueView(ma))
        flatbuffer_apply(impl.data, index)
      case _ => super.implementApply(ma,i)
    }

    /**
     * Creates an update node for a flat MD multiarray (any config)
     * @param ma - input MD array
     * @param i  - abstract (MD) index - may either be a loop index (with flat indexing and indices) or a simple index (with just indices)
     * @param x  - right hand side of update
     */
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

      // --- Technically shouldn't happen normally, but may be ok for accumulators in mutable reduction
      case FlatLayout(n, View) => 
        val impl = ma.asFlatArrayView
        val index = i.calcViewIndex(n, impl.start, impl.strides, isTrueView(ma))
        darray_update(impl.data, index, x)
      case FlatLayout(n, BufferView) => 
        val impl = ma.asFlatArrayBuffView
        val index = i.calcViewIndex(n, impl.start, impl.strides, isTrueView(ma))
        darray_buffer_update(impl.data, index, x)
      case _ => super.implementUpdate(ma,i,x)
    }

    // --- Array permute / reshaping
    /**
     * Creates a copy node of a flat MD array with reordered dimensions
     * @param ma     - input MD array
     * @param config - ordering of dimensions (e.g. for matrix transpose config = (1, 0) )
     * @param out    - symbol properties of output MD array
     */
    override def implementPermute[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
      case FlatLayout(n,_) => 
        val size = implementSize(ma)
        val dims = getDims(ma)
        val v = fresh[Int]
        val inds = calcIndices(v, dims)
        val i = loopindices_new(v, inds)
        val permuteInds = Seq.tabulate(n){d => i(config(d))}
        val permuteDims = Seq.tabulate(n){d => dims(config(d))}
        val body: Block[A] = withInnerScopeAdditions{ reifyEffects(implementApply(ma, indices_new(permuteInds))) }
        implementCollect(v, i, layout(out), body, permuteDims, size)

      case _ => super.implementPermute(ma,config,out)
    }
   
    /**
     * Creates an ND copy of a flat MD array, where output size is identical but dimensions differ
     * @param ma   - input MD array
     * @param dims - sequence of N dimensions, giving size of each dimension in output array
     * @param out  - symbol properties of output ND array
     */
    override def implementReshape[A:Manifest](ma: Exp[DeliteMultiArray[A]], dims: Seq[Exp[Int]], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
      case FlatLayout(_,_) => 
        val data = implementCopy(ma, FlatLayout(1, Plain)).asInstanceOf[Exp[DeliteArray[A]]]

        layout(out) match {
          case FlatLayout(1, Plain) => data
          case FlatLayout(n, Plain) => flatarray_plain_new(data, dims)
          case FlatLayout(n, View) => flatarray_view_new(data, dims)
          case FlatLayout(1, Buffer) => flatbuffer_wrap(data, implementSize(ma), false)
          case FlatLayout(n, Buffer) => flatarray_buff_new(flatbuffer_wrap(data, implementSize(ma), false), dims)
          case FlatLayout(n, BufferView) => flatarray_buffview_new(flatbuffer_wrap(data, implementSize(ma), false), dims)
        }

      case _ => super.implementReshape(ma,dims,out)
    }

    /**
     * Creates a view of a flat MD array with reordered dimensions
     * @param ma     - input MD array
     * @param config - ordering of dimensions (e.g. for matrix transpose config = (1, 0) )
     * @param out    - symbol properties of output MD array
     */
    override def implementPermuteView[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = (layout(ma), layout(out)) match {
      case (FlatLayout(m,_), FlatLayout(n,_)) => 
        val dims = getDims(ma)
        
        val prevStart = layout(ma) match {
          case FlatLayout(_, View) => ma.asFlatArrayView.start
          case FlatLayout(_, BufferView) => ma.asFlatArrayBuffView.start
          case _ => unit(0)
        }
        val prevStrides = layout(ma) match {
          case FlatLayout(_, View) => ma.asFlatArrayView.strides
          case FlatLayout(_, BufferView) => ma.asFlatArrayBuffView.strides
          case _ => dimsToStrides(dims)
        }
        // Scramble dims and strides per config
        val permuteStrides = Seq.tabulate(n){d => prevStrides(config(d))}
        val permuteDims = Seq.tabulate(n){d => dims(config(d))}
        
        layout(ma) match {
          case FlatLayout(1, Plain) => 
            flatarray_view_new(ma.asDeliteArray, permuteDims, prevStart, permuteStrides)
          case FlatLayout(1, Buffer) => 
            flatarray_buffview_new(ma.asDeliteArrayBuffer, permuteDims, prevStart, permuteStrides)
          case FlatLayout(_, Plain) => 
            val impl = ma.asFlatArrayPlain
            flatarray_view_new(impl.data, permuteDims, prevStart, permuteStrides)
          case FlatLayout(_, Buffer) =>
            val impl = ma.asFlatArrayBuff
            flatarray_buffview_new(impl.data, permuteDims, prevStart, permuteStrides)
          case FlatLayout(_, View) => 
            val impl = ma.asFlatArrayView
            flatarray_view_new(impl.data, permuteDims, prevStart, permuteStrides)
          case FlatLayout(_, BufferView) =>
            val impl = ma.asFlatArrayBuffView
            flatarray_buffview_new(impl.data, permuteDims, prevStart, permuteStrides)
        }
      case _ => super.implementPermuteView(ma,config,out)
    }

    /**
     * Creates an ND view of a flat MD array, where output size is identical but dimensions differ
     * If M > N, top M - N dimensions are assumed to be dropped
     * @param ma   - input MD array
     * @param dims - sequence of N dimensions, giving size of each dimension in output view
     * @param out  - symbol properties of output ND array
     */
    override def implementReshapeView[A:Manifest](ma: Exp[DeliteMultiArray[A]], dims: Seq[Exp[Int]], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = (layout(ma), layout(out)) match {
      case (FlatLayout(m,_), FlatLayout(n,_)) =>
        layout(ma) match {
          case FlatLayout(1, Plain) =>
            flatarray_view_new(ma.asDeliteArray, dims, unit(0), dimsToStrides(dims))
          case FlatLayout(1, Buffer) =>
            flatarray_buffview_new(ma.asDeliteArrayBuffer, dims, unit(0), dimsToStrides(dims))
          case FlatLayout(_, Plain) => 
            val impl = ma.asFlatArrayPlain
            flatarray_view_new(impl.data, dims, unit(0), dimsToStrides(dims))
          case FlatLayout(_, Buffer) =>
            val impl = ma.asFlatArrayBuff
            flatarray_buffview_new(impl.data, dims, unit(0), dimsToStrides(dims))
          
          // Behavior here is to ignore intermediate view parameters, viewing the entire array.
          // This is to allow things like x.reshapeView(...).reshapeView(...)
          // which is kind of dumb, but not strictly illegal. 
          case FlatLayout(_, View) =>
            val impl = ma.asFlatArrayView
            flatarray_view_new(impl.data, dims, unit(0), dimsToStrides(dims))
          case FlatLayout(_, BufferView) =>
            val impl = ma.asFlatArrayBuffView
            flatarray_buffview_new(impl.data, dims, unit(0), dimsToStrides(dims))
        }

      case _ => super.implementReshapeView(ma,dims,out)  
    }

    // --- Parallel ops
    /**
     * Creates a parallel collect node creating an MD array from an abstract parallel node
     * @param v    - original iterator
     * @param i    - original loop index
     * @param out  - layout of output MD array
     * @param body - loop body
     * @param dims - dimensions of output MD array
     * @param size - total size of loop
     * @param cond - optional conditional block of collect (default is None)
     */
    private def implementCollect[A:Manifest](v: Sym[Int], i: Exp[LoopIndices], out: Layout[_,_], body: Block[A], dims: Seq[Exp[Int]], size: Exp[Int], cond: Option[Block[Boolean]] = None)(implicit ctx: SourceContext): Exp[Any] = out match {
      case FlatLayout(_,_) => 
        val inds = calcIndices(v, dims)
        val i2 = loopindices_new(v, inds)

        val (mirroredBody, mirroredCond) = withSubstScope(i -> i2){ (f(body), cond.map{f(_)}) }
        val data = reflectPure(CollectFactory.array[A](v, size, mirroredBody, mirroredCond)).withData(FlatLayout(1,Plain)).withChild(props(mirroredBody))

        out match {
          case FlatLayout(1, Plain) => data
          case FlatLayout(n, Plain) => flatarray_plain_new(data, dims)
          case FlatLayout(n, View) => flatarray_view_new(data, dims)
          case FlatLayout(1, Buffer) => flatbuffer_wrap(data, size, false)
          case FlatLayout(n, Buffer) => flatarray_buff_new(flatbuffer_wrap(data, size, false), dims)
          case FlatLayout(n, BufferView) => flatarray_buffview_new(flatbuffer_wrap(data, size, false), dims)
        }
    }

    /**
     * Special case of collect - creates a copy node of an MD array
     * @param ma  - input MD array
     * @param out - layout of output array
     * @param dat - child of output array
     */
    private def implementCopy[A:Manifest](ma: Exp[DeliteMultiArray[A]], out: Layout[_,_]) = {
      val size = implementSize(ma)
      val dims = getDims(ma)
      val v = fresh[Int]
      val i = loopindices_new(v, calcIndices(v, dims))
      val body: Block[A] = withInnerScopeAdditions{ reifyEffects(implementApply(ma, i)) }
      implementCollect(v, i, out, body, dims, size)
    }

    /**
     * Create a flat MultiArray from the contents of a file and a mapping function
     * TODO: Binary file reading (file format with tag giving number of dimensions and size of each?)
     * @param path - relative path to input file
     * @param dels - file delimeters separating elements, dimensions
     * @param body - reified mapping function from string to element type
     * @param v    - original bound symbol for iterator
     * @param i    - original multidimensional index
     * @param rV   - original bound symbol used to reify map body
     * @param out  - symbol properties of output MultiArray
     */
    override def implementReadFile[A:Manifest](path: Exp[String], dels: Seq[Exp[String]], body: Block[A], v: Sym[Int], i: Exp[LoopIndices], rV: Sym[String], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
      case FlatLayout(_,_) =>
        var partialResults: Seq[Exp[DeliteArray[String]]] = Nil
        var dims: Seq[Exp[Int]] = Nil
        var sizes: Seq[Exp[Int]] = Nil
        
        // Read in lines - number of lines is equivalent to the first dimension size
        val readIter = fresh[Int].asInstanceOf[Sym[Int]]
        val orig = reflectPure(SimpleReadFactory.array(readIter, Seq(path))).withData(FlatLayout(1, Plain))

        partialResults = partialResults :+ orig
        dims = dims :+ orig.length
        sizes = sizes :+ orig.length
        
        // Splitting is currently implemented as a series of flatMaps on the array of Strings. 
        // Dimensions are derived as the size of the current flatmap output divided by product of previously found dimensions 
        // TODO: This may be a really bad implementation, but it seems to be one of the easier ways to implement it in the MultiArray framework
        // right now. Do these flatmaps fuse? Do we create multiple copies of the data to get the dimension sizes? What's a better way?
        for (d <- 1 until dels.length) {
          val prev = partialResults(d - 1)

          val oV = fresh[Int].asInstanceOf[Sym[Int]]
          val oI = loopindices_new(oV, Seq(oV))
          val body = withInnerScopeAdditions{ reifyEffects{ 
            val elem = implementApply(prev.asInstanceOf[Exp[DeliteMultiArray[String]]], oI)
            darray_split_string(elem.asInstanceOf[Exp[String]], dels(d), unit(-1)).withData(FlatLayout(1, Plain))
          }}

          val op = FlatMapFactory.array[String](oV, sizes(d - 1), body)
          setProps(op.eF, props(body))

          val curr = reflectPure(op).withData(FlatLayout(1, Plain))
          val currDim = delite_int_divide(curr.length, productTree(dims))

          partialResults = partialResults :+ curr
          dims = dims :+ currDim
          sizes = sizes :+ curr.length
        }

        val stringArray = partialResults.last

        val i2 = loopindices_new(v, Seq(v))
        val elem = implementApply(stringArray.asInstanceOf[Exp[DeliteMultiArray[String]]], i2)
        val mirroredBody = withInnerScopeAdditions{ withSubstScope(i -> i2, rV -> elem){ f(body) } }

        implementCollect(v, i2, layout(out), mirroredBody, dims, sizes.last)

      case _ => super.implementReadFile(path,dels,body,v,i,rV,out)
    }
    override def implementFromFunction[A:Manifest](dims: Seq[Exp[Int]], body: Block[A], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
      case FlatLayout(_,_) => implementCollect[A](v, i, layout(out), body, dims, productTree(dims))
      case _ => super.implementFromFunction(dims,body,v,i,out)
    }
    override def implementMap[A:Manifest,R:Manifest](in: Exp[DeliteMultiArray[A]], body: Block[R], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
      case FlatLayout(_,_) => implementCollect[R](v, i, layout(out), body, getDims(in), implementSize(in))
      case _ => super.implementMap(in,body,v,i,out)
    }
    override def implementZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DeliteMultiArray[A]], inB: Exp[DeliteMultiArray[B]], body: Block[R], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
      case FlatLayout(_,_) => implementCollect[R](v, i, layout(out), body, getDims(inA), implementSize(inA))

      case _ => super.implementZipWith(inA,inB,body,v,i,out)
    }
    override def implementNDMap[A:Manifest,B:Manifest](in: Exp[DeliteMultiArray[A]], body: Block[DeliteMultiArray[B]], rV: Sym[DeliteMultiArray[A]], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      // Reminder: Create substitution for rV that isn't a DeliteMultiArray!
      super.implementNDMap(in,body,rV,out) 
    }
    override def implementGroupBy[A:Manifest,K:Manifest,V:Manifest](in: Exp[DeliteMultiArray[A]], keyFunc: Block[K], valFunc: Block[V])(implicit ctx: SourceContext): Exp[Any] = {
      super.implementGroupBy(in,keyFunc,valFunc)
    }
    override def implementGroupByReduce[A:Manifest,K:Manifest,V:Manifest](in: Exp[DeliteMultiArray[A]], keyFunc: Block[K], valFunc: Block[V], redFunc: Block[V], rV: (Sym[V], Sym[V]))(implicit ctx: SourceContext): Exp[Any] = {
      // Reminder: Create substitution for rV that isn't a DeliteMultiArray!
      super.implementGroupByReduce(in,keyFunc,valFunc,redFunc,rV)
    }

    // FIXME: This may still not work if the reduction function produces a buffer from non-buffer inputs
    // TODO: Mutable reduce?
    override def implementFilterReduce[A:Manifest](in: Exp[DeliteMultiArray[A]], zero: Exp[DeliteMultiArray[A]], filter: Block[Boolean], reduce: Block[DeliteMultiArray[A]], axis: Int, v: Sym[Int], rV: (Sym[DeliteMultiArray[A]],Sym[DeliteMultiArray[A]]))(implicit ctx: SourceContext): Exp[Any] = layout(in) match {
      case FlatLayout(m,_) => 
        val dims = getDims(in)
        val start = Seq.fill(m){unit(0)}
        val stride = Seq.fill(m){unit(1)}
        val vDims = dims.zipWithIndex.filter{i => i._2 != axis}.map{i => i._1}
        val loopSize = dims(axis)

        val mirroredFilter = f(filter)

        layout(in) match {
          case FlatLayout(_,Plain) | FlatLayout(_,View) =>
            val z = zero.asFlatArrayView
            //val rV2 = ( reflectMutableSym(fresh[FlatArrayView[A]]), fresh[FlatArrayView[A]] )
            val rV2 = (fresh[FlatArrayView[A]], fresh[FlatArrayView[A]]) 
            
            val lookup = withInnerScopeAdditions{ reifyEffects(implementView(in,start,stride,vDims,Seq(axis),props(rV._1))) }.asInstanceOf[Block[FlatArrayView[A]]]
            setProps(rV2._1, props(lookup))
            setProps(rV2._2, props(lookup))

            val mirroredReduce = withSubstScope(rV._1 -> rV2._1, rV._2 -> rV2._2){ f(reduce) }.asInstanceOf[Block[FlatArrayView[A]]]
            val e = reflectPure( ReduceFactory[FlatArrayView[A]](v, rV2, loopSize, lookup, mirroredReduce, z, Some(mirroredFilter)) ).withProps(props(mirroredReduce))

            //println("Generated fold with " + e.tp + makeString(props(e)))
            (e)

          case FlatLayout(_,Buffer) | FlatLayout(_,BufferView) =>
            val z = zero.asFlatArrayBuffView
            //val rV2 = ( reflectMutableSym(fresh[FlatArrayBuffView[A]]), fresh[FlatArrayBuffView[A]] )
            val rV2 = (fresh[FlatArrayBuffView[A]], fresh[FlatArrayBuffView[A]]) 
            val lookup = withInnerScopeAdditions{ reifyEffects(implementView(in,start,stride,vDims,Seq(axis),props(rV._1))) }.asInstanceOf[Block[FlatArrayBuffView[A]]]
            
            setProps(rV2._1, props(lookup))
            setProps(rV2._2, props(lookup))
            val mirroredReduce = withSubstScope(rV._1 -> rV2._1, rV._2 -> rV2._2){ f(reduce) }.asInstanceOf[Block[FlatArrayBuffView[A]]]
            
            reflectPure( ReduceFactory[FlatArrayBuffView[A]](v, rV2, loopSize, lookup, mirroredReduce, z, Some(mirroredFilter)) ).withProps(props(mirroredReduce))
        }
      case _ => super.implementFilterReduce(in,zero,filter,reduce,axis,v,rV)
    }

    // --- 1D Parallel Ops
    override def implementMapFilter[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], mapFunc: Block[B], filtFunc: Block[Boolean], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
      case FlatLayout(_,_) => implementCollect[B](v, i, layout(out), mapFunc, getDims(in), implementSize(in), Some(filtFunc))
      case _ => super.implementMapFilter(in,mapFunc,filtFunc,v,i,out)  
    }
    override def implementFlatMap[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], body: Block[DeliteArray1D[B]], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
      case FlatLayout(_,_) => 
        val size = implementSize(in)
        val i2 = loopindices_new(v, Seq(v))

        val mirroredBody = withSubstScope(i -> i2){ f(body) }.asInstanceOf[Block[DeliteCollection[B]]]
        val op = FlatMapFactory.array[B](v, size, mirroredBody)

        // HACK --- Add metadata to the internal eF symbol to match the output of the function body so that MultiArrayImpl's dc_apply can be used
        // Relies on lazy body evaluation!
        setProps(op.eF, props(getBlockResult(mirroredBody)))

        val data = reflectPure(op).withData(FlatLayout(1, Plain)).withChild(mdat(out))

        layout(out) match {
          case FlatLayout(_, Plain) => data
          case FlatLayout(_, View) => flatarray_view_new(data, Seq(size))
          case FlatLayout(_, Buffer) => flatbuffer_wrap(data, size, false)
          case FlatLayout(_, BufferView) => flatarray_buffview_new(flatbuffer_wrap(data,size,false), Seq(size))
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
        val li = loopindices_new(v, Seq(v))
        val elem = implementApply(x, li).asInstanceOf[Exp[A]]
        val body: Block[Unit] = reifyEffects(darray_buffer_update(impl, delite_int_plus(v, i), elem))
        reflectEffect(ForeachFactory(v, length, body), summarizeEffects(body).star)

      case _ => super.implementInsertAll(ma,axis,i,x)
    }

    override def implementRemove[A:Manifest](ma: Exp[DeliteMultiArray[A]], axis: Int, start: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = layout(ma) match {
      case FlatLayout(1, Buffer) =>
        darray_buffer_remove(ma.asDeliteArrayBuffer, start, len)

      case _ => super.implementRemove(ma,axis,start,len)
    }

    // --- Misc.
    override def implementWriteFile[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]], path: Exp[String], body: Block[String], v: Sym[Int], i: Exp[LoopIndices])(implicit ctx: SourceContext): Exp[Unit] = layout(ma) match {
      //case FlatLayout(_,_) =>

      case _ => super.implementWriteFile(ma,dels,path,body,v,i)
    }

    // --- 1D ops
    override def implementSortIndices(len: Exp[Int], i: (Sym[Int], Sym[Int]), body: Block[Int], out: SymbolProperties)(implicit ctx: SourceContext) = layout(out) match {
      case FlatLayout(1, _) => 
        reflectPure(DeliteArraySortIndices(len, i, body)).withData(FlatLayout(1, Plain)).withChild(getChild(out))
      case _ => super.implementSortIndices(len,i,body,out)
    }
    override def implementStringSplit(str: Exp[String], split: Exp[String], lim: Exp[Int], out: SymbolProperties)(implicit ctx: SourceContext) = layout(out) match { 
      case FlatLayout(1, Plain) => 
        darray_split_string(str, split, lim).withData(FlatLayout(1, Plain)).withChild(getChild(out))
      case _ => super.implementStringSplit(str,split,lim,out)
    }

    // --- 2D Ops
    override def implementMatrixMultiply[A:Manifest](lhs: Exp[DeliteArray2D[A]], rhs: Exp[DeliteArray2D[A]], default: Block[DeliteArray2D[A]], rM1: Sym[DeliteArray2D[A]], rM2: Sym[DeliteArray2D[A]])(implicit ctx: SourceContext): Exp[Any] = (layout(lhs),layout(rhs)) match {
      case (FlatLayout(2, _), FlatLayout(2, _)) => 
        val mirroredBody = withSubstScope(rM1 -> lhs, rM2 -> rhs){ f(default) }
        getBlockResult(mirroredBody).withData(FlatLayout(2, Plain))
      case _ => super.implementMatrixMultiply(lhs,rhs,default,rM1,rM2) 
    }
    override def implementMatrixVectorMultiply[A:Manifest](mat: Exp[DeliteArray2D[A]], vec: Exp[DeliteArray1D[A]], default: Block[DeliteArray1D[A]], rM: Sym[DeliteArray2D[A]], rV: Sym[DeliteArray1D[A]])(implicit ctx: SourceContext): Exp[Any] = (layout(mat), layout(vec)) match {
      case (FlatLayout(2, _), FlatLayout(1, _)) => 
        val mirroredBody = withSubstScope(rM -> mat, rV -> vec){ f(default) }
        getBlockResult(mirroredBody).withData(FlatLayout(1, Plain))
      case _ => super.implementMatrixVectorMultiply(mat,vec,default,rM,rV)
    }

    // --- HashMap
    override def implementMapFromArrays[K:Manifest,V:Manifest](keys: Exp[DeliteArray1D[K]], vals: Exp[DeliteArray1D[V]])(implicit ctx: SourceContext): Exp[Any] = (layout(keys),layout(vals)) match {
      case (FlatLayout(1,Plain),FlatLayout(1,Plain)) => 
        val id: Exp[K] => Exp[K] = k => k
        val index = reflectPure(DeliteMapBuildIndex(keys.asDeliteArray,id))
        reflectPure(DeliteMapNewImm(keys.asDeliteArray, vals.asDeliteArray, index, implementSize(vals)))
      case _ => super.implementMapFromArrays(keys,vals)
    }


    // --- Wrap
    override def implementBuffify[A:Manifest](ma: Exp[DeliteMultiArray[A]], out: SymbolProperties) = (layout(ma), layout(out)) match {
      case (FlatLayout(1, Plain), FlatLayout(1, Buffer)) => flatbuffer_wrap(ma.asDeliteArray, implementSize(ma), false)
      case (FlatLayout(n, Plain), FlatLayout(_, Buffer)) => flatbuffer_wrap(ma.asFlatArrayPlain.data, implementSize(ma), false)
      case (FlatLayout(n, View), FlatLayout(_, BufferView)) => sys.error("TODO - buffify of view")
      case _ => super.implementBuffify(ma, out)
    }
    override def implementViewify[A:Manifest](ma: Exp[DeliteMultiArray[A]], out: SymbolProperties) = layout(ma) match {
      case FlatLayout(1, Plain) => flatarray_view_new(ma.asDeliteArray, getDims(ma))
      case FlatLayout(n, Plain) => flatarray_view_new(ma.asFlatArrayPlain.data, getDims(ma))
      case FlatLayout(1, Buffer) => flatarray_buffview_new(ma.asDeliteArrayBuffer, getDims(ma))
      case FlatLayout(n, Buffer) => flatarray_buffview_new(ma.asFlatArrayBuff.data, getDims(ma))
      case _ => super.implementViewify(ma, out)
    }
  }
} 
