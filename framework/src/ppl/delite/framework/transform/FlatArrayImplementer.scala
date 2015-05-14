package ppl.delite.framework.transform

import scala.virtualization.lms.common._
import scala.reflect.{SourceContext, RefinedManifest}
import ppl.delite.framework.ops._ 
import ppl.delite.framework.datastructures._

import ppl.delite.framework.analysis.LayoutMetadata
import ppl.delite.framework.visit._
import ppl.delite.framework.Util._
import ppl.delite.framework.visit.Meetable._

/////////////////
// Flat Layout //
/////////////////

trait FlatArrayPlain[T] extends MultiArrayPlainImpl[T] with Struct
trait FlatArrayView[T] extends MultiArrayViewImpl[T] with Struct
trait FlatArrayBuff[T] extends MultiArrayBuffImpl[T] with Struct
trait FlatArrayBuffView[T] extends MultiArrayBuffViewImpl[T] with Struct

trait FlatArrayImplExp extends MultiArrayImplExp with DeliteOpsExp with DeliteArrayBufferOpsExp { self => 
  override val implementer : FlatArrayImplementer

  // --- Flat indexing
  def dimsToStride(dims: Seq[Exp[Int]]): Seq[Exp[Int]] = {
    Seq.tabulate(dims.length){d => 
      if (d == dims.length - 1) {unit(1)}
      else productTree(dims.drop(d + 1))
    }
  }

  def calcFlatIndex(i: Seq[Exp[Int]], stride: Seq[Exp[Int]]): Exp[Int] = {
    sumTree(Seq.tabulate(i.length - 1){d => delite_int_times(i(d), stride(d))} :+ i.last)
  }

  // flat index = sum(ofs_i + stride_i * index_i)
  def calcFlatViewIndex(i: Seq[Exp[Int]], ofs: Exp[Int], stride: Seq[Exp[Int]]): Exp[Int] = {
    sumTree(Seq.tabulate(i.length){d => delite_int_times(i(d), stride(d)) } :+ ofs)
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
        calcFlatIndex(x.toSeq(n), dimsToStride(dims))
    }

    def calcViewIndex(n: Int, ofs: Exp[Int], stride: Seq[Exp[Int]]): Exp[Int] = calcFlatViewIndex(x.toSeq(n), ofs, stride)
  }

  // --- Convenience methods for casting DeliteMultiArray to FlatArrays
  // Casts must be guarded by assertions, esp. during compiler debugging. If an assertion here fails, something
  // went wrong with the transformer ordering!
  implicit def multiarrayToFlatCastOps[T:Manifest](x: Exp[DeliteMultiArray[T]]) = new FlatArrayCastOpsCls[T](x)
  class FlatArrayCastOpsCls[T:Manifest](x: Exp[DeliteMultiArray[T]]) { 
    def asFlatArrayPlain = {
      assert(isFlatArrayPlain(x), "Cannot cast " + x.tp + " to FlatArrayPlain")
      x.asInstanceOf[Exp[FlatArrayPlain[T]]]
    }
    def asFlatArrayView = {
      assert(isFlatArrayView(x), "Cannot cast " + x.tp + " to FlatArrayView")
      x.asInstanceOf[Exp[FlatArrayView[T]]]
    }
    def asFlatArrayBuff = {
      assert(isFlatArrayBuff(x), "Cannot cast " + x.tp + " to FlatArrayBuff")
      x.asInstanceOf[Exp[FlatArrayBuff[T]]]
    }
    def asFlatArrayBuffView = {
      assert(isFlatArrayBuffView(x), "Cannot cast " + x.tp + " to FlatArrayBuffView")
      x.asInstanceOf[Exp[FlatArrayBuffView[T]]]
    }
  }

  // --- Convenience methods for data field of FlatArray types
  implicit def flatArrayPlainToOpsCls[T:Manifest](x: Exp[FlatArrayPlain[T]])(implicit ctx: SourceContext) = new FlatArrayPlainOpsCls(x)(manifest[T],ctx)
  class FlatArrayPlainOpsCls[T:Manifest](x: Exp[FlatArrayPlain[T]])(implicit ctx: SourceContext) {
    def data = {val res = field[DeliteArray[T]](x, "data"); implementer.setProps(res, implementer.mdat(x)); (res) }
  }
  implicit def flatArrayViewToOpsCls[T:Manifest](x: Exp[FlatArrayView[T]])(implicit ctx: SourceContext) = new FlatArrayViewOpsCls(x)(manifest[T],ctx)
  class FlatArrayViewOpsCls[T:Manifest](x: Exp[FlatArrayView[T]])(implicit ctx: SourceContext) {
    def data = {val res = field[DeliteArray[T]](x, "data"); implementer.setProps(res, implementer.mdat(x)); (res) }
    def start = field[Int](x, "ofs")
  }
  implicit def flatArrayBuffToOpsCls[T:Manifest](x: Exp[FlatArrayBuff[T]])(implicit ctx: SourceContext) = new FlatArrayBuffOpsCls(x)(manifest[T],ctx)
  class FlatArrayBuffOpsCls[T:Manifest](x: Exp[FlatArrayBuff[T]])(implicit ctx: SourceContext) {
    def data = {val res = field[DeliteArrayBuffer[T]](x, "data"); implementer.setProps(res, implementer.mdat(x)); (res) }
  }
  implicit def flatArrayBuffViewToOpsCls[T:Manifest](x: Exp[FlatArrayBuffView[T]])(implicit ctx: SourceContext) = new FlatArrayBuffViewOpsCls(x)(manifest[T],ctx)
  class FlatArrayBuffViewOpsCls[T:Manifest](x: Exp[FlatArrayBuffView[T]])(implicit ctx: SourceContext) {
    def data = {val res = field[DeliteArrayBuffer[T]](x, "data"); implementer.setProps(res, implementer.mdat(x)); (res) }
    def start = field[Int](x, "ofs")
  }

  def darrayBufferManifest(typeArg: Manifest[_]): Manifest[DeliteArrayBuffer[_]] = makeManifest(classOf[DeliteArrayBuffer[_]], List(typeArg))
  private def dataField[T](tp: Manifest[T]): List[(String, Manifest[_])] = List("data" -> darrayManifest(tp))
  private def buffField[T](tp: Manifest[T]): List[(String, Manifest[_])] = List("data" -> darrayBufferManifest(tp))
  private def dimFields(n: Int): List[(String, Manifest[_])] = List.tabulate(n){i => ("dim" + i) -> manifest[Int]}
  private def viewFields(n: Int): List[(String, Manifest[_])] = dimFields(n) ++ List.tabulate(n){i => ("ofs" + i) -> manifest[Int]} ++ List.tabulate(n){i => ("stride" + i) -> manifest[Int]}

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

  def flatbuffer_wrap[A:Manifest](data: Exp[DeliteArray[A]], length: Exp[Int])(implicit ctx: SourceContext) = {
    val out = reflectPure(DeliteArrayBufferWrap(data,length))

    implementer.setMetadata(out, FlatLayout(1, Buffer))
    implementer.setField(out, implementer.getProps(data), "data")
    (out)
  }

  // --- New flat ND array
  def flatarray_plain_new[T:Manifest](data: Exp[DeliteArray[T]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayPlain[T]] = {    
    def slf(x: Rep[_]) = (y: Rep[FlatArrayPlain[T]]) => x
    val tp = flatplainManifest(manifest[T], dims.length)
    val out = record_new(("data",false,slf(data)) +: dims.zipWithIndex.map{i => ("dim" + i._2, false, slf(i._1))})(tp)
    
    implementer.setMetadata(out, FlatLayout(dims.length, Plain))
    implementer.setField(out, implementer.getProps(data), "data")
    (out)
  }

  // --- New flat view
  // If this is a view of a view, will already have accounted for dimensions
  // If this is a wrapped plain array, will calculate the stride in the overloaded version of method
  def flatarray_view_new[T:Manifest](data: Exp[DeliteArray[T]], dims: Seq[Exp[Int]], ofs: Exp[Int], stride: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayView[T]] = {    
    def slf(x: Rep[_]) = (y: Rep[FlatArrayView[T]]) => x
    val tp = flatviewManifest(manifest[T], dims.length)
    val out = record_new( (("data", false, slf(data)) +: dims.zipWithIndex.map{i => ("dim" + i._2, false, slf(i._1))}) ++ 
                          (("ofs", false, slf(ofs)) +: stride.zipWithIndex.map{d => ("stride" + d._2, false, slf(d._1))}))(tp)
    implementer.setMetadata(out, FlatLayout(dims.length, View))
    implementer.setField(out, implementer.getProps(data), "data")
    (out)
  }
  def flatarray_view_new[A:Manifest](data: Exp[DeliteArray[A]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayView[A]] = {
    flatarray_view_new(data, dims, unit(0), dimsToStride(dims))  
  }

  // --- New flat buffer
  def flatarray_buff_new[T:Manifest](data: Exp[DeliteArrayBuffer[T]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayBuff[T]] = {
    def slf(x: Rep[_]) = (y: Rep[FlatArrayBuff[T]]) => x
    val tp = flatbuffManifest(manifest[T], dims.length)
    val out = record_new(("data", false, slf(data)) +: dims.zipWithIndex.map{i => ("dim" + i._2, true, slf(i._1))})(tp)
 
    implementer.setMetadata(out, FlatLayout(dims.length, Buffer))
    implementer.setField(out, implementer.getProps(data), "data")
    (out)
  }
    
  // --- New flat buffer view
  def flatarray_buffview_new[T:Manifest](data: Exp[DeliteArrayBuffer[T]], dims: Seq[Exp[Int]], ofs: Exp[Int], stride: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayBuffView[T]] = {
    def slf(x: Rep[_]) = (y: Rep[FlatArrayBuffView[T]]) => x
    val tp = flatbuffviewManifest(manifest[T], dims.length)
    val out = record_new( (("data", false, slf(data)) +: dims.zipWithIndex.map{i => ("dim" + i._2, false, slf(i._1))}) ++ 
                          (("ofs", false, slf(ofs)) +: stride.zipWithIndex.map{d => ("stride" + d._2, false, slf(d._1))}))(tp)
    implementer.setMetadata(out, FlatLayout(dims.length, BufferView))
    implementer.setField(out, implementer.getProps(data), "data")
    (out)                        
  }
  def flatarray_buffview_new[A:Manifest](data: Exp[DeliteArrayBuffer[A]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[FlatArrayBuffView[A]] = {
    flatarray_buffview_new(data, dims, unit(0), dimsToStride(dims))
  }

  // --- Alternatives on DeliteArray, DeliteArrayBuffer for inserting metadata
  // TODO: Need to determine partition tags?
  def flatarray_new[A:Manifest](size: Exp[Int], child: SymbolProperties)(implicit ctx: SourceContext): Exp[DeliteArray[A]] = {
    val out = DeliteArray.imm[A](size)
    implementer.setMetadata(out, FlatLayout(1, Plain))
    implementer.setChild(out, Some(child))
    (out)
  }
  def flatbuffer_new[A:Manifest](size: Exp[Int], child: SymbolProperties)(implicit ctx: SourceContext): Exp[DeliteArrayBuffer[A]] = {
    val data = flatarray_new(size, child)
    flatbuffer_wrap(data, size)
  }

  def flatarray_apply[A:Manifest](a: Exp[DeliteArray[A]], i: Exp[Int])(implicit ctx: SourceContext): Exp[A] = {
    val out = darray_apply(a, i)
    implementer.setProps(out, implementer.getChild(a))
    (out)
  }
  def flatbuffer_apply[A:Manifest](a: Exp[DeliteArrayBuffer[A]], i: Exp[Int])(implicit ctx: SourceContext): Exp[A] = {
    val out = darray_buffer_apply(a, i)
    implementer.setProps(out, implementer.getField(a, "data").flatMap{_.asInstanceOf[ArrayProperties].child})
    (out)
  }

  // --- Flat Array Nodes
  // mkString function for flat 1D array
  case class FlatArrayMkString[A:Manifest](del: Exp[String], len: () => Exp[Int], lookup: Exp[Int] => Exp[A])(implicit ctx: SourceContext) extends DeliteOpSingleWithManifest[A,String](reifyEffectsHere{
    val size = len()
    if (delite_less_than(size, unit(1))) unit("[ ]")
    else {
      val i = var_new(unit(0))
      val s = var_new(unit(""))
      while(delite_less_than(i,delite_int_minus(size,unit(1)))) {
        s = delite_string_concat(readVar(s), delite_stringify( lookup(readVar(i)) ))
        s = delite_string_concat(readVar(s), del)
        i = delite_int_plus(readVar(i), unit(1))
      }
      delite_string_concat(readVar(s), delite_stringify(lookup(readVar(i))))
    }
  }) { override def toString = "FlatArrayMkString(" + block + ")" }

  // TODO: Use slices with flat_array_mkstring calls instead?
  case class FlatMatrixMkString[A:Manifest](rdel: Exp[String], cdel: Exp[String], dim: Int => Exp[Int], lookup: Seq[Exp[Int]] => Exp[A])(implicit ctx: SourceContext) extends DeliteOpSingleWithManifest[A,String](reifyEffectsHere{
    val rows = dim(0)
    val cols = dim(1)
    if (delite_less_than(rows, unit(1))) unit("[ ]")
    else if (delite_less_than(cols, unit(1))) unit("[ ]")
    else {
      val r = var_new(unit(0))
      val c = var_new(unit(0))
      val s = var_new(unit(""))

      def row_mkstring() {
        while (delite_less_than(c, delite_int_minus(cols, unit(1)))) {
          s = delite_string_concat(readVar(s), delite_stringify( lookup(Seq(readVar(r),readVar(c)))))
          s = delite_string_concat(readVar(s), cdel)
          c = delite_int_plus(readVar(c), unit(1))
        }
        s = delite_string_concat(readVar(s), delite_stringify(lookup(Seq(readVar(r),readVar(c)))))
      }
 
      while (delite_less_than(r, delite_int_minus(rows, unit(1)))) {
        row_mkstring()
        s = delite_string_concat(readVar(s), rdel)
        r = delite_int_plus(readVar(r), unit(1))
        c = unit(0)
      }
      row_mkstring()
      readVar(s)
    }
  }) { override def toString = "FlatMatrixMkString(" + block + ")" }

  // --- Delite collection dc_apply (for use as result in flatMap)
  def isFlatArrayPlain[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[FlatArrayPlain[_]])
  def isFlatArrayView[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[FlatArrayView[_]])
  def isFlatArrayBuff[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[FlatArrayView[_]])
  def isFlatArrayBuffView[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[FlatArrayBuffView[_]])

  def asFlatArrayPlain[T](x: Exp[DeliteCollection[T]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[FlatArrayPlain[T]]]
  def asFlatArrayView[T](x: Exp[DeliteCollection[T]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[FlatArrayView[T]]]
  def asFlatArrayBuff[T](x: Exp[DeliteCollection[T]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[FlatArrayBuff[T]]]
  def asFlatArrayBuffView[T](x: Exp[DeliteCollection[T]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[FlatArrayBuffView[T]]]

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@FlatArrayMkString(d,l,a) => reflectPure(new {override val original = Some(f,e)} with FlatArrayMkString(f(d),l,a)(e.mA,ctx))(mtype(manifest[A]),ctx)
    case e@FlatMatrixMkString(r,c,d,a) => reflectPure(new {override val original = Some(f,e)} with FlatMatrixMkString(f(r),f(c),d,a)(e.mA,ctx))(mtype(manifest[A]),ctx)
    case Reflect(e@FlatArrayMkString(d,l,a), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e)} with FlatArrayMkString(f(d),l,a)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case Reflect(e@FlatMatrixMkString(r,c,d,a), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e)} with FlatMatrixMkString(f(r),f(c),d,a)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]


  trait FlatArrayImplementer extends MultiArrayImplementer {  
    val IR: self.type

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
    override def getDims[T:Manifest](ma: Exp[DeliteMultiArray[T]]): Seq[Exp[Int]] = layout(ma) match {
      case FlatLayout(1, Plain) => Seq(darray_length(ma.asDeliteArray))
      case FlatLayout(1, Buffer) => Seq(darray_buffer_length(ma.asDeliteArrayBuffer))
      case _ => super.getDims(ma)
    }

    override def implementDim[T:Manifest](ma: Exp[DeliteMultiArray[T]], i: Int)(implicit ctx: SourceContext): Exp[Int] = layout(ma) match {
      case FlatLayout(1, Plain) => if (i == 0) darray_length(ma.asDeliteArray) else { warn("Geting dim " + i + " of 1D array"); Const(1) }
      case FlatLayout(1, Buffer) => if (i == 0) darray_buffer_length(ma.asDeliteArrayBuffer) else { warn("Getting dim " + i + " of 1D array"); Const(1) } 
      case _ => super.implementDim(ma,i)
    }

    override def implementSize[T:Manifest](ma: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Exp[Int] = layout(ma) match {
      case FlatLayout(1, Plain) => darray_length(ma.asDeliteArray)
      case FlatLayout(1, Buffer) => darray_buffer_length(ma.asDeliteArrayBuffer)
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
        flatarray_new[A](dims(0), mdat(out))
      case FlatLayout(n, Plain) => 
        flatarray_plain_new[A](flatarray_new[A](productTree(dims), mdat(out)), dims)
      case FlatLayout(n, View) =>
        flatarray_view_new[A](flatarray_new[A](productTree(dims), mdat(out)), dims)
      case FlatLayout(1, Buffer) =>
        flatbuffer_new[A](dims(0), mdat(out))
      case FlatLayout(n, Buffer) => 
        flatarray_buff_new[A](flatbuffer_new[A](productTree(dims), mdat(out)), dims)
      case FlatLayout(n, BufferView) =>
        flatarray_buffview_new[A](flatbuffer_new[A](productTree(dims), mdat(out)), dims)
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
      if (m == n)     (newOfs, stride.zip(prevStride).map{s => delite_int_times(s._1, s._2)})
      else if (m < n) (newOfs, stride.take(n - m) ++ stride.drop(n - m).zip(prevStride).map{s => delite_int_times(s._1, s._2)})                 // reshape view?
      else            (newOfs, prevStride.zipWithIndex.filterNot{unitDims contains _._2}.map{_._1}.zip(stride).map{s => delite_int_times(s._1, s._2)})    // sub-dim slice

      //(newOfs, newStride)
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
        val (newOfs, newStride) = createViewParams(m, unit(0), dimsToStride(impl.dims), n, start, stride, unitDims)
        flatarray_view_new[A](impl.data, dims, newOfs, newStride)
      case (FlatLayout(m, Buffer), FlatLayout(n, BufferView)) => 
        val impl = ma.asFlatArrayBuff
        val (newOfs, newStride) = createViewParams(m, unit(0), dimsToStride(impl.dims), n, start, stride, unitDims)
        flatarray_buffview_new[A](impl.data, dims, newOfs, newStride)
      case (FlatLayout(m, View), FlatLayout(n, View)) => 
        val impl = ma.asFlatArrayView
        val (newOfs, newStride) = createViewParams(m, impl.start, impl.stride, n, start, stride, unitDims)
        flatarray_view_new[A](impl.data, dims, newOfs, newStride)
      case (FlatLayout(m, BufferView), FlatLayout(n, BufferView)) => 
        val impl = ma.asFlatArrayBuffView
        val (newOfs, newStride) = createViewParams(m, impl.start, impl.stride, n, start, stride, unitDims)
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
        val index = i.calcViewIndex(n, impl.start, impl.stride)
        flatarray_apply(impl.data, index)
      case FlatLayout(n, Buffer) => 
        val impl = ma.asFlatArrayBuff
        val index = i.calcIndex(n, impl.dims)
        flatbuffer_apply(impl.data, index)
      case FlatLayout(n, BufferView) => 
        val impl = ma.asFlatArrayBuffView
        val index = i.calcViewIndex(n, impl.start, impl.stride)
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
      case _ => super.implementUpdate(ma,i,x)
    }

    // --- Array permute / reshaping
    // TODO: Should permute give a view? Should view-permute be a separate node?
    // TODO: Should reshape give a view? Should view-reshape be a separate node?

    /**
     * Creates a copy node of a flat MD array with reordered dimensions
     * @param ma     - input MD array
     * @param config - ordering of dimensions (e.g. for matrix transpose config = (1, 0) )
     * @param out    - symbol properties of output MD array
     */
    override def implementPermute[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = (layout(ma), layout(out)) match {
      case (FlatLayout(n, _), FlatLayout(_,_)) => 
        val size = implementSize(ma)
        val dims = getDims(ma)
        val v = fresh[Int]
        val inds = calcIndices(v, dims)
        val i = loopindices_new(v, inds)
        val permuteInds = Seq.tabulate(n){d => i(config(d))}
        val body: Block[A] = withInnerScopeAdditions{ reifyEffects(implementApply(ma, indices_new(permuteInds))) }
        implementCollect(v, i, layout(out), mdat(out), body, dims, size)

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
        val data = implementCopy(ma, FlatLayout(1, Plain), mdat(out)).asInstanceOf[Exp[DeliteArray[A]]]

        layout(out) match {
          case FlatLayout(1, Plain) => data
          case FlatLayout(n, Plain) => flatarray_plain_new(data, dims)
          case FlatLayout(n, View) => flatarray_view_new(data, dims)
          case FlatLayout(1, Buffer) => flatbuffer_wrap(data, implementSize(ma))
          case FlatLayout(n, Buffer) => flatarray_buff_new(flatbuffer_wrap(data, implementSize(ma)), dims)
          case FlatLayout(n, BufferView) => flatarray_buffview_new(flatbuffer_wrap(data, implementSize(ma)), dims)
        }

      case _ => super.implementReshape(ma,dims,out)
    }

    // --- Parallel ops
    /**
     * Creates a parallel collect node creating an MD array from an abstract parallel node
     * @param v    - original iterator
     * @param i    - original loop index
     * @param out  - layout of output MD array
     * @param dat  - symbol properties of output multiarray's child
     * @param body - loop body
     * @param dims - dimensions of output MD array
     * @param size - total size of loop
     * @param cond - optional conditional block of collect (default is None)
     */
    private def implementCollect[A:Manifest](v: Sym[Int], i: Exp[LoopIndices], out: Layout[_,_], dat: SymbolProperties, body: Block[A], dims: Seq[Exp[Int]], size: Exp[Int], cond: Option[Block[Boolean]] = None)(implicit ctx: SourceContext): Exp[Any] = out match {
      case FlatLayout(_,_) => 
        val inds = calcIndices(v, dims)
        val i2 = loopindices_new(v, inds)

        val (mirroredBody, mirroredCond) = withSubstScope(i -> i2){ (f(body), cond.map{f(_)}) }
        val data = reflectPure(CollectFactory.array[A](v, size, mirroredBody, mirroredCond))
        setMetadata(data, FlatLayout(1, Plain))
        setChild(data, Some(dat))

        out match {
          case FlatLayout(1, Plain) => data
          case FlatLayout(n, Plain) => flatarray_plain_new(data, dims)
          case FlatLayout(n, View) => flatarray_view_new(data, dims)
          case FlatLayout(1, Buffer) => flatbuffer_wrap(data, size)
          case FlatLayout(n, Buffer) => flatarray_buff_new(flatbuffer_wrap(data, size), dims)
          case FlatLayout(n, BufferView) => flatarray_buffview_new(flatbuffer_wrap(data, size), dims)
        }
    }

    /**
     * Special case of collect - creates a copy node of an MD array
     * @param ma  - input MD array
     * @param out - layout of output array
     * @param dat - child of output array
     */
    private def implementCopy[A:Manifest](ma: Exp[DeliteMultiArray[A]], out: Layout[_,_], dat: SymbolProperties) = {
      val size = implementSize(ma)
      val dims = getDims(ma)
      val v = fresh[Int]
      val i = loopindices_new(v, Seq(v))
      val body: Block[A] = withInnerScopeAdditions{ reifyEffects(implementApply(ma, i)) }
      implementCollect(v, i, out, dat, body, dims, size)
    }

    override def implementFileRead[A:Manifest](path: Exp[String], dels: Seq[Exp[String]], body: Block[A], rV: Sym[String], v: Sym[Int], i: Exp[LoopIndices], rV: Exp[String], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
      case FlatLayout(_,_) =>
        var partialResults: Seq[Exp[DeliteArray[String]]] = Nil
        
        partialResults = partialResults :+ reflectPure(SimpleReadFactory.array(v, Seq(path)))
        setMetadata(curr, FlatLayout(1, Plain))

        var curr = orig
        var dims: Seq[Exp[Int]] = Seq(orig.length)
        
        for (d <- 1 until dels.length) {
          val oV = fresh[Int].asInstanceOf[Sym[Int]]
          val oI = loopindices_new(oV, Seq(oV))
          val body = implementStringSplit(implementApply(curr.asInstanceOf[Exp[DeliteMultiArray[String]], oI), dels(d), unit(-1), FlatLayout(1, Plain))

          val op = FlatMapFactory.array[String](oV, dims(d - 1), body)
          setProps(op.eF, props(getBlockResult(body)))

          val curr = reflectPure(op)
          setMetadata(curr, FlatLayout(1, Plain))

        }

        layout(out) match {

        }


      case _ => super.implementFileRead(path,dels,body,v,i,out)
    }
    override def implementFromFunction[A:Manifest](dims: Seq[Exp[Int]], body: Block[A], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
      case FlatLayout(_,_) => implementCollect[A](v, i, layout(out), mdat(out), body, dims, productTree(dims))
      case _ => super.implementFromFunction(dims,body,v,i,out)
    }
    override def implementMap[A:Manifest,R:Manifest](in: Exp[DeliteMultiArray[A]], body: Block[R], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
      case FlatLayout(_,_) => implementCollect[R](v, i, layout(out), mdat(out), body, getDims(in), implementSize(in))
      case _ => super.implementMap(in,body,v,i,out)
    }
    override def implementZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DeliteMultiArray[A]], inB: Exp[DeliteMultiArray[B]], body: Block[R], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
      case FlatLayout(_,_) => implementCollect[R](v, i, layout(out), mdat(out), body, getDims(inA), implementSize(inA))
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
    override def implementMapFilter[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], mapFunc: Block[B], filtFunc: Block[Boolean], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = layout(out) match {
      case FlatLayout(_,_) => implementCollect[B](v, i, layout(out), mdat(out), mapFunc, getDims(in), implementSize(in), Some(filtFunc))
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

        val data = reflectPure(op)
        setMetadata(data, FlatLayout(1, Plain))
        setChild(data, Some(mdat(out)))

        layout(out) match {
          case FlatLayout(_, Plain) => data
          case FlatLayout(_, View) => flatarray_view_new(data, Seq(size))
          case FlatLayout(_, Buffer) => flatbuffer_wrap(data, size)
          case FlatLayout(_, BufferView) => flatarray_buffview_new(flatbuffer_wrap(data,size), Seq(size))
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

    private def flatarray_mkstring[A:Manifest](arr: Exp[DeliteArray1D[A]], del: Exp[String]): Exp[String] 
      = FlatArrayMkString(del, () => implementSize(arr), {i: Exp[Int] => implementApply(arr, indices_new(Seq(i)))})
    private def flatmatrix_mkstring[A:Manifest](mat: Exp[DeliteArray2D[A]], rdel: Exp[String], cdel: Exp[String]): Exp[String]
      = FlatMatrixMkString(rdel, cdel, {i: Int => implementDim(mat, i)}, {i: Seq[Exp[Int]] => implementApply(mat, indices_new(i))})

    // --- Misc.
    // TODO: Should darray_mkstring just be replaced by flat_array_mkstring too?
    override def implementMkString[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]]): Exp[String] = layout(ma) match {
      case FlatLayout(1, Plain) => darray_mkstring(ma.asDeliteArray, dels(0))
      case FlatLayout(1, _) => flatarray_mkstring(ma.asInstanceOf[Exp[DeliteArray1D[A]]], dels(0))
      case FlatLayout(2, _) => flatmatrix_mkstring(ma.asInstanceOf[Exp[DeliteArray2D[A]]], dels(0), dels(1))
      case _ => super.implementMkString(ma,dels)
    }

    override def implementFileWrite[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]], body: Block[String], v: Sym[Int], i: Exp[LoopIndices]): Exp[Unit] = layout(ma) match {
      case FlatLayout(_,_) =>

      case _ => super.implementFileWrite(ma,dels,body,v,i)
    }

    // --- 1D ops
    override def implementSortIndices(len: Exp[Int], i: (Sym[Int], Sym[Int]), body: Block[Int], out: SymbolProperties)(implicit ctx: SourceContext) = layout(out) match {
      case FlatLayout(1, _) => 
        val arr = reflectPure(DeliteArraySortIndices(len, i, body))
        setMetadata(arr, FlatLayout(1, Plain))
        setChild(arr, getChild(out))
        (arr)
      case _ => super.implementSortIndices(len,i,body,out)
    }
    override def implementStringSplit(str: Exp[String], split: Exp[String], lim: Exp[Int], out: SymbolProperties)(implicit ctx: SourceContext) = layout(out) match { 
      case FlatLayout(1, Plain) => 
        val arr = darray_split_string(str, split, lim)
        setMetadata(arr, FlatLayout(1, Plain))
        setChild(arr, getChild(out))
        (arr)

      case _ => super.implementStringSplit(str,split,lim,out)
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
      case (FlatLayout(1, Plain), FlatLayout(1, Buffer)) => flatbuffer_wrap(ma.asDeliteArray, implementSize(ma))
      case (FlatLayout(n, Plain), FlatLayout(_, Buffer)) => flatbuffer_wrap(ma.asFlatArrayPlain.data, implementSize(ma))
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
