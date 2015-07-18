package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.util.OverloadHack

import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.ops._
import ppl.delite.framework.analysis.LayoutMetadataOps
import ppl.delite.framework.Util._

// This is at a weird layer of abstraction between the MultiArray user level stuff and the inner flat array implementations using Records
// Records also being DeliteCollections only seems to work using transformer magic
trait Array1DView[T] extends DeliteCollection[T]
trait Array2DView[T] extends DeliteCollection[T]
trait Array2D[T] extends DeliteCollection[T]

trait FlattenedArrayOps extends DeliteSimpleOps with DeliteArrayOps with RangeVectorOps with OverloadHack { this: PPLApp => 
  type Array1D[T] = DeliteArray[T]

  def array2d_new[T:Manifest](data: Rep[DeliteArray[T]], dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[Array2D[T]]
  def array2dview_new[T:Manifest](data: Rep[DeliteArray[T]], dims: Seq[Rep[Int]], ofs: Rep[Int], strides: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[Array2DView[T]]
  def array1dview_new[T:Manifest](data: Rep[DeliteArray[T]], dims: Seq[Rep[Int]], ofs: Rep[Int], strides: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[Array1DView[T]]

  def array1d_mkstring[T:Manifest](ma: Rep[Array1D[T]], del: Rep[String])(implicit ctx: SourceContext): Rep[String]
  def array1dview_mkstring[T:Manifest](ma: Rep[Array1DView[T]], del: Rep[String])(implicit ctx: SourceContext): Rep[String]
  def array2d_mkstring[T:Manifest](ma: Rep[Array2D[T]], rdel: Rep[String], cdel: Rep[String])(implicit ctx: SourceContext): Rep[String]
  def array2dview_mkstring[T:Manifest](ma: Rep[Array2DView[T]], rdel: Rep[String], cdel: Rep[String])(implicit ctx: SourceContext): Rep[String]
  
  def annotateViewified[T:Manifest](x: Rep[T]): Rep[T]
  def isTrueView(e: Rep[Any]): Boolean

  // --- Syntactic sugar for apps
  def *(): Rep[RangeWildcard]

  def collect[T:Manifest](d0: Rep[Int], d1: Rep[Int])(func: (Rep[Int],Rep[Int]) => Rep[T])(implicit ctx: SourceContext): Rep[Array2D[T]] = {
    val data = collect(d0*d1){v => func(v / d1, v % d1) }
    array2d_new(data, Seq(d0, d1))
  }

  def forIndices(d0: Rep[Int],d1: Rep[Int])(func: (Rep[Int],Rep[Int]) => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit] = {
    forIndices(d0*d1){v => func(v / d1, v % d1) }
  }

  // --- MultiArray constructors
  object Array2D { 
    def apply[T:Manifest](data: Rep[DeliteArray[T]], nRows: Rep[Int], nCols: Rep[Int])(implicit ctx: SourceContext): Rep[Array2D[T]] 
      = array2d_new(data, Seq(nRows,nCols)) 
  }

  // --- RangeVector constructors
  // Syntax right now is, e.g., x.slice(start :@: len)
  // TODO: This syntax is strange..
  implicit def RepIntToRepIntOpsCls(x: Rep[Int])(implicit ctx: SourceContext) = new RepIntOpsCls(x)
  class RepIntOpsCls(x: Rep[Int])(implicit ctx: SourceContext) {
    def :@:(start: Rep[Int]): Rep[RangeVector] = RangeVector(start, x)
  }
  implicit def IntToIntOpsCls(x: Int)(implicit ctx: SourceContext) = new IntOpsCls(x)
  class IntOpsCls(x: Int)(implicit ctx: SourceContext) {
    def :@:(start: Rep[Int]): Rep[RangeVector] = RangeVector(start, unit(x))
  }

  // --- 1D Ops

  implicit def array1DViewtoArray1DViewOpsCls[T:Manifest](x: Rep[Array1DView[T]])(implicit ctx: SourceContext) = new Array1DViewOpsCls(x)
  class Array1DViewOpsCls[T:Manifest](x: Rep[Array1DView[T]])(implicit ctx: SourceContext) {
    // --- Extractors
    def start: Rep[Int] = if (isTrueView(x)) field[Int](x, "ofs") else unit(0)
    def data: Rep[DeliteArray[T]] = field[DeliteArray[T]](x, "data")

    def length: Rep[Int] = field[Int](x, "dim0")
    def stride: Rep[Int] = if (isTrueView(x)) field[Int](x, "stride0") else unit(1)

    // --- Single element operations    
    def apply(i:Rep[Int]): Rep[T] = x.data.apply(i*x.stride + x.start)
    // Generally unsafe to do updates on views, but in certain cases it's ok
    def update(i: Rep[Int], z: Rep[T]): Rep[Unit] = x.data.update(x.stride*i + x.start, z)

    // --- 1D slices
    def slice(iv: Rep[RangeVector]): Rep[Array1DView[T]] 
      = array1dview_new(x.data, Seq(iv.length(x.length)), iv.start + x.start, Seq(iv.stride * x.stride))

    // --- Annotations
    def notePhysViewOnly: Rep[Array1DView[T]] = annotateViewified(x)

    def mkString(del: Rep[String]): Rep[String] = array1dview_mkstring(x, del)
    def pprint: Rep[Unit] = println(x.mkString(" "))
  }
  
  implicit def array1DtoArray1DOpsCls[T:Manifest](x: Rep[Array1D[T]])(implicit ctx: SourceContext) = new Array1DOpsCls(x)
  class Array1DOpsCls[T:Manifest](x: Rep[Array1D[T]])(implicit ctx: SourceContext) {
    def asView: Rep[Array1DView[T]] 
      = array1dview_new(x, Seq(x.length), 0, Seq(unit(1))).notePhysViewOnly

    def slice(iv: Rep[RangeVector]): Rep[Array1DView[T]] 
      = array1dview_new(x, Seq(iv.length(x.length)), iv.start, Seq(iv.stride))

    def mkString(del: Rep[String]): Rep[String] = array1d_mkstring(x, del)
    def pprint: Rep[Unit] = println(x.mkString(" "))
  }

  // --- 2D Ops
  implicit def array2DViewtoArray2DViewOpsCls[T:Manifest](x: Rep[Array2DView[T]])(implicit ctx: SourceContext) = new Array2DViewOpsCls(x)
  class Array2DViewOpsCls[T:Manifest](x: Rep[Array2DView[T]])(implicit ctx: SourceContext) {
    // --- Extractors
    def start: Rep[Int] = if (isTrueView(x)) field[Int](x, "ofs") else unit(0)
    def data: Rep[DeliteArray[T]] = field[DeliteArray[T]](x, "data")

    def nRows: Rep[Int] = field[Int](x, "dim0")
    def nCols: Rep[Int] = field[Int](x, "dim1")
    def rowStride: Rep[Int] = if (isTrueView(x)) field[Int](x, "stride0") else nCols
    def colStride: Rep[Int] = if (isTrueView(x)) field[Int](x, "stride1") else unit(1)

    // --- Single element operations
    def apply(i:Rep[Int], j: Rep[Int]): Rep[T] = x.data.apply(x.rowStride*i + x.colStride*j + x.start)
    // Generally unsafe to do updates on views, but in certain cases it's ok
    def update(i: Rep[Int], j: Rep[Int], y: Rep[T]): Rep[Unit] = x.data.update(x.rowStride*i + x.colStride*j + x.start, y) 

    // --- Slicing
    // 1D column slice
    // scalac fails here if o is of type Overloaded1... why...?
    def slice(iv0: Rep[RangeVector], j: Rep[Int])(implicit o: Overloaded4): Rep[Array1DView[T]] = {
      val len = iv0.length(x.nRows)
      val ofs = j + ( iv0.start * x.nCols )
      val stride = x.rowStride * iv0.stride
      array1dview_new(x.data, Seq(len), ofs, Seq(stride))
    }
    // 1D row slice
    def slice(i: Rep[Int], iv1: Rep[RangeVector])(implicit o: Overloaded2): Rep[Array1DView[T]]
      = array1dview_new(x.data, Seq(iv1.length(x.nCols)), i*x.rowStride + iv1.start*x.colStride, Seq(x.colStride * iv1.stride))
    // 2D slice
    def slice(iv0: Rep[RangeVector], iv1: Rep[RangeVector])(implicit o: Overloaded3): Rep[Array2DView[T]] 
      = array2dview_new(x.data, Seq(iv0.length, iv1.length), iv0.start*x.nCols + iv1.start + x.start, Seq(iv0.stride * x.rowStride, iv1.stride * x.colStride))
  
    // --- Annotations
    def notePhysViewOnly: Rep[Array2DView[T]] = annotateViewified(x)

    def mkString(rdel: Rep[String], cdel: Rep[String]): Rep[String] = array2dview_mkstring(x, rdel, cdel)
    def pprint: Rep[Unit] = println(x.mkString("\n", " "))
  }

  implicit def array2DtoArray2DOpsCls[T:Manifest](x: Rep[Array2D[T]])(implicit ctx: SourceContext) = new Array2DOpsCls(x)
  class Array2DOpsCls[T:Manifest](x: Rep[Array2D[T]])(implicit ctx: SourceContext) {
    // --- Extractors
    def nRows: Rep[Int] = field[Int](x, "dim0")
    def nCols: Rep[Int] = field[Int](x, "dim1")
    def data: Rep[DeliteArray[T]] = field[DeliteArray[T]](x, "data")

    // --- Single element operations
    def apply(i:Rep[Int], j: Rep[Int]): Rep[T] = x.data.apply(x.nCols*i + j)
    def update(i: Rep[Int], j: Rep[Int], y: Rep[T]): Rep[Unit] = x.data.update(x.nCols*i + j, y) 

    // --- Conversion to view
    def asView: Rep[Array2DView[T]] 
      = array2dview_new(x.data, Seq(x.nRows,x.nCols), 0, Seq(x.nCols,unit(1))).notePhysViewOnly

    // --- Slicing
    // 1D column slice
    // scalac fails here if o is of type Overloaded1... why...?
    def slice(iv0: Rep[RangeVector], j: Rep[Int])(implicit o: Overloaded4): Rep[Array1DView[T]] = {
      val len = iv0.length(x.nRows)
      val ofs = j + ( iv0.start * x.nCols )
      val stride = x.nCols * iv0.stride
      array1dview_new(x.data, Seq(len), ofs, Seq(stride))
    }
    // 1D row slice
    def slice(i: Rep[Int], iv1: Rep[RangeVector])(implicit o: Overloaded2): Rep[Array1DView[T]]
      = array1dview_new(x.data, Seq(iv1.length(nCols)), i*x.nCols + iv1.start, Seq(iv1.stride))
    // 2D slice
    def slice(iv0: Rep[RangeVector], iv1: Rep[RangeVector])(implicit o: Overloaded3): Rep[Array2DView[T]] 
      = array2dview_new(x.data, Seq(iv0.length(x.nRows), iv1.length(x.nCols)), iv0.start*x.nCols + iv1.start, Seq(iv0.stride * x.nCols, iv1.stride))
  
    def mkString(rdel: Rep[String], cdel: Rep[String]): Rep[String] = array2d_mkstring(x, rdel, cdel)
    def pprint: Rep[Unit] = println(x.mkString("\n", " "))
  }

  // --- File reading
  // (File reading is pretty annoying to write out directly in PPL)
  //def read1D(path: Rep[String]): Rep[Array1D[Double]] = read(path).map{s => s.toDouble}
  def read2D(path: Rep[String])(implicit ctx: SourceContext): Rep[Array2D[Double]] = {
    val vec = read(path).map{s => darray_split_string(s.trim, unit("\\s+"), unit(-1)).map{s => s.toDouble} }
    collect(vec.length, vec(unit(0)).length){(i,j) => vec(i).apply(j)}
  }
}

trait FlattenedArrayOpsExp extends FlattenedArrayOps with MultiArrayExp with DeliteStructsExp { this: PPLCompiler => 
  def *(): Rep[RangeWildcard] = fresh[RangeWildcard]
  def annotateViewified[T:Manifest](x: Rep[T]): Rep[T] = x.withData(MView(PhysType))

  // Need to assume views are "true" views in cases where no phys information is available
  override def isTrueView(p: SymbolProperties) = getView(p).map{_.isTrueView}.getOrElse(true)
  override def isTrueView(e: Exp[Any]) = getView(e).map{_.isTrueView}.getOrElse(true)

  case class Array2DNew[A:Manifest](data: Rep[DeliteArray[A]], dim0: Rep[Int], dim1: Rep[Int])(implicit ctx: SourceContext) extends DeliteStruct[Array2D[A]] {
    val elems = copyTransformedElems(List("data" -> data, "dim0" -> dim0, "dim1" -> dim1))
    val mA = manifest[A]
  }
  case class Array2DViewNew[A:Manifest](data: Rep[DeliteArray[A]], dim0: Rep[Int], dim1: Rep[Int], ofs: Rep[Int], stride0: Rep[Int], stride1: Rep[Int])(implicit ctx: SourceContext) extends DeliteStruct[Array2DView[A]] {
    val elems = copyTransformedElems(List("data" -> data, "dim0" -> dim0, "dim1" -> dim1, "ofs" -> ofs, "stride0" -> stride0, "stride1" -> stride1))
    val mA = manifest[A]
  }
  case class Array1DViewNew[A:Manifest](data: Rep[DeliteArray[A]], dim0: Rep[Int], ofs: Rep[Int], stride0: Rep[Int])(implicit ctx: SourceContext) extends DeliteStruct[Array1DView[A]] {
    val elems = copyTransformedElems(List("data" -> data, "dim0" -> dim0, "ofs" -> ofs, "stride0" -> stride0))
    val mA = manifest[A]
  }

  def array2d_new[T:Manifest](data: Rep[DeliteArray[T]], dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[Array2D[T]]
    = reflectPure(Array2DNew(data, dims(0),dims(1))).withData(FlatLayout(2, Plain)).withField(getProps(data), "data")
  def array2dview_new[T:Manifest](data: Rep[DeliteArray[T]], dims: Seq[Rep[Int]], ofs: Rep[Int], strides: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[Array2DView[T]]
    = reflectPure(Array2DViewNew(data, dims(0), dims(1), ofs, strides(0), strides(1))).withData(FlatLayout(2, View)).withField(getProps(data), "data")
  def array1dview_new[T:Manifest](data: Rep[DeliteArray[T]], dims: Seq[Rep[Int]], ofs: Rep[Int], strides: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[Array1DView[T]]
    = reflectPure(Array1DViewNew(data, dims(0), ofs, strides(0))).withData(FlatLayout(1, View)).withField(getProps(data), "data")

  def array2d_new_mutable[T:Manifest](data: Rep[DeliteArray[T]], dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[Array2D[T]]
    = reflectMutable(Array2DNew(data, dims(0),dims(1))).withData(FlatLayout(2, Plain)).withField(getProps(data), "data")
  def array2dview_new_mutable[T:Manifest](data: Rep[DeliteArray[T]], dims: Seq[Rep[Int]], ofs: Rep[Int], strides: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[Array2DView[T]]
    = reflectMutable(Array2DViewNew(data, dims(0), dims(1), ofs, strides(0), strides(1))).withData(FlatLayout(2, View)).withField(getProps(data), "data")
  def array1dview_new_mutable[T:Manifest](data: Rep[DeliteArray[T]], dims: Seq[Rep[Int]], ofs: Rep[Int], strides: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[Array1DView[T]]
    = reflectMutable(Array1DViewNew(data, dims(0), ofs, strides(0))).withData(FlatLayout(1, View)).withField(getProps(data), "data")

  // Bit of a hack here - the type of the multiarray actually doesn't matter here since it's never accessed in the
  // MkString body
  def array1d_mkstring[T:Manifest](ma: Rep[Array1D[T]], del: Rep[String])(implicit ctx: SourceContext) = {
    reflectPure(ArrayMkString(ma.asInstanceOf[Exp[DeliteMultiArray[T]]], del, () => ma.length, {i => ma(i).ToString}))
  }
  def array1dview_mkstring[T:Manifest](ma: Rep[Array1DView[T]], del: Rep[String])(implicit ctx: SourceContext) = {
    reflectPure(ArrayMkString(ma.asInstanceOf[Exp[DeliteMultiArray[T]]], del, () => ma.length, {i => ma(i).ToString}))
  }
  def array2d_mkstring[T:Manifest](ma: Rep[Array2D[T]], rdel: Rep[String], cdel: Rep[String])(implicit ctx: SourceContext) = {
    reflectPure(MatrixMkString(ma.asInstanceOf[Exp[DeliteMultiArray[T]]], rdel, cdel, {i => if (i == 0) ma.nRows else ma.nCols},{i => ma(i(0),i(1)).ToString}))
  }
  def array2dview_mkstring[T:Manifest](ma: Rep[Array2DView[T]], rdel: Rep[String], cdel: Rep[String])(implicit ctx: SourceContext) = {
    reflectPure(MatrixMkString(ma.asInstanceOf[Exp[DeliteMultiArray[T]]], rdel, cdel, {i => if (i == 0) ma.nRows else ma.nCols},{i => ma(i(0),i(1)).ToString}))
  }


  private def dataField[T](tp: Manifest[T]): List[(String, Manifest[_])] = List("data" -> darrayManifest(tp))
  private def dimFields(n: Int): List[(String, Manifest[_])] = List.tabulate(n){d => s"dim$d" -> manifest[Int]}
  private def viewFields(n: Int): List[(String, Manifest[_])] = dimFields(n) ++ List("ofs" -> manifest[Int]) ++ 
                                                                List.tabulate(n){d => s"stride$d" -> manifest[Int]}
  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = manifest[T] match {
    case t if t.erasure == classOf[Array2D[_]] => Some((classTag(t), dataField(t.typeArguments(0)) ++ dimFields(2)))
    case t if t.erasure == classOf[Array2DView[_]] => Some((classTag(t), dataField(t.typeArguments(0)) ++ viewFields(2)))
    case t if t.erasure == classOf[Array1DView[_]] => Some((classTag(t), dataField(t.typeArguments(0)) ++ viewFields(1)))
    case _ => super.unapplyStructType
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@Array2DNew(d,d0,d1) => reflectPure(new {override val original = Some(f,e) } with Array2DNew(f(d),f(d0),f(d1))(e.mA,ctx))(mtype(manifest[A]),ctx)
    case e@Array2DViewNew(d,d0,d1,o,s0,s1) => reflectPure(new {override val original = Some(f,e) } with Array2DViewNew(f(d),f(d0),f(d1),f(o),f(s0),f(s1))(e.mA,ctx))(mtype(manifest[A]),ctx)
    case e@Array1DViewNew(d,d0,o,s0) => reflectPure(new {override val original = Some(f,e) } with Array1DViewNew(f(d),f(d0),f(o),f(s0))(e.mA,ctx))(mtype(manifest[A]),ctx)
    case Reflect(e@Array2DNew(d,d0,d1), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with Array2DNew(f(d),f(d0),f(d1))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case Reflect(e@Array2DViewNew(d,d0,d1,o,s0,s1), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with Array2DViewNew(f(d),f(d0),f(d1),f(o),f(s0),f(s1))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case Reflect(e@Array1DViewNew(d,d0,o,s0), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e)} with Array1DViewNew(f(d),f(d0),f(o),f(s0))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  def isArray1D[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isDeliteArray(x)  
  def isArray2D[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[Array2D[A]])
  def isArray2DView[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[Array2DView[A]])
  def isArray1DView[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[Array1DView[A]])

  private def filterUnitDims(ds: List[Exp[Int]], unitDims: List[Int]): List[Exp[Int]]
    = ds.zipWithIndex.filterNot{d => unitDims.contains(d._2)}.map(_._1)

  override def dc_alloc_block[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], ds: List[Exp[Int]], unitDims: List[Int])(implicit ctx: SourceContext): Exp[CA] = {
    if (isArray1D(x)) DeliteArray[A](productTree(ds)).asInstanceOf[Exp[CA]]
    else if (isArray2D(x)) array2d_new_mutable(DeliteArray[A](productTree(ds)), filterUnitDims(ds, unitDims)).asInstanceOf[Exp[CA]]
    else if (isArray2DView(x)) array2d_new_mutable(DeliteArray[A](productTree(ds)), filterUnitDims(ds, unitDims)).asView.asInstanceOf[Exp[CA]]
    else if (isArray1DView(x)) DeliteArray[A](productTree(ds)).asView.asInstanceOf[Exp[CA]]
    else super.dc_alloc_block[A,CA](x,ds,unitDims)
  }
  override def dc_block_apply[A:Manifest](x: Exp[DeliteCollection[A]], is: List[Exp[Int]], unitDims: List[Int])(implicit ctx: SourceContext): Exp[A] = {
    val inds = filterUnitDims(is, unitDims)
    if (isArray1D(x)) asDeliteArray(x).apply(inds(0))
    else if (isArray2D(x)) x.asInstanceOf[Exp[Array2D[A]]].apply(inds(0), inds(1))
    else if (isArray2DView(x)) x.asInstanceOf[Exp[Array2DView[A]]].apply(inds(0), inds(1))
    else if (isArray1DView(x)) x.asInstanceOf[Exp[Array1DView[A]]].apply(inds(0))
    else super.dc_block_apply[A](x,is,unitDims)
  }
  override def dc_block_update[A:Manifest](x: Exp[DeliteCollection[A]], is: List[Exp[Int]], y: Exp[A], unitDims: List[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    val inds = filterUnitDims(is, unitDims)
    if (isArray1D(x)) asDeliteArray(x).update(inds(0), y)
    else if (isArray2D(x)) x.asInstanceOf[Exp[Array2D[A]]].update(inds(0), inds(1), y)
    else if (isArray2DView(x)) x.asInstanceOf[Exp[Array2DView[A]]].update(inds(0), inds(1), y)
    else if (isArray1DView(x)) x.asInstanceOf[Exp[Array1DView[A]]].update(inds(0), y)
    else super.dc_block_update[A](x,is,y,unitDims)
  }

}
