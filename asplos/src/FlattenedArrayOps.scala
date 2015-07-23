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

trait FlattenedArrayOps extends DeliteSimpleOps with DeliteNestedOps with DeliteArrayOps with RangeVectorOps with OverloadHack { this: PPLApp => 
  type Array1D[T] = DeliteArray[T]

  // --- MultiArray constructors
  object Array2D {
    def apply[A:Manifest](nRows: Rep[Int], nCols: Rep[Int])(implicit ctx: SourceContext): Rep[Array2D[A]] 
      = array2d_new_mutable[A](DeliteArray[A](nRows*nCols), nRows, nCols)

    def apply[A:Manifest](data: Rep[DeliteArray[A]], nRows: Rep[Int], nCols: Rep[Int])(implicit ctx: SourceContext): Rep[Array2D[A]]
      = array2d_new[A](data, nRows, nCols)
  }
  object Array1D {
    def apply[A:Manifest](length: Rep[Int])(implicit ctx: SourceContext): Rep[Array1D[A]] = DeliteArray[A](length)
  }

  object Kernel1D {
    def apply[A:Manifest](ks: List[A])(implicit ctx: SourceContext): Rep[Array1D[A]]
      = kernel_array[A](unit(ks.length), ks.toList)
  }

  object Kernel2D {
    def apply[A:Manifest](ks: List[List[A]])(implicit ctx: SourceContext): Rep[Array2D[A]] = {
      val w = ks(0).length
      val h = ks.length
      val data = kernel_array[A](unit(h*w), ks.flatten)
      Array2D(data, h, w)
    }
  }

  // --- Ops
  def array2d_new[T:Manifest](data: Rep[DeliteArray[T]], dim0: Rep[Int], dim1: Rep[Int])(implicit ctx: SourceContext): Rep[Array2D[T]]
  def array2dview_new[T:Manifest](data: Rep[DeliteArray[T]], ofs: Rep[Int], stride0: Rep[Int], stride1: Rep[Int], dim0: Rep[Int], dim1: Rep[Int])(implicit ctx: SourceContext): Rep[Array2DView[T]]
  def array1dview_new[T:Manifest](data: Rep[DeliteArray[T]], ofs: Rep[Int], stride0: Rep[Int], dim0: Rep[Int])(implicit ctx: SourceContext): Rep[Array1DView[T]]
  
  def array2d_new_mutable[T:Manifest](data: Rep[DeliteArray[T]], dim0: Rep[Int], dim1: Rep[Int])(implicit ctx: SourceContext): Rep[Array2D[T]]
  def array2dview_new_mutable[T:Manifest](data: Rep[DeliteArray[T]], ofs: Rep[Int], stride0: Rep[Int], stride1: Rep[Int], dim0: Rep[Int], dim1: Rep[Int])(implicit ctx: SourceContext): Rep[Array2DView[T]]
  def array1dview_new_mutable[T:Manifest](data: Rep[DeliteArray[T]], ofs: Rep[Int], stride0: Rep[Int], dim0: Rep[Int])(implicit ctx: SourceContext): Rep[Array1DView[T]]  

  def array1d_mkstring[T:Manifest](ma: Rep[Array1D[T]], del: Rep[String])(implicit ctx: SourceContext): Rep[String]
  def array1dview_mkstring[T:Manifest](ma: Rep[Array1DView[T]], del: Rep[String])(implicit ctx: SourceContext): Rep[String]
  def array2d_mkstring[T:Manifest](ma: Rep[Array2D[T]], rdel: Rep[String], cdel: Rep[String])(implicit ctx: SourceContext): Rep[String]
  def array2dview_mkstring[T:Manifest](ma: Rep[Array2DView[T]], rdel: Rep[String], cdel: Rep[String])(implicit ctx: SourceContext): Rep[String]
  
  def block_slice[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](src: Rep[C], srcOffsets: List[Rep[Int]], srcStrides: List[Rep[Int]], destDims: List[Rep[Int]], unitDims: List[Int])(implicit ctx: SourceContext): Rep[T]
  def array_slice[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](src: Rep[C], srcOffsets: List[Rep[Int]], srcStrides: List[Rep[Int]], destDims: List[Rep[Int]], unitDims: List[Int])(implicit ctx: SourceContext): Rep[T]  
  def array_apply[A:Manifest,C<:DeliteCollection[A]:Manifest](x: Rep[C], inds: List[Rep[Int]])(implicit ctx: SourceContext): Rep[A]

  def kernel_array[A:Manifest](len: Rep[Int], ks: List[A])(implicit ctx: SourceContext): Rep[Array1D[A]]

  // These are actually blocking hacks, put here for now to be lowered with the rest of the flat array ops
  def box[A:Manifest](x: Rep[A])(implicit ctx: SourceContext): Rep[Array1D[A]]
  def debox[A:Manifest](x: Rep[Array1D[A]])(implicit ctx: SourceContext): Rep[A]

  // --- Metadata
  def annotateReuse[T:Manifest](x: Rep[T], reuse: Rep[Int]*): Rep[T]
  def annotateViewified[T:Manifest](x: Rep[T]): Rep[T]
  def isTrueView(e: Rep[Any]): Boolean

  // --- Sugar for apps
  def *(): Rep[RangeWildcard]

  // Some fun sugar for kernels using the magic of scala
  class KernelList[A:Manifest](val x: List[List[A]]) { 
    def |(that: KernelList[A]): KernelList[A] = new KernelList(this.x ++ that.x)
    def !(): Rep[Array1D[A]] = Kernel1D(x(0))
    def |(): Rep[Array2D[A]] = Kernel2D(x)
  }
  def |[A:Manifest](x: A*): KernelList[A] = new KernelList( List(x.toList) )

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

  implicit def rangeVectorToRangeMathOpsCls(x: Rep[RangeVector])(implicit ctx: SourceContext) = new RangeVectorMathOpsCls(x)
  class RangeVectorMathOpsCls(x: Rep[RangeVector])(implicit ctx: SourceContext) {
    // NOTE: These aren't defined for wildcards
    def + (c: Rep[Int]): Rep[RangeVector] = RangeVector(x.start + c, x.stride, x.length)
    def * (c: Rep[Int]): Rep[RangeVector] = RangeVector(x.start * c, x.stride * c, x.length)
    def ++ (c: Rep[Int]): Rep[RangeVector] = RangeVector(x.start, x.stride, x.length + c)
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
    def apply(i:Rep[Int]): Rep[T] = array_apply[T,Array1DView[T]](x, List(i))
    // Generally unsafe to do updates on views, but in certain cases it's ok
    def update(i: Rep[Int], z: Rep[T]): Rep[Unit] = x.data.update(x.stride*i + x.start, z)

    // --- 1D slices
    def slice(iv: Rep[RangeVector]): Rep[Array1DView[T]] 
      = array_slice[T,Array1DView[T],Array1DView[T]](x,List(iv.start),List(iv.stride),List(iv.length(x.length)),Nil)

    def bslice(iv: Rep[RangeVector]): Rep[Array1D[T]] 
      = block_slice[T,Array1D[T],Array1DView[T]](x,List(iv.start),List(iv.stride),List(iv.length(x.length)),Nil)

    // --- Annotations
    def notePhysViewOnly: Rep[Array1DView[T]] = annotateViewified(x)

    // --- Printing
    def mkString(del: Rep[String]): Rep[String] = array1dview_mkstring(x, del)
    def pprint: Rep[Unit] = println(x.mkString(" "))
    def vprint: Rep[Unit] = println(x.mkString("\n"))
  }
  
  implicit def array1DtoArray1DOpsCls[T:Manifest](x: Rep[Array1D[T]])(implicit ctx: SourceContext) = new Array1DOpsCls(x)
  class Array1DOpsCls[T:Manifest](x: Rep[Array1D[T]])(implicit ctx: SourceContext) {
    
    def asView: Rep[Array1DView[T]] = array1dview_new(x, unit(0), unit(1), x.length).notePhysViewOnly

    // --- 1D slices
    def slice(iv: Rep[RangeVector]): Rep[Array1DView[T]] 
      = array_slice[T,Array1DView[T],Array1D[T]](x,List(iv.start),List(iv.stride),List(iv.length(x.length)),Nil)

    def bslice(iv: Rep[RangeVector]): Rep[Array1D[T]] 
      = block_slice[T,Array1D[T],Array1D[T]](x,List(iv.start),List(iv.stride),List(iv.length(x.length)),Nil)

    // --- Annotations
    // Specifically for arrays created from block slices
    def noteReuse(reuse: Rep[Int])(implicit ctx: SourceContext) = annotateReuse(x,reuse)

    // --- Printing
    def mkString(del: Rep[String]): Rep[String] = array1d_mkstring(x, del)
    def pprint: Rep[Unit] = println(x.mkString(" "))
    def vprint: Rep[Unit] = println(x.mkString("\n"))
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
    def apply(i:Rep[Int], j: Rep[Int]): Rep[T] = array_apply[T,Array2DView[T]](x, List(i,j))
    // Generally unsafe to do updates on views, but in certain cases it's ok
    def update(i: Rep[Int], j: Rep[Int], y: Rep[T]): Rep[Unit] = x.data.update(x.rowStride*i + x.colStride*j + x.start, y) 

    // --- Slicing
    def slice(iv0: Rep[RangeVector], j: Rep[Int])(implicit o: Overloaded4): Rep[Array1DView[T]]             // Column slice
      = array_slice[T,Array1DView[T],Array2DView[T]](x, List(iv0.start, j), List(iv0.stride, unit(1)), List(iv0.length(x.nRows), unit(1)), List(1))
    def slice(i: Rep[Int], iv1: Rep[RangeVector])(implicit o: Overloaded2): Rep[Array1DView[T]]             // Row slice
      = array_slice[T,Array1DView[T],Array2DView[T]](x, List(i, iv1.start), List(unit(1), iv1.stride), List(unit(1), iv1.length(x.nCols)), List(0))
    def slice(iv0: Rep[RangeVector], iv1: Rep[RangeVector])(implicit o: Overloaded3): Rep[Array2DView[T]]   // 2D slice
      = array_slice[T,Array2DView[T],Array2DView[T]](x, List(iv0.start, iv1.start), List(iv0.stride, iv1.stride), List(iv0.length(x.nRows), iv1.length(x.nCols)), Nil)
  
    def bslice(iv0: Rep[RangeVector], j: Rep[Int])(implicit o: Overloaded4): Rep[Array1D[T]]            // Column slice
      = block_slice[T,Array1D[T],Array2DView[T]](x, List(iv0.start, j), List(iv0.stride, unit(1)), List(iv0.length(x.nRows), unit(1)), List(1))
    def bslice(i: Rep[Int], iv1: Rep[RangeVector])(implicit o: Overloaded2): Rep[Array1D[T]]            // Row slice
      = block_slice[T,Array1D[T],Array2DView[T]](x, List(i, iv1.start), List(unit(1), iv1.stride), List(unit(1), iv1.length(x.nCols)), List(0))
    def bslice(iv0: Rep[RangeVector], iv1: Rep[RangeVector])(implicit o: Overloaded3): Rep[Array2D[T]]  // 2D Slice
      = block_slice[T,Array2D[T],Array2DView[T]](x, List(iv0.start, iv1.start), List(iv0.stride, iv1.stride), List(iv0.length(x.nRows), iv1.length(x.nCols)), Nil)

    // --- Annotations
    def notePhysViewOnly: Rep[Array2DView[T]] = annotateViewified(x)

    // --- Printing
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
    def apply(i:Rep[Int], j: Rep[Int]): Rep[T] = array_apply[T,Array2D[T]](x, List(i,j))
    def update(i: Rep[Int], j: Rep[Int], y: Rep[T]): Rep[Unit] = x.data.update(x.nCols*i + j, y) 

    // --- Conversion to view
    def asView: Rep[Array2DView[T]] = array2dview_new(x.data, unit(0), x.nCols, unit(1), x.nRows, x.nCols).notePhysViewOnly

    // --- Slicing
    def slice(iv0: Rep[RangeVector], j: Rep[Int])(implicit o: Overloaded4): Rep[Array1DView[T]]             // Column slice
      = array_slice[T,Array1DView[T],Array2D[T]](x, List(iv0.start, j), List(iv0.stride, unit(1)), List(iv0.length(x.nRows), unit(1)), List(1))
    def slice(i: Rep[Int], iv1: Rep[RangeVector])(implicit o: Overloaded2): Rep[Array1DView[T]]             // Row slice
      = array_slice[T,Array1DView[T],Array2D[T]](x, List(i, iv1.start), List(unit(1), iv1.stride), List(unit(1), iv1.length(x.nCols)), List(0))
    def slice(iv0: Rep[RangeVector], iv1: Rep[RangeVector])(implicit o: Overloaded3): Rep[Array2DView[T]]   // 2D slice
      = array_slice[T,Array2DView[T],Array2D[T]](x, List(iv0.start, iv1.start), List(iv0.stride, iv1.stride), List(iv0.length(x.nRows), iv1.length(x.nCols)), Nil)  

    def bslice(iv0: Rep[RangeVector], j: Rep[Int])(implicit o: Overloaded4): Rep[Array1D[T]]            // Column slice
      = block_slice[T,Array1D[T],Array2D[T]](x, List(iv0.start, j), List(iv0.stride, unit(1)), List(iv0.length(x.nRows), unit(1)), List(1))
    def bslice(i: Rep[Int], iv1: Rep[RangeVector])(implicit o: Overloaded2): Rep[Array1D[T]]            // Row slice
      = block_slice[T,Array1D[T],Array2D[T]](x, List(i, iv1.start), List(unit(1), iv1.stride), List(unit(1),iv1.length(x.nCols)), List(0))
    def bslice(iv0: Rep[RangeVector], iv1: Rep[RangeVector])(implicit o: Overloaded3): Rep[Array2D[T]]  // 2D Slice
      = block_slice[T,Array2D[T],Array2D[T]](x, List(iv0.start, iv1.start), List(iv0.stride, iv1.stride), List(iv0.length(x.nRows), iv1.length(x.nCols)), Nil)

    // --- Annotations
    // Specifically for matrices created from block slices
    def noteReuse(reuseR: Rep[Int], reuseC: Rep[Int])(implicit ctx: SourceContext) = annotateReuse(x,reuseR,reuseC)

    // --- Printing
    def mkString(rdel: Rep[String], cdel: Rep[String]): Rep[String] = array2d_mkstring(x, rdel, cdel)
    def pprint: Rep[Unit] = println(x.mkString("\n", " "))
  }

  // --- File reading
  // (File reading is pretty annoying to write out directly in PPL)
  def read1D(path: Rep[String]): Rep[Array1D[Double]] = read(path).map{s => s.toDouble}
  def read2D(path: Rep[String])(implicit ctx: SourceContext): Rep[Array2D[Double]] = {
    val vec = read(path).map{s => darray_split_string(s.trim, unit("\\s+"), unit(-1)).map{s => s.toDouble} }
    collect(vec.length, vec(unit(0)).length){(i,j) => vec(i).apply(j)}
  }
  def readImg(path: Rep[String])(implicit ctx: SourceContext): Rep[Array2D[Int]] = {
    val vec = read(path).map{s => darray_split_string(s.trim, unit("\\s+"), unit(-1)).map{s => s.toInt} }
    collect(vec.length, vec(unit(0)).length){(i,j) => vec(i).apply(j)}
  }
}

// --- Concrete Ops
trait FlattenedArrayOpsExp extends FlattenedArrayOps with MultiArrayExp with DeliteStructsExp { this: PPLCompiler => 
  def *(): Rep[RangeWildcard] = fresh[RangeWildcard]
  def annotateViewified[T:Manifest](x: Rep[T]): Rep[T] = x.withData(MView(PhysType))

  def annotateReuse[T:Manifest](x: Rep[T], reuse: Rep[Int]*): Rep[T] = x match {
    case Def(e: BlockSlice[_,_,_]) => e.withReuse(reuse.toList); x
    case Def(Reflect(e: BlockSlice[_,_,_],_,_)) => e.withReuse(reuse.toList); x
    case _ => cwarn("Unable to find block slice node to annotate with reuse factors"); x
  }
  // Need to assume views are "true" views in cases where no phys information is available
  override def isTrueView(p: SymbolProperties) = getView(p).map{_.isTrueView}.getOrElse(true)
  override def isTrueView(e: Exp[Any]) = getView(e).map{_.isTrueView}.getOrElse(true)

  // --- Kernels
  // Arrays/Matrices with fixed size and constant elements
  // These should just translate to ROMs / FFs / wired constants for hardware
  case class KernelArray[A:Manifest](len: Rep[Int], ks: List[A])(implicit ctx: SourceContext) extends DefWithManifest[A, DeliteArray[A]]

  // --- Array data structures
  case class Array2DNew[A:Manifest](data: Rep[DeliteArray[A]], dim0: Rep[Int], dim1: Rep[Int])(implicit ctx: SourceContext) extends DeliteStruct[Array2D[A]] {
    val elems = copyTransformedElems(List("data" -> data, "dim0" -> dim0, "dim1" -> dim1))
    val mA = manifest[A]
  }
  case class Array2DViewNew[A:Manifest](data: Rep[DeliteArray[A]], ofs: Rep[Int], stride0: Rep[Int], stride1: Rep[Int], dim0: Rep[Int], dim1: Rep[Int])(implicit ctx: SourceContext) extends DeliteStruct[Array2DView[A]] {
    val elems = copyTransformedElems(List("data" -> data, "ofs" -> ofs, "stride0" -> stride0, "stride1" -> stride1, "dim0" -> dim0, "dim1" -> dim1))
    val mA = manifest[A]
  }
  case class Array1DViewNew[A:Manifest](data: Rep[DeliteArray[A]], ofs: Rep[Int], stride0: Rep[Int], dim0: Rep[Int])(implicit ctx: SourceContext) extends DeliteStruct[Array1DView[A]] {
    val elems = copyTransformedElems(List("data" -> data, "ofs" -> ofs, "stride0" -> stride0, "dim0" -> dim0))
    val mA = manifest[A]
  }

  /**
   * Block Slice
   * Create an m-dimensional slice from an n-dimensional collection
   * TODO: What should this be? An elem? A single task? A loop with a buffer body? Definitely can't fuse with anything right now
   *
   * destDims should have n elements, some of which may be Const(1).
   * indices of elements of destDims which are Const(1) should be in unitDims, unless all are Const(1)
   */
  case class BlockSlice[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](src: Exp[C], srcOffsets: List[Exp[Int]], srcStrides: List[Exp[Int]], destDims: List[Exp[Int]], unitDims: List[Int])(implicit ctx: SourceContext) extends DeliteOp[T] {
    type OpType <: BlockSlice[A,T,C]

    val n = srcOffsets.length
    val m = destDims.length - unitDims.length
    val deltaInds = List.tabulate(n){i=>i}.filterNot{i => unitDims.contains(i) }

    val nestLayers = m
    val sizes: List[Exp[Int]] = copyTransformedSymListOrElse(_.sizes)(destDims)     // dest dimensions
    val strides: List[Exp[Int]] = copyTransformedSymListOrElse(_.strides)(srcStrides) // src strides (for non-unit dims)

    // Bound variables
    val vs: List[Sym[Int]] = copyOrElse(_.vs)(List.fill(m)(fresh[Int].asInstanceOf[Sym[Int]]))    // dest indices
    val bV: List[Sym[Int]] = copyOrElse(_.bV)( List.fill(n)(fresh[Int].asInstanceOf[Sym[Int]]))  // src indices
    val tileVal: Sym[T] = copyTransformedOrElse(_.tileVal)(reflectMutableSym(fresh[T])).asInstanceOf[Sym[T]]      // dest buffer
    val bE: Sym[A] = copyTransformedOrElse(_.bE)(fresh[A]).asInstanceOf[Sym[A]]          // Single element (during copying out)

    // collection functions
    val bApply: Block[A] = copyTransformedBlockOrElse(_.bApply)(reifyEffects(dc_block_apply(src, bV, Nil)))           // src apply
    val tUpdate: Block[Unit] = copyTransformedBlockOrElse(_.tUpdate)(reifyEffects(dc_block_update(tileVal, vs, bE, Nil)))  // dest update
    val allocTile: Block[T] = copyTransformedBlockOrElse(_.allocTile)(reifyEffects(dc_alloc_block[A,T](tileVal, destDims, Nil))) // dest alloc 

    // HACK: Not sure if this will work 100% of the time
    // TBD: Should this just be a list of Ints instead?
    def withReuse(rs: List[Exp[Int]]): BlockSlice[A,T,C] = { this.reuse = rs; this }
    var reuse: List[Exp[Int]] = copyTransformedSymListOrElse(_.reuse)(List.fill(n)(unit(0)))

    val mA = manifest[A]
    val mT = manifest[T]
    val mC = manifest[C]
  }
  object BlockSlice {
    def mirror[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](op: BlockSlice[A,T,C], f: Transformer)(implicit ctx: SourceContext): BlockSlice[A,T,C] = op match {
      case BlockSlice(src,srcO,srcS,dD,uD) => 
        new {override val original = Some(f,op)} with BlockSlice[A,T,C](f(src),f(srcO),f(srcS),f(dD),uD)(op.mA,op.mT,op.mC,ctx)
    }
    def unerase[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](op: BlockSlice[_,_,_]): BlockSlice[A,T,C] = op.asInstanceOf[BlockSlice[A,T,C]]
  }

  def kernel_array[A:Manifest](len: Rep[Int], ks: List[A])(implicit ctx: SourceContext): Rep[Array1D[A]]
    = reflectPure(KernelArray(len, ks))

  def array2d_new[T:Manifest](data: Rep[DeliteArray[T]], dim0: Rep[Int], dim1: Rep[Int])(implicit ctx: SourceContext): Rep[Array2D[T]]
    = reflectPure(Array2DNew(data, dim0, dim1)).withData(FlatLayout(2, Plain)).withData(FlatLayout(2, Plain)).withField(getProps(data), "data")
  def array2dview_new[T:Manifest](data: Rep[DeliteArray[T]], ofs: Rep[Int], stride0: Rep[Int], stride1: Rep[Int], dim0: Rep[Int], dim1: Rep[Int])(implicit ctx: SourceContext): Rep[Array2DView[T]]
    = reflectPure(Array2DViewNew(data, ofs, stride0, stride1, dim0, dim1)).withData(FlatLayout(2, View)).withField(getProps(data), "data")
  def array1dview_new[T:Manifest](data: Rep[DeliteArray[T]], ofs: Rep[Int], stride0: Rep[Int], dim0: Rep[Int])(implicit ctx: SourceContext): Rep[Array1DView[T]]
    = reflectPure(Array1DViewNew(data, ofs, stride0, dim0)).withData(FlatLayout(1, View)).withField(getProps(data), "data")

  def array2d_new_mutable[T:Manifest](data: Rep[DeliteArray[T]], dim0: Rep[Int], dim1: Rep[Int])(implicit ctx: SourceContext): Rep[Array2D[T]]
    = reflectMutable(Array2DNew(data, dim0, dim1)).withData(FlatLayout(2, Plain)).withData(FlatLayout(2, Plain)).withField(getProps(data), "data")
  def array2dview_new_mutable[T:Manifest](data: Rep[DeliteArray[T]], ofs: Rep[Int], stride0: Rep[Int], stride1: Rep[Int], dim0: Rep[Int], dim1: Rep[Int])(implicit ctx: SourceContext): Rep[Array2DView[T]]
    = reflectMutable(Array2DViewNew(data, ofs, stride0, stride1, dim0, dim1)).withData(FlatLayout(2, View)).withField(getProps(data), "data")
  def array1dview_new_mutable[T:Manifest](data: Rep[DeliteArray[T]], ofs: Rep[Int], stride0: Rep[Int], dim0: Rep[Int])(implicit ctx: SourceContext): Rep[Array1DView[T]]
    = reflectMutable(Array1DViewNew(data, ofs, stride0, dim0)).withData(FlatLayout(1, View)).withField(getProps(data), "data")

  // TODO: Should BlockSlice only be defined for DeliteArray and use wrappers?
  // This blocks field shortcutting right now...
  def block_slice[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](src: Rep[C], srcOffsets: List[Rep[Int]], srcStrides: List[Rep[Int]], destDims: List[Rep[Int]], unitDims: List[Int])(implicit ctx: SourceContext): Rep[T]
    = reflectPure( BlockSlice[A,T,C](src,srcOffsets,srcStrides,destDims,unitDims) )

  // Bit of a hack here - the type of the multiarray actually doesn't matter here since it's never accessed in the MkString body
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

  // --- Array manifests
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
    case e@Array2DViewNew(d,o,s0,s1,d0,d1) => reflectPure(new {override val original = Some(f,e) } with Array2DViewNew(f(d),f(o),f(s0),f(s1),f(d0),f(d1))(e.mA,ctx))(mtype(manifest[A]),ctx)
    case e@Array1DViewNew(d,o,s0,d0) => reflectPure(new {override val original = Some(f,e) } with Array1DViewNew(f(d),f(o),f(s0),f(d0))(e.mA,ctx))(mtype(manifest[A]),ctx)
    case Reflect(e@Array2DNew(d,d0,d1), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with Array2DNew(f(d),f(d0),f(d1))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case Reflect(e@Array2DViewNew(d,o,s0,s1,d0,d1), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with Array2DViewNew(f(d),f(o),f(s0),f(s1),f(d0),f(d1))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case Reflect(e@Array1DViewNew(d,o,s0,d0), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e)} with Array1DViewNew(f(d),f(o),f(s0),f(d0))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    
    case e@KernelArray(len,ks) => kernel_array(f(len),ks)(e.mA,ctx)
    case Reflect(e@KernelArray(len,ks), u, es) => reflectMirrored(Reflect(KernelArray(f(len),ks)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)

    case e: BlockSlice[_,_,_] => 
      val op = BlockSlice.unerase(e)(e.mA,e.mT,e.mC)
      reflectPure(BlockSlice.mirror(op,f)(e.mA,e.mT,e.mC,ctx))(mtype(manifest[A]), ctx)
    case Reflect(e: BlockSlice[_,_,_], u, es) =>
      val op = BlockSlice.unerase(e)(e.mA,e.mT,e.mC)
      reflectMirrored(Reflect(BlockSlice.mirror(op,f)(e.mA,e.mT,e.mC,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def blocks(e: Any): List[Block[Any]] = e match {
    case op: BlockSlice[_,_,_] => Nil
    case _ => super.blocks(e)
  }

  // dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case op: BlockSlice[_,_,_] => syms(op.src) ::: syms(op.srcOffsets) ::: syms(op.strides) ::: syms(op.sizes) ::: syms(op.bApply) ::: syms(op.tUpdate) ::: syms(op.allocTile)
    case _ => super.syms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case op: BlockSlice[_,_,_] => readSyms(op.src) ::: readSyms(op.srcOffsets) ::: readSyms(op.strides) ::: readSyms(op.sizes) ::: readSyms(op.bApply) ::: readSyms(op.tUpdate) ::: readSyms(op.allocTile)
    case _ => super.readSyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case op: BlockSlice[_,_,_] => op.vs ::: op.bV ::: List(op.tileVal, op.bE) ::: effectSyms(op.bApply) ::: effectSyms(op.tUpdate) ::: effectSyms(op.allocTile) 
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case op: BlockSlice[_,_,_] => freqNormal(op.src) ::: freqNormal(op.srcOffsets) ::: freqNormal(op.strides) ::: freqNormal(op.sizes) ::: freqHot(op.bApply) ::: freqHot(op.tUpdate) ::: freqNormal(op.allocTile)
    case _ => super.symsFreq(e)
  }

  // aliases and sharing
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case op: BlockSlice[_,_,_] => Nil
    case _ => super.aliasSyms(e)
  }
  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case op: BlockSlice[_,_,_] => Nil
    case _ => super.containSyms(e)
  }
  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case op: BlockSlice[_,_,_] => Nil
    case _ => super.extractSyms(e)
  }
  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case op: BlockSlice[_,_,_] => Nil
    case _ => super.copySyms(e)
  }

  // --- Delite collection ops
  def asArray1D[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = asDeliteArray(x)
  def asArray2D[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[Array2D[A]]]
  def asArray2DView[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[Array2DView[A]]]
  def asArray1DView[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[Array1DView[A]]]

  def isArray1D[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isDeliteArray(x)  
  def isArray2D[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[Array2D[A]])
  def isArray2DView[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[Array2DView[A]])
  def isArray1DView[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[Array1DView[A]])

  def isArray1DTpe(x: Manifest[_])(implicit ctx: SourceContext) = isDeliteArrayTpe(x)  
  def isArray2DTpe(x: Manifest[_])(implicit ctx: SourceContext) = isSubtype(x.erasure,classOf[Array2D[_]])
  def isArray2DViewTpe(x: Manifest[_])(implicit ctx: SourceContext) = isSubtype(x.erasure,classOf[Array2DView[_]])
  def isArray1DViewTpe(x: Manifest[_])(implicit ctx: SourceContext) = isSubtype(x.erasure,classOf[Array1DView[_]])

  private def filterUnitDims(ds: List[Exp[Int]], unitDims: List[Int]): List[Exp[Int]]
    = ds.zipWithIndex.filterNot{d => unitDims.contains(d._2)}.map(_._1)

  override def dc_alloc_block[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], ds: List[Exp[Int]], unitDims: List[Int])(implicit ctx: SourceContext): Exp[CA] = {
    val dims = filterUnitDims(ds, unitDims)
    if (isArray1D(x)) DeliteArray[A](productTree(ds)).asInstanceOf[Exp[CA]]
    else if (isArray2D(x)) array2d_new_mutable(DeliteArray[A](productTree(ds)), dims(0), dims(1)).asInstanceOf[Exp[CA]]
    else if (isArray2DView(x)) array2d_new_mutable(DeliteArray[A](productTree(ds)), dims(0), dims(1)).asView.asInstanceOf[Exp[CA]]
    else if (isArray1DView(x)) DeliteArray[A](productTree(ds)).asView.asInstanceOf[Exp[CA]]
    else super.dc_alloc_block[A,CA](x,ds,unitDims)
  }
  override def dc_block_apply[A:Manifest](x: Exp[DeliteCollection[A]], is: List[Exp[Int]], unitDims: List[Int])(implicit ctx: SourceContext): Exp[A] = {
    val inds = filterUnitDims(is, unitDims)
    if (isArray1D(x)) array_apply[A,Array1D[A]](asArray1D(x), inds.take(1))
    else if (isArray2D(x)) array_apply[A,Array2D[A]](asArray2D(x), inds.take(2))
    else if (isArray2DView(x)) array_apply[A,Array2DView[A]](asArray2DView(x), inds.take(2))
    else if (isArray1DView(x)) array_apply[A,Array1DView[A]](asArray1DView(x), inds.take(1))
    else super.dc_block_apply[A](x,is,unitDims)
  }
  override def dc_block_update[A:Manifest](x: Exp[DeliteCollection[A]], is: List[Exp[Int]], y: Exp[A], unitDims: List[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    val inds = filterUnitDims(is, unitDims)
    if (isArray1D(x)) asArray1D(x).update(inds(0), y)
    else if (isArray2D(x)) asArray2D(x).update(inds(0), inds(1), y)
    else if (isArray2DView(x)) asArray2DView(x).update(inds(0), inds(1), y)
    else if (isArray1DView(x)) asArray1DView(x).update(inds(0), y)
    else super.dc_block_update[A](x,is,y,unitDims)
  }
  override def dc_slice[A:Manifest,TA<:DeliteCollection[A]:Manifest,CA<:DeliteCollection[A]:Manifest](src: Exp[CA], srcOffsets: List[Exp[Int]], srcStrides: List[Exp[Int]], destDims: List[Exp[Int]], unitDims: List[Int])(implicit ctx: SourceContext): Exp[TA] = {
    if (isArray1D(src) || isArray1DView(src) || isArray2D(src) || isArray2DView(src)) 
      array_slice[A,TA,CA](src, srcOffsets, srcStrides, destDims, unitDims) 
    else super.dc_slice[A,TA,CA](src, srcOffsets, srcStrides, destDims, unitDims)
  }
}

// --- Abstract Ops
trait FlattenedArrayLowerableOpsExp extends FlattenedArrayOpsExp with DeliteLowerableOpsExp { self: PPLCompiler => 
  private implicit val fc = AbstractFamily("FlatArray", skip = false)

  def array_apply[A:Manifest,C<:DeliteCollection[A]:Manifest](x: Rep[C], inds: List[Rep[Int]])(implicit ctx: SourceContext): Rep[A] = {
    if (fc.skip) ArrayApply.lower[A,C](x, inds) 
    else reflectPure( ArrayApply[A,C](x, inds) )
  }
  def array_slice[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](src: Rep[C], srcOffsets: List[Rep[Int]], srcStrides: List[Rep[Int]], destDims: List[Rep[Int]], unitDims: List[Int])(implicit ctx: SourceContext): Rep[T] = {
    if (fc.skip) ArraySlice.lower[A,T,C](src, srcOffsets, srcStrides, destDims, unitDims) 
    else reflectPure( ArraySlice[A,T,C](src, srcOffsets, srcStrides, destDims, unitDims) )
  }

  def box[A:Manifest](x: Rep[A])(implicit ctx: SourceContext): Rep[Array1D[A]] = {
    if (fc.skip) TileBoxHack.lower[A,Array1D[A]](x)
    else reflectPure( TileBoxHack[A,Array1D[A]](x) )
  }
  def debox[A:Manifest](x: Rep[Array1D[A]])(implicit ctx: SourceContext): Rep[A] = {
    if (fc.skip) TileUnboxHack.lower[A,Array1D[A]](x)
    else reflectPure( TileUnboxHack[A,Array1D[A]](x) )
  }

  case class ArrayApply[A:Manifest,C<:DeliteCollection[A]:Manifest](x: Exp[C], inds: List[Exp[Int]])(implicit ctx: SourceContext) extends AbstractDefWithManifest[C,A]
  object ArrayApply {
    def unerase[A:Manifest,C<:DeliteCollection[A]:Manifest](op: ArrayApply[_,_]): ArrayApply[A,C] = op.asInstanceOf[ArrayApply[A,C]]
    def mirror[A:Manifest,C<:DeliteCollection[A]:Manifest](op: ArrayApply[A,C], f: Transformer)(implicit ctx: SourceContext): ArrayApply[A,C]
      = ArrayApply[A,C](f(op.x),f(op.inds))
    def lower[A:Manifest,C<:DeliteCollection[A]:Manifest](x: Exp[C], inds: List[Exp[Int]])(implicit ctx: SourceContext): Exp[A] = {
      if (isArray1D(x)) 
        asArray1D(x).apply(inds(0))
      else if (isArray2D(x))  {
        val m = asArray2D(x) 
        m.data.apply(m.nCols*inds(0) + inds(1))
      }
      else if (isArray2DView(x)) {
        val m = asArray2DView(x)
        m.data.apply(m.rowStride*inds(0) + m.colStride*inds(1) + m.start)
      }
      else if (isArray1DView(x)) {
        val v = asArray1DView(x)
        v.data.apply(v.stride*inds(0) + v.start)
      }
      else sys.error("Don't know how to lower ArrayApply with type " + manifest[C].toString)
    }
  }

  case class ArraySlice[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](src: Exp[C], srcOffsets: List[Exp[Int]], srcStrides: List[Exp[Int]], destDims: List[Exp[Int]], unitDims: List[Int])(implicit ctx: SourceContext) extends AbstractDefWithManifest2[A,C,T]
  object ArraySlice {
    def unerase[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](op: ArraySlice[_,_,_]): ArraySlice[A,T,C] = op.asInstanceOf[ArraySlice[A,T,C]]
    def mirror[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](op: ArraySlice[A,T,C], f: Transformer)(implicit ctx: SourceContext): ArraySlice[A,T,C]
      = ArraySlice[A,T,C](f(op.src),f(op.srcOffsets),f(op.srcStrides),f(op.destDims),op.unitDims)
    def lower[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](src: Exp[C], srcOffsets: List[Exp[Int]], srcStrides: List[Exp[Int]], destDims: List[Exp[Int]], unitDims: List[Int])(implicit ctx: SourceContext): Exp[T] = {
      if (isArray1D(src) && isArray1DViewTpe(manifest[T]) && unitDims.isEmpty) {
        array1dview_new(asArray1D(src), srcOffsets(0), srcStrides(0), destDims(0)).asInstanceOf[Exp[T]]
      }
      else if (isArray1DView(src) && isArray1DViewTpe(manifest[T]) && unitDims.isEmpty) {
        val v = asArray1DView(src)
        array1dview_new(v.data, srcOffsets(0) + v.start, srcStrides(0)*v.stride, destDims(0)).asInstanceOf[Exp[T]]
      }
      else if (isArray2D(src)) {
        val m = asArray2D(src)
        if (isArray1DViewTpe(manifest[T]) && unitDims.contains(0))       // Row Slice
          array1dview_new(m.data, m.nCols*srcOffsets(0) + srcOffsets(1), srcStrides(1), destDims(1)).asInstanceOf[Exp[T]]
        else if (isArray1DViewTpe(manifest[T]) && unitDims.contains(1))  // Col Slice
          array1dview_new(m.data, m.nCols*srcOffsets(0) + srcOffsets(1), srcStrides(0)*m.nCols, destDims(0)).asInstanceOf[Exp[T]]
        else if (isArray2DViewTpe(manifest[T]) && unitDims.isEmpty)      // 2D Slice
          array2dview_new(m.data, m.nCols*srcOffsets(0) + srcOffsets(1), srcStrides(0)*m.nCols, srcStrides(1), destDims(0), destDims(1)).asInstanceOf[Exp[T]]
        else sys.error("Don't know how to lower ArraySlice with types " + manifest[T].toString + " and " + manifest[C].toString)
      }
      else if (isArray2DView(src)) {
        val m = asArray2DView(src)
        if (isArray1DViewTpe(manifest[T]) && unitDims.contains(0))       // Row Slice
          array1dview_new(m.data, m.start + (m.rowStride*srcOffsets(0)) + (m.colStride*srcOffsets(1)), m.colStride*srcStrides(1), destDims(1)).asInstanceOf[Exp[T]] 
        else if (isArray1DViewTpe(manifest[T]) && unitDims.contains(1))  // Col Slice
          array1dview_new(m.data, m.start + (m.rowStride*srcOffsets(0)) + (m.colStride*srcOffsets(1)), m.rowStride*srcStrides(0), destDims(0)).asInstanceOf[Exp[T]]
        else if (isArray2DViewTpe(manifest[T]) && unitDims.isEmpty)      // 2D Slice
          array2dview_new(m.data, m.start + (m.rowStride*srcOffsets(0)) + (m.colStride*srcOffsets(1)), m.rowStride*srcStrides(0), m.colStride*srcStrides(1), destDims(0), destDims(1)).asInstanceOf[Exp[T]]
        else sys.error("Don't know how to lower ArraySlice with types " + manifest[T].toString + " and " + manifest[C].toString)
      }
      else sys.error("Don't know how to lower ArraySlice with types " + manifest[T].toString + " and " + manifest[C].toString)
    }
  }

  // HACK: used when scalar reduction was wrapped with a tileAssemble (which currently only operates on tiles of DeliteCollections)
  // Should these be lowered or have special codegen rules?
  // Note that these aren't seen as array applies yet, so we won't mistakenly block them
  case class TileUnboxHack[A:Manifest,C<:DeliteCollection[A]:Manifest](x: Exp[C])(implicit ctx: SourceContext) extends AbstractDefWithManifest[C,A]
  object TileUnboxHack {
    def unerase[A:Manifest,C<:DeliteCollection[A]:Manifest](op: TileUnboxHack[_,_]): TileUnboxHack[A,C] = op.asInstanceOf[TileUnboxHack[A,C]]
    def mirror[A:Manifest,C<:DeliteCollection[A]:Manifest](op: TileUnboxHack[A,C], f: Transformer)(implicit ctx: SourceContext): TileUnboxHack[A,C]
      = TileUnboxHack[A,C](f(op.x))
    def lower[A:Manifest,C<:DeliteCollection[A]:Manifest](x: Exp[C])(implicit ctx: SourceContext): Exp[A] = {
      dc_block_apply[A](x, List.fill(10)(unit(0)), Nil)
    }
  }

  case class TileBoxHack[A:Manifest,C<:DeliteCollection[A]:Manifest](x: Exp[A])(implicit ctx: SourceContext) extends AbstractDefWithManifest[A,C]
  object TileBoxHack {
    def unerase[A:Manifest,C<:DeliteCollection[A]:Manifest](op: TileBoxHack[_,_]): TileBoxHack[A,C] = op.asInstanceOf[TileBoxHack[A,C]]
    def mirror[A:Manifest,C<:DeliteCollection[A]:Manifest](op: TileBoxHack[A,C], f: Transformer)(implicit ctx: SourceContext): TileBoxHack[A,C]
      = TileBoxHack[A,C](f(op.x))
    def lower[A:Manifest,C<:DeliteCollection[A]:Manifest](x: Exp[A])(implicit ctx: SourceContext): Exp[C] = {
      val dat = dc_alloc_block[A,C](fresh[C], List.fill(10)(unit(1)), Nil) // hack - not sure what rank to allocate for here
      dc_block_update[A](dat, List.fill(10)(unit(0)), x, Nil)
      dat.unsafeImmutable
    }
  }

  // --- Mirroring and lowering rules
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@ArrayApply(x,inds) => 
      val op = ArrayApply.unerase(e)(e.mR,e.mA)
      reflectPure(ArrayApply.mirror(op,f)(e.mR,e.mA,ctx))(mtype(manifest[A]),ctx)
    case e@ArraySlice(x,ofs,str,dims,udims) => 
      val op = ArraySlice.unerase(e)(e.mA,e.mR,e.mB)
      reflectPure(ArraySlice.mirror(op,f)(e.mA,e.mR,e.mB,ctx))(mtype(manifest[A]),ctx)
    case Reflect(e@ArrayApply(x,inds),u,es) => 
      val op = ArrayApply.unerase(e)(e.mR,e.mA)
      reflectMirrored(Reflect(ArrayApply.mirror(op,f)(e.mR,e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case Reflect(e@ArraySlice(x,ofs,str,dims,udims),u,es) => 
      val op = ArraySlice.unerase(e)(e.mA,e.mR,e.mB)
      reflectMirrored(Reflect(ArraySlice.mirror(op,f)(e.mA,e.mR,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    
    case e@TileUnboxHack(x) => 
      val op = TileUnboxHack.unerase(e)(e.mR,e.mA)
      reflectPure(TileUnboxHack.mirror(op,f)(e.mR,e.mA,ctx))(mtype(manifest[A]),ctx)
    case e@TileBoxHack(x) => 
      val op = TileBoxHack.unerase(e)(e.mA,e.mR)
      reflectPure(TileBoxHack.mirror(op,f)(e.mA,e.mR,ctx))(mtype(manifest[A]),ctx)
    case Reflect(e@TileUnboxHack(x), u, es) => 
      val op = TileUnboxHack.unerase(e)(e.mR,e.mA)
      reflectMirrored(Reflect(TileUnboxHack.mirror(op,f)(e.mR,e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@TileBoxHack(x), u, es) => 
      val op = TileBoxHack.unerase(e)(e.mA,e.mR)
      reflectMirrored(Reflect(TileBoxHack.mirror(op,f)(e.mA,e.mR,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def lower[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e: ArrayApply[a,c] => 
      val op = ArrayApply.unerase(e)(e.mR,e.mA)
      ArrayApply.lower[a,c](f(op.x), f(op.inds))(e.mR,e.mA,ctx)

    case e: ArraySlice[a,t,c] => 
      val op = ArraySlice.unerase(e)(e.mA,e.mR,e.mB)
      ArraySlice.lower[a,t,c](f(op.src),f(op.srcOffsets),f(op.srcStrides),f(op.destDims),op.unitDims)(e.mA,e.mR,e.mB,ctx)

    case e: TileUnboxHack[a,c] => 
      val op = TileUnboxHack.unerase(e)(e.mR,e.mA)
      TileUnboxHack.lower[a,c](f(op.x))(e.mR,e.mA,ctx)

    case e: TileBoxHack[a,c] => 
      val op = TileBoxHack.unerase(e)(e.mA,e.mR)
      TileBoxHack.lower[a,c](f(op.x))(e.mA,e.mR,ctx)

    case _ => super.lower(e,f)
  }).asInstanceOf[Exp[A]]

  // --- Lowering Transformer
  class ApplyLowering extends AbstractImplementer {
    val IR: self.type = self
    override val name = "Apply Lowering"
    //override def runOnce[A:Manifest](b: Block[A]): Block[A] = inDebugMode { super.runOnce(b) }

    override def transferMetadata(sub: Exp[Any], orig: Exp[Any], d: Def[Any])(implicit ctx: SourceContext) = d match {
      case e: ArrayApply[_,_] => copyMetadata(sub, props(orig))
      case e: ArraySlice[_,_,_] => copyMetadata(sub, props(orig))
      case e: TileUnboxHack[_,_] => copyMetadata(sub, props(orig))
      case e: TileBoxHack[_,_] => copyMetadata(sub, props(orig))
      case _ => // Nothing
    }
  }
  val implementer = new ApplyLowering()
  if (!fc.skip) appendVisitor(implementer)
}

trait FlattenedArrayOpsExpOpt extends FlattenedArrayLowerableOpsExp with DeliteArrayOpsExpOpt { this: PPLCompiler => 
  // Shortcutting for kernels
  // Always immediately unwrap kernel applies at constant indices
  override def array_apply[A:Manifest,C<:DeliteCollection[A]:Manifest](x: Rep[C], inds: List[Rep[Int]])(implicit ctx: SourceContext): Rep[A] = x match {
    case Def(KernelArray(len, ks)) if inds.forall(_.isInstanceOf[Const[_]]) => ArrayApply.lower[A,C](x, inds) 
    case Def(Reflect(KernelArray(len, ks), _, _)) if inds.forall(_.isInstanceOf[Const[_]]) => ArrayApply.lower[A,C](x, inds) 
    case _ => super.array_apply[A,C](x,inds)
  }

  override def darray_apply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int])(implicit ctx: SourceContext) = (da,i) match {
    case (Def(KernelArray(len, ks)), Const(i)) => unit(ks.apply(i))
    case (Def(Reflect(KernelArray(len, ks), _, _)), Const(i)) => unit(ks.apply(i))
    case _ => super.darray_apply(da,i)
  }

  override def darray_length[T:Manifest](da: Exp[DeliteArray[T]])(implicit ctx: SourceContext) = da match {
    case Def(KernelArray(len, ks)) => len
    case Def(Reflect(KernelArray(len, ks), _, _)) => len
    case _ => super.darray_length(da)
  }
}



trait ScalaGenFlattenedArrayOps extends ScalaGenDeliteDSL with ScalaGenNestedOps {
  val IR: PPLOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // TODO: This should actually be a view in software
    case op: BlockSlice[_,_,_] => 
      stream.println("// --- Block Slice")
      emitBlock(op.allocTile)
      emitValDef(op.tileVal, quote(getBlockResult(op.allocTile)))

      for (i <- 0 until op.unitDims.length) {
        emitValDef(op.bV( op.unitDims(i)), quote(op.srcOffsets(op.unitDims(i))) )
      }

      for (i <- 0 until op.m) {
        emitBoundVarDef(op.bV( op.deltaInds(i)), quote(op.srcOffsets(op.deltaInds(i))) )
        stream.println("for (" + quote(op.vs(i)) + " <- 0 until " + quote(op.destDims(op.deltaInds(i))) + ") {")
      }
      emitBlock(op.bApply)
      emitValDef(op.bE, quote(getBlockResult(op.bApply)))
      emitBlock(op.tUpdate)

      for (i <- 0 until op.m) {
        stream.println(quote(op.bV( op.deltaInds(op.m - i - 1))) + " += " + quote( op.strides( op.deltaInds(op.m - i - 1) ) ) )
        stream.println("}")
      }

      emitValDef(sym, quote(op.tileVal))

    case op: KernelArray[_] => 
      emitValDef(sym, "List[" + remap(op.mA) + "]" + op.ks.mkString("(", ",", ")") + ".toArray")

    case _ => super.emitNode(sym, rhs)
  }

}
