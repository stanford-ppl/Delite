/*package feattest.dhdl
import feattest._

import scala.virtualization.lms.common.{Base,BaseExp}
import ppl.delite.framework.datastructures.DeliteStructsExp
import ppl.delite.framework.ops.DeliteOpsExp
import scala.reflect.SourceContext

/*trait MemMetadata extends ExperimentalMetadataOps { this: DHDLNodes =>
  case class DoubleBuffer(buf: Boolean, readers: Set[CtrlNode], writer: Option[CtrlNode]) extends Metadata {
    def meet(that: DoubleBuffer) = DoubleBuffer(this.buf || that.buf, (this.readers ++ that.readers), this.writer.orElse(that.writer))
  }

  override def meetMetadata(a: Metadata, b: Metadata, t: MeetFunc): Metadata = (a,b) match {
    case (a: DoubleBuffer, b: DoubleBuffer) => a meet b
    case _ => super.meetMetadata(a,b,t)
  }

  def isDblBuf(e: Exp[Any]) = meta[DoubleBuffer](e).map(_.buf).getOrElse(false)
  def readers(e: Exp[Any]) = meta[DoubleBuffer](e).map(_.readers).getOrElse(Nil)
  def writer(e: Exp[Any]) = meta[DoubleBuffer](e).map(_.writer).getOrElse(None)

  def infix_addReader(e: Exp[Any], r: CtrlNode) { setMetadata(e, DoubleBuffer(true,Set(r),None)) }
}*/

// Memories can be reset
trait Mem[T]
trait BRAM[T] extends Mem[T]
trait DRAM[T] extends Mem[T]

trait Bundle
trait Vector[T]
trait CtrlState

trait Data[T] {
  def zero: Const[T]
}
trait SInt
trait UInt

trait DHDLAPI extends Base with SymbolMetadata {
  type Wire[T] = Rep[T]
  type Bit = Boolean
  type SrcCtx = SourceContext
  type StridedDomain = (Wire[UInt],Int)

  case class Reg[T:Data](v: Wire[Reg[T]]) extends Mem[T] {
    def :=(in: Wire[T])(implicit ctx: SrcCtx) = regWrite(this, in)
  }

  implicit def vectorManifest[T:Manifest] = manifest[Vector[T]]

  def typeOf[T:Data] = implicitly[Data[T]]

  // Metadata
  def infix_resetsWith[T,Mem<:Mem[T]](mem: Wire[M], ctrl: Wire[CtrlState]): Wire[M]
  def infix_resetsWith[T](mem: Reg[T], ctrl: Wire[CtrlState]): Reg[T]

  // --- Primitives
  def Mux[T:Data](sel: Wire[Bit])(a: Wire[T], b: Wire[T])(implicit ctx: SrcCtx) = mux(sel,a,b)

  implicit def regOps[T:Data](reg: Reg[T])(implicit ctx: SrcCtx) = new DataOps(regRead(reg))
  implicit class DataOps[T:Data](a: Wire[T])(implicit ctx: SrcCtx) {
    // TODO: type inferral
    def +(b: Wire[T]) = add[T,T,T](a,b)
    def -(b: Wire[T]) = sub[T,T,T](a,b)
    def *(b: Wire[T]) = mul[T,T,T](a,b)
    def /(b: Wire[T]) = div[T,T,T](a,b)
    def >(b: Wire[T]) = gt(a,b)
    def <(b: Wire[T]) = lt(a,b)
    def >=(b: Wire[T]) = geq(a,b)
    def <=(b: Wire[T]) = leq(a,b)
    def ==(b: Wire[T]) = eql(a,b)
    def &(b: Wire[T]) = and[T,T,T](a,b)
    def |(b: Wire[T]) = or[T,T,T](a,b)
    def unary_!() = not(a)
  }
  implicit def regUIntOps(reg: Reg[UInt])(implicit ctx: SrcCtx) = new UIntOps(regRead(reg))
  implicit def UIntOps(a: Wire[UInt])(implicit ctx: SrcCtx) {
    def by(stride: Int): StridedDomain = (a, stride)
  }

  // --- Memories
  def OffChip[T:Data](id: String, sizes: Wire[UInt]*)(implicit ctx: SrcCtx): Wire[DRAM[T]]
  def BRAM[T:Data](depth: Int, dblBuf: Boolean = false)(implicit ctx: SrcCtx): Wire[BRAM[T]]
  def Reg[T:Data](init: Wire[T] = typeOf[T].zero, dblBuf: Boolean = false)(implicit ctx: SrcCtx): Wire[Reg[T]]
  def ArgIn[T:Data](id: String)(implicit ctx: SrcCtx): Wire[T]
  def ArgOut[T:Data](id: String, in: Wire[T])(implicit ctx: SrcCtx): Wire[Unit]

  // --- Load/Storing
  def Ld[T:Data](mem: Wire[BRAM[T]], addr: Wire[UInt])(implicit ctx: SrcCtx): Wire[T]
  def St[T:Data](mem: Wire[BRAM[T]], addr: Wire[UInt], data: Wire[T])(implicit ctx: SrcCtx): Wire[T]

  def TileLd[T:Data](mem: Wire[DRAM[T]], cols: Wire[UInt], buf: Wire[BRAM[T]], indices: List[Wire[UInt]], dims: List[Int], force: Wire[Bit] = false)(implicit ctx: SrcCtx): Wire[Bit]
  def TileSt[T:Data](mem: Wire[DRAM[T]], cols: Wire[UInt], buf: Wire[BRAM[T]], indices: List[Wire[UInt]], dims: List[Int], force: Wire[Bit] = false)(implicit ctx: SrcCtx): Wire[Bit]

  // --- Controllers
  def Dummy(): Wire[CtrlState]
  def CounterChain(max_stride: StridedDomain*): Wire[Vector[UInt]]

  def parallel(body: => Wire[CtrlState]): Wire[CtrlState]
  def sequence(ctr: Wire[Vector[UInt]])(body: => Wire[CtrlState]): Wire[CtrlState]
  def sequence(max_stride: StridedDomain*)(body: Wire[Vector[UInt]] => Wire[CtrlState]): Wire[CtrlState]
  def metapipe(ctr: Wire[Vector[UInt]])(body: => Wire[CtrlState]): Wire[CtrlState]
  def metapipe(max_stride: StridedDomain*)(body: Wire[Vector[UInt]] => Wire[CtrlState]): Wire[CtrlState]
  def pipe(ctr: Wire[Vector[UInt]])(map: => Wire[Unit]): Wire[CtrlState]
  def pipe(max_stride: StridedDomain*)(map: Wire[Vector[UInt]] => Wire[Unit]): Wire[CtrlState]

  def pipetree[T:Data](ctr: Wire[Vector[UInt]])(map: => Wire[T])(tree: (Wire[T],Wire[T]) => Wire[T])(store: Wire[T] => Wire[Unit]): Wire[CtrlState]
  def pipetree[T:Data](max_stride: StridedDomain*)(map: Wire[Vector[UInt]] => Wire[Unit])(tree: (Wire[T],Wire[T]) => Wire[T])(store: Wire[T] => Wire[Unit]): Wire[CtrlState]

  // --- Stubs
  def add[A:Data,B:Data,R:Data](a: Wire[A], b: Wire[B]): Wire[R]
  def sub[A:Data,B:Data,R:Data](a: Wire[A], b: Wire[B]): Wire[R]
  def mul[A:Data,B:Data,R:Data](a: Wire[A], b: Wire[B]): Wire[R]
  def div[A:Data,B:Data,R:Data](a: Wire[A], b: Wire[B]): Wire[R]
  def gt[A:Data,B:Data](a: Wire[A], b: Wire[B]): Wire[Bit]
  def lt[A:Data,B:Data](a: Wire[A], b: Wire[B]): Wire[Bit]
  def geq[A:Data,B:Data](a: Wire[A], b: Wire[B]): Wire[Bit]
  def leq[A:Data,B:Data](a: Wire[A], b: Wire[B]): Wire[Bit]
  def eql[A:Data,B:Data](a: Wire[A], b: Wire[B]): Wire[Bit]
  def and[A:Data,B:Data,R:Data](a: Wire[A], b: Wire[B]): Wire[R]
  def or[A:Data,B:Data,R:Data](a: Wire[A], b: Wire[B]): Wire[R]

  def mux[T:Data](sel: Wire[Bit], a: Wire[T], b: Wire[T]): Wire[T]

  def regWrite[T:Data](reg: Reg[T], in: Wire[T])(implicit ctx: SrcCtx): Wire[Unit]
  implicit def regRead[T](reg: Reg[T])(implicit ctx: SrcCtx): Wire[T]
}

trait DHDLNodes extends DHDLAPI with DeliteStructsExp { this: DeliteOpsExp =>

  abstract class Module[T:Manifest] extends DeliteStruct[T]

  case class FixedPoint[T](decBits: Int, frcBits: Int, signed: Boolean) extends Data[T]
  case class FloatPoint[T](manBits: Int, expBits: Int) extends Data[T]

  abstract class Node[T] extends Def[T] {
    var parent: Option[Controller] = None
  }

  abstract class Logic[R:Data] extends Node[R] { val mR = typeOf[R] }
  abstract class Logic2[A:Data,R:Data] extends Logic[R] { val mA = typeOf[R] }
  abstract class Logic3[A:Data,B:Data,R:Data] extends Logic2[A,R] { val mB = typeOf[B] }
  abstract class Memory[T:Data,M<:Mem[T]] extends Node[M] { val mT = typeOf[T] }
  abstract class Controller extends Node[CtrlState]

  // --- Combinational logic
  case class Add[A:Data,B:Data,R:Data](a: Wire[A], b: Wire[B]) extends Logic3[A,B,R]
  case class Sub[A:Data,B:Data,R:Data](a: Wire[A], b: Wire[B]) extends Logic3[A,B,R]
  case class Mul[A:Data,B:Data,R:Data](a: Wire[A], b: Wire[B]) extends Logic3[A,B,R]
  case class Div[A:Data,B:Data,R:Data](a: Wire[A], b: Wire[B]) extends Logic3[A,B,R]
  case class Multiplexer[R:Data](s: Wire[Bit], a: Wire[R], b: Wire[R]) extends Logic[R]
  case class Lt[A:Data,B:Data](a: Wire[A], b: Wire[B]) extends Logic[Bit]
  case class Gt[A:Data,B:Data](a: Wire[A], b: Wire[B]) extends Logic[Bit]
  case class Eq[A:Data,B:Data](a: Wire[A], b: Wire[B]) extends Logic[Bit]
  case class LEq[A:Data,B:Data](a: Wire[A], b: Wire[B]) extends Logic[Bit]
  case class GEq[A:Data,B:Data](a: Wire[A], b: Wire[B]) extends Logic[Bit]
  case class And[A:Data,B:Data,R:Data](a: Wire[A], b: Wire[B]) extends Logic[R]
  case class Or[A:Data,B:Data,R:Data](a: Wire[A], b: Wire[B]) extends Logic[R]
  case class Not(a: Wire[Bit]) extends Logic[Bit]

  case class Load[R:Data](mem: Wire[BRAM[R]], addr: Wire[UInt]) extends Logic[R]
  case class Store[A:Data](mem: Wire[BRAM[A]], addr: Wire[UInt], data: Wire[A]) extends Logic2[A,Unit]

  case class ReadReg[R:Data](reg: Reg[R]) extends Logic[R]
  case class WriteReg[A:Data](reg: Reg[A], data: Wire[A]) extends Logic2[A,Unit]

  // --- Controllers
  case class DummyControl() extends Controller
  case class CountChain(max: List[Wire[UInt]], stride: List[Int]) extends Module[Vector[UInt]] {
    val size = max.length
    val iters = Seq.fill(size)(fresh[UInt])
    val elems = copyTransformedElems(iters.zipWithIndex.map{case (iter,i) => s"$i" -> iter})
  }

  case class TileLoad[T](
    src: Wire[DRAM[T]],   // Off-chip memory to load data from
    srcCols: Wire[UInt],  // Width of off-chip array's column (in terms of elements)
    idx0: Wire[UInt],     // Starting row of tile in off-chip array
    idx1: Wire[UInt],     // Starting column of tile in off-chip array
    buf: Wire[BRAM[T]],   // On-chip buffer to load the tile of data into
    tileRows: Int,        // Number of rows in a single tile (in terms of elements)
    tileCols: Int,        // Number of columns in a single tile (in terms of elements)
    force: Wire[Bit]      // When true, forces tile to load even when indices are same as previous load
  ) extends Logic[Bit]

  case class TileStore[T](
    dest: Wire[DRAM[T]],  // Off-chip array to which data is to be stored
    destCols: Wire[UInt], // Width of off-chip array's column (in terms of elements)
    idx0: Wire[UInt],     // Starting row of tile in off-chip array
    idx1: Wire[UInt],     // Starting column of tile in off-chip array
    buf: Wire[BRAM[T]],   // On-chip buffer containing the tile to be stored
    tileRows: Int,        // Number of rows in a single tile
    tileCols: Int,        // Number of columns in a single tile
    force: Wire[Bit]      // When true, forces storing even when indices are same as previous store
  ) extends Logic[Bit]

  case class Parallelize(nodes: List[Block[CtrlState]]) extends Controller
  case class Sequentialize(ctr: Wire[Vector[UInt]], body: Block[CtrlState]) extends Controller
  case class Pipeline[T](ctr: Vector[UInt], map: Block[T], reduce: Option[Block[Unit]]) extends Controller
  case class MetaPipeline[T](ctr: Vector[UInt], regChain: Block[Unit], iters: Block[UInt], body: Block[CtrlState]) extends Controller

  // --- Memories
  case class BlockRAM[T:Data](depth: Int, dblBuf: Boolean) extends Memory[T,BRAM[T]]
  case class OffChipMem[T:Data](id: String, sizes: List[Wire[UInt]]) extends Memory[T,DRAM[T]]
  case class Register[T:Data](init: Wire[A], dblBuf: Boolean) extends Memory[T,Reg[T]]
  case class InputArg[T:Data](id: String) extends Logic[T] { val n = nextArgIn }
  case class OutputArg[T:Data](id: String, in: Wire[T]) extends Logic[Unit] { val n = nextArgOut }

  //def instantiate[T](x: Node[T])

  private var memIds: Map[String,SrcCtx] = Map.empty
  private var nArgIn = -1
  private def nextArgIn = {nArgIn += 1; nArgIn}
  private var nArgOut = -1
  private def nextArgOut = {nArgOut += 1; nArgOut}
}*/