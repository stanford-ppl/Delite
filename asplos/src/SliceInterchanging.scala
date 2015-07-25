package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

import ppl.delite.framework.Config
import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.visit._
import ppl.delite.framework.ops._

// Changes slices to block slices for hardware generation
// TODO: The reverse should be done for CPU generation
trait SliceInterchangingExp extends DeliteVisit { self: PPLOpsExp => 

  val sliceInterchange = new SliceInterchangeTransformer{ val IR: self.type = self }

  trait SliceInterchangeTransformer extends TunnelingTransformer {
    val IR: self.type
    override val name = "Slice Interchange"

    //override val debugMode = true 
    //override val printBefore = true
    //override val printAfter = true
    /*override def preprocess[A:Manifest](b: Block[A]): Block[A] = { 
      printmsg("Prior to starting slice interchange, tunable annotations are:")
      for((e,t) <- tunableParams) printmsg(s"$e -> $t")
      b 
    }*/

    // TODO: Assumes the constant is small... What if it isn't? What's the cutoff?
    def requiresBlocking(d: Exp[Int]) = d match {
      case Const(i) if i < 10000 => false
      case _: Tunable => false
      case d if tunableParams.contains(d) =>
        val tunable = tunableParams(d)
        (tunable.value, tunable.maxSize) match { case (v,s) => v != s; case _ => true }
      case _ => true 
    }

    override def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = d match {
      case op: ArraySlice[a,t,c] =>
        val src = f(op.src)
        val srcOffsets = f(op.srcOffsets)
        val srcStrides = f(op.srcStrides)
        val destDims = f(op.destDims)
        val tunables = destDims.map{dim => tunableParams.get(dim) }
        
        printDebug("Found slice with output dimensions: ")
        destDims.zip(tunables).foreach{case (dim,tunable) =>
          printDebug(strDef(dim))
          printDebug(s"$dim -> $tunable")
        }

        if ( destDims.forall(!requiresBlocking(_)) ) {
          val blk = block_slice[a,t,c](src, srcOffsets, srcStrides, destDims, op.unitDims)(op.mA,op.mR,op.mB,ctx)
          //val view = blk.asView   // Types don't work out if this isn't a view 
                                  // TODO: May want to do some fancier MultiArray-style transforms to avoid this
          Some(blk)
        }
        else None

      case _ => None
    }

    // TODO: Metadata transfer
    def transferMetadata(sub: Exp[Any], orig: Exp[Any], d: Def[Any])(implicit ctx: SourceContext): Unit = {}
  }
}