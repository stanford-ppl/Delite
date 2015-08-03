package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.visit._
import ppl.delite.framework.ops._

// Performance Warning: This transformer is somewhat unstable and can have the effect of pulling operations
// INTO loops and/or duplicating work. This is desired in some specific cases, but definitely not in others. We should eventually 
// pair this transformer with a cost analysis pass, but for now only enable this transformer if you know you need it.
// TODO: Multiple layers of branching?
trait SlicePushingExp extends DeliteVisit {self: PPLOpsExp =>

  val slicePush = new SlicePushTransformer{ val IR: self.type = self }
  //appendVisitor(slicePush)

  trait SlicePushTransformer extends TunnelingTransformer {
    val IR: self.type
    override lazy val name = "Slice Pushing"
 
    // Debugging
    //override val printBefore = true
    //override val printAfter = true
    //override val debugMode = true

    // FIXME: This is only this simple when the if statement only has the slice as a dependency. Otherwise we'll
    // likely get code duplication. What to do in more complicated cases? Change to FatIf?
    override def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = d match {
      case op: BlockSlice[a,t,c] => f(op.src) match {
        case Def(brnch: DeliteOpCondition[_]) => 
          dbgmsg("Found slice of a branch: " + strDef(s))
          dbgmsg("Branch: " + strDef(op.src))
          val then1 = getBlockResult(brnch.thenp)
          val else1 = getBlockResult(brnch.elsep)
          val then2 = gen_block_slice[a,t,c](then1, f(op.srcOffsets), f(op.srcStrides), f(op.destDims), op.unitDims, op.reuse)(op.mA,op.mT,op.mC,ctx)
          val else2 = gen_block_slice[a,t,c](else1, f(op.srcOffsets), f(op.srcStrides), f(op.destDims), op.unitDims, op.reuse)(op.mA,op.mT,op.mC,ctx)
          withSubstScope(then1 -> then2, else1 -> else2) {
            Some(self_mirror(op.src.asInstanceOf[Sym[c]], brnch))
          }
        case _ => None
      }
      case _ => super.transformSym(s,d)
    } 
    
    // TODO: Metadata transfer
    def transferMetadata(sub: Exp[Any], orig: Exp[Any], d: Def[Any])(implicit ctx: SourceContext): Unit = {}
  }

  private def gen_block_slice[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](src: Rep[C], srcOffsets: List[Rep[Int]], srcStrides: List[Rep[Int]], destDims: List[Rep[Int]], unitDims: List[Int], reuse: List[Int])(implicit ctx: SourceContext): Rep[T]
    = reflectPure( BlockSlice[A,T,C](src,srcOffsets,srcStrides,destDims,unitDims).withReuse(reuse) )
}
