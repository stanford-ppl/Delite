package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.visit._
import ppl.delite.framework.ops._

trait PatternPromotingExp extends DeliteVisit { self: PPLOpsExp =>
  
  val patternPromotion = new PromotionTransformer{ val IR: self.type = self }
  //appendVisitor(patternPromotion)

  trait PromotionTransformer extends TunnelingTransformer {
    val IR: self.type
    override val name = "Pattern Promotion"
  
    //override val debugMode = true
    /*override def postprocess[A:Manifest](b: Block[A]): Block[A] = { 
      printer.run(b)
      sys.exit
      b 
    }*/

    // TODO: Generalize this to purely nested loop detection function?
    override def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = d match {
      case op: DeliteOpLoopNest[_] if op.strides.forall{case Const(1) => true; case _ => false} => op.body match {
        case cElem: DeliteCollectElem[a,_,c] => 
          var bodyStms: List[Stm] = Nil
          focusBlock(cElem.func){ bodyStms ++= innerScope }

          var innerStms: List[Stm] = Nil
          bodyStms.last match {
            case stm@TP(sr, opI: DeliteOpLoopNest[_]) if opI.strides.exists{case Const(1) => false; case _ => true} => opI.body match {
              case rElem: DeliteReduceElem[`a`] if rElem.cond.isEmpty => 
                // Check that redcue is perfectly nested within the collect by checking that the collect's innerScope
                // has only the reduce and everything in the reduce's innerScope
                // FIXME: Is this a sound check for perfect nesting?

                // Assumes that blocks has been defined correctly here...
                blocks(rElem).foreach{b => focusBlock(b){innerStms ++= innerScope }}
                innerStms = innerStms.distinct // TODO: Necessary?
                
                val diffStms = bodyStms filterNot (innerStms contains _)
                if (diffStms.length == 1 && diffStms.head == stm) {
                  dbgmsg("Found perfectly nested collect(tiledReduce): ")
                  dbgmsg("   " + strDef(s))
                  dbgmsg("   " + strDef(sr))

                  def uneraseCollect[A:Manifest,C<:DeliteCollection[A]:Manifest](op: DeliteCollectElem[_,_,_]): DeliteCollectElem[A,_,C]
                    = op.asInstanceOf[DeliteCollectElem[A,_,C]]
                  def uneraseReduce[A:Manifest](op: DeliteReduceElem[_]): DeliteReduceElem[A]
                    = op.asInstanceOf[DeliteReduceElem[A]]

                  val collect = uneraseCollect(cElem)(cElem.mA,cElem.mCA)
                  val reduce = uneraseReduce(rElem)(cElem.mA)
                  Some( swapCollectReduce(op, collect, opI, reduce)(cElem.mA,cElem.mCA,ctx) )
                }
                else None

              case _ => None
            }
            case _ => None
          }

        case _ => None
      }

      case _ => None
    }

    def swapCollectReduce[A:Manifest,C<:DeliteCollection[A]:Manifest](op: DeliteOpLoopNest[_], cElem: DeliteCollectElem[A,_,C], opI: DeliteOpLoopNest[_], rElem: DeliteReduceElem[A])(implicit ctx: SourceContext): Exp[C] = {
      val innerFunc: Block[A] = f(rElem.func)
      val innerCollect = NestedCollect[A,C](op.vs, f(op.sizes), innerFunc)

      val func: Block[C] = reifyEffects(reflectPure(innerCollect))

      // TODO: Should actually be a collect of rElem's zero - this only works for additions
      val zero: Block[C] = reifyEffects(dc_alloc_block[A,C](fresh[C], f(op.sizes), Nil))

      val rV: (Sym[C],Sym[C]) = (fresh[C], fresh[C])
      val vs: List[Sym[Int]] = List.fill(op.nestLayers)(fresh[Int])
      val sizes: List[Exp[Int]] = dc_dims[A](rV._2)

      val apply1 = dc_block_apply[A](rV._1, vs, Nil)
      val apply2 = dc_block_apply[A](rV._2, vs, Nil)

      val rFuncBody = withSubstScope(rElem.rV._1 -> apply1, rElem.rV._2 -> apply2){ f(rElem.rFunc) }
      val rFuncCollect = NestedCollect[A,C](vs, sizes, rFuncBody)

      val rFunc: Block[C] = reifyEffects(reflectPure(rFuncCollect))

      val promotedReduce = NestedReduce[C](opI.vs, rV, f(opI.sizes), Some(f(opI.strides)), func, rFunc, zero, zero, Nil, false)

      dbgmsg("Created new reduce: " + promotedReduce)
      dbgmsg("Created new collect: " + innerCollect)

      reflectPure( promotedReduce )
    }

    def transferMetadata(sub: Exp[Any], orig: Exp[Any], d: Def[Any])(implicit ctx: SourceContext): Unit = {}
  }
}
