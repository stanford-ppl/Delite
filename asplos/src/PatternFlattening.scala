package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.visit._
import ppl.delite.framework.ops._

trait PatternFlatteningExp extends DeliteVisit { self: PPLOpsExp =>
  
  val patternFlatten = new PatternFlatteningTransformer{ val IR: self.type = self }
  //appendVisitor(patternFlatten)

  trait PatternFlatteningTransformer extends TunnelingTransformer {
    val IR: self.type
    override val name = "Pattern Flattening"
    //override val debugMode = true
    //override val printBefore = true
    /*override def postprocess[A:Manifest](b: Block[A]): Block[A] = { 
      printer.run(b)
      sys.exit
      b 
    }*/

    // TODO: May want this elsewhere, or at least rewrite this in another way (maybe with EatReflect?)
    def allowWorkDuplication(stm: Stm): Boolean = stm match {
      case TP(_, slc: BlockSlice[_,_,_]) => true
      case TP(_, Reflect(slc: BlockSlice[_,_,_], _,_)) => true
      case TP(_, slc: ArraySlice[_,_,_]) => true
      case TP(_, Reflect(slc: ArraySlice[_,_,_], _,_)) => true
      case TP(_, app: ArrayApply[_,_]) => true
      case TP(_, Reflect(app: ArrayApply[_,_], _,_)) => true
      case TP(_, app: DeliteArrayApply[_]) => true
      case TP(_, Reflect(app: DeliteArrayApply[_], _,_)) => true
      case _ => false
    }
  
    // TODO: This can be neater if we use an unapply method for DeliteOpLoopNest
    override def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = d match {
      case op: DeliteOpLoopNest[_] => op.body match {
        case tElem: DeliteTileElem[_,_,_] =>
          var bodyStms: List[Stm] = Nil
          var innerStms: List[Stm] = Nil

          focusBlock(tElem.tile){ bodyStms ++= innerScope }

          bodyStms.last match {
            case lastStm@TP(sr, opI: DeliteOpLoopNest[_]) => opI.body match {
              case rElem: DeliteReduceElem[_] if tElem.rFunc.isEmpty => 

                blocks(rElem).foreach{b => focusBlock(b){innerStms ++= innerScope }}
                innerStms = innerStms.distinct // TODO: Necessary?

                val diffStms = bodyStms filterNot (innerStms contains _)

                if (diffStms.forall{stm => allowWorkDuplication(stm) || stm == lastStm}) {
                  dbgmsg("Found tileAssemble with inner reduce")
                  dbgmsg(diffStms.mkString("\n"))

                  def uneraseTileElem[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](op: DeliteTileElem[_,_,_]): DeliteTileElem[A,T,C]
                    = op.asInstanceOf[DeliteTileElem[A,T,C]]
                  def uneraseReduce[A:Manifest](op: DeliteReduceElem[_]): DeliteReduceElem[A]
                    = op.asInstanceOf[DeliteReduceElem[A]]

                  val tileAssem = uneraseTileElem(tElem)(tElem.mA,tElem.mTA,tElem.mCA)
                  val reduce = uneraseReduce(rElem)(tElem.mTA)
                  Some( flattenTileReduce(op, tileAssem, opI, reduce)(tElem.mA,tElem.mTA,tElem.mCA,ctx) )
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

    def flattenTileReduce[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](opA: DeliteOpLoopNest[_], tElem: DeliteTileElem[A,T,C], opB: DeliteOpLoopNest[_], rElem: DeliteReduceElem[T])(implicit ctx: SourceContext): Exp[C] = {
      val vs = opA.vs ++ opB.vs
      val sizes = f(opA.sizes) ++ f(opB.sizes)
      val strides = f(opA.strides) ++ f(opB.strides)

      val kFunc: List[Block[RangeVector]] = tElem.keys.map(f(_))
      val cond: List[Block[Boolean]] = tElem.cond.map(f(_)) ++ rElem.cond.map(f(_)) // TODO: Not sure if this will actually work in all cases
      val func: Block[T] = f(rElem.func)

      val rV = rElem.rV
      val rFunc: Block[T] = f(rElem.rFunc)
      val buffAlloc: Block[C] = f(tElem.buf.allocBuff)

      val (tDims, unitDims) = opA match {
        case tAssem: TileAssemble[_,_,_] => 
          (f(tAssem.tileDims), tAssem.tileUnitDims)
        case _ => sys.error("FIXME: Unable to extract tileDims and unitDims from tileAssemble")
      }
          
      val tiledReduction = TileAssemble[A,T,C](vs, rV, sizes, buffAlloc, strides, kFunc, cond, func, Some(rFunc), tDims, unitDims)

      dbgmsg("Generated new tileReduction: " + tiledReduction)

      reflectPure( tiledReduction )
    }

    // TODO: Metadata
    def transferMetadata(sub: Exp[Any], orig: Exp[Any], d: Def[Any])(implicit ctx: SourceContext): Unit = {}
  } // End of PatternFlatteningTransformer
}
