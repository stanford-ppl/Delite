package ppl.delite.framework.transform

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.{Config, DeliteApplication}
import scala.virtualization.lms.common._

trait MultiloopSoATransformExp extends DeliteTransform with LoweringTransform with DeliteApplication
  with DeliteOpsExp with DeliteArrayFatExp { self =>

  private val t = new LoweringTransformer
  appendTransformer(t) //AoS to SoA should go last, right before fusion


  //collect elems: unwrap outer struct if return type CA <: Record && perform SoA transform if element type A <: Record
  def unwrapCollect[A, I<:DeliteCollection[A], CA<:DeliteCollection[A]](body: DeliteCollectElem[A,I,CA]) = body.mCA <:< manifest[Record] /*|| (body.mA <:< manifest[Record] && Config.soaEnabled)*/

  def soaCollect[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteCollectElem[A,I,CA]): Exp[CA] = body.allocN match {
    case Block(Def(Struct(tag, elems))) => 
      println("transforming collect elem")
      
      def copyLoop[B:Manifest](f: Block[B]): Exp[DeliteArray[B]] = {
        val allocV = fresh[DeliteArray[B]]
        val elemV = fresh[B]
        val buf_aV = fresh[DeliteArray[B]]
        simpleLoop(size, v, DeliteCollectElem[B,DeliteArray[B],DeliteArray[B]](
          eV = elemV,
          sV = body.sV,
          allocVal = allocV,
          allocN = reifyEffects(DeliteArray[B](body.sV)),
          func = f,
          update = reifyEffects(dc_update(allocV,v,elemV)),
          finalizer = reifyEffects(allocV),
          cond = body.cond,
          par = body.par,
          buf = DeliteBufferElem[B,DeliteArray[B],DeliteArray[B]](
            aV = buf_aV,
            iV = body.buf.iV,
            iV2 = body.buf.iV2,
            append = reifyEffects(dc_append(allocV,v,elemV)),
            setSize = reifyEffects(dc_set_logical_size(allocV,body.sV)),
            allocRaw = reifyEffects(dc_alloc[B,DeliteArray[B]](allocV,body.sV)),
            copyRaw = reifyEffects(dc_copy(buf_aV,body.buf.iV,allocV,body.buf.iV2,body.sV))
          )
        ))
      }

      def soaTransform[B:Manifest](tag: StructTag[B], elems: Seq[(String,Exp[Any])]): Exp[DeliteArray[B]] = {
        val newElems = elems map {
          case (index, e @ Def(Struct(t,es))) => (index, soaTransform(t,es)(e.tp))
          case (index, e) => (index, copyLoop(Block(e))(e.tp))
        }
        struct[DeliteArray[B]](SoaTag(tag,null), newElems)
      }

      val newElems = elems map { e => 
        if (e._2.tp.erasure.getSimpleName == "DeliteArray") {
          body.func match {
            case Block(Def(Struct(t,es))) => (e._1, soaTransform(t,es))
            case _ => (e._1, copyLoop(body.func))
          }
        }
        else if (e._2 == body.sV) { //need a different size
          (e._1, size) //TODO: filters... call array.length?
        }
        else {
          e
        }
      }
      struct[I](tag, newElems).asInstanceOf[Exp[CA]]
      
    case Block(Def(a)) => throw new RuntimeException("error: unable to perform soa transform on " + a)
  }


  //hash collect elems: similar to collect elems; we only transform the values, not the keys
  def unwrapHashCollect[K,V,CV](body: DeliteHashCollectElem[K,V,CV]) = body.mCV <:< manifest[Record] || body.mV <:< manifest[Record]

  def soaHashCollect[K:Manifest,V:Manifest,CV](size: Exp[Int], v: Sym[Int], body: DeliteHashCollectElem[K,V,CV]) = body.valFunc match {
    case Block(Def(Struct(tag, elems))) if Config.soaEnabled =>
      
      def soaTransform[B:Manifest](tag: StructTag[B], elems: Seq[(String,Exp[Any])]): Exp[DeliteArray[DeliteArray[B]]] = {
        val newElems = elems map {
          case (index, e @ Def(Struct(t,es))) => (index, soaTransform(t,es)(e.tp))
          case (index, e) => (index, copyLoop(Block(e))(e.tp))
        }
        struct[DeliteArray[DeliteArray[B]]](SoaTag(SoaTag(tag,null),null), newElems) //TODO: length???
      }

      def copyLoop[B:Manifest](func: Block[B]): Exp[DeliteArray[DeliteArray[B]]] = {
        simpleLoop(size, v, DeliteHashCollectElem[K,B,DeliteArray[DeliteArray[B]]](body.keyFunc, func, body.cond))
      }
      
      soaTransform(tag, elems)
    
    case _ =>
      simpleLoop(size, v, DeliteHashCollectElem[K,V,DeliteArray[DeliteArray[V]]](body.keyFunc, body.valFunc, body.cond))
  }
  
  /*
  //reduce elems: unwrap result elem if A <: Record
  def unwrapReduce[A](body: DeliteReduceElem[A]) = body.mA <:< manifest[Record] && Config.soaEnabled
  
  def soaReduce[A:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteReduceElem[A]) = {
    soaTransform(body.func, body.rFunc, body.zero, body.rV._1, body.rV._2)(body.mA) //TODO: spawn new struct rV and re-bind rFunc

    def soaTransform[B:Manifest](func: Block[B], rFunc: Block[B], zero: Block[B], rv1: Exp[B], rv2: Exp[B]): Exp[B] = func match { 
      case Block(f @ Def(Struct(_,_))) => rFunc match {
        case Block(r @ Def(Struct(tag,elems))) => zero match {
          case Block(z @ Def(Struct(_,_))) => (rv1, rv2) match {
            case (v1 @ Def(Struct(_,_)), v2 @ Def(Struct(_,_))) =>
              val newElems = elems map {
                case (i, Def(Struct(_,_))) => (i, soaTransform(Block(field(f,i)), Block(field(r,i)), Block(field(z,i)), field(v1,i), field(v2,i)))
                case (i, e) => (i, copyLoop(Block(field(f,i)), Block(field(r,i)), Block(field(z,i)), field(v1,i), field(v2,i)))
              }
              struct[B](tag, newElems)
          }
        }
      }      
    }

    def copyLoop[B:Manifest](f: Block[B], r: Block[B], z: Block[B], rv1: Exp[B], rv2: Exp[B]): Exp[B] = {
      simpleLoop(size, v, DeliteReduceElem[B](f, body.cond, z, (rv1.asInstanceOf[Sym[B]], rv2.asInstanceOf[Sym[B]]), r, body.stripFirst))
    }
  }


  //hash reduce elems: similar to reduce elems; we only transform the values, not the keys
  def unwrapHashReduce[A,V,CV](body: DeliteHashReduceElem[A,V,CV]) = body.mCV <:< manifest[Record] || body.mV <:< manifest[Record]

  def soaHashReduce[K:Manifest,V:Manifest,CV](size: Exp[Int], v: Sym[Int], body: DeliteHashReduceElem[K,V,CV]) = {
    soaTransform(body.valFunc, body.rFunc, body.zero, body.rV._1, body.rV._2)

    def soaTransform[B:Manifest](func: Block[B], rFunc: Block[B], zero: Block[B], rv1: Exp[B], rv2: Exp[B]): Exp[DeliteArray[B]] = func match {
      case Block(f @ Def(Struct(_,_))) if Config.soaEnabled => rFunc match {
        case Block(r @ Def(Struct(tag,elems))) => zero match {
          case Block(z @ Def(Struct(_,_))) => (rv1, rv2) match {
            case (v1 @ Def(Struct(_,_)), v2 @ Def(Struct(_,_))) =>
              val newElems = elems map {
                case (i, Def(Struct(_,_))) => (i, soaTransform(Block(field(f,i)), Block(field(r,i)), Block(field(z,i)), field(v1,i), field(v2,i)))
                case (i, e) => (i, copyLoop(Block(field(f,i)), Block(field(r,i)), Block(field(z,i)), field(v1,i), field(v2,i)))
              }
              struct[DeliteArray[B]](SoaTag(tag), newElems)
          }
        }
      }
      
      case _ => //just unwrap the outer struct
        simpleLoop(size, v, DeliteHashReduceElem[K,A,DeliteArray[A]](body.keyFunc, body.valFunc, body.cond, body.zero, body.rV, body.rFunc)) 
    }

    def copyLoop[B:Manifest](f: Block[B], r: Block[B], z: Block[B], rv1: Exp[B], rv2: Exp[B]): Exp[DeliteArray[B]] = {
      simpleLoop(size, v, DeliteHashReduceElem[K,B,DeliteArray[B]](body.keyFunc, f, body.cond, z, (rv1.asInstanceOf[Sym[B]], rv2.asInstanceOf[Sym[B]]), r))
    }
  }
  */

  override def onCreate[A:Manifest](s: Sym[A], d: Def[A]): Exp[A] = (d match {
    case Loop(size, v, body: DeliteCollectElem[_,_,_]) if unwrapCollect(body) => 
      s.atPhase(t)(soaCollect(size,v,body)(body.mA,body.mI,body.mCA).asInstanceOf[Exp[A]])
    case _ => super.onCreate(s,d)
  }).asInstanceOf[Exp[A]]

}
