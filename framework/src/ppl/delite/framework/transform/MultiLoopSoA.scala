package ppl.delite.framework.transform

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.{Config, DeliteApplication}
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

trait MultiloopSoATransformExp extends DeliteTransform with LoweringTransform with DeliteApplication
  with DeliteOpsExp with DeliteArrayFatExp with DeliteArrayBufferOpsExp { self =>

  private val t = new ForwardPassTransformer {
    val IR: self.type = self
    override def transformStm(stm: Stm): Exp[Any] = stm match {
      case TP(sym, Loop(size, v, body: DeliteCollectElem[a,i,ca])) => body.par match {
        case ParFlat => soaFlatCollect[a,i,ca](size,v,body)(body.mA,body.mI,body.mCA) match {
          case Some(newSym) => newSym
          case None => super.transformStm(stm)
        }
        case ParBuffer => soaBufferCollect[a,i,ca](size,v,body)(body.mA,body.mI,body.mCA) match {
          case Some(newSym) => newSym
          case None => super.transformStm(stm)
        }
      }
      case TP(sym, Loop(size, v, body: DeliteHashReduceElem[k,v,cv])) => soaHashReduce[k,v,cv](size,v,body)(body.mK,body.mV,body.mCV) match {
        case Some(newSym) => newSym
        case None => super.transformStm(stm)
      }
      case _ => super.transformStm(stm)
    }
  }

  appendTransformer(t) //AoS to SoA should go last, right before fusion


  //collect elems: unwrap outer struct if return type CA <: Record && perform SoA transform if element type A <: Record
  def soaFlatCollect[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteCollectElem[A,I,CA]): Option[Exp[CA]] = t(body.finalizer) match {
    case Block(Def(Reify(Def(Struct(tag:StructTag[CA], elems)),_,_))) =>       
      def copyLoop[B:Manifest](f: Block[B]): Exp[DeliteArray[B]] = {
        val allocV = reflectMutableSym(fresh[DeliteArray[B]])
        val elemV = fresh[B]
        val sizeV = t(body.sV).asInstanceOf[Sym[Int]]
        val buf_aV = fresh[DeliteArray[B]]
        val buf_iV = t(body.buf.iV).asInstanceOf[Sym[Int]]
        val buf_iV2 = t(body.buf.iV2).asInstanceOf[Sym[Int]]
        val tv = t(v).asInstanceOf[Sym[Int]]
        simpleLoop(t(size), tv, DeliteCollectElem[B,DeliteArray[B],DeliteArray[B]](
          eV = elemV,
          sV = sizeV,
          allocVal = allocV,
          allocN = reifyEffects(DeliteArray[B](sizeV)),
          func = t(f),
          update = reifyEffects(dc_update(allocV,tv,elemV)),
          finalizer = reifyEffects(allocV),
          cond = body.cond.map(t(_)),
          par = body.par,
          buf = DeliteBufferElem[B,DeliteArray[B],DeliteArray[B]](
            aV = buf_aV,
            iV = buf_iV,
            iV2 = buf_iV2,
            append = reifyEffects(dc_append(allocV,tv,elemV)),
            setSize = reifyEffects(dc_set_logical_size(allocV,sizeV)),
            allocRaw = reifyEffects(dc_alloc[B,DeliteArray[B]](allocV,sizeV)),
            copyRaw = reifyEffects(dc_copy(buf_aV,buf_iV,allocV,buf_iV2,sizeV))
          )
        ))
      }

      def soaTransform[B:Manifest](tag: StructTag[B], elems: Seq[(String,Exp[Any])], length: Exp[Int]): Exp[DeliteArray[B]] = {
        val newElems = elems map {
          case (index, e @ Def(Struct(t,es))) => (index, soaTransform(t,es,length)(e.tp))
          case (index, Def(DeliteArrayApply(x,iv))) if (iv == v) => (index, x) //eliminate identify function loop
          case (index, e) => (index, copyLoop(Block(e))(e.tp))
        }
        struct[DeliteArray[B]](SoaTag(tag,length), newElems)
      }

      val newLoop = t(body.func) match {
        case Block(Def(Struct(ta,es))) if Config.soaEnabled => soaTransform(ta,es,t(size))
        case _ => copyLoop(t(body.func))
      }
      val newElems = elems.map { e =>
        if (e._2.tp.erasure.getSimpleName == "DeliteArray") (e._1, newLoop)
        else (e._1, t(e._2))
      }
      Some(struct[CA](tag, newElems))
      
    case Block(Def(Reify(Def(a),_,_))) => printlog("unable to transform collect elem: found " + a); None
    case _ => None
  }

  //this is extremely unfortunate code duplication, but trying to parameterize the collection type causes manifest issues
  def soaBufferCollect[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteCollectElem[A,I,CA]): Option[Exp[CA]] = t(body.finalizer) match {
    case Block(Def(Reify(Def(Struct(tag:StructTag[CA], elems)),_,_))) =>       
      def copyLoop[B:Manifest](f: Block[B]): Exp[DeliteArrayBuffer[B]] = {
        val allocV = reflectMutableSym(fresh[DeliteArrayBuffer[B]])
        val elemV = fresh[B]
        val sizeV = t(body.sV).asInstanceOf[Sym[Int]]
        val buf_aV = fresh[DeliteArrayBuffer[B]]
        val buf_iV = t(body.buf.iV).asInstanceOf[Sym[Int]]
        val buf_iV2 = t(body.buf.iV2).asInstanceOf[Sym[Int]]
        val tv = t(v).asInstanceOf[Sym[Int]]
        simpleLoop(t(size), tv, DeliteCollectElem[B,DeliteArrayBuffer[B],DeliteArrayBuffer[B]](
          eV = elemV,
          sV = sizeV,
          allocVal = allocV,
          allocN = reifyEffects(DeliteArrayBuffer[B](sizeV)),
          func = t(f),
          update = reifyEffects(dc_update(allocV,tv,elemV)),
          finalizer = reifyEffects(allocV),
          cond = body.cond.map(t(_)),
          par = body.par,
          buf = DeliteBufferElem[B,DeliteArrayBuffer[B],DeliteArrayBuffer[B]](
            aV = buf_aV,
            iV = buf_iV,
            iV2 = buf_iV2,
            append = reifyEffects(dc_append(allocV,tv,elemV)),
            setSize = reifyEffects(dc_set_logical_size(allocV,sizeV)),
            allocRaw = reifyEffects(dc_alloc[B,DeliteArrayBuffer[B]](allocV,sizeV)),
            copyRaw = reifyEffects(dc_copy(buf_aV,buf_iV,allocV,buf_iV2,sizeV))
          )
        ))
      }

      def soaTransform[B:Manifest](tag: StructTag[B], elems: Seq[(String,Exp[Any])]): Exp[DeliteArray[B]] = {
        val newElems = elems map {
          case (index, e @ Def(Struct(t,es))) => (index, soaTransform(t,es)(e.tp))
          case (index, e) => (index, darray_buffer_raw_data(copyLoop(Block(e))(e.tp)))
        }
        struct[DeliteArray[B]](SoaTag(tag, newElems(0)._2.length), newElems) //TODO: better way to get the length?
      }

      val newLoop = t(body.func) match {
        case Block(Def(Struct(t,es))) if Config.soaEnabled => soaTransform(t,es)
        case _ => darray_buffer_raw_data(copyLoop(t(body.func)))
      }
      val newElems = elems.map { e => 
        if (e._2.tp.erasure.getSimpleName == "DeliteArray") (e._1, newLoop)
        else (e._1, t(e._2))
      }
      Some(struct[CA](tag, newElems))

    case Block(Def(Reify(Def(a),_,_))) => printlog("unable to transform collect elem: found " + a); None   
    case _ => None
  }


  //hash collect elems: similar to collect elems; we only transform the values, not the keys
  /* def soaHashCollect[K:Manifest,V:Manifest,CV](size: Exp[Int], v: Sym[Int], body: DeliteHashCollectElem[K,V,CV]) = body.valFunc match {
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
  } */
  
  
  //reduce elems: unwrap result elem if A <: Record  
  /* def soaReduce[A:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteReduceElem[A]) = {
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
  } */


  //hash reduce elems: similar to reduce elems; we only transform the values, not the keys
  def soaHashReduce[K:Manifest,V:Manifest,CV:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteHashReduceElem[K,V,CV]): Option[Exp[CV]] = body /*t(body.finalizer)*/ match {
    case _ => //case Block(Def(Reify(Def(Struct(tag:StructTag[CV], elems)),_,_))) =>
      def copyLoop[B:Manifest](f: Block[B], r: Block[B], z: Block[B], rv1: Exp[B], rv2: Exp[B]): Exp[DeliteArray[B]] = {
        simpleLoop(t(size), t(v).asInstanceOf[Sym[Int]], DeliteHashReduceElem[K,B,DeliteArray[B]](
          keyFunc = t(body.keyFunc),
          valFunc = t(f),
          cond = body.cond.map(t(_)),
          zero = t(z),
          rV = (t(rv1).asInstanceOf[Sym[B]], t(rv2).asInstanceOf[Sym[B]]),
          rFunc = t(r)
        ))
      }

      def soaTransform[B:Manifest](func: Block[B], rFunc: Block[B], zero: Block[B]): Exp[DeliteArray[B]] = func match {
        case Block(f @ Def(Struct(_,_))) => rFunc match {
          case Block(r @ Def(Struct(tag,elems))) => zero match {
            case Block(z @ Def(Struct(_,_))) =>
              val rv1 = struct[B](tag, elems.map(e => (e._1, fresh(e._2.tp))))
              val rv2 = struct[B](tag, elems.map(e => (e._1, fresh(e._2.tp))))
              t.subst += body.rV._1 -> rv1
              t.subst += body.rV._2 -> rv2
              val ctx = implicitly[SourceContext]
              val newElems = elems map {
                case (i, e @ Def(Struct(_,_))) => (i, soaTransform(Block(field(f,i)(e.tp,ctx)), Block(field(r,i)(e.tp,ctx)), Block(field(z,i)(e.tp,ctx)))(e.tp))
                case (i, e) => (i, copyLoop(Block(field(f,i)(e.tp,ctx)), Block(field(r,i)(e.tp,ctx)), Block(field(z,i)(e.tp,ctx)), field(rv1,i)(e.tp,ctx), field(rv2,i)(e.tp,ctx))(e.tp))
              }
              struct[DeliteArray[B]](SoaTag(tag,newElems(0)._2.length), newElems) //TODO: which length?
            case Block(Def(a)) => 
              Console.println(a)
              sys.error("bad zero")
          }
          case Block(Def(a)) =>
            Console.println(a) 
            sys.error("bad rFunc")
        }
        case Block(Def(a)) => 
          Console.println(a)
          sys.error("bad valFunc")
      }

      val newLoop = t(body.valFunc) match {
        case Block(Def(Struct(ta,es))) if Config.soaEnabled => soaTransform(t(body.valFunc), t(body.rFunc), t(body.zero))
        case _ => copyLoop(t(body.valFunc), t(body.rFunc), t(body.zero), t(body.rV._1), t(body.rV._2))
      }
      /* val newElems = elems.map { e =>
        if (e._2.tp.erasure.getSimpleName == "DeliteArray") (e._1, newLoop)
        else (e._1, t(e._2))
      }
      Some(struct[CV](tag, newElems))*/
      Some(struct[CV](ClassTag[CV]("Collection"), "data"->newLoop, "size"->newLoop.length))
      
    //case Block(Def(Reify(Def(a),_,_))) => printlog("unable to transform collect elem: found " + a); None
    //case _ => None
  }


  def unwrapSingleTask[A:Manifest](s: DeliteOpSingleTask[A])(orig: Exp[A]): Option[Exp[A]] = s.block match {
    case Block(Def(e@Struct(_,_))) => Some(e)
    case Block(Def(Reify(e@Struct(_,_),es,u))) => Block(Reify(Const(()), es, u)); Some(e)
    case _ => None
  }

}
