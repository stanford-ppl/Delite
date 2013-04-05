package ppl.delite.framework.transform

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.{Config, DeliteApplication}
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

trait MultiloopSoATransformExp extends DeliteTransform with LoweringTransform with DeliteApplication
  with DeliteOpsExp with DeliteArrayFatExp with DeliteArrayBufferOpsExp { self =>

  private val t = new ForwardPassTransformer { //TODO: until converged...
    val IR: self.type = self
    override def transformStm(stm: Stm): Exp[Any] = stm match {
      case TP(sym, Loop(size, v, body: DeliteCollectElem[a,i,ca])) => body.par match {
        case ParFlat => soaFlatCollect[a,i,ca](size,v,body)(body.mA,body.mI,body.mCA) match {
          case Some(newSym) => 
            stm match {
              case TP(sym, z:DeliteOpZipWith[_,_,_,_]) if(Config.enableGPUObjReduce) => 
                encounteredZipWith += newSym -> z
                printdbg("Register DeliteOpZipWith symbol ")
                printdbg(newSym)
                printdbg(z)
              case _ => //
            }
            newSym
          case None => super.transformStm(stm)
        }
        case ParBuffer | ParSimpleBuffer => soaBufferCollect[a,i,ca](size,v,body)(body.mA,body.mI,body.mCA) match {
          case Some(newSym) => newSym
          case None => super.transformStm(stm)
        }
      }
      case TP(sym, Loop(size, v, body: DeliteReduceElem[a])) => soaReduce[a](size,v,body)(body.mA) match {
        case Some(newSym) => newSym
        case None => super.transformStm(stm)
      }
      case TP(sym, Loop(size, v, body: DeliteHashReduceElem[k,v,cv])) => soaHashReduce[k,v,cv](size,v,body)(body.mK,body.mV,body.mCV) match {
        case Some(newSym) => newSym
        case None => super.transformStm(stm)
      }
      /*case TP(sym, s: DeliteOpSingleTask[r]) => unwrapSingleTask(s)(s.mR) match {
        case Some(newSym) => newSym
        case None => super.transformStm(stm)
      }*/
      case _ => super.transformStm(stm)
    }
  }

  appendTransformer(t) //AoS to SoA should go last, right before fusion

  private object StructBlock {
    def unapply[A](d: Block[A]) = d match {
      case Block(Def(Struct(tag:StructTag[A],elems))) => Some((tag,elems))
      case Block(Def(Reify(Def(Struct(tag:StructTag[A],elems)),_,_))) => Some((tag,elems))
      case Block(Def(Reify(Def(Reflect(Struct(tag: StructTag[A], elems),_,_)),_,_))) => Some((tag,elems))
      case _ => None
    }
  }

  //collect elems: unwrap outer struct if return type is a Struct && perform SoA transform if element type is a Struct
  def soaFlatCollect[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteCollectElem[A,I,CA]): Option[Exp[CA]] = t(body.allocN) match {
    case StructBlock(tag,elems) =>       
      def copyLoop[B:Manifest](f: Block[B]): Exp[DeliteArray[B]] = {
        val allocV = reflectMutableSym(fresh[DeliteArray[B]])
        val elemV = fresh[B]
        val sizeV = fresh[Int]
        val buf_aV = fresh[DeliteArray[B]]
        val buf_iV = fresh[Int]
        val buf_iV2 = fresh[Int]
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
            appendable = reifyEffects(dc_appendable(allocV,tv,elemV)),
            append = reifyEffects(dc_append(allocV,tv,elemV)),
            setSize = reifyEffects(dc_set_logical_size(allocV,sizeV)),
            allocRaw = reifyEffects(dc_alloc[B,DeliteArray[B]](allocV,sizeV)),
            copyRaw = reifyEffects(dc_copy(buf_aV,buf_iV,allocV,buf_iV2,sizeV))
          )
        ))
      }

      def soaTransform[B:Manifest](tag: StructTag[B], elems: Seq[(String,Exp[Any])]): Exp[DeliteArray[B]] = {
        val newElems = elems map {
          case (index, e @ Def(Struct(t,es))) => (index, soaTransform(t,es)(e.tp))
          case (index, Def(DeliteArrayApply(x,iv))) if (iv == v) => (index, x) //eliminate identify function loop
          case (index, e) => (index, copyLoop(Block(e))(e.tp))
        }
        struct[DeliteArray[B]](SoaTag(tag,t(size)), newElems)
      }

      val dataField = dc_data_field(getBlockResult(t(body.allocN)))
      val sizeField = dc_size_field(getBlockResult(t(body.allocN)))

      if (dataField == "") printlog("unable to transform collect elem: no data field defined for " + manifest[I].toString); None

      val newLoop = t(body.func) match {
        case Block(Def(Struct(ta,es))) if Config.soaEnabled => soaTransform(ta,es)
        case _ => copyLoop(t(body.func))
      }
      val newElems = elems.map { e => 
        if (e._1 == dataField) (e._1, newLoop)
        else if (e._1 == sizeField) (e._1, t(size))
        else if (e._2.tp.erasure == classOf[Var[_]]) (e._1, readVar(Variable(t(e._2).asInstanceOf[Exp[Var[Any]]]))(mtype(e._2.tp.typeArguments(0)),implicitly[SourceContext]))
        else (e._1, t(e._2))
      }
      val res = struct[I](tag, newElems)

      t.subst += t(body.allocVal) -> res 
      Some(getBlockResult(t(body.finalizer)))
      
    case Block(Def(Reify(Def(a),_,_))) => printlog("unable to transform collect elem: found " + a); None
    case _ => None
  }

  private def getBlockResult[A](s: Block[A]): Exp[A] = s match {
    case Block(Def(Reify(x, _, _))) => x
    case Block(x) => x
  }


  //this is extremely unfortunate code duplication, but trying to parameterize the collection type causes manifest issues
  def soaBufferCollect[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteCollectElem[A,I,CA]): Option[Exp[CA]] = t(body.allocN) match {
    case StructBlock(tag,elems) =>       
      def copyLoop[B:Manifest](f: Block[B]): Exp[DeliteArrayBuffer[B]] = {
        val allocV = reflectMutableSym(fresh[DeliteArrayBuffer[B]])
        val elemV = fresh[B]
        val sizeV = fresh[Int]
        val buf_aV = fresh[DeliteArrayBuffer[B]]
        val buf_iV = fresh[Int]
        val buf_iV2 = fresh[Int]
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
            appendable = reifyEffects(dc_appendable(allocV,tv,elemV)),
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
          case (index, e) => (index, darray_buffer_raw_data(copyLoop(Block(e))(e.tp))(e.tp))
        }
        struct[DeliteArray[B]](SoaTag(tag, newElems(0)._2.length), newElems) //TODO: we want to know the output size without having to pick one of the returned arrays arbitrarily (prevents potential DCE)... can we just grab the size out of the activation record somehow?
      }

      val dataField = dc_data_field(getBlockResult(t(body.allocN)))
      val sizeField = dc_size_field(getBlockResult(t(body.allocN)))

      if (dataField == "") printlog("unable to transform collect elem: no data field defined for " + manifest[I].toString); None

      val newLoop = t(body.func) match {
        case Block(Def(Struct(ta,es))) if Config.soaEnabled => soaTransform(ta,es)
        case _ => darray_buffer_raw_data(copyLoop(t(body.func)))
      }
      val newElems = elems.map { e => 
        if (e._1 == dataField) (e._1, newLoop)
        else if (e._1 == sizeField) (e._1, newLoop.length)
        else if (e._2.tp.erasure == classOf[Var[_]]) (e._1, readVar(Variable(t(e._2).asInstanceOf[Exp[Var[Any]]]))(mtype(e._2.tp.typeArguments(0)),implicitly[SourceContext]))
        else (e._1, t(e._2))
      }
      val res = struct[I](tag, newElems)

      t.subst += t(body.allocVal) -> res 
      Some(getBlockResult(t(body.finalizer)))

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
  
  
  //reduce elems: unwrap result if elem is a Struct
  def soaReduce[A:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteReduceElem[A]): Option[Exp[A]] = t(body.func) match {
    case StructBlock(tag,elems) =>       
      def copyLoop[B:Manifest](f: Block[B], r: Block[B], z: Block[B], rv1: Exp[B], rv2: Exp[B]): Exp[B] = {
        simpleLoop(t(size), t(v).asInstanceOf[Sym[Int]], DeliteReduceElem[B](
          func = t(f),
          cond = body.cond.map(t(_)),
          zero = t(z),
          accInit = reifyEffects(fatal(unit("accInit not transformed")))(manifest[B]), //unwrap this as well to support mutable reduce
          rV = (t(rv1).asInstanceOf[Sym[B]], t(rv2).asInstanceOf[Sym[B]]),
          rFunc = t(r),
          stripFirst = !isPrimitiveType(manifest[B])
        ))
      }

      def soaTransform[B:Manifest](func: Block[B], rFunc: Block[B], zero: Block[B]): Exp[B] = func match {
        case Block(f @ Def(Struct(_,_))) => rFunc match {
          case Block(r @ Def(Struct(tag,elems))) => zero match { //TODO: mutable reduce? reflectMutableSym on rV causes issues...
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
              struct[B](tag, newElems)
            case Block(Def(a)) => 
              Console.println(a)
              sys.error("transforming reduce elem but zero is not a struct and func is")
          }
          case Block(Def(a)) =>
            Console.println(a) 
            sys.error("transforming reduce elem but rFunc is not a struct and func is")
        }
        case Block(Def(a)) => 
          Console.println(a)
          sys.error("transforming reduce elem but func is not a struct and rFunc is")
      }

      Some(soaTransform(t(body.func), t(body.rFunc), t(body.zero)))
      
    case _ => None
  }


  //hash reduce elems: similar to reduce elems; we only transform the values, not the keys
  def soaHashReduce[K:Manifest,V:Manifest,CV:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteHashReduceElem[K,V,CV]): Option[Exp[CV]] = t(body.alloc) match {
    case StructBlock(tag,elems) =>
      def copyLoop[B:Manifest](f: Block[B], r: Block[B], z: Block[B], rv1: Exp[B], rv2: Exp[B]): Exp[DeliteArray[B]] = {
        val allocV = fresh[DeliteArray[B]]
        simpleLoop(t(size), t(v).asInstanceOf[Sym[Int]], DeliteHashReduceElem[K,B,DeliteArray[B]](
          allocVal = allocV,
          alloc = reifyEffects(DeliteArray.imm[B](unit(0))),
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
              struct[DeliteArray[B]](SoaTag(tag, newElems(0)._2.length), newElems)
            case Block(Def(a)) => 
              Console.println(a)
              sys.error("transforming hashReduce elem but zero is not a struct and valFunc is")
          }
          case Block(Def(a)) =>
            Console.println(a) 
            sys.error("transforming hashReduce elem but rFunc is not a struct and valFunc is")        
        }
        case Block(Def(a)) => 
          Console.println(a)
          sys.error("transforming hashReduce elem but valFunc is not a struct and rFunc is")      
      }

      val dataField = dc_data_field(getBlockResult(t(body.alloc)).asInstanceOf[Exp[DeliteCollection[V]]]) //TODO: ?
      val sizeField = dc_size_field(getBlockResult(t(body.alloc)).asInstanceOf[Exp[DeliteCollection[V]]])

      if (dataField == "") printlog("unable to transform hashReduce elem: no data field defined for " + manifest[CV].toString); None

      val newLoop = t(body.valFunc) match {
        case Block(Def(Struct(ta,es))) if Config.soaEnabled => soaTransform(t(body.valFunc), t(body.rFunc), t(body.zero))
        case _ => copyLoop(t(body.valFunc), t(body.rFunc), t(body.zero), t(body.rV._1), t(body.rV._2))
      }
      val newElems = elems.map { e => 
        if (e._1 == dataField) (e._1, newLoop)
        else if (e._1 == sizeField) (e._1, newLoop.length)
        else (e._1, t(e._2))
      }
      Some(struct[CV](tag, newElems))
      
    case Block(Def(Reify(Def(a),_,_))) => printlog("unable to transform hashReduce elem: found " + a); None
    case _ => None
  }

  //case class FieldFromEffect[T](field: Exp[T], effects: Block[Unit]) extends Def[T]
  //val block = Block(Reify(Const(()), es, u))
  //struct(tag, elems.map(e => (e._1, effectField(e._2, block))))

  def unwrapSingleTask[A:Manifest](s: DeliteOpSingleTask[A]): Option[Exp[A]] = s.block match {
    case Block(Def(e@Struct(_,_))) => Console.println("unwrapped sequential: " + e.toString); Some(e)
    //case Block(Def(Reify(Def(Struct(_,_)),es,u))) => Console.println("unwrapped sequential with reify: " + e.toString)
    case Block(Def(a)) => Console.println("failed on sequential: " + a.toString); None
  }

}

trait LoopSoAOpt extends BaseGenLoopsFat with LoopFusionOpt {
  val IR: DeliteOpsExp
  import IR._

  //pre-fuse any loops that have been split by SoA transform
  //such loops have the same 'size' sym, the same 'v' sym, and are in the same scope
  override def focusExactScopeFat[A](resultB: List[Block[Any]])(body: List[Stm] => A): A = {
    val result = resultB.map(getBlockResultFull) flatMap { case Combine(xs) => xs case x => List(x) }
    val currentScope = innerScope
    val levelScope = getExactScope(currentScope)(result) //top-level scope
    
    val loops = levelScope collect { case t@TTP(_, _, SimpleFatLoop(_,_,_)) => t }
    val splitLoops = loops groupBy { case TTP(_, _, SimpleFatLoop(size,v,bodies)) => v }

    val fusedLoops = splitLoops map {
      case (v, l) if l.length == 1 => l.head
      case (v, l) => 
        val size = l.map(_.rhs.asInstanceOf[AbstractFatLoop].size) reduceLeft { (s1,s2) => assert(s1 == s2); s1 }
        val t = TTP(l.flatMap(_.lhs), l.flatMap(_.mhs), SimpleFatLoop(size, v, l.flatMap(_.rhs.asInstanceOf[AbstractFatLoop].body)))
        printlog("fusing split SoA loop: " + t.toString)
        t
    }

    val remainder = currentScope diff loops
    val newScope = remainder ++ fusedLoops
    innerScope = getSchedule(newScope)(result) //get the order right
    super.focusExactScopeFat(resultB)(body) //call LoopFusionOpt
  }

}
