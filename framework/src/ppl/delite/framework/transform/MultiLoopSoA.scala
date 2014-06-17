package ppl.delite.framework.transform

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.Util._
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

//NOTE: unwrapping reduces isn't always safe (rFunc may not be separable); only for use in DSLs with appropriate restrictions
trait MultiloopSoATransformWithReduceExp extends MultiloopSoATransformExp {

  //TODO: merge this into standard SoA transform and check safety
  override def transformLoop(stm: Stm): Option[Exp[Any]] = stm match {
    case TP(sym, r:DeliteOpReduceLike[_]) if r.mutable => None // mutable reduces don't work yet
    case TP(sym, Loop(size, v, body: DeliteReduceElem[a])) => soaReduce[a](size,v,body)(body.mA)
    case TP(sym, Loop(size, v, body: DeliteHashReduceElem[k,v,i,cv])) => soaHashReduce[k,v,i,cv](size,v,body)(body.mK,body.mV,body.mI,body.mCV)
    case _ => super.transformLoop(stm)
  }

}

trait MultiloopSoATransformExp extends DeliteTransform with LoweringTransform with DeliteApplication
  with DeliteOpsExp with DeliteArrayFatExp { self =>

  private val t = new ForwardPassTransformer {
    val IR: self.type = self
    override def transformStm(stm: Stm): Exp[Any] = transformLoop(stm) match {
      case Some(newSym) => newSym
      case None => super.transformStm(stm)
    }

    def replace[A](oldExp: Exp[A], newExp: Exp[A]) {
      subst += this(oldExp) -> newExp
      printlog("replacing " + oldExp.toString + " with " + newExp.toString)
    }
  }

  def transformLoop(stm: Stm): Option[Exp[Any]] = stm match {
      case TP(sym, Loop(size, v, body: DeliteCollectElem[a,i,ca])) => 
        val pos: SourceContext = if (sym.pos.length > 0) sym.pos(0) else null
        soaCollect[a,i,ca](size,v,body)(body.mA,body.mI,body.mCA,pos) match {
      case s@Some(newSym) => stm match {
        case TP(sym, z:DeliteOpZipWith[_,_,_,_]) if(Config.enableGPUObjReduce) => //TODO: remove this
          encounteredZipWith += newSym -> z
          printdbg("Register DeliteOpZipWith symbol: " + z.toString + " -> " + newSym.toString)
          s
        case _ => s
      }
      case None => None
    }
    //TODO: unwrap SingleTask / Composite nodes to expose the struct return type (have to handle Reifys)
    //case TP(sym, s: DeliteOpSingleTask[r]) => unwrapSingleTask(s)(s.mR)
    case _ => None
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
  def soaCollect[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteCollectElem[A,I,CA])(implicit pos: SourceContext): Option[Exp[CA]] = {
    val alloc = t(body.buf.alloc)
    alloc match {
    case StructBlock(tag,elems) =>
      val condT = body.cond.map(t(_))
      def copyLoop[B:Manifest](f: Block[B]): Exp[DeliteArray[B]] = f match {
        case Block(Def(DeliteArrayApply(x,iv))) if (iv.equals(v) && body.par == ParFlat) => 
          x.asInstanceOf[Exp[DeliteArray[B]]] //eliminate identity function loop
        case _ =>
          val allocV = reflectMutableSym(fresh[DeliteArray[B]])
          val elemV = fresh[B]
          val sizeV = fresh[Int]
          val buf_aV = fresh[DeliteArray[B]]
          val buf_iV = fresh[Int]
          val buf_iV2 = fresh[Int]
          val tv = t(v).asInstanceOf[Sym[Int]]
          simpleLoop(t(size), tv, DeliteCollectElem[B,DeliteArray[B],DeliteArray[B]](
            func = f,
            cond = condT,
            par = body.par,
            buf = DeliteBufferElem[B,DeliteArray[B],DeliteArray[B]](
              eV = elemV,
              sV = sizeV,
              allocVal = allocV,
              aV2 = buf_aV,
              iV = buf_iV,
              iV2 = buf_iV2,
              alloc = reifyEffects(DeliteArray[B](sizeV)),
              apply = unusedBlock,
              update = reifyEffects(dc_update(allocV,tv,elemV)),
              appendable = reifyEffects(dc_appendable(allocV,tv,elemV)),
              append = reifyEffects(dc_append(allocV,tv,elemV)),
              setSize = reifyEffects(dc_set_logical_size(allocV,sizeV)),
              allocRaw = reifyEffects(dc_alloc[B,DeliteArray[B]](allocV,sizeV)),
              copyRaw = reifyEffects(dc_copy(buf_aV,buf_iV,allocV,buf_iV2,sizeV)),
              finalizer = reifyEffects(allocV)
            ),
            numDynamicChunks = body.numDynamicChunks
          ))
      }

      def soaTransform[B:Manifest](tag: StructTag[B], elems: Seq[(String,Exp[Any])]): Exp[DeliteArray[B]] = {
        val newElems = elems map {
          case (index, e @ Def(Struct(t,es))) => (index, soaTransform(t,es)(e.tp))
          case (index, e) => (index, copyLoop(Block(e))(e.tp))
        }
        val sz = body.par match {
          case ParFlat => t(size)
          case ParBuffer | ParSimpleBuffer => 

          newElems(0)._2.length //TODO: we want to know the output size without having to pick one of the returned arrays arbitrarily (prevents potential DCE)... can we just grab the size out of the activation record somehow?
          /* //determine output size by counting:
          val rV1 = fresh[Int]
          val rV2 = fresh[Int]

          simpleLoop(t(size), t(v).asInstanceOf[Sym[Int]], DeliteReduceElem[Int](
            func = reifyEffects(unit(1)),
            cond = condT,
            zero = reifyEffects(unit(0)),
            accInit = reifyEffects(unit(0)),
            rV = (rV1,rV2),
            rFunc = reifyEffects(int_plus(rV1,rV2)),
            stripFirst = false
          ))*/
        }
        struct[DeliteArray[B]](SoaTag(tag,sz), newElems)
      }

      val dataField = dc_data_field(t.getBlockResult(alloc))
      val sizeField = dc_size_field(t.getBlockResult(alloc))

      if (dataField == "" && !isSubtype(manifest[I].erasure,classOf[DeliteArray[_]])) {
        printlog("unable to transform collect elem: no data field defined for " + manifest[I].toString)
        return None
      }

      if (body.iFunc.nonEmpty) {
        printlog("unable to transform flatmap elem")
        return None
      }

      val newLoop = t(body.func) match {
        case u@Block(Def(a@Struct(ta,es))) if Config.soaEnabled => 
          printlog("*** SOA " + u + " / " + a)
          soaTransform(ta,es)
        case f2@Block(Def(a)) => 
          printlog("*** Unable to SOA " + f2 + " / " + a)
          copyLoop(f2)
        case f2 => copyLoop(f2)
      }

      val res = if (isSubtype(manifest[I].erasure, classOf[DeliteArray[_]])) {
        newLoop
      }
      else {
        val newElems = elems.map {
          case (df, _) if df == dataField => (df, newLoop)
          case (sf, _) if (sf == sizeField && body.par == ParFlat) => (sf, t(size))
          case (sf, _) if (sf == sizeField && (body.par == ParBuffer || body.par == ParSimpleBuffer)) => (sf, newLoop.length)
          case (f, Def(Reflect(NewVar(init),_,_))) => (f, t(init))
          case (f,v) => (f, t(v))
        }
        struct[I](tag, newElems)
      }

      t.replace(body.buf.allocVal, res) // TODO: use withSubstScope
      printlog("successfully transformed collect elem with type " + manifest[I].toString + " to " + res.toString)
      Some(t.getBlockResult(t(body.buf.finalizer)))
      
    case Block(Def(Reify(s@Def(a),_,_))) => printlog("unable to transform collect elem: found " + s.toString + ": " + a + " with type " + manifest[I].toString); None
    case a => printlog("unable to transform collect elem: found " + a + " with type " + manifest[I].toString); None
  }}


  //hash collect elems: similar to collect elems; we only transform the values, not the keys
  //TODO
  
  
  //reduce elems: unwrap result if elem is a Struct
  def soaReduce[A:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteReduceElem[A]): Option[Exp[A]] = t(body.func) match {
    case StructBlock(tag,elems) =>       
      def copyLoop[B:Manifest](f: Block[B], r: Block[B], z: Block[B], rv1: Exp[B], rv2: Exp[B]): Exp[B] = {
        simpleLoop(t(size), t(v).asInstanceOf[Sym[Int]], DeliteReduceElem[B](
          func = f,
          cond = body.cond.map(t(_)),
          zero = z,
          accInit = reifyEffects(fatal(unit("accInit not transformed")))(manifest[B]), //unwrap this as well to support mutable reduce
          rV = (rv1.asInstanceOf[Sym[B]], rv2.asInstanceOf[Sym[B]]),
          rFunc = r,
          stripFirst = !isPrimitiveType(manifest[B]),
          numDynamicChunks = body.numDynamicChunks
        ))
      }

      def soaTransform[B:Manifest](func: Block[B], rFunc: Block[B], zero: Block[B], rv1: Exp[B], rv2: Exp[B]): Exp[B] = func match {
        case Block(f @ Def(Struct(_,_))) => rFunc match {
          case Block(r @ Def(Struct(tag,elems))) => zero match { //TODO: mutable reduce? reflectMutableSym on rV causes issues...
            case Block(z @ Def(Struct(_,_))) =>
              val ctx = implicitly[SourceContext]
              val newElems = elems map {
                case (i, e @ Def(Struct(_,_))) => (i, soaTransform(Block(field(f,i)(e.tp,ctx)), Block(field(r,i)(e.tp,ctx)), Block(field(z,i)(e.tp,ctx)), field(rv1,i)(e.tp,ctx), field(rv2,i)(e.tp,ctx))(e.tp))
                case (i, c@Const(_)) => (i, c)
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

      val (rv1, rv2) = unwrapRV(tag, elems, body.rV)
      val res = soaTransform(t(body.func), t(body.rFunc), t(body.zero), rv1, rv2)
      printlog("successfully transformed reduce elem with type " + manifest[A])
      Some(res)
      
    case a => printlog("unable to transform reduce elem: found " + a + " with type " + manifest[A]); None
  }

  private def unwrapRV[B:Manifest](tag: StructTag[B], elems: Seq[(String,Exp[Any])], rv: (Exp[B], Exp[B])): (Exp[B], Exp[B]) = {
    def makeRV[B:Manifest](tag: StructTag[B], elems: Seq[(String,Exp[Any])]): Exp[B] = {
      val newElems = elems.map {
        case (index, e @ Def(Struct(t,es))) => (index, makeRV(t,es)(e.tp))
        case (index, e) => (index, fresh(e.tp))
      }
      struct[B](tag, newElems)
    }

    val new_rv1 = makeRV(tag,elems)
    val new_rv2 = makeRV(tag,elems)
    t.replace(rv._1, new_rv1)
    t.replace(rv._2, new_rv2)
    (new_rv1, new_rv2)
  }


  //hash reduce elems: similar to reduce elems; we only transform the values, not the keys
  def soaHashReduce[K:Manifest,V:Manifest,I:Manifest,CV:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteHashReduceElem[K,V,I,CV]): Option[Exp[CV]] = {
    val alloc = t(body.buf.alloc) 
    alloc match {
    case StructBlock(tag,elems) =>
      val condT = body.cond.map(t(_))
      val keyT = t(body.keyFunc)
      val valT = t(body.valFunc)
      val tv = t(v).asInstanceOf[Sym[Int]]
      val sizeT = t(size)

      def copyLoop[B:Manifest](f: Block[B], r: Block[B], z: Block[B], rv1: Exp[B], rv2: Exp[B]): Exp[DeliteArray[B]] = {
        val allocV = reflectMutableSym(fresh[DeliteArray[B]])
        val indexV = fresh[Int]
        val sizeV = fresh[Int]
        val elemV = fresh[B]
        simpleLoop(sizeT, tv, DeliteHashReduceElem[K,B,DeliteArray[B],DeliteArray[B]](
          keyFunc = keyT,
          valFunc = f,
          cond = condT,
          zero = z,
          rV = (rv1.asInstanceOf[Sym[B]], rv2.asInstanceOf[Sym[B]]),
          rFunc = r,
          buf = DeliteBufferElem[B,DeliteArray[B],DeliteArray[B]](
            eV = elemV,
            sV = sizeV,
            iV = indexV,
            iV2 = unusedSym,
            allocVal = allocV,
            aV2 = unusedSym,
            alloc = reifyEffects(DeliteArray[B](sizeV)),
            apply = reifyEffects(dc_apply(allocV,indexV)),
            update = reifyEffects(dc_update(allocV,indexV,elemV)),
            appendable = unusedBlock,
            append = reifyEffects(dc_append(allocV,tv,elemV)),
            setSize = reifyEffects(dc_set_logical_size(allocV,sizeV)),
            allocRaw = unusedBlock,
            copyRaw = unusedBlock,
            finalizer = reifyEffects(allocV)
          ),
          numDynamicChunks = body.numDynamicChunks
        ))
      }

      def soaTransform[B:Manifest](func: Block[B], rFunc: Block[B], zero: Block[B], rv1:Exp[B], rv2: Exp[B]): Exp[DeliteArray[B]] = func match {
        case Block(f @ Def(vf@Struct(_,_))) => rFunc match {
          case Block(r @ Def(Struct(tag,elems))) => zero match {
            case Block(z @ Def(Struct(_,_))) =>
              val ctx = implicitly[SourceContext]
              val newElems = elems map {
                case (i, e @ Def(Struct(_,_))) => (i, soaTransform(Block(field(f,i)(e.tp,ctx)), Block(field(r,i)(e.tp,ctx)), Block(field(z,i)(e.tp,ctx)), field(rv1,i)(e.tp,ctx), field(rv2,i)(e.tp,ctx))(e.tp))
                case (i, e) => (i, copyLoop(Block(field(f,i)(e.tp,ctx)), Block(field(r,i)(e.tp,ctx)), Block(field(z,i)(e.tp,ctx)), field(rv1,i)(e.tp,ctx), field(rv2,i)(e.tp,ctx))(e.tp))
              }
              struct[DeliteArray[B]](SoaTag(tag, newElems(0)._2.length), newElems) //TODO: output size
            case Block(s@Def(a)) => 
              Console.println(f.toString + ": " + vf.toString + " with type " + f.tp.toString)
              Console.println(s.toString + ": " + a.toString + " with type " + s.tp.toString)
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

      val dataField = dc_data_field(t.getBlockResult(alloc).asInstanceOf[Exp[DeliteCollection[V]]]) //FIXME
      val sizeField = dc_size_field(t.getBlockResult(alloc).asInstanceOf[Exp[DeliteCollection[V]]])

      if (dataField == "" && !isSubtype(manifest[I].erasure,classOf[DeliteArray[_]])) {
        printlog("unable to transform hashReduce elem: no data field defined for " + manifest[I].toString)
        return None
      }

      val newLoop = valT match {
        case Block(Def(Struct(ta,es))) if Config.soaEnabled => 
          val (rv1, rv2) = unwrapRV(ta,es,body.rV)
          soaTransform(valT, t(body.rFunc), t(body.zero), rv1, rv2)
        case _ => copyLoop(valT, t(body.rFunc), t(body.zero), t(body.rV._1), t(body.rV._2))
      }

      val res = if (isSubtype(manifest[I].erasure, classOf[DeliteArray[_]])) {
        newLoop
      }
      else {
        val newElems = elems.map {
          case (df, _) if df == dataField => (df, newLoop)
          case (sf, _) if sf == sizeField => (sf, newLoop.length)
          case (f, Def(Reflect(NewVar(init),_,_))) => (f, t(init))
          case (f,v) => (f, t(v))
        }
        struct[I](tag, newElems)
      }

      t.replace(body.buf.allocVal, res) // TODO: use withSubstScope
      printlog("successfully transformed hashReduce elem with type " + manifest[I].toString + " to " + res.toString)
      Some(t.getBlockResult(t(body.buf.finalizer)))
      
    case Block(Def(Reify(Def(a),_,_))) => printlog("unable to transform hashReduce elem: found " + a + " with type " + manifest[CV].toString); None
    case _ => None
  } }

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
