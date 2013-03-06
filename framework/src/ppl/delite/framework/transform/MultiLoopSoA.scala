package ppl.delite.framework.transform

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.Util._
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

trait MultiloopSoATransformExp extends DeliteTransform with LoweringTransform with DeliteApplication
  with DeliteOpsExp with DeliteArrayFatExp with DeliteArrayBufferOpsExp { self =>

  private val t = new ForwardPassTransformer {
    val IR: self.type = self
    override def transformStm(stm: Stm): Exp[Any] = stm match {
      case TP(sym, Loop(size, v, body: DeliteCollectElem[a,i,ca])) if body.par == ParFlat /*FIXME: ParBuffer*/ => soaCollect[a,i,ca](size,v,body)(body.mA,body.mI,body.mCA) match {
        case Some(newSym) => newSym
        case None => super.transformStm(stm)
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

    def replace[A](oldExp: Exp[A], newExp: Exp[A]) {
      subst += this(oldExp) -> newExp
      printlog("replacing " + oldExp.toString + " with " + newExp.toString)
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
  def soaCollect[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest](size: Exp[Int], v: Sym[Int], body: DeliteCollectElem[A,I,CA]): Option[Exp[CA]] = {
    val alloc = t(body.allocN)
    alloc match {
    case StructBlock(tag,elems) =>
      val cond2 = body.cond.map(t(_))
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
          func = f,//t(f),
          update = reifyEffects(dc_update(allocV,tv,elemV)),
          finalizer = reifyEffects(allocV),
          cond = cond2,
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

      def soaTransform[B:Manifest](tag: StructTag[B], elems: Seq[(String,Exp[Any])]): Exp[DeliteArray[B]] = {
        val newElems = elems map {
          case (index, e @ Def(Struct(t,es))) => (index, soaTransform(t,es)(e.tp))
          case (index, Def(DeliteArrayApply(x,iv))) if (iv.equals(v) && body.par == ParFlat) => (index, x) //eliminate identify function loop
          case (index, e) => (index, copyLoop(Block(e))(e.tp))
        }
        val sz = body.par match {
          case ParFlat => t(size)
          case ParBuffer => 

          newElems(0)._2.length //TODO: we want to know the output size without having to pick one of the returned arrays arbitrarily (prevents potential DCE)... can we just grab the size out of the activation record somehow?
          /*
          val rV1 = fresh[Int]
          val rV2 = fresh[Int]

          simpleLoop(t(size), t(v).asInstanceOf[Sym[Int]], DeliteReduceElem[Int](
            func = reifyEffects(unit(1)),
            cond = cond2,
            zero = reifyEffects(unit(0)),
            accInit = reifyEffects(unit(0)), //?
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

      val newLoop = t(body.func) match {
        case u@Block(Def(a@Struct(ta,es))) if Config.soaEnabled => 
          println("*** SOA " + u + " / " + a)
          soaTransform(ta,es)
        case f2 => copyLoop(f2)
      }

      val res = if (isSubtype(manifest[I].erasure, classOf[DeliteArray[_]])) {
        newLoop
      }
      else {
        val newElems = elems.map {
          case (df, _) if df == dataField => (df, newLoop)
          case (sf, _) if (sf == sizeField && body.par == ParFlat) => (sf, t(size))
          case (sf, _) if (sf == sizeField && body.par == ParBuffer) => (sf, newLoop.length)
          case (f, Def(Reflect(NewVar(init),_,_))) => (f, t(init))
          case (f,v) => (f, t(v))
        }
        struct[I](tag, newElems)
      }

      t.replace(body.allocVal, res) // TODO: use withSubstScope
      printlog("successfully transformed collect elem with type " + manifest[I].toString + " to " + res.toString)
      Some(t.getBlockResult(t(body.finalizer)))
      
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
          func = f,//t(f),
          cond = body.cond.map(t(_)),
          zero = z,//t(z),
          accInit = reifyEffects(fatal(unit("accInit not transformed")))(manifest[B]), //unwrap this as well to support mutable reduce
          rV = (t(rv1).asInstanceOf[Sym[B]], t(rv2).asInstanceOf[Sym[B]]),
          rFunc = r,//t(r),
          stripFirst = !isPrimitiveType(manifest[B])
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

      def soaTransform[B:Manifest](func: Block[B], rFunc: Block[B], zero: Block[B], rv1:Exp[B], rv2: Exp[B]): Exp[DeliteArray[B]] = func match {
        case Block(f @ Def(Struct(_,_))) => rFunc match {
          case Block(r @ Def(Struct(tag,elems))) => zero match {
            case Block(z @ Def(Struct(_,_))) =>
              val ctx = implicitly[SourceContext]
              val newElems = elems map {
                case (i, e @ Def(Struct(_,_))) => (i, soaTransform(Block(field(f,i)(e.tp,ctx)), Block(field(r,i)(e.tp,ctx)), Block(field(z,i)(e.tp,ctx)), field(rv1,i)(e.tp,ctx), field(rv2,i)(e.tp,ctx))(e.tp))
                case (i, e) => (i, copyLoop(Block(field(f,i)(e.tp,ctx)), Block(field(r,i)(e.tp,ctx)), Block(field(z,i)(e.tp,ctx)), field(rv1,i)(e.tp,ctx), field(rv2,i)(e.tp,ctx))(e.tp))
              }
              struct[DeliteArray[B]](SoaTag(tag, newElems(0)._2.length), newElems) //TODO: output size
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

      val dataField = dc_data_field(t.getBlockResult(t(body.alloc)).asInstanceOf[Exp[DeliteCollection[V]]]) //TODO: ?
      val sizeField = dc_size_field(t.getBlockResult(t(body.alloc)).asInstanceOf[Exp[DeliteCollection[V]]])

      if (dataField == "" && manifest[CV].erasure != classOf[DeliteArray[_]]) {
        printlog("unable to transform hashReduce elem: no data field defined for " + manifest[CV].toString) 
        return None
      }

      val newLoop = t(body.valFunc) match {
        case Block(Def(Struct(ta,es))) if Config.soaEnabled => 
          val (rv1, rv2) = unwrapRV(ta,es,body.rV)
          soaTransform(t(body.valFunc), t(body.rFunc), t(body.zero), rv1, rv2)
        case _ => copyLoop(t(body.valFunc), t(body.rFunc), t(body.zero), t(body.rV._1), t(body.rV._2))
      }

      val res = if (manifest[CV].erasure == classOf[DeliteArray[_]]) {
        newLoop.asInstanceOf[Exp[CV]]
      }
      else {
        val newElems = elems.map {
          case (df, _) if df == dataField => (df, newLoop)
          case (sf, _) if sf == sizeField => (sf, newLoop.length)
          case (f, Def(Reflect(NewVar(init),_,_))) => (f, t(init))
          case (f,v) => (f, t(v))
        }
        struct[CV](tag, newElems)
      }
      Some(res)
      
    case Block(Def(Reify(Def(a),_,_))) => printlog("unable to transform hashReduce elem: found " + a + " with type " + manifest[CV].toString); None
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