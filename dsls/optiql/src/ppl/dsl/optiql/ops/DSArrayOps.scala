package ppl.dsl.optiql.ops

import java.io.PrintWriter
import scala.virtualization.lms.common.{Base, ScalaGenFat, CLikeGenFat, CudaGenFat, OpenCLGenFat, BaseFatExp, LoopsFatExp, IfThenElseFatExp, TupleOpsExp, ArrayOpsExp, LoopFusionOpt}
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.optiql.OptiQLExp
import ppl.delite.framework.datastructures.DeliteArray
import scala.reflect.SourceContext

trait DSArrayOpsExp extends BaseFatExp with TupleOpsExp with LoopsFatExp with IfThenElseFatExp { this: OptiQLExp =>
  /*
  //object ToReduceElem {
  //  def unapply[T](x: Def)
  //}

  // TODO: temporarily we need to structurally recurse over group reductions. 
  // this will go away once groupBy fusion is integrated in lms-core

  //TODO: hack to provide sourceContexts to framework methods
  def ctx(implicit pos: SourceContext) = pos

  object HashAccess0 {
    def unapply[T](e: Exp[T]): Option[(Exp[Int], Sym[Int], Block[_], Block[T], Exp[Int], List[Block[Boolean]])] = e match {
      case Def(DeliteArrayApply(
          Def(SimpleLoop(origSize, grpV, grpBody: DeliteHashCollectElem[aa,`T`,_])), idx1)) =>
        Some((origSize, grpV, grpBody.keyFunc, grpBody.valFunc, idx1, grpBody.cond))
      case _ => None
    }
  }

  object HashAccess {
    def unapply[T](e: Exp[T]): Option[(Exp[Int], Sym[Int], Block[_], Block[T], Exp[Int], Exp[Int], List[Block[Boolean]])] = e match {
      case Def(DeliteArrayApply(Def(DeliteArrayApply(
          Def(SimpleLoop(origSize, grpV, grpBody: DeliteHashCollectElem[aa,`T`,_])), idx1)), idx2)) =>
        Some((origSize, grpV, grpBody.keyFunc, grpBody.valFunc, idx1, idx2, grpBody.cond))


      case Def(n@NumericPlus(HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2, cond1), 
                             HashAccess(origSize2, grpV2, keyFunc2, valFunc2, idx12, idx22, cond2))) =>
        Some((origSize, grpV, keyFunc, Block(numeric_plus(valFunc.res,valFunc2.res)(n.aev,n.mev,ctx)), idx1, idx2, cond1))
      case Def(n@NumericPlus(HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2, cond), y)) =>
        Some((origSize, grpV, keyFunc, Block(numeric_plus(valFunc.res,y)(n.aev,n.mev,ctx)), idx1, idx2, cond))
      case Def(n@NumericPlus(y, HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2, cond))) =>
        Some((origSize, grpV, keyFunc, Block(numeric_plus(y, valFunc.res)(n.aev,n.mev,ctx)), idx1, idx2, cond))

      case Def(n@NumericMinus(HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2, cond1), 
                             HashAccess(origSize2, grpV2, keyFunc2, valFunc2, idx12, idx22, cond2))) =>
        Some((origSize, grpV, keyFunc, Block(numeric_minus(valFunc.res,valFunc2.res)(n.aev,n.mev,ctx)), idx1, idx2, cond1))
      case Def(n@NumericMinus(HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2, cond), y)) =>
        Some((origSize, grpV, keyFunc, Block(numeric_minus(valFunc.res,y)(n.aev,n.mev,ctx)), idx1, idx2, cond))
      case Def(n@NumericMinus(y, HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2, cond))) =>
        Some((origSize, grpV, keyFunc, Block(numeric_minus(y, valFunc.res)(n.aev,n.mev,ctx)), idx1, idx2, cond))

      case Def(n@NumericTimes(HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2, cond1), 
                             HashAccess(origSize2, grpV2, keyFunc2, valFunc2, idx12, idx22, cond2))) =>
        Some((origSize, grpV, keyFunc, Block(numeric_times(valFunc.res,valFunc2.res)(n.aev,n.mev,ctx)), idx1, idx2, cond1))
      case Def(n@NumericTimes(HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2, cond), y)) =>
        Some((origSize, grpV, keyFunc, Block(numeric_times(valFunc.res,y)(n.aev,n.mev,ctx)), idx1, idx2, cond))
      case Def(n@NumericTimes(y, HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2, cond))) =>
        Some((origSize, grpV, keyFunc, Block(numeric_times(y, valFunc.res)(n.aev,n.mev,ctx)), idx1, idx2, cond))

      case _ => None
    }
  }
  
  
  /*object HashReduction {
    def unapply[T](e: Exp[T]) = e match {
      case Def(SimpleLoop(nestedSize /*Def(DeliteArrayLength(input))*/, redV, redBody: DeliteReduceElem[cc])) =>
        Console.println("******* investigating nested collect reduce " + redBody)
        Console.println("size " + nestedSize + "=" + (nestedSize match {case Def(x) => x}))
        redBody.func match {
          case Block(HashAccess(origSize, grpV, keyFunc:Block[aa], valFunc, idx1, idx2)) =>
            Console.println("******* TADAAA!!! ")
            Console.println(""+idx1 + " " + v)
            Console.println(""+idx2 + " " + redV)
            assert(idx1 == v, "TODO: case not handled" + idx1 + " " + grpV)
            assert(idx2 == redV, "TODO: case not handled")
            Some(origSize, grpV, keyFun, valFunc, redBody.zero, redBody.rV, redBody.rFunc)
          case _ => None
        }
      case Def(DeliteArrayLength(DefHashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2))) =>
        Some(origSize, grpV, keyFun)
      case _ => None
    }
  }*/


  // overrides for optimization
  override def simpleLoop[A:Manifest](size: Exp[Int], v: Sym[Int], body: Def[A]): Exp[A] = body match {
    case b: DeliteCollectElem[A,DeliteArray[A]] => b.func match { // unchecked!
      // split collect of struct
      case Block(Def(Struct(tag, elems))) => 
        assert(b.alloc == Block(b.aV), "TODO: only works on simple arrays for now")
        def copyLoop[B:Manifest](func: Block[B]): Exp[DeliteArray[B]] = {
          val aV = fresh[DeliteArray[B]]
          simpleLoop(size, v, DeliteCollectElem[B,DeliteArray[B]](aV = aV, alloc = reifyEffects(aV), cond = b.cond, func = func))
        }
        
        struct[A]("Array"::tag, elems.map(p=>(p._1, copyLoop(Block(p._2))(p._2.tp))))

      // collect(reduce(hashCollect)) --> hashReduce
      case Block(Def(SimpleLoop(nestedSize /*Def(ArrayLength(input))*/, redV, redBody: DeliteReduceElem[cc]))) =>
        // sz is not guaranteed to match ....
        //Console.println("******* investigating nested collect reduce " + redBody)
        //Console.println("size " + nestedSize + "=" + (nestedSize match {case Def(x) => x}))
        //recurseGroupReduce(redBody.func.res, v, redV, redBody)
        redBody.func match {
          case Block(HashAccess(origSize, grpV, keyFunc:Block[aa], valFunc, idx1, idx2, hashCond)) =>
            //Console.println("******* TADAAA!!! ")
            //Console.println(""+idx1 + " " + v)
            //Console.println(""+idx2 + " " + redV)
            assert(idx1 == v, "TODO: case not handled" + idx1 + " " + grpV)
            assert(idx2 == redV, "TODO: case not handled")
            
            simpleLoop(origSize, grpV, DeliteHashReduceElem(
              keyFunc = keyFunc, valFunc = valFunc, cond = redBody.cond ++ hashCond,
              zero = redBody.zero, rV = redBody.rV, rFunc = redBody.rFunc))
          case _ =>
            super.simpleLoop(size, v, body)
        }
      // hash collect with average -- TODO: generalize??
      case Block(Def(DeliteArrayLength(HashAccess0(origSize, grpV, keyFunc, valFunc, idx1, hashCond)))) =>
        //Console.println("******* TADAAA!!! (count)")
        assert(idx1 == v, "TODO: case not handled" + idx1 + " " + v)
        val rV = (fresh[Int],fresh[Int])
        simpleLoop(origSize, grpV, DeliteHashReduceElem(
          keyFunc = keyFunc, valFunc = Block(Const(1)), cond = hashCond, 
          zero = Block(Const(0)), rV = rV, rFunc = reifyEffects(numeric_plus(rV._1,rV._2))))
        
      // hash collect with average -- TODO: generalize??
      case Block(Def(d@NumericDivide(Def(SimpleLoop(nestedSize1 /*Def(DeliteArrayLength(input))*/, redV1, redBody1: DeliteReduceElem[cc1])),
                                   Def(SimpleLoop(nestedSize2 /*Def(DeliteArrayLength(input))*/, redV2, redBody2: DeliteReduceElem[cc2])) ))) =>
        // sz is not guaranteed to match ....
        //Console.println("******* investigating nested collect reduce / reduce " + redBody1 + "," + redBody2)

        // FIXME: code duplication is bad. extract common functionality.

        (redBody1.func, redBody2.func) match {
          case (Block(HashAccess(origSize, grpV, keyFunc, valFunc1, idxA1, idxB1, hashCond)),
            valFunc2 @ Block(Const(_))) =>

           //Console.println("******* TADAAA!!! (average)")
           assert(idxA1 == v, "TODO: case not handled")
           assert(idxB1 == redV1, "TODO: case not handled")

           val l1 = simpleLoop[A](origSize, grpV, DeliteHashReduceElem(
             keyFunc = keyFunc, valFunc = valFunc1, cond = hashCond ++ redBody1.cond,
             zero = redBody1.zero, rV = redBody1.rV, rFunc = redBody1.rFunc))

           val l2 = simpleLoop[A](origSize, grpV, DeliteHashReduceElem(
             keyFunc = keyFunc, valFunc = valFunc2, hashCond ++ redBody2.cond,
             zero = redBody2.zero, rV = redBody2.rV, rFunc = redBody2.rFunc))

           arraySelect[cc1](size) { i => 
             numeric_divide(
               darray_apply(l1.asInstanceOf[Exp[DeliteArray[cc1]]],i)(mtype(d.mev),ctx),
               darray_apply(l2.asInstanceOf[Exp[DeliteArray[cc1]]],i)(mtype(d.mev),ctx)
              )(d.aev.asInstanceOf[Numeric[cc1]],mtype(d.mev),ctx)
            } (mtype(d.mev)) .asInstanceOf[Exp[A]]

          case _ =>
            super.simpleLoop(size, v, body)
        }
        
      // remove identity collect FIXME: length check!!
      case Block(Def(DeliteArrayApply(xs,`v`))) if b.cond == Nil && !size.isInstanceOf[Const[_]] /*&& darray_length(xs) == size*/ =>
        xs.asInstanceOf[Exp[A]] // eta-reduce! <--- should live elsewhere, not specific to struct
      case _ => super.simpleLoop(size, v, body)
    }

    case b: DeliteHashCollectElem[k,A,DeliteArray[DeliteArray[A]]] => b.valFunc match { // unchecked!
      // split hashcollectgit  of struct values
      case Block(Def(Struct(tag, elems))) => 
        //assert(b.alloc == Block(b.aV), "TODO: only works on simple arrays for now")
        def copyLoop[B:Manifest](valFunc: Block[B]): Exp[DeliteArray[DeliteArray[B]]] = {
          //val aV = fresh[DeliteArray[B]]
          simpleLoop(size, v, DeliteHashCollectElem[k,B,DeliteArray[DeliteArray[B]]](cond = b.cond, keyFunc = b.keyFunc, valFunc = valFunc))
        }

        struct[A]("Array"::"Array"::tag, elems.map(p=>(p._1, copyLoop(Block(p._2))(p._2.tp))))

      //TODO!!case Block(Def(DeliteArrayApply(xs,v))) if b.cond == Nil && darray_length(xs) == size => xs.asInstanceOf[Exp[A]] // eta-reduce! <--- should live elsewhere, not specific to struct
      case _ => super.simpleLoop(size, v, body)
    }

    //fuse filter-hashReduce
    //TODO: why doesn't LMS fusing handle this?
    /*case b: DeliteHashReduceElem[k,A,DeliteArray[A]] => 
      b.valFunc match {
      case Block(Def(DeliteArrayApply(Def(SimpleLoop(sizeFull, vFull, bd: DeliteCollectElem[A, DeliteArray[A]])),`v`))) => 
        bd.func match {
        case Block(Def(DeliteArrayApply(da, vi))) if (vFull == vi) => //identity function, can eliminate
          Console.println("******* TADAAA!!! (filter)")
          val valFunc = reifyEffects(darray_apply(da, vFull))
          val keyFunc = reifyEffects(unit(0)) //TODO!!!
          simpleLoop(sizeFull, vFull, DeliteHashReduceElem(keyFunc, valFunc, b.cond ++ bd.cond, b.zero, b.rV, b.rFunc)) //fuse

        case _ => super.simpleLoop(size, v, body)
      }
      case _ => super.simpleLoop(size, v, body)
    }*/

    case _ => super.simpleLoop(size, v, body)
  } */

  
  // write these as: (not so easy, polymorphic function)
  // def array_apply = distributeStruct(a) ( e => array_apply(e,i) ) getOrElse (super.array_apply(a,i))
  /*
  override def array_apply[A:Manifest](a: Rep[Array[A]], i: Rep[Int])(implicit ctx: SourceContext): Rep[A] = a match {
    case Def(Struct(pre::tag,elems:Map[String,Exp[Array[A]]])) =>
      assert(pre == "Array")
      def unwrap[A](e:Exp[Array[A]],m:Manifest[Array[A]]): Manifest[A] = m.typeArguments match {
        case a::_ => mtype(a)
        case _ => 
          if (m.erasure.isArray) mtype(Manifest.classType(m.erasure.getComponentType))
          else { printerr("warning: array_apply expect type Array[A] but got "+m+" for input " + e.toString + "/" + Def.unapply(e).toString); mtype(manifest[Any]) }
      }
      struct[A](tag, elems.map(p=>(p._1,array_apply(p._2, i)(unwrap(p._2,p._2.tp)))))
    case _ => super.array_apply(a,i)
  }
  

  // NOTE: We should think about structs with properties/data common to all the fields.
  // 1) A soa-transformed array is a Struct of arrays that all have the same length. So what
  // is 'the length' of the transformed array? Currently we forward to field #0, which works
  // but is not ideal (holds on to that field even if not used otherwise, complicates 
  // matching on length node).
  // 2) An Option[Struct] value, represented as a Struct, is set or empty for all the fields
  // together, not individually. What is the result of .isEmpty on the transformed value?

  override def array_length[A:Manifest](a: Rep[Array[A]])(implicit ctx: SourceContext): Rep[Int] = a match {
    case Def(SimpleLoop(size,v,body)) => body match {
      case b: DeliteCollectElem[_,_] if b.cond == Nil => size
      case _ => super.array_length(a) // TODO: might want to construct reduce elem with same condition?
    }
    case Def(Struct(pre::tag,elems:Map[String,Exp[Array[A]]])) =>
      assert(pre == "Array")
      val ll = elems.map(p=>array_length(p._2)) // all arrays must have same length! TODO: might not want to gen code for this
      //ll reduceLeft { (a1,a2) => assert(a1 == a2, "lengths don't match for " + a + "," + elems + ": "+ll); a1 }
      ll.head
    case _ => super.array_length(a)
  }
  

  // ******** api *********

  def arraySelect[A:Manifest](size: Exp[Int])(func: Exp[Int]=>Exp[A]): Exp[DeliteArray[A]] = {
    val v = fresh[Int]
    val aV = fresh[DeliteArray[A]]
    simpleLoop(size,v,DeliteCollectElem[A, DeliteArray[A]](
      aV = aV,
      alloc = reifyEffects(aV),
      func = reifyEffects(func(v))
    ))
  }
  
  def arrayWhere[A:Manifest](size: Exp[Int])(cond: Exp[Int]=>Exp[Boolean])(func: Exp[Int]=>Exp[A]): Exp[DeliteArray[A]] = {
    val v = fresh[Int]
    val aV = fresh[DeliteArray[A]]
    simpleLoop(size,v,DeliteCollectElem[A, DeliteArray[A]](
      aV = aV,
      alloc = reifyEffects(aV),
      func = reifyEffects(func(v)),
      cond = reifyEffects(cond(v))::Nil
    ))
  }

  case class DArrayFlatten[A](data: Exp[DeliteArray[DeliteArray[A]]]) extends Def[DeliteArray[A]]
  def arrayFlatten[A:Manifest](data: Exp[DeliteArray[DeliteArray[A]]]): Exp[DeliteArray[A]] = data match {
    case Def(Struct(pre::tag,elems:Map[String,Exp[DeliteArray[DeliteArray[A]]]])) =>
      assert(pre == "Array")
      def unwrap[A](m:Manifest[DeliteArray[A]]): Manifest[A] = m.typeArguments match {
        case a::_ => mtype(a)
        case _ => 
          if (m.erasure.isArray) mtype(Manifest.classType(m.erasure.getComponentType))
          else { printerr("warning: arrayFlatten expect type Array[A] but got "+m+" for input " + data.toString + "/" + elems.toString); mtype(manifest[Any]) }
      }
      struct[DeliteArray[A]](tag, elems.map(p=>(p._1, arrayFlatten(p._2)(unwrap(unwrap(p._2.tp))))))
    case _ => DArrayFlatten(data)
  }
    
  // ---- hashing

  def arrayDistinct[A:Manifest](size: Exp[Int])(func: Exp[Int]=>Exp[A], condFunc: List[Exp[Int] => Exp[Boolean]] = Nil): Exp[DeliteArray[A]] = {
    val v = fresh[Int]
    val rV = (fresh[A], fresh[A])
    simpleLoop(size,v,DeliteHashReduceElem[A,A, DeliteArray[A]](
      //aV = aV,
      //alloc = reifyEffects(aV),
      keyFunc = reifyEffects(func(v)),
      valFunc = reifyEffects(func(v)),
      zero = reifyEffects(unit(null).AsInstanceOf[A]),
      rV = rV,
      rFunc = reifyEffects(rV._1),
      cond = condFunc.map(c => reifyEffects(c(v)))
    ))
  }
  
  def arrayGroup[K:Manifest,V:Manifest](size: Exp[Int])(keyFunc: Exp[Int]=>Exp[K])(valFunc: Exp[Int]=>Exp[V], condFunc: List[Exp[Int] => Exp[Boolean]] = Nil): Exp[DeliteArray[DeliteArray[V]]] = {
    val v = fresh[Int]
    //val aV = fresh[DeliteArray[A]]
    simpleLoop(size,v,DeliteHashCollectElem[K,V, DeliteArray[DeliteArray[V]]](
      //aV = aV,
      //alloc = reifyEffects(aV),
      keyFunc = reifyEffects(keyFunc(v)),
      valFunc = reifyEffects(valFunc(v)),
      cond = condFunc.map(c => reifyEffects(c(v)))
    ))
  }

  def indexBuild[K:Manifest](size: Exp[Int])(keyFunc: Exp[Int]=>Exp[K]): Exp[Map[K,Int]] = {
    val v = fresh[Int]
    //val aV = fresh[DeliteArray[A]]
    simpleLoop(size,v,DeliteHashIndexElem[K,Map[K,Int]](
      //aV = aV,
      //alloc = reifyEffects(aV),
      keyFunc = reifyEffects(keyFunc(v))
      //valFunc = reifyEffects(valFunc(v))
      //cond = reifyEffects(cond(v))::Nil
    ))
  }

  case class IndexLookup[K](map: Exp[Map[K,Int]], key: Exp[K]) extends Def[Int]
  
  def indexLookup[K:Manifest](map: Exp[Map[K,Int]], key: Exp[K]): Exp[Int] = {
    IndexLookup(map, key)
  }



  // ---- reduce

  def reducePlain[A:Manifest](size: Exp[Int])(func: Exp[Int]=>Exp[A])(zero: =>Exp[A])(red: (Exp[A],Exp[A])=>Exp[A]): Exp[A] = {
    val v = fresh[Int]
    val rV = (fresh[A], fresh[A])
    simpleLoop(size,v,DeliteReduceElem[A](
      func = reifyEffects(func(v)),
      zero = reifyEffects(zero),
      rV = rV,
      rFunc = reifyEffects(red(rV._1, rV._2)),
      stripFirst = false
    ))
  }

  def reduceTuple[A:Manifest,B:Manifest](size: Exp[Int])(funcA: Exp[Int]=>Exp[A], funcB: Exp[Int]=>Exp[B])(zeroA: =>Exp[A], zeroB: =>Exp[B])(red: ((Exp[A],Exp[B]),(Exp[A],Exp[B]))=>(Block[A],Block[B])): Exp[A] = {
    /*lazy val body: Def[R] = copyBodyOrElse(DeliteReduceTupleElem[R,Q](
      func = /*reifyEffects*/(zip(dc_apply(inA,v), dc_apply(inB,v))), //FIXME: tupled reify
      zero = this.zero,
      rVPar = this.rV,
      rVSeq = this.rV,
      rFuncPar = /*reifyEffects*/(reduce(rV._1, rV._2)),  //FIXME: tupled reify
      rFuncSeq = /*reifyEffects*/(reduce(rV._1, rV._2)),  //FIXME: tupled reify
      stripFirst = (!isPrimitiveType(manifest[R]) || !isPrimitiveType(manifest[R])) && !this.mutable
    ))*/
    val v = fresh[Int]
    val rVPar = ((fresh[A], fresh[B]), (fresh[A], fresh[B]))
    val rVSeq = ((fresh[A], fresh[B]), (fresh[A], fresh[B]))
    simpleLoop(size,v,DeliteReduceTupleElem[A,B](
      func = (reifyEffects(funcA(v)), reifyEffects(funcB(v))),
      zero = (reifyEffects(zeroA), reifyEffects(zeroB)),
      rVPar = rVPar,
      rVSeq = rVSeq,
      rFuncPar = /*reifyEffects*/(red(rVPar._1, rVPar._2)),  //FIXME: tupled reify
      rFuncSeq = /*reifyEffects*/(red(rVSeq._1, rVSeq._2)),  //FIXME: tupled reify
      cond = Nil,
      stripFirst = false
    ))
  }


  // ---- sorting
  //TODO: this shouldn't really live here
  case class DArraySort(len: Exp[Int], v: (Sym[Int],Sym[Int]), comp: Block[Boolean]) extends Def[DeliteArray[Int]]

  def arraySort(size: Exp[Int])(compFunc: (Exp[Int],Exp[Int])=>Exp[Boolean]): Exp[DeliteArray[Int]] = {
    val v = (fresh[Int],fresh[Int])
    DArraySort(size, v, reifyEffects(compFunc(v._1,v._2)))
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case DArraySort(len, v, comp) => syms(v) ++ effectSyms(comp)
    case _ => super.boundSyms(e)
  }


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case DArraySort(len, v, comp) => toAtom(DArraySort(f(len),(f(v._1).asInstanceOf[Sym[Int]],f(v._2).asInstanceOf[Sym[Int]]),f(comp)))
    case IndexLookup(map, key) => toAtom(IndexLookup(f(map),f(key)))
    case DArrayFlatten(data) => toAtom(DArrayFlatten(f(data)))(mtype(manifest[A]),ctx)
    //case Field(o,key,manif) => toAtom(Field(f(o),key,manif))(mtype(manifest[A])) // TODO: shouldn't be here
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]



  // override -- shouldn't belong here
  //override def struct[T:Manifest](tag: List[String], elems: Map[String, Rep[Any]]): Rep[T] =
  //  toAtom(GenericStruct[T](tag, elems))(mtype(manifest[Map[String,Any]]))


  //case class CharTuple(a: Exp[Char], b: Exp[Char]) extends Def[(Char,Char)]
  /*override def make_tuple2[A:Manifest,B:Manifest](t: (Exp[A],Exp[B])): Exp[(A,B)] = 
    if (manifest[A] == manifest[Char] && manifest[B] == manifest[Char])
      toAtom(ETuple2(t._1, t._2))(mtype(manifest[Int]))
    else super.make_tuple2(t)*/
  */
}


trait ScalaGenDSArrayOps extends ScalaGenFat with LoopFusionOpt {
  val IR: DSArrayOpsExp with OptiQLExp
  import IR._  
  /*
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@ETuple2(a, b) if e.m1 == manifest[Boolean] && e.m2 == manifest[Boolean] =>
      emitValDef(sym, "((if("+ quote(a) + ") 1 else 0) << 1) + " + "(if("+ quote(b) + ") 1 else 0)")
    case a@Tuple2Access1(t) if a.m == manifest[Boolean] =>
      emitValDef(sym, "((" + quote(t) + " & 0xfffffffe) >>> 1) == 1")
    case a@Tuple2Access2(t) if a.m == manifest[Boolean] =>
      emitValDef(sym, "(" + quote(t) + " & 0x00000001) == 1")
    case e@ETuple2(a, b) if e.m1 == manifest[Char] && e.m2 == manifest[Char] =>
      emitValDef(sym, "("+ quote(a) + ".toInt << 16) + " + quote(b)) 
    case a@Tuple2Access1(t) if a.m == manifest[Char] =>
      emitValDef(sym, "((" + quote(t) + " & 0xffff0000) >>> 16).toChar")
    case a@Tuple2Access2(t) if a.m == manifest[Char] =>
      emitValDef(sym, "(" + quote(t) + " & 0xffff).toChar")
    case IndexLookup(map, key) =>
      emitValDef(sym, quote(map) + ".get(" + quote(key) + ")") // it's a HashMapImpl object, so get instead of apply
    case DArrayFlatten(data) =>
      emitValDef(sym, quote(data) + ".flatten")
    case DArraySort(len, (v1,v2), comp) =>
      emitValDef(sym, "{"/*}*/)
      stream.println("val array = new Array[Integer](" + quote(len) + ")")
      stream.println("var i=0; while (i < array.length) {array(i)=i;i+=1}")
      stream.println("//FIXME: probably inefficient because of boxing")
      stream.println("val comp: java.util.Comparator[Integer] = new java.util.Comparator[Integer] { def compare("+quote(v1)+": Integer,"+quote(v2)+": Integer) = {"/*}}*/)
      emitBlock(comp)
      stream.println("if ("+quote(getBlockResult(comp))+") -1 else 1")
      stream.println(/*{{*/"}}")
      stream.println("java.util.Arrays.sort(array, comp)")
      stream.println("array map { e => e.toInt }")
      stream.println(/*{*/"}")
    case _ => super.emitNode(sym, rhs)
  } */
}


trait CLikeGenDSArrayOps extends CLikeGenFat with LoopFusionOpt {
  val IR: DSArrayOpsExp with OptiQLExp
  import IR._
  /*
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@ETuple2(a, b) if e.m1 == manifest[Boolean] && e.m2 == manifest[Boolean] =>
      emitValDef(sym, "((unsigned int)" + quote(a) + " << 1) + " + quote(b))
    case a@Tuple2Access1(t) if a.m == manifest[Boolean] =>
      emitValDef(sym, "((unsigned char)(" + quote(t) + " & 0xfffffffe)) >> 1")
    case a@Tuple2Access2(t) if a.m == manifest[Boolean] =>
      emitValDef(sym, "(unsigned char)(" + quote(t) + " & 0x0001)")
    case e@ETuple2(a, b) if e.m1 == manifest[Char] && e.m2 == manifest[Char] =>
      emitValDef(sym, "((unsigned int)" + quote(a) + " << 16) + " + quote(b))
    case a@Tuple2Access1(t) if a.m == manifest[Char] =>
      emitValDef(sym, "((unsigned short)(" + quote(t) + " & 0xffff0000)) >> 16")
    case a@Tuple2Access2(t) if a.m == manifest[Char] =>
      emitValDef(sym, "(unsigned short)(" + quote(t) + " & 0xffff)")
    case _ => super.emitNode(sym, rhs)
  } */
}

trait CudaGenDSArrayOps extends CudaGenFat with CLikeGenDSArrayOps 
trait OpenCLGenDSArrayOps extends OpenCLGenFat with CLikeGenDSArrayOps
