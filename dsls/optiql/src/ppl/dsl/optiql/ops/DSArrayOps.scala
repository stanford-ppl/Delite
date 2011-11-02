package ppl.dsl.optiql.ops

import ppl.dsl.optiql.datastruct.scala.container.{DataTable, Grouping}
import java.io.PrintWriter
import scala.virtualization.lms.common.{Base, ScalaGenFat, BaseFatExp, LoopsFatExp, IfThenElseFatExp, ArrayOpsExp, LoopFusionOpt}
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.optiql.OptiQLExp
import ppl.delite.framework.datastructures.FieldAccessOpsExp

trait DSArrayOpsExp extends BaseFatExp with ArrayOpsExp with LoopsFatExp with IfThenElseFatExp { this: OptiQLExp =>

  //object ToReduceElem {
  //  def unapply[T](x: Def)
  //}

  // overrides for optimization
  override def simpleLoop[A:Manifest](size: Exp[Int], v: Sym[Int], body: Def[A]): Exp[A] = body match {
    case b: DeliteCollectElem[A,Array[A]] => b.func match { // unchecked!
      // split collect of struct
      case Block(Def(Struct(tag, elems))) => 
        assert(b.alloc == Block(b.aV), "TODO: only works on simple arrays for now")
        def copyLoop[B:Manifest](func: Block[B]): Exp[Array[B]] = {
          val aV = fresh[Array[B]]
          simpleLoop(size, v, DeliteCollectElem[B,Array[B]](aV = aV, alloc = reifyEffects(aV), cond = b.cond, func = func))
        }
        
        struct[A]("Array"::tag, elems.map(p=>(p._1, copyLoop(Block(p._2))(p._2.Type))))

      // collect(reduce(hashCollect)) --> hashReduce
      case Block(Def(SimpleLoop(nestedSize /*Def(ArrayLength(input))*/, redV, redBody: DeliteReduceElem[cc]))) =>
        // sz is not guaranteed to match ....
        Console.println("******* investigating nested collect reduce " + redBody)
        redBody.func match {
          case Block(Def(ArrayApply(Def(ArrayApply(
              Def(SimpleLoop(origSize, grpV, grpBody: DeliteHashCollectElem[aa,`cc`,_])), idx1)), idx2))) =>
            Console.println("******* TADAAA!!! ")
            Console.println(""+idx1 + " " + v)
            Console.println(""+idx2 + " " + redV)
            assert(idx1 == v, "TODO: case not handled" + idx1 + " " + grpV)
            assert(idx2 == redV, "TODO: case not handled")
            
            simpleLoop(origSize, grpV, DeliteHashReduceElem[aa,cc, A](
              keyFunc = grpBody.keyFunc, valFunc = grpBody.valFunc, 
              zero = redBody.zero, rV = redBody.rV, rFunc = redBody.rFunc))
          case _ =>
            super.simpleLoop(size, v, body)
        }
      // hash collect with average -- TODO: generalize??
      case Block(Def(d@NumericDivide(Def(SimpleLoop(nestedSize1 /*Def(ArrayLength(input))*/, redV1, redBody1: DeliteReduceElem[cc1])), 
                                   Def(SimpleLoop(nestedSize2 /*Def(ArrayLength(input))*/, redV2, redBody2: DeliteReduceElem[cc2])) ))) =>
        // sz is not guaranteed to match ....
        Console.println("******* investigating nested collect reduce / reduce " + redBody1 + "," + redBody2)

        // FIXME: code duplication is bad. extract common functionality.

        (redBody1.func, redBody2.func) match {
          case (Block(Def(ArrayApply(Def(ArrayApply(
              Def(SimpleLoop(origSize1, grpV1, grpBody1: DeliteHashCollectElem[aa1,`cc1`,_])), idxA1)), idxB1))),
            valFunc2 @ Block(Const(_))) =>

           Console.println("******* TADAAA!!! (average)")
           assert(idxA1 == v, "TODO: case not handled")
           assert(idxB1 == redV1, "TODO: case not handled")

           val l1 = simpleLoop(origSize1, grpV1, DeliteHashReduceElem[aa1,cc1, A](
             keyFunc = grpBody1.keyFunc, valFunc = grpBody1.valFunc, 
             zero = redBody1.zero, rV = redBody1.rV, rFunc = redBody1.rFunc))

           val l2 = simpleLoop(origSize1, grpV1, DeliteHashReduceElem[aa1,cc2, A](
             keyFunc = grpBody1.keyFunc, valFunc = valFunc2, 
             zero = redBody2.zero, rV = redBody2.rV, rFunc = redBody2.rFunc))

           numeric_divide(l1,l2)(d.aev,d.mev)

          case _ =>
            super.simpleLoop(size, v, body)
        }
        
      // remove identity collect
      case Block(Def(ArrayApply(xs,v))) if b.cond == Nil /*&& array_length(xs) == size*/ => xs.asInstanceOf[Exp[A]] // eta-reduce! <--- should live elsewhere, not specific to struct
      case _ => super.simpleLoop(size, v, body)
    }

    case b: DeliteHashCollectElem[k,A,Array[Array[A]]] => b.valFunc match { // unchecked!
      // split hashcollect of struct values
      case Block(Def(Struct(tag, elems))) => 
        //assert(b.alloc == Block(b.aV), "TODO: only works on simple arrays for now")
        def copyLoop[B:Manifest](valFunc: Block[B]): Exp[Array[Array[B]]] = {
          //val aV = fresh[Array[B]]
          simpleLoop(size, v, DeliteHashCollectElem[k,B,Array[Array[B]]](cond = b.cond, keyFunc = b.keyFunc, valFunc = valFunc))
        }

        struct[A]("Array"::"Array"::tag, elems.map(p=>(p._1, copyLoop(Block(p._2))(p._2.Type))))

      //TODO!!case Block(Def(ArrayApply(xs,v))) if b.cond == Nil && array_length(xs) == size => xs.asInstanceOf[Exp[A]] // eta-reduce! <--- should live elsewhere, not specific to struct
      case _ => super.simpleLoop(size, v, body)
    }
    case _ => super.simpleLoop(size, v, body)
  }

  override def array_apply[A:Manifest](a: Rep[Array[A]], i: Rep[Int]): Rep[A] = a match {
    case Def(Struct(pre::tag,elems:Map[String,Exp[Array[A]]])) =>
      assert(pre == "Array")
      def unwrap[A](m:Manifest[Array[A]]): Manifest[A] = m.typeArguments match {
        case a::_ => mtype(a)
        case _ => 
          if (m.erasure.isArray) mtype(Manifest.classType(m.erasure.getComponentType))
          else { printerr("warning: expect type Array[A] but got "+m); mtype(manifest[Any]) }
      }
      struct[A](tag, elems.map(p=>(p._1,array_apply(p._2, i)(unwrap(p._2.Type)))))
    case _ => super.array_apply(a,i)
  }
  
  override def array_length[A:Manifest](a: Rep[Array[A]]): Rep[Int] = a match {
    case Def(SimpleLoop(size,v,body)) => body match {
      case b: DeliteCollectElem[_,_] if b.cond == Nil => size
      case _ => super.array_length(a)
    }
    case Def(Struct(pre::tag,elems:Map[String,Exp[Array[A]]])) =>
      assert(pre == "Array")
      val ll = elems.map(p=>array_length(p._2)) // all arrays must have same length! TODO: might not want to gen code for this
      //ll reduceLeft { (a1,a2) => assert(a1 == a2, "lengths don't match for " + a + "," + elems + ": "+ll); a1 }
      ll.head
    case _ => super.array_length(a)
  }
  
  // api

  def arraySelect[A:Manifest](size: Exp[Int])(func: Exp[Int]=>Exp[A]): Exp[Array[A]] = {
    val v = fresh[Int]
    val aV = fresh[Array[A]]
    simpleLoop(size,v,DeliteCollectElem[A, Array[A]](
      aV = aV,
      alloc = reifyEffects(aV),
      func = reifyEffects(func(v))
    ))
  }
  
  def arrayWhere[A:Manifest](size: Exp[Int])(cond: Exp[Int]=>Exp[Boolean])(func: Exp[Int]=>Exp[A]): Exp[Array[A]] = {
    val v = fresh[Int]
    val aV = fresh[Array[A]]
    simpleLoop(size,v,DeliteCollectElem[A, Array[A]](
      aV = aV,
      alloc = reifyEffects(aV),
      func = reifyEffects(func(v)),
      cond = reifyEffects(cond(v))::Nil
    ))
  }

  def arrayDistinct[A:Manifest](size: Exp[Int])(func: Exp[Int]=>Exp[A]): Exp[Array[A]] = {
    val v = fresh[Int]
    val rV = (fresh[A], fresh[A])
    simpleLoop(size,v,DeliteHashReduceElem[A,A, Array[A]](
      //aV = aV,
      //alloc = reifyEffects(aV),
      keyFunc = reifyEffects(func(v)),
      valFunc = reifyEffects(func(v)),
      zero = reifyEffects(unit(null).asInstanceOfL[A]),
      rV = rV,
      rFunc = reifyEffects(rV._2)
    ))
  }
  
  def arrayGroup[K:Manifest,V:Manifest](size: Exp[Int])(keyFunc: Exp[Int]=>Exp[K])(valFunc: Exp[Int]=>Exp[V]): Exp[Array[Array[V]]] = {
    val v = fresh[Int]
    //val aV = fresh[Array[A]]
    simpleLoop(size,v,DeliteHashCollectElem[K,V, Array[Array[V]]](
      //aV = aV,
      //alloc = reifyEffects(aV),
      keyFunc = reifyEffects(keyFunc(v)),
      valFunc = reifyEffects(valFunc(v))
      //cond = reifyEffects(cond(v))::Nil
    ))
  }

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


  // override -- shouldn't belong here
  override def struct[T:Manifest](tag: List[String], elems: Map[String, Rep[Any]]): Rep[T] = 
    toAtom(SimpleStruct[T](tag, elems))(mtype(manifest[Map[String,Any]]))


}


trait ScalaGenDSArrayOps extends ScalaGenFat with LoopFusionOpt {
  val IR: DSArrayOpsExp with OptiQLExp
  import IR._  

  override def unapplySimpleIndex(e: Def[Any]) = e match {
    case ArrayApply(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }
  override def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = e match {
    case ArrayLength(a @ Def(SimpleLoop(_,_,_:DeliteCollectElem[_,_]))) => Some(a) // exclude hash collect!
    case _ => super.unapplySimpleDomain(e)
  }
  
/*
  // override -- shouldn't belong here
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Struct(tag, elems) =>
      //emitValDef(sym, "new " + tag.last + "(" + elems.map(e => quote(e._2)).mkString(",") + ")")
      emitValDef(sym, "Map(" + elems.map(e => "\"" + e._1 + "\"->" + quote(e._2)).mkString(",") + ") //" + tag.mkString(" "))
    case Field(struct, index, tp) =>  
      //emitValDef(sym, quote(struct) + "." + index)
      println("WARNING: emitting field access: " + quote(struct) + "." + index)
      emitValDef(sym, quote(struct) + "(\"" + index + "\").asInstanceOf[" + remap(tp) + "]")
    case _ => super.emitNode(sym, rhs)
  }*/
  
  
}
