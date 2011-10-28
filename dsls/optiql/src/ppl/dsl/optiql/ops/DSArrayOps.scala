package ppl.dsl.optiql.ops

import ppl.dsl.optiql.datastruct.scala.container.{DataTable, Grouping}
import java.io.PrintWriter
import scala.virtualization.lms.common.{Base, ScalaGenFat, BaseFatExp, LoopsFatExp, IfThenElseFatExp, ArrayOpsExp, LoopFusionOpt}
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.optiql.OptiQLExp
import ppl.delite.framework.datastructures.FieldAccessOpsExp

trait DSArrayOpsExp extends BaseFatExp with ArrayOpsExp with LoopsFatExp with IfThenElseFatExp { this: OptiQLExp =>

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
        Console.println("******* investigation nested collect reduce " + redBody)
        redBody.func match {
          case Block(Def(ArrayApply(Def(ArrayApply(Def(SimpleLoop(origSize, grpV, grpBody: DeliteHashCollectElem[aa,`cc`,_])), idx1)), idx2))) =>
            Console.println("******* TADAAA!!! ")
            Console.println(""+idx1 + " " + v)
            Console.println(""+idx2 + " " + redV)
            assert(idx1 == v, "TODO: case not handled" + idx1 + " " + grpV)
            assert(idx2 == redV, "TODO: case not handled")
            //Console.println("******* ALMOST " + idx1 + "," + idx2)
            
            simpleLoop(origSize, grpV, DeliteHashReduceElem[aa,cc, A](
              keyFunc = grpBody.keyFunc, valFunc = grpBody.valFunc, 
              zero = redBody.zero, rV = redBody.rV, rFunc = redBody.rFunc))
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
      zero = reifyEffects(Const(null.asInstanceOf[A])),
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
}
