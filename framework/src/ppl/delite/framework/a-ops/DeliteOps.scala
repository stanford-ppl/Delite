package ppl.delite.framework.ops

import java.io.{FileWriter, File, PrintWriter}
import ppl.delite.framework.DeliteCollection
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

trait DeliteOpsExp extends EffectExp with VariablesExp with VariantsOpsExp with DeliteCollectionOpsExp
  with OrderingOpsExp with CastingOpsExp with ImplicitOpsExp {

  /**
   * The base type of the DeliteOp hierarchy.
   */
  /*sealed*/ abstract class DeliteOp[A]() extends Def[A]

  /**
   * A sequential task - will execute block in a single thread and respect any free variable dependencies inside it.
   *
   * @param  block   the task to execute; must be reified if it contains effectful operations!
   */
  class DeliteOpSingleTask[A](val block: Exp[A]) extends DeliteOp[A]

  /**
   * A Conditional task - will emit a Conditional DEG node as well as kernels for the then and else clauses
   *
   * @param  cond    the condition of the Conditional
   * @param  thenp   the Then block to execute if condition is true
   * @param  elsep   the Else block to execute if condition is false
   */
  trait DeliteOpCondition[A] extends DeliteOp[A] {
    val cond: Exp[Boolean]
    val thenp: Exp[A]
    val elsep: Exp[A]
  }

  /**
   * An indexed loop - will emit an indexed loop DEG node as well as a kernel for the body
   *
   * @param  start  starting index
   * @param  end    ending index (not included in loop)
   * @param  idx    index id that will be refered to in the body, this could be passed in as input to the body or the body could be inlined
   * @param  body   the body of the loop
   */
  trait DeliteOpIndexedLoop extends DeliteOp[Unit] {
    val start: Exp[Int]
    val end: Exp[Int]
    val index: Exp[Int]
    val body: Exp[Unit]
  }

  /**
   * An while loop - will emit an while loop DEG node as well as a kernel for the body
   *
   * @param  cond  condition expression, will be emitted as a kernel
   * @param  body   the body of the loop
   */
  trait WhileLoopLike {
    val cond: Exp[Boolean]
    val body: Exp[Unit]
  }

  trait DeliteOpWhileLoop extends DeliteOp[Unit] with WhileLoopLike

  /**
   * Parallel map from DeliteCollection[A] => DeliteCollection[B]. Input functions can depend on free
   * variables, but they cannot depend on other elements of the input or output collection (disjoint access).
   *
   * @param  in    the input collection
   * @param  v     the bound symbol that the mapping function operates over
   * @param  func  the mapping function; reified version of Exp[A] => Exp[B]
   * @param  alloc function returning the output collection. if it is the same as the input collection,
   *               the operation is mutable; reified version of Unit => DeliteCollection[B].
   */

  trait DeliteOpMap[A,B,C[X] <: DeliteCollection[X]] extends DeliteOp[C[B]] with DeliteOpMapLikeWhileLoopVariant {
    val in: Exp[C[A]]
    val v: Exp[A]
    val func: Exp[B]
    val alloc: Exp[C[B]]

    // DeliteOpWhileLoopVariant
    //var index = unit(0)
    //lazy val output = alloc
    // TODO: we need a way to make variables and reify work together -- we lose access to the var once we reify it
    // TODO: there should be a better way of doing this
    //val i : Var[Int] = index match { case Def(Reify(x, effects)) => x.asInstanceOf[Var[Int]] }
    lazy implicit val mA = v.Type.asInstanceOf[Manifest[A]]
    lazy implicit val mB = func.Type.asInstanceOf[Manifest[B]]
    val vs = () => __newVar(unit(null).asInstanceOfL[A])
    lazy val Vs = vs()
    // this does not get initialized properly in body -- perhaps a scala bug?
    //lazy val vs = __newVar(unit(null)).asInstanceOfL[A]
    val index = __newVar(unit(0))
    lazy val cond = reifyEffects(index < in.size)
    lazy val body =
      reifyEffects {
        //vs = in(index)
        //val Vs = vs()
        Vs = in(index)
        // in the variant version, we need to actually emit the bound symbol as a kernel so it can be passed in as an input
        reflectEffect(createDefinition(v.asInstanceOf[Sym[A]], ReadVar(Vs)).rhs)
        alloc(index) = func
        index += 1
      }
  }

  /**
   * Parallel 2 element zipWith from (DeliteCollection[A],DeliteCollection[B]) => DeliteCollection[R].
   * Input functions can depend on free variables, but they cannot depend on other elements of the input or
   * output collection (disjoint access).
   *
   * @param  inA   the first input collection
   * @param  inB   the second input collection
   * @param  v     the bound symbol that the zipWith function operates over
   * @param  func  the zipWith function; reified version of ([Exp[A],Exp[B]) => Exp[R]
   * @param  alloc function returning the output collection. if it is the same as the input collection,
   *               the operation is mutable; reified version of Unit => DeliteCollection[B].
   */
  abstract class DeliteOpZipWith[A,B,R,C[X] <: DeliteCollection[X]]() extends DeliteOp[C[R]] {
    val inA: Exp[C[A]]
    val inB: Exp[C[B]]
    val v: (Exp[A],Exp[B])
    val func: Exp[R]
    val alloc: Exp[C[R]]
  }

  /**
   * Parallel reduction of a DeliteCollection[A]. Reducing function must be associative.
   *
   * @param  in    the input collection
   * @param  v     the bound symbol that the reducing function operates over
   * @param  func  the reduction function; reified version of ([Exp[A],Exp[A]) => Exp[A]. Must be associative.
   */
  abstract class DeliteOpReduce[A]() extends DeliteOp[A] {
    val in: Exp[DeliteCollection[A]]
    val v: (Exp[A],Exp[A])
    val func: Exp[A]
  }


  /**
   * Parallel map-reduction from a DeliteCollection[A] => R. The map-reduce is composed, so no temporary collection
   * is instantiated to hold the result of the map.
   *
   * @param  in      the input collection
   * @param  mV      the bound symbol that the mapping function operates over
   * @param  map     the mapping function; reified version of Exp[A] => Exp[R]
   * @param  rV      the bound symbol that the reducing function operates over
   * @param  reduce  the reduction function; reified version of ([Exp[R],Exp[R]) => Exp[R]. Must be associative.
   */
  abstract class DeliteOpMapReduce[A,R,C[X] <: DeliteCollection[X]]() extends DeliteOp[R] with DeliteOpMapLikeWhileLoopVariant {
    val in: Exp[C[A]]
    //val acc: Exp[R]

    // for accumulating each partial sum
    val mV: Exp[A]
    //val mapreduce: Exp[R] // reified of Exp[(R,A)] => Exp[R] composition of map and reduce
    val map: Exp[R]

    // for reducing remaining partial sums
    val rV: (Exp[R],Exp[R])
    val reduce: Exp[R]

    // DeliteOpWhileLoopVariant
    lazy implicit val mA = mV.Type.asInstanceOf[Manifest[A]]
    lazy implicit val mR = map.Type.asInstanceOf[Manifest[R]]
    val acc = () => __newVar(unit(null).asInstanceOfL[R])
    lazy val Acc = {
      //acc()
      Vs = in(0)
      val x = acc()
      reifyEffects(reflectEffect(createDefinition(mV.asInstanceOf[Sym[A]], ReadVar(Vs)).rhs))
      x = map
      x
    }
    lazy val alloc = reifyEffects(readVar(Acc))
    val vs = () => __newVar(unit(null).asInstanceOfL[A])
    lazy val Vs = vs()
    lazy val cond = reifyEffects(index < in.size)
    val index = __newVar(unit(1))
    lazy val body =
      reifyEffects {
        Vs = in(index)
        // in the variant version, we need to actually emit the bound symbol as a kernel so it can be passed in as an input
        // map
        //reflectEffect(createDefinition(mV.asInstanceOf[Sym[A]], ReadVar(Vs)).rhs)
        var x = map

        // reduce
        reflectEffect(createDefinition(rV._1.asInstanceOf[Sym[R]], ReadVar(Acc)).rhs)
        reflectEffect(createDefinition(rV._2.asInstanceOf[Sym[R]], ReadVar(x)).rhs)
        Acc = reduce
        index += 1
      }
  }


  /**
   * Parallel zipWith-reduction from a (DeliteCollection[A],DeliteCollection[A]) => R. The map-reduce is composed,
   * so no temporary collection is instantiated to hold the result of the map.
   *
   * @param  inA     the first input collection
   * @param  inB     the second input collection
   * @param  zV      the bound symbol that the zipWith function operates over
   * @param  zip     the zipWith function; reified version of (Exp[A],Exp[B]) => Exp[R]
   * @param  rV      the bound symbol that the reducing function operates over
   * @param  reduce  the reduction function; reified version of ([Exp[R],Exp[R]) => Exp[R]. Must be associative.
   */
  abstract class DeliteOpZipWithReduce[A,B,R,C[X] <: DeliteCollection[X]]() extends DeliteOp[R] {
    val inA: Exp[C[A]]
    val inB: Exp[C[B]]
    // for accumulating each partial sum
    val zV: (Exp[A],Exp[B])
    val zip: Exp[R]
    // for reducing remaining partial sums
    val rV: (Exp[R],Exp[R])
    val reduce: Exp[R]
  }


  /**
   * Parallel foreach from DeliteCollection[A] => Unit. Input functions must specify any free variables that it
   * requires are protected (e.g. locked before chunk execution) using the sync list.
   *
   * @param  in     the input collection
   * @param  v      the bound symbol that the foreach function operates over
   * @param  func   the foreach function; reified version of Exp[A] => Exp[Unit]
   * @param  i      the bound symbol that the sync function operates over
   * @param  sync   a function from an index to a list of objects that should be locked, in a total ordering,
   *                prior to chunk execution, and unlocked after; reified version of Exp[Int] => Exp[List[_]]
   */
  abstract class DeliteOpForeach[A,C[X] <: DeliteCollection[X]]() extends DeliteOp[Unit] {
    val in: Exp[C[A]]
    val v: Exp[A]
    val func: Exp[Unit]
    val i: Exp[Int]
    val sync: Exp[List[_]]
  }

  // used by delite code generators to handle nested delite ops
  var deliteKernel: Boolean = _
  var deliteResult: Option[Sym[Any]] = _
  var deliteInputs: List[Sym[Any]] = _
}

trait BaseGenDeliteOps extends GenericNestedCodegen {
  val IR: DeliteOpsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case s: DeliteOpSingleTask[_] => if (shallow) super.syms(e) else super.syms(e) ::: syms(s.block)
    case map: DeliteOpMap[_,_,_] => if (shallow) syms(map.in) else syms(map.in) ::: syms(map.alloc) ::: syms(map.func)
    case zip: DeliteOpZipWith[_,_,_,_] => if (shallow) syms(zip.inA) ::: syms(zip.inB) else syms(zip.inA) ::: syms(zip.inB) ::: syms(zip.alloc) ::: syms(zip.func)
    case red: DeliteOpReduce[_] => if (shallow) syms(red.in) else syms(red.in) ::: syms(red.func)
    case mapR: DeliteOpMapReduce[_,_,_] => if (shallow) syms(mapR.in) else syms(mapR.in) ::: syms(mapR.map) ::: syms(mapR.reduce)
    case zipR: DeliteOpZipWithReduce[_,_,_,_] => if (shallow) syms(zipR.inA) ::: syms(zipR.inB) else syms(zipR.inA) ::: syms(zipR.inB) ::: syms(zipR.zip) ::: syms(zipR.reduce)
    case foreach: DeliteOpForeach[_,_] => if (shallow) syms(foreach.in) else syms(foreach.in) ::: syms(foreach.func)
    // always try to hoist free dependencies out of delite ops, if possible
//    case s: DeliteOpSingleTask[_] => if (shallow) super.syms(e) else super.syms(e) ++ syms(s.block)
//    case map: DeliteOpMap[_,_,_] => if (shallow) syms(map.in) ++ syms(map.func) else syms(map.in) ++ syms(map.func) ++ syms(map.alloc)
//    case zip: DeliteOpZipWith[_,_,_,_] => if (shallow) syms(zip.inA) ++ syms(zip.inB) ++ syms(zip.func) else syms(zip.inA) ++ syms(zip.inB) ++ syms(zip.alloc) ++ syms(zip.func)
//    case red: DeliteOpReduce[_] => syms(red.in) ++ syms(red.func)
//    case mapR: DeliteOpMapReduce[_,_,_] => syms(mapR.in) ++ syms(mapR.map) ++ syms(mapR.reduce)
//    case zipR: DeliteOpZipWithReduce[_,_,_,_] => syms(zipR.inA) ++ syms(zipR.inB) ++ syms(zipR.zip) ++ syms(zipR.reduce)
//    case foreach: DeliteOpForeach[_,_] => syms(foreach.in) ++ syms(foreach.func)
    case _ => super.syms(e)
  }

  /*
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case map: DeliteOpMap[_,_,_] => map.func match {
      case Def(Reify(y, es)) => map.v.asInstanceOf[Sym[Any]] :: syms(map.alloc) ::: es.asInstanceOf[List[Sym[Any]]] ::: boundSyms(y)
      case _ => map.v.asInstanceOf[Sym[Any]] :: syms(map.alloc) ::: boundSyms(map.func)
    }
    case zip: DeliteOpZipWith[_,_,_,_] => zip.func match {
      case Def(Reify(y, es)) => zip.v._1.asInstanceOf[Sym[Any]] :: zip.v._2.asInstanceOf[Sym[Any]] :: es.asInstanceOf[List[Sym[Any]]] ::: boundSyms(y)
      case _ => zip.v._1.asInstanceOf[Sym[Any]] :: zip.v._2.asInstanceOf[Sym[Any]] :: boundSyms(zip.func)
    }
    case red: DeliteOpReduce[_] => red.func match {
      case Def(Reify(y, es)) => red.v._1.asInstanceOf[Sym[Any]] :: red.v._2.asInstanceOf[Sym[Any]] :: es.asInstanceOf[List[Sym[Any]]] ::: boundSyms(y)
      case _ => red.v._1.asInstanceOf[Sym[Any]] :: red.v._2.asInstanceOf[Sym[Any]] :: boundSyms(red.func)
    }
    case mapR: DeliteOpMapReduce[_,_,_] => (mapR.map, mapR.reduce) match {
      case (Def(Reify(y, es)), Def(Reify(y2,es2))) => mapR.mV.asInstanceOf[Sym[Any]] :: mapR.rV._1.asInstanceOf[Sym[Any]] :: mapR.rV._2.asInstanceOf[Sym[Any]] :: es.asInstanceOf[List[Sym[Any]]] ::: es2.asInstanceOf[List[Sym[Any]]] ::: boundSyms(y) ::: boundSyms(y2)
      case (Def(Reify(y, es)), y2) => mapR.mV.asInstanceOf[Sym[Any]] :: mapR.rV._1.asInstanceOf[Sym[Any]] :: mapR.rV._2.asInstanceOf[Sym[Any]] :: es.asInstanceOf[List[Sym[Any]]] ::: boundSyms(y) ::: boundSyms(y2)
      case (y, Def(Reify(y2, es2))) => mapR.mV.asInstanceOf[Sym[Any]] :: mapR.rV._1.asInstanceOf[Sym[Any]] :: mapR.rV._2.asInstanceOf[Sym[Any]] :: es2.asInstanceOf[List[Sym[Any]]] ::: boundSyms(y) ::: boundSyms(y2)
      case _ => mapR.mV.asInstanceOf[Sym[Any]] :: mapR.rV._1.asInstanceOf[Sym[Any]] :: mapR.rV._2.asInstanceOf[Sym[Any]] :: boundSyms(mapR.map) ::: boundSyms(mapR.reduce)
    }
    case zipR: DeliteOpZipWithReduce[_,_,_,_] => (zipR.zip, zipR.reduce) match {
      case (Def(Reify(y, es)), Def(Reify(y2,es2))) => zipR.zV._1.asInstanceOf[Sym[Any]] :: zipR.zV._2.asInstanceOf[Sym[Any]] :: zipR.rV._1.asInstanceOf[Sym[Any]] :: zipR.rV._2.asInstanceOf[Sym[Any]] :: es.asInstanceOf[List[Sym[Any]]] ::: es2.asInstanceOf[List[Sym[Any]]] ::: boundSyms(y) ::: boundSyms(y2)
      case (Def(Reify(y, es)), y2) => zipR.zV._1.asInstanceOf[Sym[Any]] :: zipR.zV._2.asInstanceOf[Sym[Any]] :: zipR.rV._1.asInstanceOf[Sym[Any]] :: zipR.rV._2.asInstanceOf[Sym[Any]] :: es.asInstanceOf[List[Sym[Any]]] ::: boundSyms(y) ::: boundSyms(y2)
      case (y, Def(Reify(y2, es2))) => zipR.zV._1.asInstanceOf[Sym[Any]] :: zipR.zV._2.asInstanceOf[Sym[Any]] :: zipR.rV._1.asInstanceOf[Sym[Any]] :: zipR.rV._2.asInstanceOf[Sym[Any]] :: es2.asInstanceOf[List[Sym[Any]]] ::: boundSyms(y) ::: boundSyms(y2)
      case _ => zipR.zV._1.asInstanceOf[Sym[Any]] :: zipR.zV._2.asInstanceOf[Sym[Any]] :: zipR.rV._1.asInstanceOf[Sym[Any]] :: zipR.rV._2.asInstanceOf[Sym[Any]] :: boundSyms(zipR.zip) ::: boundSyms(zipR.reduce)
    }
    case foreach: DeliteOpForeach[_,_] => foreach.func match {
      case Def(Reify(y, es)) => foreach.v.asInstanceOf[Sym[Any]] :: es.asInstanceOf[List[Sym[Any]]] ::: boundSyms(y)
      case _ => foreach.v.asInstanceOf[Sym[Any]] :: boundSyms(foreach.func)
    }
    case _ => super.boundSyms(e)
  }
  */

  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case s: DeliteOpSingleTask[_] => getFreeVarBlock(s.block,Nil)
    case map: DeliteOpMap[_,_,_] => getFreeVarBlock(List(map.func,map.alloc),List(map.v.asInstanceOf[Sym[_]]))
    case zip: DeliteOpZipWith[_,_,_,_] => getFreeVarBlock(List(zip.func,zip.alloc),List(zip.v._1.asInstanceOf[Sym[_]], zip.v._2.asInstanceOf[Sym[_]]))
    case red: DeliteOpReduce[_] => getFreeVarBlock(red.func,List(red.v._1.asInstanceOf[Sym[_]], red.v._2.asInstanceOf[Sym[_]]))
    case mapR: DeliteOpMapReduce[_,_,_] => getFreeVarBlock(mapR.map, List(mapR.mV.asInstanceOf[Sym[_]])) ++ getFreeVarBlock(mapR.reduce, List(mapR.rV._1.asInstanceOf[Sym[_]], mapR.rV._2.asInstanceOf[Sym[_]]))
    case zipR: DeliteOpZipWithReduce[_,_,_,_] => getFreeVarBlock(zipR.zip, List(zipR.zV._1.asInstanceOf[Sym[_]], zipR.zV._2.asInstanceOf[Sym[_]])) ++ getFreeVarBlock(zipR.reduce, List(zipR.rV._1.asInstanceOf[Sym[_]], zipR.rV._2.asInstanceOf[Sym[_]]))
    case foreach: DeliteOpForeach[_,_] => getFreeVarBlock(foreach.func,List(foreach.v.asInstanceOf[Sym[_]]))
    case _ => super.getFreeVarNode(rhs)
  }

}

trait ScalaGenDeliteOps extends ScalaGenEffect with BaseGenDeliteOps {
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case s:DeliteOpSingleTask[_] => {
      val save = deliteKernel
      deliteKernel = false
      val b = s.block
      stream.println("def " + quote(sym) + "_block = { ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      deliteKernel = save
    }
    case map:DeliteOpMap[_,_,_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        emitBlock(map.alloc)
        stream.println("var mapIdx = 0")
        stream.println("while (mapIdx < " + quote(getBlockResult(map.in)) + ".size) {")
        stream.println("val " + quote(map.v) + " = " + quote(getBlockResult(map.in)) + ".dcApply(mapIdx)")
        stream.println(quote(getBlockResult(map.alloc)) + ".dcUpdate(mapIdx, " + " {")
        emitBlock(map.func)
        stream.println(quote(getBlockResult(map.func)))
        stream.println("})")
        stream.println("mapIdx += 1")
        stream.println("} // end while")
        stream.println(quote(getBlockResult(map.alloc)))
        stream.println("}")
	      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpMap[" + remap(map.v.Type) + "," + remap(map.func.Type) + "," + remap(map.alloc.Type) + "] {")
        stream.println("def in = " + quote(getBlockResult(map.in)))
        stream.println("def alloc = {")
        emitBlock(map.alloc)
        stream.println(quote(getBlockResult(map.alloc)))
        stream.println("}")
        stream.println("def map(" + quote(map.v) + ": " + remap(map.v.Type) + ") = {")
        emitBlock(map.func)
        stream.println(quote(getBlockResult(map.func)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case zip: DeliteOpZipWith[_,_,_,_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        emitBlock(zip.alloc)
        stream.println("var zipIdx = 0")
        stream.println("while (zipIdx < " + quote(getBlockResult(zip.inA)) + ".size) {")
        stream.println("val " + quote(zip.v._1) + " = " + quote(getBlockResult(zip.inA)) + ".dcApply(zipIdx)")
        stream.println("val " + quote(zip.v._2) + " = " + quote(getBlockResult(zip.inB)) + ".dcApply(zipIdx)")
        stream.println(quote(getBlockResult(zip.alloc)) + ".dcUpdate(zipIdx, " + " {")
        emitBlock(zip.func)
        stream.println(quote(getBlockResult(zip.func)))
        stream.println("})")
        stream.println("zipIdx += 1")
        stream.println("} // end while")
        stream.println(quote(getBlockResult(zip.alloc)))
        stream.println("}")
	      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpZipWith[" + remap(zip.v._1.Type) + "," + remap(zip.v._2.Type) + "," + remap(zip.func.Type) + "," + remap(zip.alloc.Type) +"] {")
        stream.println("def inA = " + quote(getBlockResult(zip.inA)))
        stream.println("def inB = " + quote(getBlockResult(zip.inB)))
        stream.println("def alloc = {")
        emitBlock(zip.alloc)
        stream.println(quote(getBlockResult(zip.alloc)))
        stream.println("}")
        stream.println("def zip(" + quote(zip.v._1) + ": " + remap(zip.v._1.Type) + ", " + quote(zip.v._2) + ": " + remap(zip.v._2.Type) + ") = {")
        emitBlock(zip.func)
        stream.println(quote(getBlockResult(zip.func)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case red: DeliteOpReduce[_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        stream.println("var " + quote(red.v._1) + " = " + quote(getBlockResult(red.in)) + ".dcApply(0)")
        stream.println("var reduceIdx = 1")
        stream.println("while (reduceIdx < " + quote(getBlockResult(red.in)) + ".size) {")
        stream.println("val " + quote(red.v._2) + " = " + quote(getBlockResult(red.in)) + ".dcApply(reduceIdx)")
        stream.println(quote(red.v._1) + " = {")
        emitBlock(red.func)
        stream.println(quote(getBlockResult(red.func)))
        stream.println("}")
        stream.println("reduceIdx += 1")
        stream.println("} // end while")
        stream.println(quote(red.v._1))
        stream.println("}")
	      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpReduce[" + remap(red.func.Type) + "] {")
        stream.println("def in = " + quote(getBlockResult(red.in)))
        stream.println("def reduce(" + quote(red.v._1) + ": " + remap(red.v._1.Type) + "," + quote(red.v._2) + ": " + remap(red.v._2.Type) + ") = {")
        emitBlock(red.func)
        stream.println(quote(getBlockResult(red.func)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case mapR:DeliteOpMapReduce[_,_,_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        stream.println("val " + quote(mapR.mV) + " = " + quote(getBlockResult(mapR.in)) + ".dcApply(0)")
        stream.println("var " + quote(mapR.rV._1) + " = {")
        emitBlock(mapR.map)
        stream.println(quote(getBlockResult(mapR.map)))
        stream.println("}")
        stream.println("var mapReduceIdx = 1")
        stream.println("while (mapReduceIdx < " + quote(getBlockResult(mapR.in)) + ".size) {")
        stream.println("val " + quote(mapR.mV) + " = " + quote(getBlockResult(mapR.in)) + ".dcApply(mapReduceIdx)")
        stream.println("val " + quote(mapR.rV._2) + " = {")
        emitBlock(mapR.map)
        stream.println(quote(getBlockResult(mapR.map)))
        stream.println("}")
        stream.println(quote(mapR.rV._1) + " = {")
        emitBlock(mapR.reduce)
        stream.println(quote(getBlockResult(mapR.reduce)))
        stream.println("}")
        stream.println("mapReduceIdx += 1")
        stream.println("} // end while")
        stream.println(quote(mapR.rV._1))
        stream.println("}")
	      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpMapReduce[" + remap(mapR.mV.Type) + "," + remap(mapR.reduce.Type) + "] {")
        stream.println("def in = " + quote(getBlockResult(mapR.in)))
        stream.println("def map(" + quote(mapR.mV) + ": " + remap(mapR.mV.Type) + ") = {")
        emitBlock(mapR.map)
        stream.println(quote(getBlockResult(mapR.map)))
        stream.println("}")
        stream.println("")
        stream.println("def reduce(" + quote(mapR.rV._1) + ": " + remap(mapR.rV._1.Type) + "," + quote(mapR.rV._2) + ": " + remap(mapR.rV._2.Type) + ") = {")
        emitBlock(mapR.reduce)
        stream.println(quote(getBlockResult(mapR.reduce)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case zipR:DeliteOpZipWithReduce[_,_,_,_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        stream.println("val " + quote(zipR.zV._1) + " = " + quote(getBlockResult(zipR.inA)) + ".dcApply(0)")
        stream.println("val " + quote(zipR.zV._2) + " = " + quote(getBlockResult(zipR.inB)) + ".dcApply(0)")
        stream.println("var " + quote(zipR.rV._1) + " = {")
        emitBlock(zipR.zip)
        stream.println(quote(getBlockResult(zipR.zip)))
        stream.println("}")
        stream.println("var zipReduceIdx = 1")
        stream.println("while (zipReduceIdx < " + quote(getBlockResult(zipR.inA)) + ".size) {")
        stream.println("val " + quote(zipR.zV._1) + " = " + quote(getBlockResult(zipR.inA)) + ".dcApply(zipIdx)")
        stream.println("val " + quote(zipR.zV._2) + " = " + quote(getBlockResult(zipR.inB)) + ".dcApply(zipIdx)")
        stream.println("val " + quote(zipR.rV._2) + " = {")
        emitBlock(zipR.zip)
        stream.println(quote(getBlockResult(zipR.zip)))
        stream.println("}")
        stream.println(quote(zipR.rV._1) + " = {")
        emitBlock(zipR.reduce)
        stream.println(quote(getBlockResult(zipR.reduce)))
        stream.println("}")
        stream.println("zipReduceIdx += 1")
        stream.println("} // end while")
        stream.println(quote(zipR.rV._1))
        stream.println("}")
	      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpZipWithReduce[" + remap(zipR.zV._1.Type) + "," + remap(zipR.zV._2.Type) + "," + remap(zipR.reduce.Type) + "] {")
        stream.println("def inA = " + quote(getBlockResult(zipR.inA)))
        stream.println("def inB = " + quote(getBlockResult(zipR.inB)))
        stream.println("def zip(" + quote(zipR.zV._1) + ": " + remap(zipR.zV._1.Type) + ", " + quote(zipR.zV._2) + ": " + remap(zipR.zV._2.Type) + ") = {")
        emitBlock(zipR.zip)
        stream.println(quote(getBlockResult(zipR.zip)))
        stream.println("}")
        stream.println("")
        stream.println("def reduce(" + quote(zipR.rV._1) + ": " + remap(zipR.rV._1.Type) + "," + quote(zipR.rV._2) + ": " + remap(zipR.rV._2.Type) + ") = {")
        emitBlock(zipR.reduce)
        stream.println(quote(getBlockResult(zipR.reduce)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case foreach:DeliteOpForeach[_,_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        stream.println("var forIdx = 0")
        stream.println("while (forIdx < " + quote(getBlockResult(foreach.in)) + ".size) {")
        stream.println("val " + quote(foreach.v) + " = " + quote(getBlockResult(foreach.in)) + ".dcApply(forIdx)")
        emitBlock(foreach.func)
        stream.println(quote(getBlockResult(foreach.func)))
        stream.println("forIdx += 1")
        stream.println("} // end while")
        stream.println("}")
      	stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      else {
        deliteKernel = false
        stream.println("val " + quote(sym) + " = new generated.scala.DeliteOpForeach[" + remap(foreach.v.Type) + "] {")
        stream.println("def in = " + quote(getBlockResult(foreach.in)))
        stream.println("def sync(" + quote(foreach.i) + ": " + remap(foreach.i.Type) + ") = {")
        emitBlock(foreach.sync)
        stream.println(quote(getBlockResult(foreach.sync)))
        stream.println("}")
        stream.println("def foreach(" + quote(foreach.v) + ": " + remap(foreach.v.Type) + ") = {")
        emitBlock(foreach.func)
        stream.println(quote(getBlockResult(foreach.func)))
        stream.println("}}")
        deliteKernel = true
      }
    }
    case _ => super.emitNode(sym,rhs)
  }
}

trait CudaGenDeliteOps extends CudaGenEffect with BaseGenDeliteOps {
  import IR._
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case s:DeliteOpSingleTask[_] => throw new GenerationFailedException("CudaGen: DeliteOpSingleTask is not GPUable.")
      // TODO: Generate single thread version of this work
      //if(idxX == 0) {}
    case map:DeliteOpMap[_,_,_] => {
      if (parallelCudagen == false) {
        throw new GenerationFailedException("CudaGen: Nested DeliteOpMap is not GPUable.")
      }
      else {
        parallelCudagen = false
        gpuBlockSizeX = quote(map.in)+"->size()"
        val freeVars = getFreeVarBlock(map.func,Nil).filterNot(ele => ele==map.v)
        stream.println(addTab()+"if( %s < %s ) {".format("idxX",quote(map.in)+".size()"))
        tabWidth += 1
        val mapFunc = emitDevFunc(map.func, map.alloc.Type.typeArguments(0), List(map.v)++freeVars)
        if(freeVars.length==0)
          stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s)));".format(quote(sym),"idxX",mapFunc,quote(map.in),"idxX"))
        else
          stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s),%s));".format(quote(sym),"idxX",mapFunc,quote(map.in),"idxX",freeVars.map(quote).mkString(",")))
        if(getVarLink(sym) != null)
            stream.println(addTab()+"%s.dcUpdate(%s, %s.dcApply(%s));".format(getVarLink(sym),"idxX",quote(sym),"idxX"))
        tabWidth -= 1
        stream.println(addTab()+"}")
        allocOutput(sym,map.in.asInstanceOf[Sym[_]])
        parallelCudagen = true
      }
    }
    case zip: DeliteOpZipWith[_,_,_,_] => {
      if (parallelCudagen == false) {
        throw new GenerationFailedException("CudaGen: Nested DeliteOpZipWith is not GPUable.")
      }
      else {
        parallelCudagen = false
        gpuBlockSizeX = quote(zip.inA)+"->size()"
        val freeVars = getFreeVarBlock(zip.func,Nil).filterNot(ele => (ele==zip.v._1)||(ele==zip.v._2))
        stream.println(addTab()+"if( %s < %s ) {".format("idxX",quote(zip.inA)+".size()"))
        tabWidth += 1
        val zipFunc = emitDevFunc(zip.func, zip.alloc.Type.typeArguments(0), List(zip.v._1, zip.v._2))
        if(freeVars.length==0)
          stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s),%s.dcApply(%s)));".format(quote(sym),"idxX", zipFunc, quote(zip.inA),"idxX",quote(zip.inB),"idxX"))
        else
          stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s),%s.dcApply(%s),%s));".format(quote(sym),"idxX", zipFunc, quote(zip.inA),"idxX",quote(zip.inB),"idxX",freeVars.map(quote).mkString(",")))
        if(getVarLink(sym) != null)
            stream.println(addTab()+"%s.dcUpdate(%s, %s.dcApply(%s));".format(getVarLink(sym),"idxX",quote(sym),"idxX"))
        tabWidth -= 1
        stream.println(addTab()+"}")
        allocOutput(sym,zip.inA.asInstanceOf[Sym[_]])
        parallelCudagen = true
      }
    }
    case mapR:DeliteOpMapReduce[_,_,_] => {
      if (parallelCudagen == false) {
        // When nested, only pritimive type result can be generated
        stream.println(addTab()+"int %s = %s.apply(0);".format(quote(mapR.mV),quote(mapR.in)))
        emitBlock(mapR.map)
        emitValDef(mapR.rV._1.asInstanceOf[Sym[_]],quote(getBlockResult(mapR.map))) 
        stream.println(addTab()+"for(int cnt=1; cnt<%s.size(); cnt++) {".format(quote(mapR.in)))
        tabWidth += 1
        stream.println(addTab()+"%s = %s.apply(cnt);".format(quote(mapR.mV),quote(mapR.in)))
        emitBlock(mapR.map)
        emitValDef(mapR.rV._2.asInstanceOf[Sym[_]],quote(getBlockResult(mapR.map)))
        emitBlock(mapR.reduce)
        stream.println(addTab()+"%s = %s;".format(quote(mapR.rV._1.asInstanceOf[Sym[_]]),quote(getBlockResult(mapR.reduce))))
        tabWidth -= 1
        stream.println(addTab()+"}")
        emitValDef(sym,quote(mapR.rV._1))
      }
      else {
        emitValDef(mapR.rV._1.asInstanceOf[Sym[_]],quote(sym))
        emitValDef(mapR.rV._2.asInstanceOf[Sym[_]],quote(getBlockResult(mapR.map)))
        stream.println(addTab()+"int %s = %s.apply(0);".format(quote(mapR.mV),quote(mapR.in)))
        addVarLink(getBlockResult(mapR.map),sym)
        emitBlock(mapR.map)
        removeVarLink(getBlockResult(mapR.map),sym)
        stream.println(addTab()+"for(int cnt=1; cnt<%s.size(); cnt++) {".format(quote(mapR.in)))
        tabWidth += 1
        stream.println(addTab()+"%s = %s.apply(cnt);".format(quote(mapR.mV),quote(mapR.in)))
        emitBlock(mapR.map)
        emitBlock(mapR.reduce)
        tabWidth -= 1
        stream.println(addTab()+"}")
        allocOutput(sym,getBlockResult(mapR.map).asInstanceOf[Sym[_]])
      }
    }

    case foreach:DeliteOpForeach[_,_] =>
      if (parallelCudagen == false) {
        stream.println(addTab()+"for(int i_%s=0; i_%s < %s.size(); i_%s++) {".format(quote(sym),quote(sym),quote(foreach.in),quote(sym)))
        tabWidth += 1
        stream.println(addTab()+"%s %s = %s.apply(i_%s);".format(remap(foreach.v.Type),quote(foreach.v),quote(foreach.in),quote(sym)))
        emitBlock(foreach.func)
        tabWidth -= 1
        stream.println(addTab() + "}")
      }
      else {
        parallelCudagen = false
        gpuBlockSizeX = quote(foreach.in)+"->size()"
        val freeVars = getFreeVarBlock(foreach.func,Nil).filterNot(ele => ele==foreach.v)
        stream.println(addTab()+"if( %s < %s ) {".format("idxX",quote(foreach.in)+".size()"))
        tabWidth += 1
        val foreachFunc = emitDevFunc(foreach.func, null, List(foreach.v)++freeVars)
        if(freeVars.length==0)
          stream.println(addTab()+"%s(%s.dcApply(%s));".format(foreachFunc,quote(foreach.in),"idxX"))
        else
          stream.println(addTab()+"%s(%s.dcApply(%s),%s);".format(foreachFunc,quote(foreach.in),"idxX",freeVars.map(quote).mkString(",")))
        tabWidth -= 1
        stream.println(addTab()+"}")
        parallelCudagen = true
      }

    case _ => super.emitNode(sym,rhs)
  }
}

trait CGenDeliteOps extends CGenEffect with BaseGenDeliteOps {
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case s:DeliteOpSingleTask[_] =>
      emitBlock(s.block)
      emitValDef(sym,quote(getBlockResult(s.block)))

    //TODO: implement deliteops
    //case map:DeliteOpMap[_,_,_] =>
    //case zip: DeliteOpZipWith[_,_,_,_] =>
    //case mapR:DeliteOpMapReduce[_,_,_] =>
    case _ => super.emitNode(sym,rhs)
  }
}
