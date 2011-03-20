package ppl.delite.framework.ops

import java.io.{FileWriter, File, PrintWriter}

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericCodegen, GenericFatCodegen, GenerationFailedException}
import ppl.delite.framework.{DeliteCollection,Config}

//trait DeliteOpsExp extends BaseFatExp with EffectExp with VariablesExp with LoopsFatExp {
trait DeliteOpsExp extends BaseFatExp with EffectExp with VariablesExp with LoopsFatExp 
    with VariantsOpsExp with DeliteCollectionOpsExp
    with OrderingOpsExp with CastingOpsExp with ImplicitOpsExp with WhileExp  {
  
  /**
   * The base type of the DeliteOp hierarchy.
   */
  /*sealed*/ trait DeliteOp[A] extends Def[A]
//  sealed trait DeliteFatOp extends FatDef

  /**
   * A sequential task - will execute block in a single thread and respect any free variable dependencies inside it.
   *
   * @param  block   the task to execute; must be reified if it contains effectful operations!
   */
  class DeliteOpSingleTask[A](val block: Exp[A], val requireInputs: Boolean = false) extends DeliteOp[A]

  abstract class DeliteOpLoop[A] extends AbstractLoop[A] with DeliteOp[A]

//  case class DeliteOpFatLoop(val size: Exp[Int], val v: Sym[Int], val body: List[Def[Any]]) extends AbstractFatLoop with DeliteFatOp
  
  
  // for use in loops:

  case class DeliteCollectElem[A, C[X] <: DeliteCollection[X]](
    alloc: Exp[C[A]],
    func: Exp[A]
    // TODO: note that the alloc block right now directly references the size
    // which is not part of DeliteCollectElem instance. we might want to fix that 
  ) extends Def[C[A]]
  
  case class DeliteReduceElem[A](
    func: Exp[A],
    rV: (Sym[A], Sym[A]),
    rFunc: Exp[A]
  ) extends Def[A]
  
  
  class DeliteOpLoopMirrored[A](f: Transformer, o: DeliteOpLoop[A]) extends DeliteOpLoop[A] {   // TODO!
    val size = f(o.size)
    val v = f(o.v).asInstanceOf[Sym[Int]]
    val body = mirrorLoopBody(o.body, f)
  }
  
  
/*
  abstract class DeliteOpMapNew[A,C[X] <: DeliteCollection[X]]() extends DeliteOp[C[A]] {
    val size: Exp[Int]
    val v: Sym[Int]
    val func: Exp[A]
    val alloc: Exp[C[A]]
  }

  abstract class DeliteOpReduceNew[A]() extends DeliteOp[A] {
    val size: Exp[Int]
    val v: Sym[Int]
    val func: Exp[A]
    val rV: (Sym[A], Sym[A])
    val rFunc: Exp[A]
  }
*/



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
    val index: Sym[Int]
    val body: Exp[Unit]
  }

  /**
   * An while loop - will emit an while loop DEG node as well as a kernel for the body
   *
   * @param  cond  condition expression, will be emitted as a kernel
   * @param  body   the body of the loop
   */
  trait DeliteOpWhileLoop extends DeliteOp[Unit] {
    val cond: Exp[Boolean]
    val body: Exp[Unit]
  }

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
    val v: Sym[A]
    val func: Exp[B]
    val alloc: Exp[C[B]]

    lazy val variant = {
      implicit val mA: Manifest[A] = v.Type.asInstanceOf[Manifest[A]]
      implicit val mB: Manifest[B] = func.Type.asInstanceOf[Manifest[B]]
      implicit val mCA: Manifest[C[A]] = in.Type.asInstanceOf[Manifest[C[A]]]
      implicit val mCB: Manifest[C[B]] = alloc.Type.asInstanceOf[Manifest[C[B]]]
      reifyEffects {
        var i = var_new(unit(0))
        var vs = var_new(unit(null).asInstanceOfL[A])
        while (i < in.size) {
          vs = in(i)
          rebind(v.asInstanceOf[Sym[A]], ReadVar(vs))
          // why is the alloc dependency not lifted out of the loop when the variant block is emitted?
          alloc(i) = func
          i += 1
        }
        alloc
      }
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
    val v: (Sym[A],Sym[B])
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
    val v: (Sym[A],Sym[A])
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
  abstract class DeliteOpMapReduce[A,R,C[X] <: DeliteCollection[X]]() extends DeliteOp[R] with DeliteOpReduceLikeWhileLoopVariant {
    val in: Exp[C[A]]
    //val acc: Exp[R]

    // for accumulating each partial sum
    val mV: Sym[A]
    //val mapreduce: Exp[R] // reified of Exp[(R,A)] => Exp[R] composition of map and reduce
    val map: Exp[R]

    // for reducing remaining partial sums
    val rV: (Sym[R],Sym[R])
    val reduce: Exp[R]

    lazy val acc = {
      implicit val mR = map.Type.asInstanceOf[Manifest[R]]
      var_new(unit(null).asInstanceOfL[R])
    }
    lazy val index = {
      var_new(unit(0))
    }
    // this is a workaround for reification and vars not really working well together
    // we need to output the block as an Exp, but the nested scopes need to use it as a var
    lazy val Acc = {
      implicit val mR = map.Type.asInstanceOf[Manifest[R]]
      reifyEffects(acc)
    }
    lazy val Index = reifyEffects(index)
    // end workaround
    lazy val init = {
      implicit val mA = mV.Type.asInstanceOf[Manifest[A]]
      implicit val mR = map.Type.asInstanceOf[Manifest[R]]
      reifyEffects {
        var vs = var_new(in(index))
        rebind(mV.asInstanceOf[Sym[A]], ReadVar(vs))
        acc = map
      }
    }
    lazy val variant = {
      implicit val mA = mV.Type.asInstanceOf[Manifest[A]]
      implicit val mR = map.Type.asInstanceOf[Manifest[R]]
      reifyEffects {
        index += 1
        while (index < in.size) {
          var vs = var_new(in(index))
          //rebind(mV.asInstanceOf[Sym[A]], ReadVar(vs))
          var x = var_new(map)
          rebind(rV._1.asInstanceOf[Sym[R]], ReadVar(acc))
          rebind(rV._2.asInstanceOf[Sym[R]], ReadVar(x))
          acc = reduce
          index += 1
        }
        acc
      }
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
    val zV: (Sym[A],Sym[B])
    val zip: Exp[R]
    // for reducing remaining partial sums
    val rV: (Sym[R],Sym[R])
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
  abstract class DeliteOpForeach[A,C[X] <: DeliteCollection[X]]() extends DeliteOp[Unit] with DeliteOpMapLikeWhileLoopVariant {
    val in: Exp[C[A]]
    val v: Sym[A]
    val func: Exp[Unit]
    val i: Sym[Int]
    val sync: Exp[List[Any]]

    lazy val alloc = Const()
    lazy val variant = {
      implicit val mA: Manifest[A] = v.Type.asInstanceOf[Manifest[A]]
      reifyEffects {
        var index = var_new(unit(0))
        var vs = var_new(unit(null).asInstanceOfL[A])
        while (index < in.size) {
          vs = in(index)
          rebind(v.asInstanceOf[Sym[A]], ReadVar(vs))
          //reflectEffect(findDefinition(func.asInstanceOf[Sym[Unit]]).get.rhs)
          var x = var_new(func)
          index += 1
        }
        alloc
      }
    }
  }

  // TODO: should we make all DeliteOps be boundable? This is probably not the right way to do this anyways.
  abstract class DeliteOpForeachBounded[B,A <: B,C[X <: B] <: DeliteCollection[X]] extends DeliteOp[Unit] {
    val in: Exp[C[A]]
    val v: Sym[A]
    val func: Exp[Unit]
    val i: Sym[Int]
    val sync: Exp[List[Any]]
  }

  // used by delite code generators to handle nested delite ops
  var deliteKernel: Boolean = false //_
  var deliteResult: Option[List[Exp[Any]]] = None//_
  var deliteInputs: List[Sym[Any]] = Nil//_

  // TODO: move to lms?
  def rebind(sym: Sym[Any], rhs: Def[Any]) = createDefinition(sym, rhs).rhs
  def getVar[A](e: Exp[A]) = e match {
    case Def(Reify(x, u, effects)) if x.isInstanceOf[Var[A]] => x.asInstanceOf[Var[A]]
    case _ => throw new Exception("getVar called on non-var type")
  }


  //////////////
  // mirroring

  override def mirrorFatDef[A:Manifest](d: Def[A], f: Transformer): Def[A] = mirrorLoopBody(d,f) // TODO: cleanup

  def mirrorLoopBody[A](d: Def[A], f: Transformer): Def[A] = {
    d match {
      case e: DeliteCollectElem[a,DeliteCollection] => 
        DeliteCollectElem[a,DeliteCollection]( // need to be a case class for equality (do we rely on equality?)
          alloc = f(e.alloc),
          func = f(e.func)
        ).asInstanceOf[Def[A]]
      case e: DeliteReduceElem[a] => 
        DeliteReduceElem[a](
          func = f(e.func),
          rV = (f(e.rV._1).asInstanceOf[Sym[a]], f(e.rV._2).asInstanceOf[Sym[a]]), // need to transform bound vars ??
          rFunc = f(e.rFunc)
        ).asInstanceOf[Def[A]]
    }
  }
  

/*  
  // heavy type casting ahead!
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = e match {
    case e: DeliteCollectElem[a,b] => 
//    toFatPieceAtom(DeliteCollectElem[a,DeliteCollection]( // need to be a case class for equality!
    toAtom(DeliteCollectElem[a,DeliteCollection]( // need to be a case class for equality!
      alloc = f(e.alloc).asInstanceOf[Exp[DeliteCollection[a]]],
      func = f(e.func)
    ).asInstanceOf[Def[A]])
    case e: DeliteReduceElem[a] => 
    toAtom(DeliteReduceElem[a](
      func = f(e.func),
      rV = (f(e.rV._1).asInstanceOf[Sym[a]], f(e.rV._2).asInstanceOf[Sym[a]]), // should transform bound vars as well ??
      rFunc = f(e.rFunc)
    ).asInstanceOf[Def[A]])
    case _ => super.mirror(e, f)
  }
*/
}

trait BaseGenDeliteOps extends BaseGenLoopsFat with LoopFusionOpt {
  val IR: DeliteOpsExp
  import IR._

/*
  // overridden only to attach DeliteFatOp trait to result ...
  override def fatten(e: TP[Any]): TTP = e.rhs match {
    case op: DeliteOpLoop[_] => 
      TTP(List(e.sym), DeliteFatLoop(op.size, op.v, List(op.body)))
    case _ => super.fatten(e)
  }
*/


  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case e: DeliteCollectElem[_,_] => Some(e.func)
    case _ => super.unapplySimpleCollect(e)
  }

  override def shouldApplyFusion(currentScope: List[TTP])(result: Exp[Any]) = Config.opfusionEnabled


  override def syms(e: Any): List[Sym[Any]] = e match { //TR TODO: question -- is alloc a dependency (should be part of result) or a definition (should not)???
                                                        // aks: answer -- we changed it to be internal to the op to make things easier for CUDA. not sure if that still needs
                                                        // to be the case. similar question arises for sync
    case s: DeliteOpSingleTask[_] if !s.requireInputs => syms(s.block)
    case s: DeliteOpSingleTask[_] => syms(s.block) ++ super.syms(e) // super call: add case class syms!
    case op: DeliteCollectElem[_,_] => syms(op.func) ++ syms(op.alloc)
    case op: DeliteReduceElem[_] => syms(op.func) ++ syms(op.rFunc)
    case map: DeliteOpMap[_,_,_] => /*if (shallow) syms(map.in) else */ syms(map.in) ++ syms(map.alloc) ++ syms(map.func)
    case zip: DeliteOpZipWith[_,_,_,_] => /*if (shallow) syms(zip.inA) ++ syms(zip.inB) else*/ syms(zip.inA) ++ syms(zip.inB) ++ syms(zip.alloc) ++ syms(zip.func)
    case red: DeliteOpReduce[_] => /*if (shallow) syms(red.in) else*/ syms(red.in) ++ syms(red.func)
    case mapR: DeliteOpMapReduce[_,_,_] => /*if (shallow) syms(mapR.in) else*/ syms(mapR.in) ++ syms(mapR.map) ++ syms(mapR.reduce)
    case zipR: DeliteOpZipWithReduce[_,_,_,_] => /*if (shallow) syms(zipR.inA) ++ syms(zipR.inB) else*/ syms(zipR.inA) ++ syms(zipR.inB) ++ syms(zipR.zip) ++ syms(zipR.reduce)
    case foreach: DeliteOpForeach[_,_] => /*if (shallow) syms(foreach.in) else*/ syms(foreach.in) ++ syms(foreach.func) ++ syms(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => /*if (shallow) syms(foreach.in) else*/ syms(foreach.in) ++ syms(foreach.func) ++ syms(foreach.sync)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match { //TR TODO
    case s: DeliteOpSingleTask[_] => effectSyms(s.block)
    case op: DeliteCollectElem[_,_] => effectSyms(op.func) ++ effectSyms(op.alloc)
    case op: DeliteReduceElem[_] => List(op.rV._1, op.rV._2) ++ effectSyms(op.func) ++ effectSyms(op.rFunc)
    case zip: DeliteOpZipWith[_,_,_,_] => zip.v._1::zip.v._2::effectSyms(zip.alloc):::effectSyms(zip.func)
    case map: DeliteOpMap[_,_,_] => map.v::effectSyms(map.alloc):::effectSyms(map.func)
    case mapR: DeliteOpMapReduce[_,_,_] => mapR.mV::mapR.rV._1::mapR.rV._2::effectSyms(mapR.map):::effectSyms(mapR.reduce)
    case zipR: DeliteOpZipWithReduce[_,_,_,_] => zipR.zV._1::zipR.zV._2::zipR.rV._1::zipR.rV._2::effectSyms(zipR.zip) ++ effectSyms(zipR.reduce)
    case red: DeliteOpReduce[_] => red.v._1::red.v._2::effectSyms(red.func)
    case foreach: DeliteOpForeach[_,_] => foreach.v::foreach.i::effectSyms(foreach.func):::effectSyms(foreach.sync)
    case foreach: DeliteOpForeachBounded[_,_,_] => foreach.v::foreach.i::effectSyms(foreach.func):::effectSyms(foreach.sync)
    case _ => super.boundSyms(e)
  }

}

trait ScalaGenDeliteOps extends ScalaGenLoopsFat with BaseGenDeliteOps {
  import IR._

  def quotearg(x: Sym[Any]) = quote(x) + ": " + quotetp(x)
  def quotetp(x: Sym[Any]) = remap(x.Type)
  def quoteZero(x: Sym[Any]) = x.Type.toString match { 
    case "Int" | "Long" | "Float" | "Double" => "0" 
    case "Boolean" => "false"
    case _ => "null" 
  }

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case op: AbstractFatLoop =>
        if (!deliteKernel) {
          (symList zip op.body) foreach {
            case (sym, elem: DeliteCollectElem[_,_]) =>
              stream.println("val " + quote(sym) + " = {")
              emitBlock(elem.alloc)
              stream.println(quote(getBlockResult(elem.alloc)))
              stream.println("}")
            case (sym, elem: DeliteReduceElem[_]) =>
              stream.println("var " + quotearg(sym) + " = " + quoteZero(sym)) // TODO: need explicit zero?
          }
          stream.println("var " + quote(op.v) + " = 0")
          stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
          val elemFuncs = op.body map { // don't emit dependencies twice!
            case elem: DeliteCollectElem[_,_] => elem.func
            case elem: DeliteReduceElem[_] => elem.func
          }
          emitFatBlock(elemFuncs)
          (symList zip op.body) foreach {
            case (sym, elem: DeliteCollectElem[_,_]) =>
              //emitBlock(elem.func)
              stream.println(quote(sym) + ".dcUpdate(" + quote(op.v) + ", " + quote(getBlockResult(elem.func)) + ")")
            case (sym, elem: DeliteReduceElem[_]) =>
              //emitBlock(elem.func)
              stream.println("val " + quote(elem.rV._1) + " = " + quote(sym))
              stream.println("val " + quote(elem.rV._2) + " = " + quote(getBlockResult(elem.func)))
              emitBlock(elem.rFunc)
              stream.println(quote(sym) + " = " + quote(getBlockResult(elem.rFunc)))
          }
          stream.println(quote(op.v) + " += 1")
          stream.println("} // end fat loop " + symList.map(quote).mkString(","))
        } else {
          // kernel mode
          val kernelName = symList.map(quote).mkString("")
          val actType = "activation_"+kernelName
          deliteKernel = false
          stream.println("val " + kernelName + " = new generated.scala.DeliteOpMultiLoop[" + actType + "] {")

          stream.println("def size = " + quote(op.size))
          stream.println("def alloc: " + actType + " = {")
          stream.println("val __act = new " + actType)
          (symList zip op.body) foreach {
            case (sym, elem: DeliteCollectElem[_,_]) =>
              emitBlock(elem.alloc)
              stream.println("__act." + quote(sym) + " = " + quote(getBlockResult(elem.alloc)))
            case (sym, elem: DeliteReduceElem[_]) =>
              // default zero, might need to go explicit
          }
          stream.println("__act")
          stream.println("}")
          stream.println("def split(__act: " + actType + "): " + actType + " = {")
          if (op.body.exists(_.isInstanceOf[DeliteReduceElem[_]])) {
            stream.println("val __act2 = new " + actType)
            (symList zip op.body) foreach {
              case (sym, elem: DeliteCollectElem[_,_]) =>
                stream.println("__act2." + quote(sym) + " = " + "__act." + quote(sym))
              case (sym, elem: DeliteReduceElem[_]) =>
                // default zero, might need to go explicit
            }
            stream.println("__act2")
          } else {
            stream.println("__act")
          }
          stream.println("}")
          stream.println("def process(__act: " + actType + ", " + quotearg(op.v) + "): Unit = {")
          val elemFuncs = op.body map { // don't emit dependencies twice!
            case elem: DeliteCollectElem[_,_] => elem.func
            case elem: DeliteReduceElem[_] => elem.func
          }
          emitFatBlock(elemFuncs)
          (symList zip op.body) foreach {
            case (sym, elem: DeliteCollectElem[_,_]) =>
              //emitBlock(elem.func)
              stream.println("__act." + quote(sym) + ".dcUpdate(" + quote(op.v) + ", " + quote(getBlockResult(elem.func)) + ")")
            case (sym, elem: DeliteReduceElem[_]) =>
              //emitBlock(elem.func)
              stream.println("val " + quote(elem.rV._1) + " = " + "__act." + quote(sym))
              stream.println("val " + quote(elem.rV._2) + " = " + quote(getBlockResult(elem.func)))
              emitBlock(elem.rFunc)
              stream.println("__act." + quote(sym) + " = " + quote(getBlockResult(elem.rFunc)))
          }
          stream.println("}")
          stream.println("def combine(__act: " + actType + ", rhs: " + actType + "): Unit = {")
          (symList zip op.body) foreach {
            case (sym, elem: DeliteCollectElem[_,_]) =>
            case (sym, elem: DeliteReduceElem[_]) =>
              stream.println("val " + quote(elem.rV._1) + " = " + "__act." + quote(sym))
              stream.println("val " + quote(elem.rV._2) + " = " + "rhs." + quote(sym))
              emitBlock(elem.rFunc)
              stream.println("__act." + quote(sym) + " = " + quote(getBlockResult(elem.rFunc)))
          }
          stream.println("}")

          stream.println("}")
          deliteKernel = true
        }
      case _ => super.emitFatNode(symList, rhs)
  }


  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case s:DeliteOpSingleTask[_] => {
      println("EMIT single "+s)
      val save = deliteKernel
      deliteKernel = false
      val b = s.block
      if (!save) {
        // straight-line
        stream.println("val " + quote(sym) + " = { " )
        emitBlock(b)
        stream.println(quote(getBlockResult(b)))
        stream.println("}")
      }
      else {
        // method wrapper
        stream.println("def " + quote(sym) + "_block = { ")
        emitBlock(b)
        stream.println(quote(getBlockResult(b)))
        stream.println("}")
        stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
      }
      deliteKernel = save
    }
    case op: AbstractLoop[_] => // TODO: we'd like to always have fat loops but currently they are not allowed to have effects
      if (!deliteKernel) {
        op.body match {
          case elem: DeliteCollectElem[_,_] =>
            stream.println("val " + quote(sym) + " = {")
            emitBlock(elem.alloc)
            stream.println(quote(getBlockResult(elem.alloc)))
            stream.println("}")
          case elem: DeliteReduceElem[_] =>
            stream.println("var " + quotearg(sym) + " = " + quoteZero(sym)) // TODO: need explicit zero?
        }
        stream.println("var " + quote(op.v) + " = 0")
        stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin thin loop " + quote(sym))
        op.body match {
          case elem: DeliteCollectElem[_,_] =>
            emitBlock(elem.func)
            stream.println(quote(sym) + ".dcUpdate(" + quote(op.v) + ", " + quote(getBlockResult(elem.func)) + ")")
          case elem: DeliteReduceElem[_] =>
            emitBlock(elem.func)
            stream.println("val " + quote(elem.rV._1) + " = " + quote(sym))
            stream.println("val " + quote(elem.rV._2) + " = " + quote(getBlockResult(elem.func)))
            emitBlock(elem.rFunc)
            stream.println(quote(sym) + " = " + quote(getBlockResult(elem.rFunc)))
        }
        stream.println(quote(op.v) + " += 1")
        stream.println("} // end thin loop " + quote(sym))
      } else {
        stream.println("TODO: thin loop codegen")
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
    case foreach:DeliteOpForeachBounded[_,_,_] => {
      if (deliteKernel == false){
        stream.println("def " + quote(sym) + "_block = {")
        stream.println("var forIdx = 0")
        stream.println("while (forIdx < " + quote(getBlockResult(foreach.in.asInstanceOf[Exp[Any]])) + ".size) {")
        stream.println("val " + quote(foreach.v) + " = " + quote(getBlockResult(foreach.in.asInstanceOf[Exp[Any]])) + ".dcApply(forIdx)")
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
        stream.println("def in = " + quote(getBlockResult(foreach.in.asInstanceOf[Exp[Any]])))
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

trait CudaGenDeliteOps extends CudaGenLoopsFat with BaseGenDeliteOps {
  import IR._
  
  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case op: AbstractFatLoop =>
      println("TODO: implement emitFatNode in CudaGenDeliteOps")
      throw new GenerationFailedException("TODO: implement emitFatNode in CudaGenDeliteOps")
    case _ => super.emitFatNode(symList, rhs)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case s:DeliteOpSingleTask[_] => throw new GenerationFailedException("CudaGen: DeliteOpSingleTask is not GPUable.")
      // TODO: Generate single thread version of this work

    case map:DeliteOpMap[_,_,_] => {
      if(!isPrimitiveType(map.func.Type)) new GenerationFailedException("CudaGen: Only primitive Types are allowed for map.")
      if(!isPrimitiveType(map.v.Type)) new GenerationFailedException("CudaGen: Only primitive Types are allowed for map.")
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength(quote(map.in)+"->size()")
      stream.println(addTab()+"if( %s < %s ) {".format(currDimStr,quote(map.in)+".size()"))
      tabWidth += 1
      val (mapFunc,freeVars) = emitDevFunc(map.func, List(map.v))
      if(freeVars.length==0)
        stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s)));".format(quote(sym),currDimStr,mapFunc,quote(map.in),currDimStr))
      else
        stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s),%s));".format(quote(sym),currDimStr,mapFunc,quote(map.in),currDimStr,freeVars.map(quote).mkString(",")))
      tabWidth -= 1
      stream.println(addTab()+"}")
	    if(map.in!=map.alloc)
      	allocOutput(sym,map.in.asInstanceOf[Sym[_]])
	    else
      	allocReference(sym,map.in.asInstanceOf[Sym[_]])
      currDim -= 1
    }

    case zip: DeliteOpZipWith[_,_,_,_] => {
      if(!isPrimitiveType(zip.func.Type)) new GenerationFailedException("CudaGen: Only primitive Types are allowed for zipwith.")
      if(!isPrimitiveType(zip.v._1.Type )) new GenerationFailedException("CudaGen: Only primitive Types are allowed for zipwith.")
      if(!isPrimitiveType(zip.v._2.Type)) new GenerationFailedException("CudaGen: Only primitive Types are allowed for zipwith.")
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength(quote(zip.inA)+"->size()")
      stream.println(addTab()+"if( %s < %s ) {".format(currDimStr,quote(zip.inA)+".size()"))
      tabWidth += 1
      val (zipFunc,freeVars) = emitDevFunc(zip.func, List(zip.v._1, zip.v._2))
      if(freeVars.length==0)
        stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s),%s.dcApply(%s)));".format(quote(sym),currDimStr, zipFunc, quote(zip.inA),currDimStr,quote(zip.inB),currDimStr))
      else
        stream.println(addTab()+"%s.dcUpdate(%s, %s(%s.dcApply(%s),%s.dcApply(%s),%s));".format(quote(sym),currDimStr, zipFunc, quote(zip.inA),currDimStr,quote(zip.inB),currDimStr,freeVars.map(quote).mkString(",")))
      tabWidth -= 1
      stream.println(addTab()+"}")
	    if(zip.inA==zip.alloc)
      	allocReference(sym,zip.inA.asInstanceOf[Sym[_]])
	    else if(zip.inB==zip.alloc)
      	allocReference(sym,zip.inB.asInstanceOf[Sym[_]])
	    else
      	allocOutput(sym,zip.inA.asInstanceOf[Sym[_]])
      currDim -= 1
    }

    case foreach:DeliteOpForeach[_,_] => {
      if(!isPrimitiveType(foreach.v.Type)) new GenerationFailedException("CudaGen: Only primitive Types are allowed for input of foreach.")
      currDim += 1
      setCurrDimLength(quote(foreach.in)+"->size()")
      val currDimStr = getCurrDimStr()
      stream.println(addTab()+"if( %s < %s ) {".format(currDimStr,quote(foreach.in)+".size()"))
      tabWidth += 1
      val (foreachFunc,freeVars) = emitDevFunc(foreach.func, List(foreach.v))
      if(freeVars.length==0)
        stream.println(addTab()+"%s(%s.dcApply(%s));".format(foreachFunc,quote(foreach.in),currDimStr))
      else
        stream.println(addTab()+"%s(%s.dcApply(%s),%s);".format(foreachFunc,quote(foreach.in),currDimStr,freeVars.map(quote).mkString(",")))
      tabWidth -= 1
      stream.println(addTab()+"}")
      currDim -= 1
    }

    case red: DeliteOpReduce[_] => {
      if(!isPrimitiveType(red.func.Type)) new GenerationFailedException("CudaGen: Only primitive Types are allowed for reduce.")
      if(currDim < 1) new GenerationFailedException("CudaGen: Reduction on the 1'st dimension is not supported yet.")
      val (reducFunc,freeVars) = emitDevFunc(red.func, List(red.v._1, red.v._2))
      stream.println(addTab()+"%s reducVal = %s.apply(0);".format(remap(sym.Type),quote(red.in)))
      stream.println(addTab()+"for(int i=1; i<%s.size(); i++) {".format(quote(red.in)))
      tabWidth += 1
	    if(freeVars.length==0)
      	stream.println(addTab()+"reducVal = %s(reducVal,%s.apply(i));".format(reducFunc,quote(red.in)))
	    else
      	stream.println(addTab()+"reducVal = %s(reducVal,%s.apply(i),%s);".format(reducFunc,quote(red.in),freeVars.map(quote).mkString(",")))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitValDef(sym,"reducVal")
    }

    case mapR:DeliteOpMapReduce[_,_,_] => {
      //if(!isPrimitiveType(mapR.mV.Type)) new GenerationFailedException("CudaGen: Only primitive Types are allowed for MapReduce.")
      //if(!isPrimitiveType(mapR.reduce.Type)) new GenerationFailedException("CudaGen: Only primitive Types are allowed for MapReduce.")
      val constrained = isPrimitiveType(mapR.mV.Type) && isPrimitiveType(mapR.reduce.Type)
      if(constrained) {
        stream.println(addTab()+"%s %s = %s.apply(0);".format(remap(mapR.mV.Type),quote(mapR.mV),quote(mapR.in)))
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
        useLocalVar = true
        stream.println(addTab()+"%s = %s;".format(quote(mapR.rV._1),quote(sym)))
        val reduceLocalVar = getNewLocalVar()
        val outLocalVar = getNewLocalVar()
        val nextDimStr = getNextDimStr()
        saveLocalVar(sym,nextDimStr,outLocalVar)
        stream.println(addTab()+"%s %s = 0;".format(remap(sym.Type.typeArguments(0)),outLocalVar))
        stream.println(addTab()+"for(int cnt=0; cnt<%s.size(); cnt++) {".format(quote(mapR.in)))
        tabWidth += 1
        stream.println(addTab()+"%s %s = %s.dcApply(cnt);".format(remap(mapR.mV.Type),quote(mapR.mV),quote(mapR.in)))
        emitBlock(mapR.map)
        stream.println(addTab()+"%s = %s;".format(quote(mapR.rV._2),quote(getBlockResult(mapR.map))))
        stream.println(addTab()+"%s %s = %s;".format(remap(sym.Type.typeArguments(0)),reduceLocalVar,getLocalVar(getBlockResult(mapR.map),nextDimStr)))
        saveLocalVar(mapR.rV._1,nextDimStr,outLocalVar)
        saveLocalVar(mapR.rV._2,nextDimStr,reduceLocalVar)
        emitBlock(mapR.reduce)
        tabWidth -= 1
        stream.println(addTab()+"}")
        allocReference(mapR.rV._1.asInstanceOf[Sym[_]],getBlockResult(mapR.map).asInstanceOf[Sym[_]])
        allocReference(mapR.rV._2.asInstanceOf[Sym[_]],getBlockResult(mapR.map).asInstanceOf[Sym[_]])
        allocOutput(sym,getBlockResult(mapR.map).asInstanceOf[Sym[_]],true)
        stream.println(addTab()+"%s.dcUpdate(%s,%s);".format(quote(sym),nextDimStr,outLocalVar))
        useLocalVar = false
      }
    }

    case _ => super.emitNode(sym,rhs)
  }
}

trait CGenDeliteOps extends CGenEffect with BaseGenDeliteOps {
  import IR._

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case op: AbstractFatLoop =>
      println("TODO: implement emitFatNode in CGenDeliteOps")
      throw new GenerationFailedException("TODO: implement emitFatNode in CGenDeliteOps")
    case _ => super.emitFatNode(symList, rhs)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
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
