package ppl.dsl.optigraph.ops

import java.io.PrintWriter

import reflect.{Manifest,SourceContext}

import scala.virtualization.lms.common._
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import scala.virtualization.lms.util._

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures.DeliteArray
import ppl.delite.framework.Util._

import ppl.dsl.optigraph._

trait GIterableOps extends Variables {
  this: OptiGraph =>

  /**
   * GIterable objects are returned by range words on graphs, nodes, and collections
   * and can be used in iterations and parallel filter and reduction expressions
   * (they cannot be created)
   */

  implicit def repGIterableToGIterableOps[T:Manifest](iter: Rep[GIterable[T]]) = new GIterableOpsCls(iter)

  /** Operations of GIterables */
  class GIterableOpsCls[T:Manifest](iter: Rep[GIterable[T]]) {
    /** Parallel iteration */
    def foreach(block: Rep[T] => Rep[Unit]): Rep[Unit] = iter_foreach(iter, block)
    /** Sequential iteration */
    def forseq(block: Rep[T] => Rep[Unit]): Rep[Unit] = iter_for(iter, None, block)
    def forseq(filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[Unit]): Rep[Unit] = iter_for(iter, Some(filter), block)
    /** Returns a filtered GIterable collection of elements */
    def filter(pred: Rep[T] => Rep[Boolean]): Rep[GIterable[T]] = iter_filter(iter, pred)
    /** Reductions */
    def sum[A:Manifest:Numeric](block: Rep[T] => Rep[A]): Rep[A] = iter_sum(iter, block)
    def sum[A:Manifest:Numeric](filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A] = iter_sumIf(iter, filter, block)
    def product[A:Manifest:Numeric](block: Rep[T] => Rep[A]): Rep[A] = iter_product(iter, block)
    def product[A:Manifest:Numeric](filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A] = iter_productIf(iter, filter, block)
    def max[A:Manifest:Ordering](block: Rep[T] => Rep[A]): Rep[A] = iter_max(iter, block)
    def max[A:Manifest:Ordering](filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A] = iter_maxIf(iter, filter, block)
    def min[A:Manifest:Ordering](block: Rep[T] => Rep[A]): Rep[A] = iter_min(iter, block)
    def min[A:Manifest:Ordering](filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A] = iter_minIf(iter, filter, block)
    // counts the number of elements for which the predicate holds
    def count(pred: Rep[T] => Rep[Boolean]): Rep[Int] = iter_count(iter, pred)
    // boolean AND
    def all(block: Rep[T] => Rep[Boolean]): Rep[Boolean] = iter_all(iter, block)
    def all(filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[Boolean]): Rep[Boolean] = iter_allIf(iter, filter, block)
    // boolean OR
    def any(block: Rep[T] => Rep[Boolean]): Rep[Boolean] = iter_any(iter, block)
    def any(filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[Boolean]): Rep[Boolean] = iter_anyIf(iter, filter, block)
    /** Returns the elements as a set */
    def toSet(): Rep[GSet[T]] = iter_toset(iter)

    /* Returns true if the object is contained in the GIterable's array */
    def contains(n: Rep[T]): Rep[Boolean] = iter_contains(iter, n)

    // accessors
    def length(implicit ctx: SourceContext) = giterable_raw_size(iter)
  }

  def iter_foreach[T:Manifest](iter: Rep[GIterable[T]], block: Rep[T] => Rep[Unit]): Rep[Unit]
  def iter_for[T:Manifest](iter: Rep[GIterable[T]], filter: Option[Rep[T] => Rep[Boolean]], block: Rep[T] => Rep[Unit]): Rep[Unit]
  def iter_filter[T:Manifest](iter: Rep[GIterable[T]], pred: Rep[T] => Rep[Boolean]): Rep[GIterable[T]]
  def iter_sum[T:Manifest, A:Manifest:Numeric](iter: Rep[GIterable[T]], block: Rep[T] => Rep[A]): Rep[A]
  def iter_sumIf[T:Manifest, A:Manifest:Numeric](iter: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A]
  def iter_product[T:Manifest, A:Manifest:Numeric](iter: Rep[GIterable[T]], block: Rep[T] => Rep[A]): Rep[A]
  def iter_productIf[T:Manifest, A:Manifest:Numeric](iter: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A]
  def iter_max[T:Manifest, A:Manifest:Ordering](iter: Rep[GIterable[T]], block: Rep[T] => Rep[A]): Rep[A]
  def iter_maxIf[T:Manifest, A:Manifest:Ordering](iter: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A]
  def iter_min[T:Manifest, A:Manifest:Ordering](iter: Rep[GIterable[T]], block: Rep[T] => Rep[A]): Rep[A]
  def iter_minIf[T:Manifest, A:Manifest:Ordering](iter: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[A]): Rep[A]
  def iter_count[T:Manifest](iter: Rep[GIterable[T]], block: Rep[T] => Rep[Boolean]): Rep[Int]
  def iter_all[T:Manifest](iter: Rep[GIterable[T]], block: Rep[T] => Rep[Boolean]): Rep[Boolean]
  def iter_allIf[T:Manifest](iter: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[Boolean]): Rep[Boolean]
  def iter_any[T:Manifest](iter: Rep[GIterable[T]], block: Rep[T] => Rep[Boolean]): Rep[Boolean]
  def iter_anyIf[T:Manifest](iter: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean], block: Rep[T] => Rep[Boolean]): Rep[Boolean]
  def iter_toset[T:Manifest](iter: Rep[GIterable[T]]): Rep[GSet[T]]
  def iter_contains[T:Manifest](iter: Rep[GIterable[T]], n: Rep[T]): Rep[Boolean]

  //TODO do this for all of the methods below
  def new_iterable[T:Manifest](data: Rep[DeliteArray[T]], offset: Rep[Int], size: Rep[Int])(implicit ctx: SourceContext): Rep[GIterable[T]]
  def new_iterable_imm[T:Manifest](data: Rep[DeliteArray[T]], offset: Rep[Int], size: Rep[Int])(implicit ctx: SourceContext): Rep[GIterable[T]]
  def new_empty_iterable[T:Manifest](): Rep[GIterable[T]]
  def giterable_raw_size[T:Manifest](iter: Rep[GIterable[T]])(implicit ctx: SourceContext): Rep[Int]
  def delitearray_giterable_append[T:Manifest](x: Rep[DeliteArray[GIterable[T]]], i: Rep[Int], y: Rep[T]): Rep[Unit]
}

trait GIterableCompilerOps extends GIterableOps {
  this: OptiGraphCompiler =>

  def giterable_raw_data[A:Manifest](x: Rep[GIterable[A]])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
  def giterable_set_raw_data[A:Manifest](x: Rep[GIterable[A]], data: Rep[DeliteArray[A]])(implicit ctx: SourceContext): Rep[Unit]
  def giterable_raw_size[A:Manifest](x: Rep[GIterable[A]])(implicit ctx: SourceContext): Rep[Int]
  def giterable_set_raw_size[A:Manifest](x: Rep[GIterable[A]], newVal: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def giterable_raw_update[A:Manifest](x: Rep[GIterable[A]], i: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def giterable_raw_apply[A:Manifest](x: Rep[GIterable[A]], i: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  def giterable_raw_insert[A:Manifest](x: Rep[GIterable[A]], i: Rep[Int], y: Rep[A]): Rep[Unit]
}

trait GIterableOpsExp extends GIterableOps with VariablesExp with BaseFatExp with DeliteCollectionOpsExp {
  this: GIterableImplOps with OptiGraphExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class GIterableNew[T:Manifest](data: Exp[DeliteArray[T]], offset: Exp[Int], size: Exp[Int]) extends DeliteStruct[GIterable[T]] {
    val elems = copyTransformedElems(collection.Seq("_data" -> data, "_offset" -> offset, "_size" -> size))
    val mT = manifest[T]
  }
  case class GIterableNewImm[T:Manifest](data: Exp[DeliteArray[T]], offset: Exp[Int], size: Exp[Int]) extends DeliteStruct[GIterable[T]] {
    val elems = copyTransformedElems(collection.Seq("_data" -> data, "_offset" -> offset, "_size" -> size))
    val mT = manifest[T]
  } 
  case class GIterableNewEmpty[T:Manifest]() extends DeliteStruct[GIterable[T]] {
    val elems = copyTransformedElems(collection.Seq("_data" -> var_new(DeliteArray[T](0)).e, "_offset" -> var_new(unit(0)).e, "_size" -> var_new(unit(0)).e))
    val mT = manifest[T]
  }

  ///////////////////////////////////////////////////
  // implemented via DeliteOps

  // parallel iteration (no reduction assignments in the loop body)
  case class GIterableForeach[T:Manifest](in: Exp[GIterable[T]], func: Exp[T] => Exp[Unit])
    extends DeliteOpForeach[T] {
    //extends DeliteOpForeachReduce[T] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    def sync = n => List()

    val mA = manifest[T]
  }

  // parallel filter
  case class GIterableFilter[T:Manifest](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean])
    extends DeliteOpFilter[T,T,GIterable[T]] {
    override def alloc = new_empty_iterable()
    def func = e => e
    val size = copyTransformedOrElse(_.size)(dc_size(in))
  }

  // called during a BFS traversal
  // returns the down neighbors of all the nodes in the GItarable 'in' object (i.e. the next BFS level)
  case class GIterableNextBFSLevel(in: Exp[GIterable[Node]]) extends DeliteOpMapReduce[Node, GSet[Node]] {
    //override val mutable = true
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero: Exp[NodeSet] = NodeSet()
    // get the down neighbors of each node
    def map: Exp[Node] => Exp[NodeSet] = {
      n => iter_toset(n.DownNbrs)
    }
    // concatenate the down neighbor sets of the nodes
    // TODO: optimize such that elements are added to one set
    def reduce: (Exp[NodeSet], Exp[NodeSet]) => Exp[NodeSet] = {
      (a,b) => {
        val out = NodeSet()
        for(n <- a.Items) {
          out.Add(n)
        }
        for(n <- b.Items) {
          out.Add(n)
        }
        out

        //gset_addset(a, b); a
      }
    }
  }

  // Parallel reductions

  case class GIterableSum[T:Manifest, A:Manifest:Numeric](in: Exp[GIterable[T]], map: Exp[T] => Exp[A])
   extends DeliteOpMapReduce[T, A] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero = unit(0.asInstanceOf[A])
    def reduce = (a,b) => a + b

    val mT = manifest[T]
    val mA = manifest[A]
    val n = implicitly[Numeric[A]]
  }

  case class GIterableSumIf[T:Manifest, A:Manifest:Numeric](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean], func: Exp[T] => Exp[A])
   extends DeliteOpFilterReduce[T, A] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero = unit(0.asInstanceOf[A])
    def reduce = (a,b) => a + b

    val mT = manifest[T]
    val mA = manifest[A]
    val n = implicitly[Numeric[A]]
  }

  case class GIterableProduct[T:Manifest, A:Manifest:Numeric](in: Exp[GIterable[T]], map: Exp[T] => Exp[A])
   extends DeliteOpMapReduce[T,A] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero = unit(1.asInstanceOf[A])
    def reduce = (a,b) => a * b

    val mT = manifest[T]
    val mA = manifest[A]
    val n = implicitly[Numeric[A]]
  }

  case class GIterableProductIf[T:Manifest, A:Manifest:Numeric](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean], func: Exp[T] => Exp[A])
   extends DeliteOpFilterReduce[T,A] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero = unit(1.asInstanceOf[A])
    def reduce = (a,b) => a * b

    val mT = manifest[T]
    val mA = manifest[A]
    val n = implicitly[Numeric[A]]
  }

  case class GIterableMax[T:Manifest, A:Manifest:Ordering](in: Exp[GIterable[T]], map: Exp[T] => Exp[A])
   extends DeliteOpMapReduce[T,A] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero = (manifest[A] match {
      case Manifest.Double => MIN_DOUBLE
      case Manifest.Float => MIN_FLOAT
      case Manifest.Int =>  MIN_INT
      case _ => throw new RuntimeException("no zero value for " + mA.toString)
    }).asInstanceOf[Exp[A]]
    def reduce = (a,b) => if (a > b) a else b

    val mT = manifest[T]
    val mA = manifest[A]
    val o = implicitly[Ordering[A]]
  }

  case class GIterableMaxIf[T:Manifest, A:Manifest:Ordering](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean], func: Exp[T] => Exp[A])
   extends DeliteOpFilterReduce[T,A] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero = (manifest[A] match {
      case Manifest.Double => MIN_DOUBLE
      case Manifest.Float => MIN_FLOAT
      case Manifest.Int =>  MIN_INT
      case _ => throw new RuntimeException("no zero value for " + mA.toString)
    }).asInstanceOf[Exp[A]]
    def reduce = (a,b) => if (a > b) a else b

    val mT = manifest[T]
    val mA = manifest[A]
    val o = implicitly[Ordering[A]]
  }

  case class GIterableMin[T:Manifest, A:Manifest:Ordering](in: Exp[GIterable[T]], map: Exp[T] => Exp[A])
   extends DeliteOpMapReduce[T,A] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero = (manifest[A] match {
      case Manifest.Double => MAX_DOUBLE
      case Manifest.Float => MAX_FLOAT
      case Manifest.Int =>  MAX_INT
      case _ => throw new RuntimeException("no zero value for " + mA.toString)
    }).asInstanceOf[Exp[A]]
    def reduce = (a,b) => if (a < b) a else b

    val mT = manifest[T]
    val mA = manifest[A]
    val o = implicitly[Ordering[A]]
  }

  case class GIterableMinIf[T:Manifest, A:Manifest:Ordering](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean], func: Exp[T] => Exp[A])
   extends DeliteOpFilterReduce[T,A] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero = (manifest[A] match {
      case Manifest.Double => MAX_DOUBLE
      case Manifest.Float => MAX_FLOAT
      case Manifest.Int =>  MAX_INT
      case _ => throw new RuntimeException("no zero value for " + mA.toString)
    }).asInstanceOf[Exp[A]]
    def reduce = (a,b) => if (a < b) a else b

    val mT = manifest[T]
    val mA = manifest[A]
    val o = implicitly[Ordering[A]]
  }

  case class GIterableCount[T:Manifest](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean])
   extends DeliteOpFilterReduce[T,Int] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero = unit(0)
    def func = e => unit(1)
    def reduce = (a,b) => a + b
  }

  case class GIterableAll[T:Manifest](in: Exp[GIterable[T]], map: Exp[T] => Exp[Boolean])
   extends DeliteOpMapReduce[T,Boolean] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero = unit(true)
    def reduce = (a,b) => a && b
  }

  case class GIterableAllIf[T:Manifest](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean], func: Exp[T] => Exp[Boolean])
   extends DeliteOpFilterReduce[T,Boolean] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero = unit(true)
    def reduce = (a,b) => a && b
  }

  case class GIterableAny[T:Manifest](in: Exp[GIterable[T]], map: Exp[T] => Exp[Boolean])
   extends DeliteOpMapReduce[T,Boolean] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero = unit(false)
    def reduce = (a,b) => a || b
  }

  case class GIterableAnyIf[T:Manifest](in: Exp[GIterable[T]], cond: Exp[T] => Exp[Boolean], func: Exp[T] => Exp[Boolean])
   extends DeliteOpFilterReduce[T,Boolean] {
    val size = copyTransformedOrElse(_.size)(dc_size(in))
    val zero = unit(false)
    def reduce = (a,b) => a || b
  }

  /////////////////////////////////////////////////
  // implemented via kernel embedding (sequential)

  //case class GIterableToList[T:Manifest](iter: Exp[GIterable[T]]) extends DefWithManifest[T,List[T]]

  // TODO Figure out how to do toList using DeliteArray
  /*
  case class GIterableToList[T:Manifest](iter: Exp[GIterable[T]])
    extends DeliteOpSingleWithManifest[T,List[T]](reifyEffectsHere(giterable_tolist_impl(iter)))
  */

  case class GIterableToSet[T:Manifest](iter: Exp[GIterable[T]])
    extends DeliteOpSingleWithManifest[T,GSet[T]](reifyEffectsHere(giterable_toset_impl(iter)))

  case class GIterableRawInsert[T:Manifest](x: Exp[GIterable[T]], i: Exp[Int], y: Exp[T])
    extends DeliteOpSingleWithManifest[T,Unit](reifyEffectsHere(giterable_insert_impl(x, i, y)))

  case class GIterableContains[T:Manifest](iter: Exp[GIterable[T]], n: Exp[T])
    extends DeliteOpSingleWithManifest[T,Boolean](reifyEffectsHere(giterable_contains_impl(iter, n)))

  case class DeliteArrayGIterableAppend[T:Manifest](x: Exp[DeliteArray[GIterable[T]]], i: Exp[Int], y: Exp[T])
    extends DeliteOpSingleWithManifest[T,Unit](reifyEffectsHere(delitearray_giterable_append_impl(x, i, y)))

  def new_iterable[T:Manifest](data: Exp[DeliteArray[T]], offset: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(GIterableNew(data, offset, size))
  def new_iterable_imm[T:Manifest](data: Exp[DeliteArray[T]], offset: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext) = reflectPure(GIterableNewImm(data, offset, size))
  def new_empty_iterable[T:Manifest]() = reflectMutable(GIterableNewEmpty())
  def iter_sum[T:Manifest, A:Manifest:Numeric](iter: Exp[GIterable[T]], block: Exp[T] => Exp[A]) = reflectPure(GIterableSum(iter, block))
  def iter_sumIf[T:Manifest, A:Manifest:Numeric](iter: Exp[GIterable[T]], filter: Exp[T] => Exp[Boolean], block: Exp[T] => Exp[A]) = reflectPure(GIterableSumIf(iter, filter, block))
  def iter_product[T:Manifest, A:Manifest:Numeric](iter: Exp[GIterable[T]], block: Exp[T] => Exp[A]) = {
    if(dc_size(iter) == 0) {
      unit(0.asInstanceOf[A])
    } else {
      reflectPure(GIterableProduct(iter, block))
    }
  }
  def iter_productIf[T:Manifest, A:Manifest:Numeric](iter: Exp[GIterable[T]], filter: Exp[T] => Exp[Boolean], block: Exp[T] => Exp[A]) = {
    if(dc_size(iter) == 0) {
      unit(0.asInstanceOf[A])
    } else {
      reflectPure(GIterableProductIf(iter, filter, block))
    }
  }
  def iter_max[T:Manifest, A:Manifest:Ordering](iter: Exp[GIterable[T]], block: Exp[T] => Exp[A]) = reflectPure(GIterableMax(iter, block))
  def iter_maxIf[T:Manifest, A:Manifest:Ordering](iter: Exp[GIterable[T]], filter: Exp[T] => Exp[Boolean], block: Exp[T] => Exp[A]) = reflectPure(GIterableMaxIf(iter, filter, block))
  def iter_min[T:Manifest, A:Manifest:Ordering](iter: Exp[GIterable[T]], block: Exp[T] => Exp[A]) = reflectPure(GIterableMin(iter, block))
  def iter_minIf[T:Manifest, A:Manifest:Ordering](iter: Exp[GIterable[T]], filter: Exp[T] => Exp[Boolean], block: Exp[T] => Exp[A]) = reflectPure(GIterableMinIf(iter, filter, block))
  def iter_count[T:Manifest](iter: Exp[GIterable[T]], block: Exp[T] => Exp[Boolean]) = reflectPure(GIterableCount(iter, block))
  def iter_all[T:Manifest](iter: Exp[GIterable[T]], block: Exp[T] => Exp[Boolean]) = reflectPure(GIterableAll(iter, block))
  def iter_allIf[T:Manifest](iter: Exp[GIterable[T]], filter: Exp[T] => Exp[Boolean], block: Exp[T] => Exp[Boolean]) = reflectPure(GIterableAllIf(iter, filter, block))
  def iter_any[T:Manifest](iter: Exp[GIterable[T]], block: Exp[T] => Exp[Boolean]) = reflectPure(GIterableAny(iter, block))
  def iter_anyIf[T:Manifest](iter: Exp[GIterable[T]], filter: Exp[T] => Exp[Boolean], block: Exp[T] => Exp[Boolean]) = reflectPure(GIterableAnyIf(iter, filter, block))
  //def iter_tolist[T:Manifest](iter: Exp[GIterable[T]]) = reflectPure(GIterableToList(iter))
  def iter_toset[T:Manifest](iter: Exp[GIterable[T]]) = reflectPure(GIterableToSet(iter))
  def iter_next_bfs_level(iter: Exp[GIterable[Node]]) = reflectPure(GIterableNextBFSLevel(iter))
  def iter_filter[T:Manifest](iter: Exp[GIterable[T]], pred: Exp[T] => Exp[Boolean]) = reflectPure(GIterableFilter(iter, pred))
  def iter_foreach_default[T:Manifest](iter: Exp[GIterable[T]], block: Exp[T] => Exp[Unit]) = {
    // have to collect the effects inside the block and properly reflect them!
    val gf = GIterableForeach(iter, block) 
    //reflectEffect(gf, summarizeEffects(gf.funcBody).star /*andAlso Simple()*/)  
    reflectEffect(gf, summarizeEffects(gf.body.asInstanceOf[DeliteForeachElem[_]].func).star /*andAlso Simple()*/) 
  }
  def iter_contains[T:Manifest](iter: Exp[GIterable[T]], n: Exp[T]) = reflectPure(GIterableContains(iter, n))

  // sequential iteration
  def iter_for[T:Manifest](iter: Exp[GIterable[T]], filter: Option[Exp[T] => Exp[Boolean]], block: Exp[T] => Exp[Unit]) = {
    var i = var_new(unit(0))
    while(i < dc_size(iter)) {
      val elem = dc_apply(iter, i)
      filter match {
        case None => block(elem)
        case Some(pred) => if(pred(elem)) { block(elem) }
      }
      i += 1
    }
  }

  // parallel iteration
  def iter_foreach[T:Manifest](in: Exp[GIterable[T]], func: Exp[T] => Exp[Unit]): Exp[Unit] = {
    iter_foreach_default(in, func)
  }

  def delitearray_giterable_append[T:Manifest](x: Exp[DeliteArray[GIterable[T]]], i: Exp[Int], y: Exp[T]) = reflectWrite(x)(DeliteArrayGIterableAppend(x,i,y))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@GIterableFilter(x,b) => reflectPure(new { override val original = Some(f,e) } with GIterableFilter(f(x),f(b)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableNextBFSLevel(x) => reflectPure(new { override val original = Some(f,e) } with GIterableNextBFSLevel(f(x)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableSum(x,b) => reflectPure(new { override val original = Some(f,e) } with GIterableSum(f(x),f(b))(mtype(e.mT),mtype(e.mA),ntype(e.n)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableSumIf(x,c,b) => reflectPure(new { override val original = Some(f,e) } with GIterableSumIf(f(x),f(c),f(b))(mtype(e.mT),mtype(e.mA),ntype(e.n)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableProduct(x,b) => reflectPure(new { override val original = Some(f,e) } with GIterableProduct(f(x),f(b))(mtype(e.mT),mtype(e.mA),ntype(e.n)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableProductIf(x,c,b) => reflectPure(new { override val original = Some(f,e) } with GIterableProductIf(f(x),f(c),f(b))(mtype(e.mT),mtype(e.mA),ntype(e.n)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableMax(x,b) => reflectPure(new { override val original = Some(f,e) } with GIterableMax(f(x),f(b))(mtype(e.mT),mtype(e.mA),otype(e.o)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableMaxIf(x,c,b) => reflectPure(new { override val original = Some(f,e) } with GIterableMaxIf(f(x),f(c),f(b))(mtype(e.mT),mtype(e.mA),otype(e.o)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableMin(x,b) => reflectPure(new { override val original = Some(f,e) } with GIterableMin(f(x),f(b))(mtype(e.mT),mtype(e.mA),otype(e.o)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableMinIf(x,c,b) => reflectPure(new { override val original = Some(f,e) } with GIterableMinIf(f(x),f(c),f(b))(mtype(e.mT),mtype(e.mA),otype(e.o)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableCount(x,b) => reflectPure(new { override val original = Some(f,e) } with GIterableCount(f(x),f(b)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableAll(x,b) => reflectPure(new { override val original = Some(f,e) } with GIterableAll(f(x),f(b)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableAllIf(x,c,b) => reflectPure(new { override val original = Some(f,e) } with GIterableAllIf(f(x),f(c),f(b)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableAny(x,b) => reflectPure(new { override val original = Some(f,e) } with GIterableAny(f(x),f(b)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@GIterableAnyIf(x,c,b) => reflectPure(new { override val original = Some(f,e) } with GIterableAnyIf(f(x),f(c),f(b)))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@GIterableForeach(x,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableForeach(f(x),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableFilter(x,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableFilter(f(x),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableNextBFSLevel(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableNextBFSLevel(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableSum(x,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableSum(f(x),f(b))(mtype(e.mT),mtype(e.mA),ntype(e.n)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableSumIf(x,c,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableSumIf(f(x),f(c),f(b))(mtype(e.mT),mtype(e.mA),ntype(e.n)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableProduct(x,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableProduct(f(x),f(b))(mtype(e.mT),mtype(e.mA),ntype(e.n)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableProductIf(x,c,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableProductIf(f(x),f(c),f(b))(mtype(e.mT),mtype(e.mA),ntype(e.n)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableMax(x,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableMax(f(x),f(b))(mtype(e.mT),mtype(e.mA),otype(e.o)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableMaxIf(x,c,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableMaxIf(f(x),f(c),f(b))(mtype(e.mT),mtype(e.mA),otype(e.o)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableMin(x,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableMin(f(x),f(b))(mtype(e.mT),mtype(e.mA),otype(e.o)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableMinIf(x,c,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableMinIf(f(x),f(c),f(b))(mtype(e.mT),mtype(e.mA),otype(e.o)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableCount(x,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableCount(f(x),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableAll(x,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableAll(f(x),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableAllIf(x,c,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableAllIf(f(x),f(c),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableAny(x,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableAny(f(x),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableAnyIf(x,c,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableAnyIf(f(x),f(c),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    //case e@GIterableToList(x) => iter_tolist(f(x))(e.mA) //TODO?
    case e@GIterableToSet(x) => iter_toset(f(x))(e.mA) //TODO?
    case GIterableRawSize(x) => giterable_raw_size(f(x))
    case e@GIterableRawApply(x,i) => giterable_raw_apply(f(x),f(i))(e.mA,ctx)
    case GIterableRawData(x) => giterable_raw_data(f(x))
    case e@GIterableContains(x, n) => reflectPure(new { override val original = Some(f,e) } with GIterableContains(f(x),f(n))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    //case Reflect(e@GIterableToList(x), u, es) => reflectMirrored(Reflect(GIterableToList(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@GIterableToSet(x), u, es) => reflectMirrored(Reflect(GIterableToSet(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableNew(d, o, s), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableNew(f(d), f(o), f(s))(e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableNewEmpty(), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableNewEmpty()(e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableRawSize(x), u, es) => reflectMirrored(Reflect(GIterableRawSize(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableRawApply(x,i), u, es) => reflectMirrored(Reflect(GIterableRawApply(f(x),f(i))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableRawData(x), u, es) => reflectMirrored(Reflect(GIterableRawData(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableSetRawData(x,g), u, es) => reflectMirrored(Reflect(GIterableSetRawData(f(x),f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableRawUpdate(x,i,y), u, es) => reflectMirrored(Reflect(GIterableRawUpdate(f(x),f(i),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableSetRawSize(x,s), u, es) => reflectMirrored(Reflect(GIterableSetRawSize(f(x),f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableRawInsert(x,i,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableRawInsert(f(x),f(i),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableRawAlloc(x,s), u, es) => reflectMirrored(Reflect(GIterableRawAlloc(f(x),f(s))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@GIterableContains(x,n), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with GIterableContains(f(x),f(n))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteArrayGIterableAppend(x,i,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayGIterableAppend(f(x),f(i),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??

  // provides access to underlying GIterable fields required by DeliteCollection
  case class GIterableRawSize[A:Manifest](x: Exp[GIterable[A]]) extends Def[Int]
  case class GIterableRawApply[A:Manifest](x: Exp[GIterable[A]], i: Exp[Int]) extends DefWithManifest[A,A]
  case class GIterableRawUpdate[A:Manifest](x: Exp[GIterable[A]], i: Exp[Int], y: Exp[A]) extends Def[Unit]
  case class GIterableSetRawSize[A:Manifest](x: Exp[GIterable[A]], newSz: Exp[Int]) extends Def[Unit]
  case class GIterableRawData[A:Manifest](x: Exp[GIterable[A]]) extends DefWithManifest[A,DeliteArray[A]] //Def[DeliteArray[A]]
  case class GIterableSetRawData[A:Manifest](x: Exp[GIterable[A]], newVal: Exp[DeliteArray[A]]) extends DefWithManifest[A,Unit]

  case class GIterableRawAlloc[A:Manifest](x: Exp[DeliteArray[A]], sz: Exp[Int]) extends Def[GIterable[A]] {
    val mA = manifest[A]
    val mGIt = manifest[GIterable[A]]
  }


  /////////////
  // internal

  def giterable_raw_size[A:Manifest](x: Exp[GIterable[A]])(implicit ctx: SourceContext) = field[Int](x, "_size")
  def giterable_raw_apply[A:Manifest](x: Exp[GIterable[A]], i: Exp[Int])(implicit ctx: SourceContext) = field[DeliteArray[A]](x, "_data").apply(i)
  def giterable_raw_update[A:Manifest](x: Exp[GIterable[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = field[DeliteArray[A]](x, "_data").update(i,y)
  def giterable_set_raw_size[A:Manifest](x: Exp[GIterable[A]], newSz: Exp[Int])(implicit ctx: SourceContext) = field_update[Int](x, "_size", newSz)
  def giterable_raw_data[A:Manifest](x: Exp[GIterable[A]])(implicit ctx: SourceContext) = field[DeliteArray[A]](x, "_data")
  def giterable_set_raw_data[A:Manifest](x: Exp[GIterable[A]], g: Exp[DeliteArray[A]])(implicit ctx: SourceContext) = field_update[DeliteArray[A]](x, "_data", g)

  def giterable_raw_insert[A:Manifest](x: Exp[GIterable[A]], i: Exp[Int], y: Exp[A]): Exp[Unit] = reflectWrite(x)(GIterableRawInsert(x,i,y))
  def giterable_raw_alloc[A:Manifest](x: Exp[DeliteArray[A]], sz: Exp[Int]): Exp[GIterable[A]] = new_iterable(x, unit(0), sz) //reflectMutable(GIterableRawAlloc(x,sz))

  // /////////////////////
  // delite collection

  def isGIterable[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[GIterable[A]])
  def asGIterable[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[GIterable[A]]]

  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = {
    if (isGIterable(x)) giterable_raw_size(asGIterable(x))
    else super.dc_size(x)
  }

  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isGIterable(x)) giterable_raw_apply(asGIterable(x), n)
    else super.dc_apply(x,n)
  }

  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isGIterable(x)) giterable_raw_update(asGIterable(x), n, y)
    else super.dc_update(x,n,y)
  }

  // ParBuffer methods (for filters)

  override def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext) = {
    if (isGIterable(x)) giterable_set_raw_size(asGIterable(x), y)
    else super.dc_set_logical_size(x,y)
  }

  override def dc_appendable[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isGIterable(x)) unit(true)
    else super.dc_appendable(x,i,y)        
  }  
  
  override def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isGIterable(x)) { giterable_raw_insert(asGIterable(x),asGIterable(x).length,y) }
    else super.dc_append(x,i,y)
  }

  override def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext): Exp[CA] = {
    if (isGIterable(x)) {
      val out = giterable_raw_alloc[A](DeliteArray[A](size), size)
      out.asInstanceOf[Exp[CA]]
    }
    else super.dc_alloc[A,CA](x,size)
  }

  override def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    if (isGIterable(src) && isGIterable(dst)) {
      darray_unsafe_copy(giterable_raw_data(asGIterable(src)), srcPos, giterable_raw_data(asGIterable(dst)), dstPos, size)
    }
    else super.dc_copy(src,srcPos,dst,dstPos,size)
  }
}

trait BaseGenGIterableOps extends GenericFatCodegen {
  val IR: GIterableOpsExp
  import IR._

}

trait ScalaGenGIterableOps extends BaseGenGIterableOps with ScalaGenFat {
  val IR: GIterableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      //case it@GIterableNew(data, offset, size) => emitValDef(sym, "new " + remap(it.mR) + "(" + quote(data) + ", " + quote(offset) + ", " + quote(size) + ")")
      //case it@GIterableNewEmpty() => emitValDef(sym, "new " + remap(it.mGIt) +"()")
      //case GIterableRawSize(x) => emitValDef(sym, quote(x) + "._size")
      //case GIterableRawApply(x,i) => emitValDef(sym, quote(x) + "._data(" + quote(x) + "._offset + " + quote(i) + ")")
      //case GIterableRawUpdate(x, i, y) => emitValDef(sym, quote(x) + "._data(" + quote(x) + "._offset + " + quote(i) + ") = " + quote(y))
      //case GIterableSetRawSize(x, newSz) => emitValDef(sym, quote(x) + "._size = " + quote(newSz))
      //case GIterableRawData(x) => emitValDef(sym, quote(x) + "._data")
      //case GIterableSetRawData(x, g) => emitValDef(sym, quote(x) + "._data = " + quote(g))
      //case it@GIterableRawAlloc(x, sz) => emitValDef(sym, "new " + remap(it.mGIt) + "(" + quote(x) + ", 0, " + quote(sz) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}
