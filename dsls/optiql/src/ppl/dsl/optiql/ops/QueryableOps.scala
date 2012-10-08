package ppl.dsl.optiql.ops

import java.io.PrintWriter
import ppl.dsl.optiql.{OptiQL, OptiQLExp}
import ppl.delite.framework.ops.DeliteCollection
import scala.virtualization.lms.common.{LiftPrimitives, Base, ScalaGenFat, BaseFatExp}
import scala.reflect.SourceContext

trait QueryableOps extends Base { this: OptiQL =>
  
  implicit def repToQueryableOps[T:Manifest](r: Rep[Table[T]]) = new QOpsCls(r) 
  implicit def repGroupingToQueryableOps[K:Manifest, T:Manifest](g: Rep[Grouping[K, T]]) = new QOpsCls(queryable_grouping_toDatatable(g))

  class QOpsCls[T:Manifest](s: Rep[Table[T]]) {
    def Where(predicate: Rep[T] => Rep[Boolean]) = queryable_where(s, predicate)
    def GroupBy[K:Manifest](keySelector: Rep[T] => Rep[K]) = queryable_groupby(s,keySelector)
    def Select[R:Manifest](resultSelector: Rep[T] => Rep[R]) = queryable_select(s, resultSelector)
    def Sum[N:Numeric:Manifest](sumSelector: Rep[T] => Rep[N]) = queryable_sum(s, sumSelector)
    def Average[N:Numeric:Manifest](avgSelector: Rep[T] => Rep[N]) = queryable_average(s, avgSelector)
    def Count() = queryable_count(s)
    def OrderBy[K:Ordering:Manifest](keySelector: Rep[T] => Rep[K]) = queryable_orderby(s, keySelector)
    def OrderByDescending[K:Ordering:Manifest](keySelector: Rep[T] => Rep[K]) = queryable_orderbydescending(s, keySelector)
    def ThenBy[K:Ordering:Manifest](keySelector: Rep[T] => Rep[K]) = queryable_thenby(s, keySelector)
    def ThenByDescending[K:Ordering:Manifest](keySelector: Rep[T] => Rep[K]) = queryable_thenbydescending(s, keySelector) 
    def Max[N:Ordering:Manifest](maxSelector: Rep[T] => Rep[N]) = queryable_max(s, maxSelector)
    def Min[N:Ordering:Manifest](minSelector: Rep[T] => Rep[N]) = queryable_min(s, minSelector)
    def First() = queryable_first(s)
    def Last() = queryable_last(s)
    def Join[T2:Manifest](second: Rep[Table[T2]]) = new JoinableOps(s, second)
  }

  class JoinableOps[T1:Manifest, T2:Manifest](first: Rep[Table[T1]], second: Rep[Table[T2]]) {
    def Where(predicate: Rep[T2] => Rep[Boolean]) = new JoinableOps(first, second.Where(predicate))
    def WhereEq[K:Manifest](firstKeySelector: Rep[T1] => Rep[K], secondKeySelector: Rep[T2] => Rep[K]) = new Joinable2(first, firstKeySelector, second, secondKeySelector)
  }

  class Joinable2[T1:Manifest, T2:Manifest, K:Manifest](
    val first: Rep[Table[T1]],
    val firstKeySelector: Rep[T1] => Rep[K],
    val second: Rep[Table[T2]],
    val secondKeySelector: Rep[T2] => Rep[K] 
  ) {
    def Select[R:Manifest](resultSelector: (Rep[T1], Rep[T2]) => Rep[R]) = queryable_join2(first, firstKeySelector, second, secondKeySelector, resultSelector)
  }
  
  //Grouping
  def infix_key[K:Manifest, T:Manifest](g: Rep[Grouping[K, T]]) = queryable_grouping_key(g) 

  def queryable_where[T:Manifest](s: Rep[Table[T]], predicate: Rep[T] => Rep[Boolean]): Rep[Table[T]]
  def queryable_groupby[T:Manifest, K:Manifest](s: Rep[Table[T]], keySelector: Rep[T] => Rep[K]): Rep[Table[Grouping[K, T]]]
  def queryable_select[T:Manifest, R:Manifest](s: Rep[Table[T]], resultSelector: Rep[T] => Rep[R]): Rep[Table[R]]
  def queryable_sum[T:Manifest, N:Numeric:Manifest](s: Rep[Table[T]], sumSelector: Rep[T] => Rep[N]): Rep[N]
  def queryable_average[T:Manifest, N:Numeric:Manifest](s: Rep[Table[T]], avgSelector: Rep[T] => Rep[N]): Rep[N]
  def queryable_count[T:Manifest](s: Rep[Table[T]]): Rep[Int]  
  def queryable_max[T:Manifest, N:Ordering:Manifest](s: Rep[Table[T]], maxSelector: Rep[T] => Rep[N]): Rep[N]
  def queryable_min[T:Manifest, N:Ordering:Manifest](s: Rep[Table[T]], minSelector: Rep[T] => Rep[N]): Rep[N]
  def queryable_first[T:Manifest](s: Rep[Table[T]]): Rep[T]
  def queryable_last[T:Manifest](s: Rep[Table[T]]): Rep[T]
  def queryable_orderby[T:Manifest, K:Ordering:Manifest](s: Rep[Table[T]], keySelector: Rep[T] => Rep[K]): Rep[Table[T]]
  def queryable_orderbydescending[T:Manifest, K:Ordering:Manifest](s: Rep[Table[T]], keySelector: Rep[T] => Rep[K]): Rep[Table[T]]
  def queryable_thenby[T:Manifest, K:Ordering:Manifest](s: Rep[Table[T]], keySelector: Rep[T] => Rep[K]): Rep[Table[T]]
  def queryable_thenbydescending[T:Manifest, K:Ordering:Manifest](s: Rep[Table[T]], keySelector: Rep[T] => Rep[K]): Rep[Table[T]]
  
  def queryable_grouping_toDatatable[K:Manifest, T:Manifest](g: Rep[Grouping[K, T]]): Rep[Table[T]]
  def queryable_grouping_key[K:Manifest, T:Manifest](g: Rep[Grouping[K, T]]): Rep[K]
  
  def queryable_join2[T1:Manifest, T2:Manifest, K2:Manifest, R:Manifest](first: Rep[Table[T1]], firstKeySelector: Rep[T1] => Rep[K2], 
    second: Rep[Table[T2]], secondKeySelector: Rep[T2] => Rep[K2], resultSelector: (Rep[T1], Rep[T2]) => Rep[R]):Rep[Table[R]]
  
}

trait QueryableOpsExp extends QueryableOps with BaseFatExp { this: OptiQLExp with LiftPrimitives =>
  
  case class QueryableWhere[T:Manifest](in: Exp[Table[T]], cond: Exp[T] => Exp[Boolean]) extends DeliteOpFilter[T, T, Table[T]] {
    override def alloc(len: Exp[Int]) = Table(len)
    val size = copyTransformedOrElse(_.size)(in.size)

    def func = e => e
  }
     
  case class QueryableSelect[T:Manifest, R:Manifest](in: Exp[Table[T]], func: Exp[T] => Exp[R]) extends DeliteOpMap[T, R, Table[R]] {
    override def alloc(len: Exp[Int]) = Table(len)
    val size = copyTransformedOrElse(_.size)(in.size)
  }

  case class QueryableSum[T:Manifest, N:Numeric:Manifest](in: Exp[Table[T]], map: Exp[T] => Exp[N]) extends DeliteOpMapReduce[T, N] {
    val size = copyTransformedOrElse(_.size)(in.size)
    def zero = unit(0).AsInstanceOf[N] //TODO: member of Numeric
    def reduce = _ + _

    val N = implicitly[Numeric[N]]
    val mN = manifest[N]
    val mT = manifest[T]
  }

  case class QueryableMax[T:Manifest, N:Ordering:Manifest](in: Exp[Table[T]], map: Exp[T] => Exp[N]) extends DeliteOpMapReduce[T, N] {
    val size = copyTransformedOrElse(_.size)(in.size)
    def zero = unit(null).AsInstanceOf[N] //TODO: ??
    def reduce = (a,b) => a max b
  }

  case class QueryableMin[T:Manifest, N:Ordering:Manifest](in: Exp[Table[T]], map: Exp[T] => Exp[N]) extends DeliteOpMapReduce[T, N] {
    val size = copyTransformedOrElse(_.size)(in.size)
    def zero = unit(null).AsInstanceOf[N] //TODO: ??
    def reduce = (a,b) => a min b
  }

  case class QueryableSort[T:Manifest](in: Exp[Table[T]], compare: (Exp[T],Exp[T]) => Exp[Int]) extends Def[Table[T]]

  case class QueryableGroupBy[T:Manifest, K:Manifest](in: Exp[Table[T]], keyFunc: Exp[T] => Exp[K]) extends DeliteOpHash[K, T, Table[Grouping[K,T]]] {
    def alloc(len: Exp[Int]) = Table(len)
    val size = copyTransformedOrElse(_.size)(in.size)
  }

  def queryable_select[T:Manifest, R:Manifest](s: Exp[Table[T]], resultSelector: Exp[T] => Exp[R]) = QueryableSelect(s, resultSelector)

  def queryable_where[T:Manifest](s: Exp[Table[T]], predicate: Exp[T] => Exp[Boolean]) = QueryableWhere(s, predicate)
  
  def queryable_groupby[T:Manifest, K:Manifest](s: Exp[Table[T]], keySelector: Exp[T] => Exp[K]) = QueryableGroupBy(s, keySelector)

  //TODO: should Join be a DeliteOp?; could also create an abstract Join op for pattern matching and later lower to this
  def queryable_join2[T1:Manifest, T2:Manifest, K:Manifest, R:Manifest](first: Exp[Table[T1]], firstKeySelector: Exp[T1] => Exp[K], 
    second: Exp[Table[T2]], secondKeySelector: Exp[T2] => Exp[K], resultSelector: (Exp[T1], Exp[T2]) => Exp[R]): Exp[Table[R]] = {

    /* val firstGrouped = first.GroupBy(firstKeySelector)

    //val result = second flatMap { x2 => firstMap(secondKeySelector(x2)) map { x1 => resultSelector(x1,x2) } }
    val nestedResult = second.Select { secondValue => 
      val key = secondKeySelector(secondValue)
      val firstValues = firstGrouped(key)
      firstValues.Select(firstValue => resultSelector(firstValue, secondValue))
    }
    nestedResult.flatten */
    throw new RuntimeException("Join2 not yet implemented")
  }

  def queryable_sum[T:Manifest, N:Numeric:Manifest](s: Exp[Table[T]], sumSelector: Exp[T] => Exp[N]) = QueryableSum(s, sumSelector)
  
  def queryable_max[T:Manifest, N:Ordering:Manifest](s: Exp[Table[T]], maxSelector: Exp[T] => Exp[N]) = QueryableMax(s, maxSelector)

  def queryable_min[T:Manifest, N:Ordering:Manifest](s: Exp[Table[T]], minSelector: Exp[T] => Exp[N]) = QueryableMin(s, minSelector)

  def queryable_average[T:Manifest, N:Numeric:Manifest](s: Exp[Table[T]], avgSelector: Exp[T] => Exp[N]) = //QueryableAverage(s, avgSelector)
    s.Sum(avgSelector)/s.Count.AsInstanceOf[N] //TODO: this only works for primitive types
    
  def queryable_count[T:Manifest](s: Exp[Table[T]]) = s.size

  def queryable_first[T:Manifest](s: Exp[Table[T]]) = s(0)

  def queryable_last[T:Manifest](s: Exp[Table[T]]) = s(s.Count-1)

  def compareAsc[T:Manifest, K:Ordering:Manifest](a: Exp[T], b: Exp[T], keySelector: Exp[T] => Exp[K]): Exp[Int] = {
    if (keySelector(a) < keySelector(b)) unit(-1) else if (keySelector(a) > keySelector(b)) unit(1) else unit(0)
  }

  def compareDsc[T:Manifest, K:Ordering:Manifest](a: Exp[T], b: Exp[T], keySelector: Exp[T] => Exp[K]): Exp[Int] = {
    if (keySelector(a) > keySelector(b)) unit(-1) else if (keySelector(a) < keySelector(b)) unit(1) else unit(0)
  }
  
  def queryable_orderby[T:Manifest, K:Ordering:Manifest](s: Exp[Table[T]], keySelector: Exp[T] => Exp[K]) = QueryableSort(s, (a:Exp[T], b:Exp[T]) => compareAsc(a,b,keySelector))

  def queryable_orderbydescending[T:Manifest, K:Ordering:Manifest](s: Exp[Table[T]], keySelector: Exp[T] => Exp[K]) = QueryableSort(s, (a:Exp[T], b:Exp[T]) => compareDsc(a,b,keySelector))

  def queryable_thenby[T:Manifest, K:Ordering:Manifest](s: Exp[Table[T]], keySelector: Exp[T] => Exp[K]) = s match {
    case Def(QueryableSort(orig, sel)) =>
      val compoundSel = (a:Exp[T], b:Exp[T]) => {
        val prev = sel(a,b)
        if (prev == unit(0)) compareAsc(a,b,keySelector)
        else prev
      }
      QueryableSort(orig, compoundSel)

    case _ => throw new IllegalArgumentException("ERROR: ThenBy must be preceded by OrderBy or OrderByDescending")
  }

  def queryable_thenbydescending[T:Manifest, K:Ordering:Manifest](s: Exp[Table[T]], keySelector: Exp[T] => Exp[K]) = s match {
    case Def(QueryableSort(orig, sel)) =>
      val compoundSel = (a:Exp[T], b:Exp[T]) => {
        val prev = sel(a,b)
        if (prev == unit(0)) compareDsc(a,b,keySelector)
        else prev
      }
      QueryableSort(orig, compoundSel)

    case _ => throw new IllegalArgumentException("ERROR: ThenByDescending must be preceded by OrderBy or OrderByDescending")
  }
  
  def grouping_apply[K:Manifest, T:Manifest](k: Exp[K], v: Exp[Table[T]]): Exp[Grouping[K,T]] =
    struct[Grouping[K,T]](ClassTag[Grouping[K,T]]("Grouping"), "key"->k, "values"->v)
  
  def queryable_grouping_key[K:Manifest, T:Manifest](g: Exp[Grouping[K, T]]): Exp[K] = field[K](g, "key")
  def queryable_grouping_toDatatable[K:Manifest, T:Manifest](g: Exp[Grouping[K, T]]): Exp[Table[T]] = field[Table[T]](g, "values")
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@QueryableSum(in,g) => reflectPure(new { override val original = Some(f,e) } with QueryableSum(f(in),f(g))(e.mT,e.N,e.mN))(mtype(manifest[A]),implicitly[SourceContext])      
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] 
  
}

trait QueryableOpsExpOpt extends QueryableOpsExp { this: OptiQLExp =>

  case class QueryableSelectWhere[T:Manifest, R:Manifest](in: Exp[Table[T]], func: Exp[T] => Exp[R], cond: Exp[T] => Exp[Boolean]) extends DeliteOpFilter[T, R, Table[R]] {
    override def alloc(len: Exp[Int]) = Table(len)
    val size = copyTransformedOrElse(_.size)(in.size)
  }

  case class QueryableGroupByWhere[T:Manifest, K:Manifest](in: Exp[Table[T]], keyFunc: Exp[T] => Exp[K], cond: Exp[T] => Exp[Boolean]) extends DeliteOpHashFilter[T, K, T, Table[Grouping[K,T]]] {
    def alloc(len: Exp[Int]) = Table(len)
    val size = copyTransformedOrElse(_.size)(in.size)

    def mapFunc = v => v
  }

  case class QueryableReduceToCount[T:Manifest](in: Exp[Table[T]]) extends DeliteOpMapReduce[T, Int] {
    val size = copyTransformedOrElse(_.size)(in.size)
    def zero = unit(0)
    def map = a => 1
    def reduce = (a,b) => a + b 
  }

  case class QueryableReduceToAverage[T:Manifest, N:Numeric:Manifest](in: Exp[Table[T]], map: Exp[T] => Exp[N]) extends Def[N] //DeliteOpFilterReduceFold

  case class QueryableReduceToFirst[T:Manifest](in: Exp[Table[T]]) extends DeliteOpReduce[T] {
    val size = copyTransformedOrElse(_.size)(in.size)
    def zero = unit(null).AsInstanceOf[T]
    def func = (a,b) => a
  }

  case class QueryableReduceToLast[T:Manifest](in: Exp[Table[T]]) extends DeliteOpReduce[T] {
    val size = copyTransformedOrElse(_.size)(in.size)
    def zero = unit(null).AsInstanceOf[T]
    def func = (a,b) => b
  }

  case class QueryableHashReduce[T:Manifest, K:Manifest, R:Manifest](in: Exp[Table[T]], keyFunc: Exp[T] => Exp[K], mapFunc: Exp[T] => Exp[R], reduceFunc: (Exp[R],Exp[R]) => Exp[R], cond: Exp[T] => Exp[Boolean]) 
    extends DeliteOpHashFilterReduce[T, K, R, Table[R]] {

    def alloc(len: Exp[Int]) = Table[R](len)
    val size = copyTransformedOrElse(_.size)(in.size)
    def zero = unit(null).AsInstanceOf[R]
  }

  //Where-GroupBy fusion
  override def queryable_groupby[T:Manifest, K:Manifest](s: Exp[Table[T]], keySelector: Exp[T] => Exp[K]) = s match {
    case Def(QueryableWhere(origS, predicate)) => //TODO: this special-cases filter fusion (only for groupBy); LMS fusion should take care of it generically for us
      QueryableGroupByWhere(origS, keySelector, predicate)
    case _ => super.queryable_groupby(s, keySelector)
  }
  
  override def queryable_select[T:Manifest, R:Manifest](s: Exp[Table[T]], resultSelector: Exp[T] => Exp[R]): Exp[Table[R]] = s match {
    case Def(QueryableWhere(origS, predicate)) => //Where-Select fusion
      QueryableSelectWhere(origS, resultSelector, predicate)

    case Def(QueryableGroupBy(origS: Exp[Table[a]], keySelector)) => //GroupBy-Select fusion
      def abort(): Exp[Table[R]] = { 
        printlog("WARNING: unable to fuse GroupBy-Select")
        return super.queryable_select(s, resultSelector)
      }

      resultSelector(fresh[T]) match {
        case Def(Struct(tag: StructTag[R], elems)) => 
          val valueFunc = (a:Exp[a]) => struct[R](tag, elems map { case (key, value) => (key, value match {
            case Def(QueryableSum(s, sumSelector)) => sumSelector(a)
            case _ => abort() //unknown operator, abort
          })})

          val ctx = implicitly[SourceContext]
          val reduceFunc = (a:Exp[R],b:Exp[R]) => struct[R](tag, elems map { case (key, value) => (key, value match {
            case Def(qs@QueryableSum(s, sumSelector)) => numeric_plus(field(a,key)(value.tp,ctx), field(b,key)(value.tp,ctx))(qs.N,qs.mN,ctx)
            case _ => abort()
          })})

          printlog("fused GroupBy-Select successfully")
          QueryableHashReduce(origS, keySelector, valueFunc, reduceFunc, (e:Exp[a]) => unit(true))

        case _ => abort()
      }

    case _ => super.queryable_select(s, resultSelector)
  }

  private var queryable_special: QueryableSpecial = Default
  private abstract class QueryableSpecial
  private case object Default extends QueryableSpecial
  private case object GroupBySelect extends QueryableSpecial

  //rewrite these as simple reductions for GroupBy-Select fusion
  override def queryable_average[T:Manifest, N:Numeric:Manifest](s: Exp[Table[T]], avgSelector: Exp[T] => Exp[N]) = queryable_special match {
    case GroupBySelect => QueryableReduceToAverage(s, avgSelector) //TODO: running average or sum-divide?
    case _ => s.Sum(avgSelector)/s.Count.AsInstanceOf[N]
  }
    
  override def queryable_count[T:Manifest](s: Exp[Table[T]]) = queryable_special match {
    case GroupBySelect => QueryableReduceToCount(s)
    case _ => s.size
  }

  override def queryable_first[T:Manifest](s: Exp[Table[T]]) = queryable_special match {
    case GroupBySelect => QueryableReduceToFirst(s)
    case _ => s(0)
  }

  override def queryable_last[T:Manifest](s: Exp[Table[T]]) = queryable_special match {
    case GroupBySelect => QueryableReduceToLast(s)
    case _ => s(s.Count-1)
  }

}
