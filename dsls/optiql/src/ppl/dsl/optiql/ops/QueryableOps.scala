package ppl.dsl.optiql.ops

import java.io.PrintWriter
import ppl.dsl.optiql.{OptiQL, OptiQLExp}
import ppl.delite.framework.ops.DeliteCollection
import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenFat, BaseFatExp}
import scala.reflect.{RefinedManifest, SourceContext}

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

trait QueryableOpsExp extends QueryableOps with EffectExp with BaseFatExp { this: OptiQLExp =>
  
  case class QueryableWhere[T:Manifest](in: Exp[Table[T]], cond: Exp[T] => Exp[Boolean]) extends DeliteOpFilter[T, T, Table[T]] {
    override def alloc(len: Exp[Int]) = Table(len)
    val size = copyTransformedOrElse(_.size)(in.size)
    def func = e => e
    val mT = manifest[T]
  }
     
  case class QueryableSelect[T:Manifest, R:Manifest](in: Exp[Table[T]], func: Exp[T] => Exp[R]) extends DeliteOpMap[T, R, Table[R]] {
    override def alloc(len: Exp[Int]) = Table(len)
    val size = copyTransformedOrElse(_.size)(in.size)
    val mT = manifest[T]
    val mR = manifest[R]
  }

  case class QueryableSum[T:Manifest, N:Numeric:Manifest](in: Exp[Table[T]], map: Exp[T] => Exp[N]) extends DeliteOpMapReduce[T, N] {
    val size = copyTransformedOrElse(_.size)(in.size)
    def zero = zeroType[N] //TODO: member of Numeric
    def reduce = _ + _

    val N = implicitly[Numeric[N]]
    val mN = manifest[N]
    val mT = manifest[T]
  }

  case class QueryableMax[T:Manifest, N:Ordering:Manifest](in: Exp[Table[T]], map: Exp[T] => Exp[N]) extends DeliteOpMapReduce[T, N] {
    val size = copyTransformedOrElse(_.size)(in.size)
    def zero = unit(null).AsInstanceOf[N] //TODO: ??
    def reduce = (a,b) => a max b

    val oN = implicitly[Ordering[N]]
    val mN = manifest[N]
    val mT = manifest[T]
  }

  case class QueryableMin[T:Manifest, N:Ordering:Manifest](in: Exp[Table[T]], map: Exp[T] => Exp[N]) extends DeliteOpMapReduce[T, N] {
    val size = copyTransformedOrElse(_.size)(in.size)
    def zero = unit(null).AsInstanceOf[N] //TODO: ??
    def reduce = (a,b) => a min b

    val oN = implicitly[Ordering[N]]
    val mN = manifest[N]
    val mT = manifest[T]
  }

  case class QueryableSort[T:Manifest](in: Exp[Table[T]], compare: (Exp[T],Exp[T]) => Exp[Int]) extends Def[Table[T]] {
    val mT = manifest[T]
  }

  //TODO: more user-friendly support for HashCollect
  /* case class QueryableGroupBy[T:Manifest, K:Manifest](in: Exp[Table[T]], keyFunc: Exp[T] => Exp[K]) extends DeliteOpHash[K, T, Table[Grouping[K,T]]] {
    def alloc(len: Exp[Int]) = Table(len)
    val size = copyTransformedOrElse(_.size)(in.size)
  } */

  case class QueryableGroupBy[T:Manifest, K:Manifest](in: Exp[Table[T]], keyFunc: Exp[T] => Exp[K]) extends Def[Table[Grouping[K,T]]] {
    val mT = manifest[T]
    val mK = manifest[K]
  }

  case class QueryableCount[T:Manifest](s: Exp[Table[T]]) extends DeliteOpSingleWithManifest[T,Int](reifyEffectsHere(s.size))
  case class QueryableFirst[T:Manifest](s: Exp[Table[T]]) extends DeliteOpSingleTask[T](reifyEffectsHere(s(unit(0))))
  case class QueryableLast[T:Manifest](s: Exp[Table[T]]) extends DeliteOpSingleTask[T](reifyEffectsHere(s(s.size-1)))

  protected def zeroType[T:Manifest]: Exp[T] = manifest[T] match { //need a more robust solution, e.g. type class
    case r: RefinedManifest[T] => struct[T](AnonTag(r), r.fields.map(e => (e._1, zeroType(e._2))))
    case s if s == manifest[String] => unit("").asInstanceOf[Rep[T]]
    case v if v <:< manifest[AnyVal] => unit(0).AsInstanceOf[T]
    case o => throw new IllegalArgumentException("Unknown Numeric type: " + o)
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

  def queryable_average[T:Manifest, N:Numeric:Manifest](s: Exp[Table[T]], avgSelector: Exp[T] => Exp[N]) = //QueryableAverage(s, avgSelector) //TODO: DeliteOpComposite
    s.Sum(avgSelector)/s.Count.asInstanceOf[Exp[N]] //TODO: this only works for primitive types

  object QueryableAverage {
    def unapply[T](d: Def[T]) = d match {
      case NumericDivide(Def(sum@QueryableSum(a,sel)),Def(QueryableCount(b))) if (a == b) => Some((a, sel, sum))
      case _ => None
    }
  }
    
  def queryable_count[T:Manifest](s: Exp[Table[T]]) = reflectPure(QueryableCount(s))

  def queryable_first[T:Manifest](s: Exp[Table[T]]) = reflectPure(QueryableFirst(s))

  def queryable_last[T:Manifest](s: Exp[Table[T]]) = reflectPure(QueryableLast(s))

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
    case e@QueryableSelect(in,g) => reflectPure(new { override val original = Some(f,e) } with QueryableSelect(f(in),f(g))(mtype(e.mT),mtype(e.mR)))(mtype(manifest[A]),implicitly[SourceContext])      
    case e@QueryableWhere(in,c) => reflectPure(new { override val original = Some(f,e) } with QueryableWhere(f(in),f(c))(mtype(e.mT)))(mtype(manifest[A]),implicitly[SourceContext])      
    case e@QueryableSum(in,g) => reflectPure(new { override val original = Some(f,e) } with QueryableSum(f(in),f(g))(mtype(e.mT),ntype(e.N),mtype(e.mN)))(mtype(manifest[A]),implicitly[SourceContext])      
    case e@QueryableMax(in,g) => reflectPure(new { override val original = Some(f,e) } with QueryableMax(f(in),f(g))(mtype(e.mT),otype(e.oN),mtype(e.mN)))(mtype(manifest[A]),implicitly[SourceContext])      
    case e@QueryableMin(in,g) => reflectPure(new { override val original = Some(f,e) } with QueryableMin(f(in),f(g))(mtype(e.mT),otype(e.oN),mtype(e.mN)))(mtype(manifest[A]),implicitly[SourceContext])      
    case e@QueryableGroupBy(in,g) => reflectPure(QueryableGroupBy(f(in),f(g))(mtype(e.mT),mtype(e.mK)))(mtype(manifest[A]),implicitly[SourceContext])      
    case e@QueryableCount(in) => reflectPure(new { override val original = Some(f,e) } with QueryableCount(f(in))(mtype(e.mA)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@QueryableFirst(in) => reflectPure(new { override val original = Some(f,e) } with QueryableFirst(f(in))(mtype(e.mR)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@QueryableLast(in) => reflectPure(new { override val original = Some(f,e) } with QueryableLast(f(in))(mtype(e.mR)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@QueryableSort(in,comp) => reflectPure(QueryableSort(f(in),f(comp))(mtype(e.mT)))(mtype(manifest[A]),implicitly[SourceContext])
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] 
  
}

trait QueryableOpsExpOpt extends QueryableOpsExp { this: OptiQLExp =>

  case class QueryableSelectWhere[T:Manifest, R:Manifest](in: Exp[Table[T]], func: Exp[T] => Exp[R], cond: Exp[T] => Exp[Boolean]) extends DeliteOpFilter[T, R, Table[R]] {
    override def alloc(len: Exp[Int]) = Table(len)
    val size = copyTransformedOrElse(_.size)(in.size)
    val mT = manifest[T]
    val mR = manifest[R]
  }

  /* case class QueryableGroupByWhere[T:Manifest, K:Manifest](in: Exp[Table[T]], keyFunc: Exp[T] => Exp[K], cond: Exp[T] => Exp[Boolean]) extends DeliteOpHashFilter[T, K, T, Table[Grouping[K,T]]] {
    def alloc(len: Exp[Int]) = Table(len)
    val size = copyTransformedOrElse(_.size)(in.size)

    def mapFunc = v => v
  } */

  case class QueryableGroupByWhere[T:Manifest, K:Manifest](in: Exp[Table[T]], keyFunc: Exp[T] => Exp[K], cond: Exp[T] => Exp[Boolean]) extends Def[Table[Grouping[K,T]]] {
    val mT = manifest[T]
    val mK = manifest[K]
  }

  case class QueryableKeysDistinct[T:Manifest, K:Manifest](in: Exp[Table[T]], keyFunc: Exp[T] => Exp[K], cond: Exp[T] => Exp[Boolean]) extends DeliteOpHashFilterReduce[T, K, K, Table[K]] {
    def alloc = Table[K](unit(0))
    val size = copyTransformedOrElse(_.size)(in.size)
    def zero = unit(null).AsInstanceOf[K]
    def mapFunc = keyFunc
    def reduceFunc = (a,b) => a
  }

  case class QueryableHashReduce[T:Manifest, K:Manifest, R:Manifest](in: Exp[Table[T]], keyFunc: Exp[T] => Exp[K], mapFunc: Exp[T] => Exp[R], reduceFunc: (Exp[R],Exp[R]) => Exp[R], cond: Exp[T] => Exp[Boolean]) 
    extends DeliteOpHashFilterReduce[T, K, R, Table[R]] {

    def alloc = Table[R](unit(0))
    val size = copyTransformedOrElse(_.size)(in.size)
    def zero = zeroType[R]

    val mT = manifest[T]
    val mK = manifest[K]
    val mR = manifest[R]
  }

  case class QueryableDivide[T:Manifest](inA: Exp[Table[T]], inB: Exp[Table[Int]], func: (Exp[T],Exp[Int]) => Exp[T]) extends DeliteOpZipWith[T,Int,T,Table[T]] {
    override def alloc(len: Exp[Int]) = Table(len)
    val size = copyTransformedOrElse(_.size)(inA.size)
    val mT = manifest[T]
  }

  //Where-GroupBy fusion
  override def queryable_groupby[T:Manifest, K:Manifest](s: Exp[Table[T]], keySelector: Exp[T] => Exp[K]) = s match {
    case Def(QueryableWhere(origS, predicate)) => //TODO: this special-cases filter fusion (only for groupBy); LMS fusion should take care of it generically for us
      QueryableGroupByWhere(origS, keySelector, predicate)
    case _ => super.queryable_groupby(s, keySelector)
  }

  private def hashReduce[A:Manifest,K:Manifest,T:Manifest,R:Manifest](resultSelector: Exp[T] => Exp[R], keySelector: Exp[A] => Exp[K]): Option[(Exp[A]=>Exp[R], (Exp[R],Exp[R])=>Exp[R], (Exp[R],Exp[Int])=>Exp[R])] = {
    var failed = false
    val ctx = implicitly[SourceContext]
    def rewriteMap(value: Exp[Any]) = (value match {
      case Def(Field(s,"key")) => keySelector
      case Def(Field(Def(Field(s,"key")),index)) => (a:Exp[A]) => field(keySelector(a),index)(value.tp,ctx)
      case Def(QueryableSum(s, sumSelector)) => sumSelector
      case Def(QueryableAverage(s, avgSelector, sum)) => avgSelector
      case Def(QueryableCount(s)) => (a:Exp[A]) => unit(1)
      case Def(QueryableMin(s, minSelector)) => minSelector
      case Def(QueryableMax(s, maxSelector)) => maxSelector
      //case Def(QueryableFirst(s)) => a
      //case Def(QueryableLast(s)) => a
      case Def(a) => printlog("found unknown: " + a.toString); failed = true; null
      case _ => printlog("found unknown: " + value.toString); failed = true; null
    }).asInstanceOf[Exp[A]=>Exp[R]]

    def rewriteReduce[N](value: Exp[Any]) = (value match {
      case Def(Field(s,"key")) => (a:Exp[N],b:Exp[N]) => a
      case Def(Field(Def(Field(s,"key")),index)) => (a:Exp[N],b:Exp[N]) => a
      case Def(sum@QueryableSum(s, sumSelector)) => (a:Exp[N],b:Exp[N]) => numeric_plus(a,b)(ntype(sum.N),mtype(sum.mN),ctx)
      case Def(QueryableAverage(s, avgSelector, sum)) => (a:Exp[N],b:Exp[N]) => numeric_plus(a,b)(ntype(sum.N),mtype(sum.mN),ctx)
      case Def(QueryableCount(s)) => (a:Exp[N],b:Exp[N]) => numeric_plus(a,b)(ntype(implicitly[Numeric[Int]]),mtype(manifest[Int]),ctx)
      case Def(min@QueryableMin(s, minSelector)) => (a:Exp[N],b:Exp[N]) => ordering_min(a,b)(otype(min.oN),mtype(min.mN),ctx)
      case Def(max@QueryableMax(s, maxSelector)) => (a:Exp[N],b:Exp[N]) => ordering_max(a,b)(otype(max.oN),mtype(max.mN),ctx)
      case _ => failed = true; null
    }).asInstanceOf[(Exp[N],Exp[N])=>Exp[N]]

    def rewriteAverage[N](value: Exp[Any]) = (value match {
      case Def(QueryableAverage(_,_,sum)) => (a:Exp[N],count:Exp[Int]) => numeric_divide(a, count)(sum.N,sum.mN,ctx)
      case _ => (a:Exp[N],count:Exp[Int]) => a
    }).asInstanceOf[(Exp[N],Exp[Int])=>Exp[N]]


    val funcs = resultSelector(fresh[T]) match {
      case Def(Struct(tag: StructTag[R], elems)) => 
        val valueFunc = (a:Exp[A]) => struct[R](tag, elems map { case (key, value) => (key, rewriteMap(value)(a)) })
        val reduceFunc = (a:Exp[R],b:Exp[R]) => struct[R](tag, elems map { case (key, value) => (key, rewriteReduce(value)(field(a,key)(value.tp,ctx), field(b,key)(value.tp,ctx))) })
        val averageFunc = (a:Exp[R],count:Exp[Int]) => struct[R](tag, elems map { case (key, value) => (key, rewriteAverage(value)(field(a,key)(value.tp,ctx), count)) })
        (valueFunc, reduceFunc, averageFunc)

      case a => (rewriteMap(a), rewriteReduce[R](a), rewriteAverage[R](a))
    }

    if (failed) None else Some(funcs)
  }
  
  override def queryable_select[T:Manifest, R:Manifest](s: Exp[Table[T]], resultSelector: Exp[T] => Exp[R]): Exp[Table[R]] = s match {
    case Def(QueryableWhere(origS, predicate)) => //Where-Select fusion
      QueryableSelectWhere(origS, resultSelector, predicate) 
    
    case Def(g@QueryableGroupBy(origS: Exp[Table[a]], keySelector)) => hashReduce(resultSelector, keySelector)(g.mT,g.mK,manifest[T],manifest[R]) match { //GroupBy-Select fusion
      case Some((valueFunc, reduceFunc, averageFunc)) => 
        val hr = QueryableHashReduce(origS, keySelector, valueFunc, reduceFunc, (e:Exp[a]) => unit(true))(g.mT,g.mK,manifest[R])
        val count = QueryableHashReduce(origS, keySelector, (e:Exp[a])=>unit(1), (a:Exp[Int],b:Exp[Int])=>a+b, (e:Exp[a])=>unit(true))(g.mT,g.mK,manifest[Int])
        QueryableDivide(hr, count, averageFunc)
      case None => 
        Console.println("ERROR: unable to fuse GroupBy-Select")
        return super.queryable_select(s, resultSelector)
    } 
    case Def(g@QueryableGroupByWhere(origS: Exp[Table[a]], keySelector, cond)) => hashReduce(resultSelector, keySelector)(g.mT,g.mK,manifest[T],manifest[R]) match {
      case Some((valueFunc, reduceFunc, averageFunc)) => 
        val hr = QueryableHashReduce(origS, keySelector, valueFunc, reduceFunc, cond)(g.mT,g.mK,manifest[R])
        val count = QueryableHashReduce(origS, keySelector, (e:Exp[a]) => unit(1), (a:Exp[Int],b:Exp[Int])=>a+b, cond)(g.mT,g.mK,manifest[Int])
        QueryableDivide(hr, count, averageFunc)
      case None => 
        Console.println("ERROR: unable to fuse GroupBy-Select")
        return super.queryable_select(s, resultSelector)
    }
    case _ => super.queryable_select(s, resultSelector)
  }


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@QueryableSelectWhere(in,g,c) => reflectPure(new { override val original = Some(f,e) } with QueryableSelectWhere(f(in),f(g),f(c))(mtype(e.mT),mtype(e.mR)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@QueryableHashReduce(in,key,m,r,c) => reflectPure(new { override val original = Some(f,e) } with QueryableHashReduce(f(in),f(key),f(m),f(r),f(c))(mtype(e.mT),mtype(e.mK),mtype(e.mR)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@QueryableDivide(x,y,g) => reflectPure(new { override val original = Some(f,e) } with QueryableDivide(f(x),f(y),f(g))(mtype(e.mT)))(mtype(manifest[A]),implicitly[SourceContext])
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] 

}

trait ScalaGenQueryableOps extends ScalaGenFat {
  val IR:QueryableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case QueryableSort(in, compare) => emitValDef(sym, quote(in) + " //TODO: sort")
    case _ => super.emitNode(sym,rhs)
  }

}
