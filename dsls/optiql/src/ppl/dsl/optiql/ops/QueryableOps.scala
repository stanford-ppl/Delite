package ppl.dsl.optiql.ops

import ppl.dsl.optiql.datastruct.scala.container.{DataTable, Grouping}
import java.io.PrintWriter
import scala.virtualization.lms.common.{Base, ScalaGenFat, BaseFatExp}
import scala.virtualization.lms.internal.GenericFatCodegen
import scala.reflect.SourceContext
import ppl.dsl.optiql.OptiQLExp
import ppl.delite.framework.datastructures.FieldAccessOpsExp

trait QueryableOps extends Base {

  //type TransparentProxy[+T] = Rep[T]

  implicit def repToQueryableOps[TSource:Manifest](r: Rep[DataTable[TSource]]) = new QOpsCls(r) 
  implicit def repGroupingToQueryableOps[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]) = new QOpsCls(queryable_grouping_toDatatable(g))

  //override def __forward[A,B,C](self: TransparentProxy[A], method: String, x: TransparentProxy[B]*): TransparentProxy[C] = throw new RuntimeException("forwarding to " + method)

  class QOpsCls[TSource:Manifest](s: Rep[DataTable[TSource]]) {
    def Where(predicate: Rep[TSource] => Rep[Boolean]) = queryable_where(s, predicate)
    def GroupBy[TKey:Manifest](keySelector: Rep[TSource] => Rep[TKey]) = queryable_groupby(s,keySelector)
    def Select[TResult:Manifest](resultSelector: Rep[TSource] => Rep[TResult]) = queryable_select(s, resultSelector)
    def Sum(sumSelector: Rep[TSource] => Rep[Double]) = queryable_sum(s, sumSelector)
    def Average(avgSelector: Rep[TSource] => Rep[Double]) = queryable_average(s, avgSelector)
    def Count() = queryable_count(s)
  }
  
  //Grouping stuff
  def infix_key[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]) = queryable_grouping_key(g) 

  def queryable_where[TSource:Manifest](s: Rep[DataTable[TSource]], predicate: Rep[TSource] => Rep[Boolean]): Rep[DataTable[TSource]]
  def queryable_groupby[TSource:Manifest, TKey:Manifest](s: Rep[DataTable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[DataTable[Grouping[TKey, TSource]]]
  def queryable_select[TSource:Manifest, TResult:Manifest](s: Rep[DataTable[TSource]], resultSelector: Rep[TSource] => Rep[TResult]): Rep[DataTable[TResult]]
  def queryable_sum[TSource:Manifest](s: Rep[DataTable[TSource]], sumSelector: Rep[TSource] => Rep[Double]): Rep[Double]
  def queryable_average[TSource:Manifest](s: Rep[DataTable[TSource]], avgSelector: Rep[TSource] => Rep[Double]): Rep[Double]
  def queryable_count[TSource:Manifest](s: Rep[DataTable[TSource]]): Rep[Int]  
  
  def queryable_grouping_toDatatable[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[DataTable[TSource]]
  def queryable_grouping_key[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[TKey]
  
  
}

trait QueryableOpsExp extends QueryableOps with BaseFatExp {
  this: QueryableOps with OptiQLExp =>

  case class QueryableWhere[TSource:Manifest](in: Exp[DataTable[TSource]], cond: Exp[TSource] => Exp[Boolean]) extends DeliteOpFilter[TSource, TSource,DataTable[TSource]] {
    override def alloc(size: Exp[Int]) = DataTable[TSource](size)
    def func = e => e
    val size = in.size
  }
     
  case class QueryableSelect[TSource:Manifest, TResult:Manifest](in: Exp[DataTable[TSource]], func: Exp[TSource] => Exp[TResult]) extends DeliteOpMap[TSource, TResult, DataTable[TResult]] {
    override def alloc(size: Exp[Int]) = DataTable[TResult](size)
    val size = in.size
  }
  
  
  //these are hacked up for now untill we have proper Delite support
  case class HackQueryableGroupBy[TSource:Manifest, TKey:Manifest](s: Exp[DataTable[TSource]], v:Sym[TSource], key: Block[TKey]) extends Def[DataTable[Grouping[TKey, TSource]]]
  case class HackQueryableSum[TSource:Manifest](s:Exp[DataTable[TSource]], sym: Sym[TSource], value: Block[Double]) extends Def[Double]
  
  /*
  case class QueryableSum[TSource:Manifest](s: Exp[DataTable[TSource]], sumSelector: Rep[TSource] => Rep[Double]) extends DeliteOpReduce[Double] {
    val size = copyTransformedOrElse(_.size)(s.size)
    
    def func = (a,b) => sumSelector(a) + sumSelector(b)
    val zero = unit(0.0f)
    /*
    val body: Def[Double] = DeliteReduceElem[Double](
      func = sumSelector(s(v)),
      zero = unit(0.0f),
      rV = rV,
      rFunc = rV._1 + rV._2,
      stripFirst = false
    )*/
  }*/
  case class QueryableAverage[TSource:Manifest](s: Exp[DataTable[TSource]], avgSelector: Rep[TSource] => Rep[Double]) extends Def[Double]
  
  //case class QueryableCount[TSource:Manifest](s: Exp[DataTable[TSource]]) extends Def[Int]

  case class QueryableGroupingToDataTable[TSource:Manifest, TKey:Manifest](g: Rep[Grouping[TKey, TSource]]) extends Def[DataTable[TSource]]
  case class QueryableGroupingKey[TSource:Manifest, TKey:Manifest](g: Rep[Grouping[TKey, TSource]]) extends Def[TKey]

  def queryable_select[TSource:Manifest, TResult:Manifest](s: Rep[DataTable[TSource]], resultSelector: Rep[TSource] => Rep[TResult]) = {
//  val v = fresh[TSource]
//  val func = reifyEffects(resultSelector(v))
    QueryableSelect(s, resultSelector)
  }
  
  def queryable_where[TSource:Manifest](s: Exp[DataTable[TSource]], predicate: Exp[TSource] => Exp[Boolean]) = QueryableWhere(s,predicate)
  def queryable_groupby[TSource:Manifest, TKey:Manifest](s: Exp[DataTable[TSource]], keySelector: Exp[TSource] => Exp[TKey]) = {
    val v = fresh[TSource]
    val key = reifyEffects(keySelector(v))
    HackQueryableGroupBy(s, v, key)
  }
  def queryable_sum[TSource:Manifest](s: Rep[DataTable[TSource]], sumSelector: Rep[TSource] => Rep[Double]) = {
    val sym = fresh[TSource]
    val value = reifyEffects(sumSelector(sym))
    HackQueryableSum(s,sym,value)
  }
  def queryable_average[TSource:Manifest](s: Rep[DataTable[TSource]], avgSelector: Rep[TSource] => Rep[Double]) = s.Sum(avgSelector)/s.size()
  def queryable_count[TSource:Manifest](s: Rep[DataTable[TSource]]) = s.size()
  
  def queryable_grouping_toDatatable[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]) = QueryableGroupingToDataTable(g)
  def queryable_grouping_key[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[TKey] = QueryableGroupingKey(g)
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {    
    // FIXME: this won't work. mirroring delite ops needs to look like this:
    //case e@VectorMap(x,p) => reflectPure(new { override val original = Some(f,e) } with VectorMap(f(x),f(p))(e.mA,e.mB))(mtype(manifest[A]))
    case QueryableWhere(s,p) => queryable_where(f(s), p)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] //todo fix asInstanceOf
  
  override def syms(e: Any): List[Sym[Any]] = e match { 
    //case QueryableGroupBy(s,v,k) => syms(s) 
    case _ => super.syms(e)
  }
  
  override def boundSyms(e: Any): List[Sym[Any]] = e match {    
    case HackQueryableGroupBy(s,v,k) => v::syms(k)
    case HackQueryableSum(s,sym,value) => sym::syms(value)
    case _ => super.boundSyms(e)
  }  
  
}

trait ScalaGenQueryableOps extends ScalaGenFat {  
  val IR: QueryableOpsExp with OptiQLExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case HackQueryableGroupBy(s, v, k) =>  {
      stream.println("val " + quote(sym) + " =  " + quote(s) + ".GroupBy( " + quote(v) + " => {")   
      emitBlock(k)   
      stream.println(quote(getBlockResult(k)) + "})")
    }
    case HackQueryableSum(s,sym2,value) => {
      stream.println("val " + quote(sym) + " = " + quote(s) + ".Sum( " + quote(sym2) + " => {")
      emitBlock(value)
      stream.println(quote(getBlockResult(value)) + "})")    
    }
    
    case QueryableGroupingToDataTable(g) => emitValDef(sym, "generated.scala.container.DataTable.convertIterableToDataTable(" + quote(g) + ")")
    case QueryableGroupingKey(g) => emitValDef(sym, quote(g) + ".key")
    case _ => super.emitNode(sym,rhs)
  }
}
