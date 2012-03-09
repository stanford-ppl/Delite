package ppl.dsl.optiql.ops

import ppl.dsl.optiql.datastruct.scala.container.{DataTable, Grouping}
import java.io.PrintWriter
import scala.virtualization.lms.common.{Base, ScalaGenFat, BaseFatExp}
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.optiql.OptiQLExp
import ppl.delite.framework.datastructures.FieldAccessOpsExp

trait QueryableOps extends Base {

  //type TransparentProxy[+T] = Rep[T]

  implicit def repToQueryableOps[TSource:Manifest](r: Rep[DataTable[TSource]]) = new QOpsCls(r) 
  implicit def repGroupingToQueryableOps[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]) = new QOpsCls(queryable_grouping_toDatatable(g))
  
  abstract class OrderedQueryable[TSource:Manifest]
  implicit def repOQtoOQOps[TSource:Manifest](r: Rep[OrderedQueryable[TSource]]) = new OQOpsCls(r)  
  implicit def repOQtoDT[TSource:Manifest](r: Rep[OrderedQueryable[TSource]]): Rep[DataTable[TSource]]
  
  //override def __forward[A,B,C](self: TransparentProxy[A], method: String, x: TransparentProxy[B]*): TransparentProxy[C] = throw new RuntimeException("forwarding to " + method)

  class QOpsCls[TSource:Manifest](s: Rep[DataTable[TSource]]) {
    def Where(predicate: Rep[TSource] => Rep[Boolean]) = queryable_where(s, predicate)
    def GroupBy[TKey:Manifest](keySelector: Rep[TSource] => Rep[TKey]) = queryable_groupby(s,keySelector)
    def Select[TResult:Manifest](resultSelector: Rep[TSource] => Rep[TResult]) = queryable_select(s, resultSelector)
    def Sum(sumSelector: Rep[TSource] => Rep[Double]) = queryable_sum(s, sumSelector)
    def Average(avgSelector: Rep[TSource] => Rep[Double]) = queryable_average(s, avgSelector)
    def Count() = queryable_count(s)
    def OrderBy[TKey:Ordering:Manifest](keySelector: Rep[TSource] => Rep[TKey]) = queryable_orderby(s, keySelector)
    def OrderByDescending[TKey:Ordering:Manifest](keySelector: Rep[TSource] => Rep[TKey]) = queryable_orderbydescending(s, keySelector)
    def Min(minSelector: Rep[TSource] => Rep[Double]) = queryable_min(s, minSelector)
    def MinIndex(minSelector: Rep[TSource] => Rep[Double]) = queryable_min_index(s, minSelector)
    def Join[TSecond:Manifest](second: Rep[DataTable[TSecond]]) = new JoinableOps(s, second)
  }

  class JoinableOps[TFirst:Manifest, TSecond:Manifest](first: Rep[DataTable[TFirst]], second: Rep[DataTable[TSecond]]) {
    def Where(predicate: Rep[TSecond] => Rep[Boolean]) = new JoinableOps(first, second.Where(predicate))
    def WhereEq[TKey2:Manifest](firstKeySelector: Rep[TFirst] => Rep[TKey2],secondKeySelector: Rep[TSecond] => Rep[TKey2]) = new Joinable2(first, firstKeySelector, second, secondKeySelector)
  }

  class Joinable2[TFirst:Manifest, TSecond:Manifest, TKey2:Manifest](
    val first: Rep[DataTable[TFirst]],
    val firstKeySelector: Rep[TFirst] => Rep[TKey2],
    val second: Rep[DataTable[TSecond]],
    val secondKeySelector: Rep[TSecond] => Rep[TKey2] 
  ) {
    def Select[TResult:Manifest](resultSelector: (Rep[TFirst], Rep[TSecond]) => Rep[TResult]) = queryable_join2(first, firstKeySelector, second, secondKeySelector, resultSelector)
  }
  
  class OQOpsCls[TSource:Manifest](oq: Rep[OrderedQueryable[TSource]]) {
    def ThenBy[TKey:Ordering:Manifest](keySelector: Rep[TSource] => Rep[TKey]) = queryable_thenby(oq, keySelector) 
  }
  
  //Grouping stuff
  def infix_key[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]) = queryable_grouping_key(g) 

  def queryable_where[TSource:Manifest](s: Rep[DataTable[TSource]], predicate: Rep[TSource] => Rep[Boolean]): Rep[DataTable[TSource]]
  def queryable_groupby[TSource:Manifest, TKey:Manifest](s: Rep[DataTable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[DataTable[Grouping[TKey, TSource]]]
  def queryable_select[TSource:Manifest, TResult:Manifest](s: Rep[DataTable[TSource]], resultSelector: Rep[TSource] => Rep[TResult]): Rep[DataTable[TResult]]
  def queryable_sum[TSource:Manifest](s: Rep[DataTable[TSource]], sumSelector: Rep[TSource] => Rep[Double]): Rep[Double]
  def queryable_min[TSource:Manifest](s: Rep[DataTable[TSource]], minSelector: Rep[TSource] => Rep[Double]): Rep[TSource]
  def queryable_min_index[TSource:Manifest](s: Rep[DataTable[TSource]], minSelector: Rep[TSource] => Rep[Double]): Rep[Int]
  def queryable_average[TSource:Manifest](s: Rep[DataTable[TSource]], avgSelector: Rep[TSource] => Rep[Double]): Rep[Double]
  def queryable_count[TSource:Manifest](s: Rep[DataTable[TSource]]): Rep[Int]  
  def queryable_orderby[TSource:Manifest, TKey:Ordering:Manifest](s: Rep[DataTable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[OrderedQueryable[TSource]]
  def queryable_orderbydescending[TSource:Manifest, TKey:Ordering:Manifest](s: Rep[DataTable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[OrderedQueryable[TSource]]
  def queryable_thenby[TSource:Manifest, TKey:Ordering:Manifest](oq: Rep[OrderedQueryable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[OrderedQueryable[TSource]]
  
  def queryable_grouping_toDatatable[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[DataTable[TSource]]
  def queryable_grouping_key[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[TKey]
  
  def queryable_join2[TFirst:Manifest, TSecond:Manifest, TKey2:Manifest, TResult:Manifest](first: Rep[DataTable[TFirst]], firstKeySelector: Rep[TFirst] => Rep[TKey2], 
    second: Rep[DataTable[TSecond]], secondKeySelector: Rep[TSecond] => Rep[TKey2], resultSelector: (Rep[TFirst], Rep[TSecond]) => Rep[TResult]):Rep[DataTable[TResult]]
  
}

trait QueryableOpsExp extends QueryableOps with BaseFatExp {
  this: QueryableOps with OptiQLExp =>



  case class QueryableWhere[TSource:Manifest](in: Exp[DataTable[TSource]], cond: Exp[TSource] => Exp[Boolean]) extends DeliteOpFilter[TSource, TSource,DataTable[TSource]] {
    def alloc = DataTable[TSource]()
    def func = e => e
    val size = in.size
  }
     
  case class QueryableSelect[TSource:Manifest, TResult:Manifest](in: Exp[DataTable[TSource]], func: Exp[TSource] => Exp[TResult]) extends DeliteOpMap[TSource, TResult, DataTable[TResult]] {
    def alloc = DataTable[TResult](in.size)
    val size = in.size
  }
  
  
  //these are hacked up for now untill we have proper Delite support
  case class HackQueryableGroupBy[TSource:Manifest, TKey:Manifest](s: Exp[DataTable[TSource]], v:Sym[TSource], key: Block[TKey]) extends Def[DataTable[Grouping[TKey, TSource]]]
  case class HackQueryableSum[TSource:Manifest](s:Exp[DataTable[TSource]], sym: Sym[TSource], value: Block[Double]) extends Def[Double]
  
  case class QueryableAverage[TSource:Manifest](s: Exp[DataTable[TSource]], avgSelector: Rep[TSource] => Rep[Double]) extends Def[Double]
  
  //case class QueryableCount[TSource:Manifest](s: Exp[DataTable[TSource]]) extends Def[Int]

  case class QueryableGroupingToDataTable[TSource:Manifest, TKey:Manifest](g: Rep[Grouping[TKey, TSource]]) extends Def[DataTable[TSource]]
  case class QueryableGroupingKey[TSource:Manifest, TKey:Manifest](g: Rep[Grouping[TKey, TSource]]) extends Def[TKey]
  
  case class NewOrderedQueryable[TSource:Manifest](s: Exp[DataTable[TSource]], orig: Exp[DataTable[TSource]], sel: (Exp[Int], Exp[Int]) => Exp[Int]) extends Def[OrderedQueryable[TSource]]

  implicit def repOQtoDT[TSource:Manifest](x: Exp[OrderedQueryable[TSource]]) = x match {
    case Def(NewOrderedQueryable(s,_,_)) => s
  }

  def queryable_select[TSource:Manifest, TResult:Manifest](s: Rep[DataTable[TSource]], resultSelector: Rep[TSource] => Rep[TResult]) = {
//  val v = fresh[TSource]
//  val func = reifyEffects(resultSelector(v))
//    QueryableSelect(s, resultSelector)
    val data = arraySelect(s.size)(i => resultSelector(s(i)))
    DataTable(data, data.length)
  }
  
  def queryable_where[TSource:Manifest](s: Exp[DataTable[TSource]], predicate: Exp[TSource] => Exp[Boolean]) = {
    //QueryableWhere(s,predicate)
    val data = arrayWhere(s.size)(i => predicate(s(i)))(i => s(i))
    DataTable(data, data.length)
  }
  
  def queryable_groupby[TSource:Manifest, TKey:Manifest](s: Exp[DataTable[TSource]], keySelector: Exp[TSource] => Exp[TKey]) = {
    /*val v = fresh[TSource]
    val key = reifyEffects(keySelector(v))
    HackQueryableGroupBy(s, v, key)*/
    val keyData = arrayDistinct(s.size)(i => keySelector(s(i)))
    val valData = arrayGroup(s.size)(i => keySelector(s(i)))(i => s(i))
    val data = arraySelect(keyData.length)(i => 
      grouping_apply(keyData(i), DataTable(valData(i), valData(i).length)))
    DataTable(data, data.length)
  }

  def queryable_join2[TFirst:Manifest, TSecond:Manifest, TKey2:Manifest, TResult:Manifest](first: Rep[DataTable[TFirst]], firstKeySelector: Rep[TFirst] => Rep[TKey2], 
    second: Rep[DataTable[TSecond]], secondKeySelector: Rep[TSecond] => Rep[TKey2], resultSelector: (Rep[TFirst], Rep[TSecond]) => Rep[TResult]):Rep[DataTable[TResult]] = {

    /* first firstKeySelector second secondKeySelector resultSelector */
    
    val firstMap = indexBuild(first.size)(i => firstKeySelector(first(i)))
    val firstGrouped = arrayGroup(first.size)(i => firstKeySelector(first(i)))(i => first(i))
    
    val empty = arraySelect(0)(i => first(i))
    
    val dataNested = arraySelect(second.size) { j => 
      val secondItem = second(j)
      val key = secondKeySelector(secondItem)
      val firstIdx = indexLookup(firstMap,key)
      val firstItems = if (firstIdx >= 0) firstGrouped(firstIdx) else empty
      arraySelect(firstItems.length)(i => resultSelector(firstItems(i), secondItem))
    }
    
    val data = arrayFlatten(dataNested)
    DataTable(data, data.length)    
    
    /*
    val firstMap = first.groupBy(firstKeySelector)
    second flatMap { x2 => firstMap(secondKeySelector(x2)) map { x1 => resultSelector(x1,x2) } }
    */
  }


  def queryable_sum[TSource:Manifest](s: Rep[DataTable[TSource]], sumSelector: Rep[TSource] => Rep[Double]) = {
    /*val sym = fresh[TSource]
    val value = reifyEffects(sumSelector(sym))
    HackQueryableSum(s,sym,value)*/
    val res = reducePlain(s.size)(i => sumSelector(s(i)))(0)(_ + _)
    res
  }

  def queryable_min_index[TSource:Manifest](s: Rep[DataTable[TSource]], minSelector: Rep[TSource] => Rep[Double]) = {
    /*
    CAVEAT: must not fuse if s is a filter loop because that will mess with index i !!!
    */
    reduceTuple(1 + s.size - 1)(i => i, i => minSelector(s(i)))(unit(-1), unit(scala.Double.MaxValue)) { 
      case ((i0,v0),(i1,v1)) => 
        (reifyEffects { if (v0 <= v1) i0 else i1 }, 
         reifyEffects { if (v0 <= v1) v0 else v1 })
    }
  }
  
  def queryable_min[TSource:Manifest](s: Rep[DataTable[TSource]], minSelector: Rep[TSource] => Rep[Double]) = {
    // first compute min index, then get item at index
    val minIdx = queryable_min_index(s,minSelector)
    // the conditional below forces Map creation.
    // TODO: refactor to return Option and implement soa transform for option values.
    if (minIdx >= 0) s(minIdx) else null  // <--- should have struct(k -> null)  for all k in null
  }

  def queryable_average[TSource:Manifest](s: Rep[DataTable[TSource]], avgSelector: Rep[TSource] => Rep[Double]) = 
    s.Sum(avgSelector)/s.Sum(_ => 1)
    
  def queryable_count[TSource:Manifest](s: Rep[DataTable[TSource]]) = s.size()

  def queryable_compare[TSource:Manifest, TKey:Ordering:Manifest](i: Rep[Int], j: Rep[Int], s: Rep[DataTable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[Int] = {
    if (keySelector(s(i)) < keySelector(s(j))) unit(-1) else if (keySelector(s(i)) > keySelector(s(j))) unit(1) else unit(0)
  }
  
  def queryable_orderby[TSource:Manifest, TKey:Ordering:Manifest](s: Rep[DataTable[TSource]], keySelector: Rep[TSource] => Rep[TKey]) = {
    val permutation = arraySort(s.size)((i,j) => keySelector(s(i)) <= keySelector(s(j)))    
    val data = arraySelect(s.size)(i => s(permutation(i)))
    val sel: (Rep[Int],Rep[Int]) => Rep[Int] = (i,j) => queryable_compare(i,j,s,keySelector)
    val table = DataTable(data, data.length)
    Console.println("NewOrderedQueryable type " + table.Type)
    toAtom(NewOrderedQueryable(table, s, sel))(mtype(table.Type))
  }
  def queryable_orderbydescending[TSource:Manifest, TKey:Ordering:Manifest](s: Rep[DataTable[TSource]], keySelector: Rep[TSource] => Rep[TKey]) = {
    // TODO: copy pasted from above --> refactor
    val permutation = arraySort(s.size)((i,j) => keySelector(s(i)) >= keySelector(s(j)))
    val data = arraySelect(s.size)(i => s(permutation(i)))
    val sel: (Rep[Int],Rep[Int]) => Rep[Int] = (i,j) => queryable_compare(i,j,s,keySelector)
    val table = DataTable(data, data.length)
    Console.println("NewOrderedQueryable type " + table.Type)
    toAtom(NewOrderedQueryable(table, s, sel))(mtype(table.Type))
  }

  def queryable_thenby[TSource:Manifest, TKey:Ordering:Manifest](oq: Rep[OrderedQueryable[TSource]], keySelector: Rep[TSource] => Rep[TKey]) = {
    val (s,sel) = oq match {
      case Def(NewOrderedQueryable(n, s, sel)) => (s,sel)
    }
    val permutation = arraySort(s.size)((i,j) => {
      val prev = sel(i,j)
      if (unit(-1) == prev) unit(true) else if (prev == unit(1)) unit(false) else keySelector(s(i)) <= keySelector(s(j))
    })
    val data = arraySelect(s.size)(i => s(permutation(i)))
    val compoundSel: (Rep[Int],Rep[Int]) => Rep[Int] = (i,j) => {
      val prev = sel(i,j)
      if (prev == unit(0)) queryable_compare(i,j,s,keySelector)
      else prev 
    }
    val table = DataTable(data, data.length)
    Console.println("NewOrderedQueryable type " + table.Type)
    toAtom(NewOrderedQueryable(table, s, compoundSel))(mtype(table.Type))
  }
  
  def grouping_apply[TKey:Manifest, TSource:Manifest](k: Rep[TKey], v: Rep[DataTable[TSource]]): Rep[Grouping[TKey, TSource]] =
    struct[Grouping[TKey,TSource]](List("Grouping"), Map("key"->k, "values"->v))
  
  //def queryable_grouping_toDatatable[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]) = QueryableGroupingToDataTable(g)
  //def queryable_grouping_key[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[TKey] = QueryableGroupingKey(g)
  
  def queryable_grouping_key[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[TKey] = field[TKey](g, "key")
  def queryable_grouping_toDatatable[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[DataTable[TSource]] = field[DataTable[TSource]](g, "values")
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {    
    // FIXME: this won't work. mirroring delite ops needs to look like this:
    //case e@VectorMap(x,p) => reflectPure(new { override val original = Some(f,e) } with VectorMap(f(x),f(p))(e.mA,e.mB))(mtype(manifest[A]))
    case QueryableWhere(s,p) => queryable_where(f(s), p)
    case NewOrderedQueryable(n,s,sel) => reflectPure(NewOrderedQueryable(f(n),f(s),f(sel)))(mtype(f(n).Type))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] //todo fix asInstanceOf
  
  override def syms(e: Any): List[Sym[Any]] = e match { 
    //case QueryableGroupBy(s,v,k) => syms(s) 
    case NewOrderedQueryable(n,s,sel) => syms(n)
    case _ => super.syms(e)
  }
  
  override def symsFreq(e: Any): List[(Sym[Any],Double)] = e match { 
    //case QueryableGroupBy(s,v,k) => syms(s) 
    case NewOrderedQueryable(n,s,sel) => freqNormal(n)
    case _ => super.symsFreq(e)
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

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case NewOrderedQueryable(n,s,sel) => //super.emitNode()
                                      /*val d = findDefinition(s.asInstanceOf[Sym[Any]]).get; emitNode(d.sym,d.rhs)*/ 
                                       stream.println("val " + quote(sym) + " = {")
                                       emitBlock(Block(n)) 
                                       stream.println(quote(n))
                                       stream.println("}")
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
