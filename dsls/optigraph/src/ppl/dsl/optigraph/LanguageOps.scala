package ppl.dsl.optigraph

import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import reflect.Manifest
import scala.virtualization.lms.internal.GenericFatCodegen
import scala.virtualization.lms.common._
import scala.virtualization.lms.util._

trait LanguageOps extends Base with OverloadHack { this: OptiGraph =>

  /** Iterations */
  
  // sequential for loop
  def For[T:Manifest, GT<:GIterable[T]](items: Rep[GT])(block: Rep[T] => Rep[Unit]): Rep[Unit] = repGIterableToGIterableOps(items).forseq(block)
  // sequential for loop with filter
  def For[T:Manifest, GT<:GIterable[T]](items: Rep[GT], filter: Rep[T] => Rep[Boolean])(block: Rep[T] => Rep[Unit]): Rep[Unit] = repGIterableToGIterableOps(items).forseq(filter, block)
  
  // parallel for-each loop
  def Foreach[T:Manifest](items: Rep[GIterable[T]])(block: Rep[T] => Rep[Unit]): Rep[Unit] = repGIterableToGIterableOps(items).foreach(block)
  // parallel for-each loop with filter
  def Foreach[T:Manifest](items: Rep[GIterable[T]], pred: Rep[T] => Rep[Boolean])(block: Rep[T] => Rep[Unit]): Rep[Unit] = repGIterableToGIterableOps(items).filter(pred).foreach(block)
  
  /** Reductions (optional filter predicate) */
  
  def Sum[T:Manifest, A:Manifest:Numeric](items: Rep[GIterable[T]])(block: Rep[T] => Rep[A]): Rep[A] = repGIterableToGIterableOps(items).sum(block)
  def Sum[T:Manifest, A:Manifest:Numeric](items: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean])(block: Rep[T] => Rep[A]): Rep[A] = repGIterableToGIterableOps(items).sum(filter, block)
  def Product[T:Manifest, A:Manifest:Numeric](items: Rep[GIterable[T]])(block: Rep[T] => Rep[A]): Rep[A] = repGIterableToGIterableOps(items).product(block)
  def Product[T:Manifest, A:Manifest:Numeric](items: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean])(block: Rep[T] => Rep[A]): Rep[A] = repGIterableToGIterableOps(items).product(filter, block)
  def Max[T:Manifest, A:Manifest:Ordering](items: Rep[GIterable[T]])(block: Rep[T] => Rep[A]): Rep[A] = repGIterableToGIterableOps(items).max(block)
  def Max[T:Manifest, A:Manifest:Ordering](items: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean])(block: Rep[T] => Rep[A]): Rep[A] = repGIterableToGIterableOps(items).max(filter, block)
  def Min[T:Manifest, A:Manifest:Ordering](items: Rep[GIterable[T]])(block: Rep[T] => Rep[A]): Rep[A] = repGIterableToGIterableOps(items).min(block)
  def Min[T:Manifest, A:Manifest:Ordering](items: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean])(block: Rep[T] => Rep[A]): Rep[A] = repGIterableToGIterableOps(items).min(filter, block)
  def Count[T:Manifest](items: Rep[GIterable[T]])(block: Rep[T] => Rep[Boolean]): Rep[Int] = repGIterableToGIterableOps(items).count(block)
  def All[T:Manifest](items: Rep[GIterable[T]])(block: Rep[T] => Rep[Boolean]): Rep[Boolean] = repGIterableToGIterableOps(items).all(block)
  def All[T:Manifest](items: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean])(block: Rep[T] => Rep[Boolean]): Rep[Boolean] = repGIterableToGIterableOps(items).all(filter, block)
  def Any[T:Manifest](items: Rep[GIterable[T]])(block: Rep[T] => Rep[Boolean]): Rep[Boolean] = repGIterableToGIterableOps(items).any(block)
  def Any[T:Manifest](items: Rep[GIterable[T]], filter: Rep[T] => Rep[Boolean])(block: Rep[T] => Rep[Boolean]): Rep[Boolean] = repGIterableToGIterableOps(items).any(filter, block)
  
  /** Traversals (optional navigator and post-traversal clauses) */
  
  // DFS order traversal (sequential)
  
  def InDFS(g: Rep[Graph], from: Rep[Node], block: Rep[Node] => Rep[Unit]): Rep[Unit] = repGraphToGraphOps(g).InDFS(from, block)
  def InDFS(g: Rep[Graph], from: Rep[Node], filter: Rep[Node] => Rep[Boolean], block: Rep[Node] => Rep[Unit]): Rep[Unit] = repGraphToGraphOps(g).InDFS(from, filter, block)
  def InDFS(g: Rep[Graph], from: Rep[Node], block: Rep[Node] => Rep[Unit], inPost: Rep[Node] => Rep[Unit])(implicit o: Overloaded1): Rep[Unit] = repGraphToGraphOps(g).InDFS(from, block, inPost)
  def InDFS(g: Rep[Graph], from: Rep[Node], filter: Rep[Node] => Rep[Boolean], block: Rep[Node] => Rep[Unit], inPost: Rep[Node] => Rep[Unit]): Rep[Unit] = repGraphToGraphOps(g).InDFS(from, filter, block, inPost)
  // post-dfs-traversal clause
  def InPost(block: Rep[Node] => Rep[Unit]): Rep[Node] => Rep[Unit] = block
  
  // BFS order traversal (parallel on every level)
  
  def InBFS(g: Rep[Graph], from: Rep[Node], block: Rep[Node] => Rep[Unit]): Rep[Unit] = repGraphToGraphOps(g).InBFS(from, block)
  def InBFS(g: Rep[Graph], from: Rep[Node], filter: Rep[Node] => Rep[Boolean], block: Rep[Node] => Rep[Unit]): Rep[Unit] = repGraphToGraphOps(g).InBFS(from, filter, block)
  def InBFS(g: Rep[Graph], from: Rep[Node], block: Rep[Node] => Rep[Unit], inReverse: Rep[Node] => Rep[Unit])(implicit o: Overloaded1): Rep[Unit] = repGraphToGraphOps(g).InBFS(from, block, inReverse)
  def InBFS(g: Rep[Graph], from: Rep[Node], filter: Rep[Node] => Rep[Boolean], block: Rep[Node] => Rep[Unit], inReverse: Rep[Node] => Rep[Unit]): Rep[Unit] = repGraphToGraphOps(g).InBFS(from, block, inReverse)
  // post-bfs-traversal clause
  def InReverse(block: Rep[Node] => Rep[Unit]): Rep[Node] => Rep[Unit] = block
  
  /** Graph Kernels */
  
  // def btw_centrality()...
  
  /** MISC */
  
  // Random graph generators
  //def RandDGraph(numNodes: Rep[Int], numEdges: Rep[Int]): Rep[Graph] = new_rand_graph(numNodes, numEdges)
  //def RandUGraph(numNodes: Rep[Int], numEdges: Rep[Int]): Rep[Graph] = new_rand_graph(numNodes, numEdges)
  
  def wall_time(): Rep[Double]
  def tic(deps: Rep[Any]*) = profile_start(deps)
  def toc(deps: Rep[Any]*) = profile_stop(deps)
  
  def profile_start(deps: Seq[Rep[Any]]): Rep[Unit]
  def profile_stop(deps: Seq[Rep[Any]]): Rep[Unit]
  
  /* I/O */
  //def loadGraph(filename: Rep[String]) = GraphInputReader.read(filename, delim)

  /** Special values */
  
  def MAX_INT = unit(scala.Int.MaxValue)
  def MAX_FLOAT = unit(scala.Float.MaxValue)
  def MAX_DOUBLE = unit(scala.Double.MaxValue)
  def MIN_INT = unit(scala.Int.MinValue)
  def MIN_FLOAT = unit(scala.Float.MinValue)
  def MIN_DOUBLE = unit(scala.Double.MinValue)
    
  /*def INF*/
}

trait LanguageOpsExp extends LanguageOps with BaseFatExp with EffectExp {
  this: OptiGraphExp with LanguageImplOps =>
  
    case class WallTime() extends Def[Double]  
    def wall_time() = reflectEffect(WallTime())
    
    case class ProfileStart(deps: List[Exp[Any]]) extends Def[Unit]
    case class ProfileStop(deps: List[Exp[Any]]) extends Def[Unit]

    def profile_start(deps: Seq[Exp[Any]]) = reflectEffect(ProfileStart(deps.toList))
    def profile_stop(deps: Seq[Exp[Any]]) = reflectEffect(ProfileStop(deps.toList))
  
}

trait BaseGenLanguageOps extends GenericFatCodegen {
  val IR: LanguageOpsExp
  import IR._

}

trait ScalaGenLanguageOps extends ScalaGenEffect with BaseGenLanguageOps {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case WallTime() => emitValDef(sym, "System.currentTimeMillis")
      case ProfileStart(deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.start(\"app\", false)")
      case ProfileStop(deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.stop(\"app\", false)")
      case _ => super.emitNode(sym, rhs)
    }
  }
}
