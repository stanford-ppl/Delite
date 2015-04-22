//package ppl.delite.framework.codegen.hw
//
//import scala.virtualization.lms.internal._
//import scala.collection.mutable.ListBuffer
//import scala.collection.mutable.HashMap
//import ppl.delite.framework.Config
//
//class HwGraph {
//  /* Isn't there a better way to use Syms directly */
//  val IR: Expressions = null
//  val HW_IR: ThorIR = null
//
//  import IR.Sym
//  import HW_IR.Module
//
//  val nodes: ListBuffer[Module] = new ListBuffer[Module]()
//  val symNodeMap: HashMap[Sym[Any], Module] = new HashMap[Sym[Any], Module]()
//  var rootNode: Module = null
//
//  def add(m: Module) = {
//    if (Config.debugCodegen) {
//      println(s"[HwGraph::add] Map before: $symNodeMap")
//      println(s"[HwGraph::add] Nodes before: $nodes")
//      println(s"[HwGraph::add] Adding module $m")
//    }
//    if (!nodes.exists(x => x.sym == m.sym)) {
//      nodes.append(m)
//      symNodeMap(m.sym.asInstanceOf[IR.Sym[Any]]) = m
//      if (rootNode == null) {
//        rootNode = m
//      }
//      if (Config.debugCodegen) {
//        println(s"[HwGraph::add] Map before: $symNodeMap")
//        println(s"[HwGraph::add] Nodes before: $nodes")
//        println(s"[HwGraph::add] Adding module $m")
//      }
//
//    }
//    else {
//      if (Config.debugCodegen) {
//        println(s"Node $m exists, nodes and symNodeMap unchanged")
//      }
//    }
//
//  }
//
//  def getModules(sl: List[Sym[Any]]): List[Module] = {
//    if (Config.debugCodegen) {
//      println(s"[HwGraph::getModules] sl  = $sl")
//      println(s"[HwGraph::getModules] map = $symNodeMap")
//    }
//    val lb: ListBuffer[Module] = new ListBuffer[Module]()
//    for (s <- sl) {
//      if (symNodeMap.contains(s)) {
//        lb.append(symNodeMap(s))
//      }
//    }
//    lb.toList
//  }
//
////  def bfsWalker() = {
////
////  }
////
////  def dfsWalker() = {
////  }
//
//}
//
