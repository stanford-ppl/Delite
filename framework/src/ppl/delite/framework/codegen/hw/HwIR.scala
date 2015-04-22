package ppl.delite.framework.codegen.hw

import scala.virtualization.lms.internal._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import ppl.delite.framework.Config

/*
 * Template Hardware Object Representation (THOR) Hardware IR
 * This IR follows the same sea-of-nodes representation used in Delite.
 * Each node in the IR is a Module, and each Module has dependencies on zero
 * or more other Modules.
 *
 * The ThorIR trait describes the class hierarchy implementing the
 * hardware intermediate representation
 */
trait ThorIR {

  val IR: Expressions
  import IR._

  abstract class Module {
    // Things that every module must provide an implementation of
    val sym: Exp[Any]
    val deps: List[Module]  // List of data dependencies
    def area() = 0  // Area occupied by module in some units
  }
  abstract class CombModule extends Module
  abstract class TemplateModule extends Module
  abstract class MemoryModule extends Module
  abstract class MemoryInterfaceModule extends Module

  /*
   * Dummy IR module for debugging purposes
   */
  case class Dummy()(s: Exp[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = 1
  }

  /*
   * Combinational logic block modules
   */
  case class CAdd()(s: Exp[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = 1
  }
  case class CSub()(s: Exp[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = 1
  }
  case class CMul()(s: Exp[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = 5
  }
  case class CDiv()(s: Exp[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = 12
  }
  case class CMux()(s: Exp[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = 1
  }
  // Combination of combinational logic block modules is also a
  // combinational logic block module - using the composite design pattern
  case class CComposite(modules: List[CombModule])(s: Exp[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = {
      modules.map(x => x.area).sum
    }
  }

  /*
   * Template modules
   */
  case class Pipeline(stages: List[Module])(s: Exp[Any], depList: List[Module]) extends TemplateModule {
    override val sym = s
    override val deps = depList
    override def area = {
      stages.map(x => x.area).sum
    }

  }
  case class Parallel(m: Module, n: Int)(s: Exp[Any], depList: List[Module]) extends TemplateModule {
    override val sym = s
    override val deps = depList
    override def area = {
     m.area * n
    }

  }
  case class TreeRed()(s: Exp[Any], depList: List[Module])  extends TemplateModule {
    override val sym = s
    override val deps = depList
  }
  case class FSM()(s: Exp[Any], depList: List[Module])      extends TemplateModule {
    override val sym = s
    override val deps = depList
  }

  /*
   * Address Generation Unit (AGU): Templatized FSM that automatically generates addresses
   * and other necessary control signals to interface with memory based upon the data structure's
   * access pattern
   */
  case class AGU()(s: Exp[Any], depList: List[Module]) extends MemoryInterfaceModule {
    override val sym = s
    override val deps = depList
  }

  /*
   * Read Unit (RU): Provides a read-only interface to memory. Input wire is used as an address
   * directly into memory
   */
  case class RU()(s: Exp[Any], depList: List[Module]) extends MemoryInterfaceModule {
    override val sym = s
    override val deps = depList
  }

//
//  /*
//   * Memory modules
//   */
//  case class BRAM()(s: Exp[Any], depList: List[Module]) extends MemoryModule {
//    override val sym = s
//    override val deps = depList
//  }
//  case class FIFO()(s: Exp[Any], depList: List[Module]) extends MemoryModule {
//    override val sym = s
//    override val deps = depList
//  }
//  case class FF()(s: Exp[Any], depList: List[Module]) extends MemoryModule {
//    override val sym = s
//    override val deps = depList
//  }

  class HwGraph {
    val nodes: ListBuffer[Module] = new ListBuffer[Module]()
    val symNodeMap: HashMap[Exp[Any], Module] = new HashMap[Exp[Any], Module]()
    var rootNode: Module = null

    def add(m: Module) = {
      if (Config.debugCodegen) {
        println(s"[HwGraph::add] Map before: $symNodeMap")
        println(s"[HwGraph::add] Nodes before: $nodes")
        println(s"[HwGraph::add] Adding module $m")
      }
      if (!nodes.exists(x => x.sym == m.sym)) {
        nodes.append(m)
        symNodeMap(m.sym) = m
        if (rootNode == null) {
          rootNode = m
        }
        if (Config.debugCodegen) {
          println(s"[HwGraph::add] Map before: $symNodeMap")
          println(s"[HwGraph::add] Nodes before: $nodes")
          println(s"[HwGraph::add] Adding module $m")
        }

      }
      else {
        if (Config.debugCodegen) {
          println(s"Node $m exists, nodes and symNodeMap unchanged")
        }
      }

    }

    def getModules(sl: List[Exp[Any]]): List[Module] = {
      if (Config.debugCodegen) {
        println(s"[HwGraph::getModules] sl  = $sl")
        println(s"[HwGraph::getModules] map = $symNodeMap")
      }
      val lb: ListBuffer[Module] = new ListBuffer[Module]()
      for (s <- sl.distinct) {
        if (symNodeMap.contains(s)) {
          lb.append(symNodeMap(s))
        }
      }
      lb.toList
    }

  //  def bfsWalker() = {
  //
  //  }
  //
  //  def dfsWalker() = {
  //  }

  }



}
