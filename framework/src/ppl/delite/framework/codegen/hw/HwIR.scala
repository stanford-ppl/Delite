package ppl.delite.framework.codegen.hw

import scala.virtualization.lms.internal._
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
    val sym: Sym[Any]
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
  case class Dummy()(s: Sym[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = 1
  }

  /*
   * Combinational logic block modules
   */
  case class CAdd()(s: Sym[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = 1
  }
  case class CSub()(s: Sym[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = 1
  }
  case class CMul()(s: Sym[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = 5
  }
  case class CDiv()(s: Sym[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = 12
  }
  case class CMux()(s: Sym[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = 1
  }
  // Combination of combinational logic block modules is also a
  // combinational logic block module - using the composite design pattern
  case class CComposite(modules: List[CombModule])(s: Sym[Any], depList: List[Module]) extends CombModule {
    override val sym = s
    override val deps = depList
    override def area = {
      modules.map(x => x.area).sum
    }
  }

  /*
   * Template modules
   */
  case class Pipeline(stages: List[Module])(s: Sym[Any], depList: List[Module]) extends TemplateModule {
    override val sym = s
    override val deps = depList
    override def area = {
      stages.map(x => x.area).sum
    }

  }
  case class Parallel(m: Module, n: Int)(s: Sym[Any], depList: List[Module]) extends TemplateModule {
    override val sym = s
    override val deps = depList
    override def area = {
     m.area * n
    }

  }
  case class TreeRed()(s: Sym[Any], depList: List[Module])  extends TemplateModule {
    override val sym = s
    override val deps = depList
  }
  case class FSM()(s: Sym[Any], depList: List[Module])      extends TemplateModule {
    override val sym = s
    override val deps = depList
  }

  /*
   * Address Generation Unit (AGU): Templatized FSM that automatically generates addresses
   * and other necessary control signals to interface with memory based upon the data structure's
   * access pattern
   */
  case class AGU()(s: Sym[Any], depList: List[Module]) extends MemoryInterfaceModule {
    override val sym = s
    override val deps = depList
  }

  /*
   * Read Unit (RU): Provides a read-only interface to memory. Input wire is used as an address
   * directly into memory
   */
  case class RU()(s: Sym[Any], depList: List[Module]) extends MemoryInterfaceModule {
    override val sym = s
    override val deps = depList
  }

//
//  /*
//   * Memory modules
//   */
//  case class BRAM()(s: Sym[Any], depList: List[Module]) extends MemoryModule {
//    override val sym = s
//    override val deps = depList
//  }
//  case class FIFO()(s: Sym[Any], depList: List[Module]) extends MemoryModule {
//    override val sym = s
//    override val deps = depList
//  }
//  case class FF()(s: Sym[Any], depList: List[Module]) extends MemoryModule {
//    override val sym = s
//    override val deps = depList
//  }
}
