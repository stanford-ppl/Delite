package ppl.delite.framework.codegen.delite

import generators.{DeliteGenScalaVariables, DeliteGenTaskGraph}
import java.io.PrintWriter
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._
import ppl.delite.framework.{Config, DeliteApplication}

/**
 * Notice that this is using Effects by default, also we are mixing in the Delite task graph code generator
 */
trait DeliteCodegen extends GenericNestedCodegen {
  val IR: Expressions with Effects
  import IR._

  // these are the target-specific kernel generators (e.g. scala, cuda, etc.)
  type Generator = GenericNestedCodegen{val IR: DeliteCodegen.this.IR.type}
  val generators : List[Generator]

  def ifGenAgree[A](f: Generator => A, shallow: Boolean): A = {
    val save = generators map { _.shallow }
    generators foreach { _.shallow = shallow }
    val result = generators map f
    if (result.distinct.length != 1){
      throw new RuntimeException("DeliteCodegen: generators disagree")
    }
    for (i <- 0 until generators.length) {
      generators(i).shallow = save(i)
    }
    result(0)
  }

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {

    val x = fresh[A]
    val y = reifyEffects(f(x))

    val sA = mA.toString
    val sB = mB.toString

    stream.println("{\"DEG\":{\n"+
                   "\"version\" : 0.1,\n"+
                   "\"kernelpath\" : \"" + Config.kernel_path  + "\"\n"+
                   "\"ops\": [")

    emitBlock(y)(stream)
    //stream.println(quote(getBlockResult(y)))
    stream.println("{\"type\":\"EOP\"}\n]}}")


    stream.flush
  }

  /**
   * DeliteCodegen expects there to be a single schedule across all generators, so a single task graph
   * can be generated. This implies that every generator object must compute internal dependencies (syms)
   * the same way.
   *
   * This is all because we allow individual generators to refine their dependencies, which directly impacts
   * the generated schedule. We may want to consider another organization.
   */
  override def emitBlock(start: Exp[_])(implicit stream: PrintWriter): Unit = {
    if (generators.length < 1) return

    // verify our single schedule assumption
    val e1 = ifGenAgree(_.buildScheduleForResult(start), false) // deep
    val e2 = ifGenAgree(_.buildScheduleForResult(start), true) // shallow

    //println("==== deep")
    //e1.foreach(println)
    //println("==== shallow")
    //e2.foreach(println)

    val e3 = e1.filter(e2 contains _) // shallow, but with the ordering of deep!!

    val e4 = e3.filterNot(scope contains _) // remove stuff already emitted

    val save = scope
    scope = e4 ::: scope
    generators foreach { _.scope = scope }
    
    for (TP(sym, rhs) <- e4) {
      emitNode(sym, rhs)
    }

    start match {
      case Def(Reify(x, effects0)) =>
        val effects = effects0.map { case s: Sym[a] => findDefinition(s).get }
        val actual = e4.filter(effects contains _)

        // actual must be a prefix of effects!
        assert(effects.take(actual.length) == actual,
            "violated ordering of effects: expected \n    "+effects+"\nbut got\n    " + actual)

        val e5 = effects.drop(actual.length)

        for (TP(_, rhs) <- e5) {
          emitNode(Sym(-1), rhs)
        }
      case _ =>
    }

    generators.foreach(_.scope = save)
    scope = save
  }



  def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs)
  }
  def emitVarDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs)
  }
  def emitAssignment(lhs: String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(lhs + " = " + rhs)
  }

  override def quote(x: Exp[_]) = x match { // TODO: quirk!
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }

}

trait DeliteCodeGenPkg extends DeliteGenTaskGraph

trait DeliteCodeGenOverridesScala extends DeliteGenScalaVariables
