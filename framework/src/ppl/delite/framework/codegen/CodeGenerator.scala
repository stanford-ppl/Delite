package ppl.delite.framework.codegen

import _root_.scala.virtualization.lms.common.BaseExp
import _root_.scala.virtualization.lms.internal.{Effects, GenericNestedCodegen}
import _root_.scala.virtualization.lms.util.GraphUtil
import java.io.PrintWriter
import ppl.delite.framework.DeliteApplication

trait CodeGenerator {
  val intermediate: DeliteApplication
  import intermediate._

  // merged codegen and nested codegen
  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter):Unit = rhs match {
    case Reflect(s, effects) =>  emitNode(sym, s)
    case Reify(s, effects) =>
      // just ignore -- effects are accounted for in emitBlock
    case _ => throw new Exception("don't know how to generate code for: " + rhs)
  }

  // default syms that each generator can override if it needs to
  def syms2(e: Any, shallow: Boolean) : Option[List[Sym[Any]]] = None

  //Codegen piece
  //def emitValDef[A](sym: Sym[_], rhs: String)(implicit stream: PrintWriter, mA: Manifest[A]): Unit
  //def emitVarDef[A](sym: Sym[_], rhs: String)(implicit stream: PrintWriter, mA: Manifest[A]): Unit

  def quote(x: Exp[_]) : String = x match {
    case Const(s: String) => "\""+s+"\""
    case Const(z) => z.toString
    case Sym(n) => "x"+n
    case External(s: String, args: List[Exp[Any]]) => s.format(args map (quote(_)) : _*)
    case null => "null"
    case _ => throw new RuntimeException("could not quote " + x)
  }

  //Nested Codegen piece
  // !! there are two instances of emitBlock now (one for CodeGenerator, one for Target), which violates its original design
  // we need to privatize or share shallow and scope
  //var shallow = false

  //var scope: List[TP[_]] = Nil

  def emitBlock(start: Exp[_], target: Target{val intermediate: CodeGenerator.this.intermediate.type})(implicit stream: PrintWriter): Unit = {
    // try to push stuff as far down into more control-dependent parts as
    // possible. this is the right thing to do for conditionals, but
    // for loops we'll need to do it the other way round (hoist stuff out).
    // for lambda expressions there is no clear general strategy.

    val e1 = buildScheduleForResult(start, target, false) // deep list of deps
    //shallow = true
    val e2 = buildScheduleForResult(start, target, true) // shallow list of deps (exclude stuff only needed by nested blocks)
    //shallow = false

    //println("==== deep")
    //e1.foreach(println)
    //println("==== shallow")
    //e2.foreach(println)

    val e3 = e1.filter(e2 contains _) // shallow, but with the ordering of deep!!

    val e4 = e3.filterNot(scope contains _) // remove stuff already emitted

    val save = scope
    scope = e4 ::: scope

    for (TP(sym, rhs) <- e4) {
      target.emitTargetNode(sym, rhs)
    }

    start match {
      case Def(Reify(x, effects0)) =>
        // with the current implementation the code below is not
        // really necessary. all effects should have been emitted
        // because of the Reflect dependencies. it's still a good
        // sanity check though

        val effects = effects0.map { case s: Sym[a] => findDefinition(s).get }
        val actual = e4.filter(effects contains _)

        // actual must be a prefix of effects!
        assert(effects.take(actual.length) == actual,
            "violated ordering of effects: expected \n    "+effects+"\nbut got\n    " + actual)

        val e5 = effects.drop(actual.length)

        for (TP(_, rhs) <- e5) {
          target.emitTargetNode(Sym(-1), rhs)
        }
      case _ =>
    }

    scope = save
  }

  // merged codegen and nested codegen
  def getBlockResult[A](s: Exp[A]): Exp[A] = s match {
    case Def(Reify(x, _)) => x
    case _ => s
  }

  // from expressions (syms)
  def findSyms(e: Any, target: Target{val intermediate: CodeGenerator.this.intermediate.type}, shallow: Boolean): List[Sym[Any]] = {
    target.getTargetSyms(e, shallow).getOrElse(
      e match {
        case s: Sym[Any] => List(s)
        case p: Product => p.productIterator.toList.flatMap(findSyms(_,target,shallow))
        case _ => Nil
      }
    )
  }

  def dep2(e: Exp[Any], target: Target{val intermediate: CodeGenerator.this.intermediate.type}, shallow: Boolean): List[Sym[Any]] = e match {
    case Def(d: Product) => findSyms(d, target, shallow)
    case _ => Nil
  }


  // from scheduling
  def buildScheduleForResult(start: Exp[_], target: Target{val intermediate: CodeGenerator.this.intermediate.type}, shallow: Boolean): List[TP[_]] = {
    val st = findSyms(start, target, shallow)
    GraphUtil.stronglyConnectedComponents[TP[_]](st.flatMap(e => findDefinition(e).toList), { d =>
      //println("dep"+d +"="+dep(d.rhs))
      //todo had to add toAtom
      dep2(toAtom(d.rhs), target, shallow).flatMap { e =>
        //println(d + "->" + e)
        findDefinition(e).toList
      }
    }).flatten.reverse // inefficient!
  }
}