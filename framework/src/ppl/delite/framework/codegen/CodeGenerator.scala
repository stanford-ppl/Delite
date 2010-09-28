package ppl.delite.framework.codegen

import _root_.scala.virtualization.lms.common.BaseExp
import _root_.scala.virtualization.lms.internal.{Effects, GenericNestedCodegen}
import _root_.scala.virtualization.lms.util.GraphUtil
import java.io.PrintWriter

trait CodeGenerator {

  val intermediate: BaseExp with Effects
  import intermediate._

  val name: String

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit


  //Codegen piece
  def emitValDef(tp: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit
  def emitVarDef(tp: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit

  def quote(x: Exp[_]) = x match {
    case Const(s: String) => "\""+s+"\""
    case Const(z) => z.toString
    case Sym(n) => "x"+n
  }

  //Nested Codegen piece
  var shallow = false

  var scope: List[TP[_]] = Nil
  
  def emitBlock(start: Exp[_])(implicit stream: PrintWriter): Unit = {
    // try to push stuff as far down into more control-dependent parts as
    // possible. this is the right thing to do for conditionals, but
    // for loops we'll need to do it the other way round (hoist stuff out).
    // for lambda expressions there is no clear general strategy.

    val e1 = buildScheduleForResult(start) // deep list of deps
    shallow = true
    val e2 = buildScheduleForResult(start) // shallow list of deps (exclude stuff only needed by nested blocks)
    shallow = false

    //println("==== deep")
    //e1.foreach(println)
    //println("==== shallow")
    //e2.foreach(println)

    val e3 = e1.filter(e2 contains _) // shallow, but with the ordering of deep!!

    val e4 = e3.filterNot(scope contains _) // remove stuff already emitted

    val save = scope
    scope = e4 ::: scope

    for (TP(sym, rhs) <- e4) {
      emitNode(sym, rhs)
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
          emitNode(Sym(-1), rhs)
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

  // merged codegen and nested codegen
  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter):Unit = rhs match {
    case Reflect(s, effects) =>  emitNode(sym, s)
    case Reify(s, effects) =>
      // just ignore -- effects are accounted for in emitBlock
    case _ => throw new Exception("don't know how to generate code for: " + rhs)   
  }


   //from scheduling
  def buildScheduleForResult(start: Exp[_]): List[TP[_]] = {
    val st = syms(start)
    GraphUtil.stronglyConnectedComponents[TP[_]](st.flatMap(e => findDefinition(e).toList), { d =>
      //println("dep"+d +"="+dep(d.rhs))
      //todo had to add toAtom
      dep(toAtom(d.rhs)).flatMap { e =>
        //println(d + "->" + e)
        findDefinition(e).toList
      }
    }).flatten.reverse // inefficient!
  }
}