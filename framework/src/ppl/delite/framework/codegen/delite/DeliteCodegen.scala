package ppl.delite.framework.codegen.delite

import generators.DeliteGenMiscOps
import java.io.PrintWriter
import scala.virtualization.lms.internal._

/**
 * Notice that this is using Effects by default
 */
trait DeliteCodegen extends GenericNestedCodegen {
  val IR: Expressions with Effects
  import IR._

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {

    val x = fresh[A]
    val y = reifyEffects(f(x))

    val sA = mA.toString
    val sB = mB.toString

    stream.println("#*****************************************\n"+
                   "#  Emitting Delite Execution Graph        \n"+
                   "#******************************************/")
  
    emitBlock(y)(stream)
    //stream.println(quote(getBlockResult(y)))


    stream.println("#*****************************************\n"+
                   "#  End of Delite Execution Graph                 \n"+
                   "#*******************************************/")

    stream.flush
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

trait DeliteCodeGenPkg extends DeliteGenMiscOps


