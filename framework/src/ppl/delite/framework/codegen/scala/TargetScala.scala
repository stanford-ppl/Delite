package ppl.delite.framework.codegen.scala

import java.io.PrintWriter
import collection.mutable.ListBuffer
import ppl.delite.framework.codegen.{CodeGenerator, Target}


abstract class TargetScala extends Target {

  import intermediate._

  val name = "Scala"

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter, generators:ListBuffer[CodeGenerator{val intermediate: TargetScala.this.intermediate.type}])(implicit mA: Manifest[A], mB: Manifest[B] ): Unit = {

    val x = intermediate.fresh
    //todo note that I am reifying effects, I don't think we still need a pure version of emitSource
    val y =  reifyEffects(f(x))

    val sA = mA.toString
    val sB = mB.toString

    stream.println("/*****************************************\n"+
                   "  Emitting Scala Generated Code                  \n"+
                   "*******************************************/")
    stream.println("class "+className+" extends (("+sA+")=>("+sB+")) {")
    stream.println("def apply("+ quote(x)+":"+sA+"): "+sB+" = {")

    emitBlock(y,generators)(stream)
    stream.println(quote(getBlockResult(y)))

    stream.println("}")
    stream.println("}")
    stream.println("/*****************************************\n"+
                   "  End of Scala Generated Code                  \n"+
                   "*******************************************/")

    stream.flush
  }

    
  def emitAssignment(lhs: String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(lhs + " = " + rhs)
  }

  override def quote(x: Exp[_]) = x match { // TODO: quirk!
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
}