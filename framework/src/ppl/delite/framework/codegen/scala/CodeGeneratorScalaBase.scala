package ppl.delite.framework.codegen.scala

import ppl.delite.framework.codegen.CodeGenerator
import java.io.PrintWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen, GenericCodegen, Expressions}
import scala.virtualization.lms.common.BaseExp
import scala.virtualization.lms.ppl.ScalaOpsPkgExp
import scala.virtualization.lms.util.GraphUtil

trait CodeGeneratorScalaBase extends CodeGenerator {

  //todo should be ScalaExp
  //val intermediate: BaseExp
  import intermediate._


  val name ="Scala Code Generator"

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {

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

    emitBlock(y)(stream)
    stream.println(quote(getBlockResult(y)))

    stream.println("}")
    stream.println("}")
    stream.println("/*****************************************\n"+
                   "  End of Scala Generated Code                  \n"+
                   "*******************************************/")

    stream.flush
  }

  def emitValDef(tp: String="", sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs)
  }
  def emitVarDef(tp: String="", sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
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