package ppl.delite.framework.codegen.scala

import ppl.delite.framework.codegen.CodeGenerator
import java.io.PrintWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen, GenericCodegen, Expressions}

object CodeGeneratorScala extends CodeGenerator {

  def emitSource[A,B,Exp[T]](app: GenericNestedCodegen, f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {

    def quote[A](x: Exp[A]) = app.quote(x.asInstanceOf[app.Exp[A]]).asInstanceOf[Exp[A]]
    def emitBlock[A](x: Exp[A])(steam: PrintWriter) = app.emitBlock(x.asInstanceOf[app.Exp[A]])(stream).asInstanceOf[Exp[A]]
    def getBlockResult[A](x: Exp[A]) = app.getBlockResult(x.asInstanceOf[app.Exp[A]]).asInstanceOf[Exp[A]]
    def reifyEffects[A](block: => Exp[A]) = app.reifyEffects(block).asInstanceOf[Exp[A]]


    val x:Exp[A] = app.fresh[A].asInstanceOf[Exp[A]]
    //todo note that I am reifying effects, I don't think we still need a pure version of emitSource
    val y =  reifyEffects(f(x))    

    val sA = mA.toString
    val sB = mB.toString

    stream.println("/*****************************************\n"+
                   "  Emitting Generated Code                  \n"+
                   "*******************************************/")
    stream.println("class "+className+" extends (("+sA+")=>("+sB+")) {")
    stream.println("def apply("+ quote(x)+":"+sA+"): "+sB+" = {")

    emitBlock(y)(stream)
    stream.println(quote(getBlockResult(y)))

    stream.println("}")
    stream.println("}")
    stream.println("/*****************************************\n"+
                   "  End of Generated Code                  \n"+
                   "*******************************************/")

    stream.flush


  }
  
}