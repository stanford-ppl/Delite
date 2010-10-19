package ppl.delite.framework.codegen.scala

import java.io.PrintWriter
import ppl.delite.framework.codegen.CodeGeneratorApplication


trait CodeGeneratorScalaApplication extends CodeGeneratorApplication with CodeGeneratorScalaBase {
  import intermediate._

  def emitSource[A,B](x: Exp[A], f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B] ): Unit = {

    //val x = intermediate.fresh
    //todo note that I am reifying effects, I don't think we still need a pure version of emitSource
    val y =  reifyEffects(f(x))

    val sA = mA.toString
    val sB = mB.toString

    stream.println("/*****************************************\n"+
                   "  Emitting Scala Generated Code                  \n"+
                   "*******************************************/")
    stream.println("class "+className+" extends (("+sA+")=>("+sB+")) {")
    stream.println("def apply("+ quote(x)+":"+sA+"): "+sB+" = {")

    emitBlock(y, intermediate.targets.get("Scala").get)(stream)
    stream.println(quote(getBlockResult(y)))

    stream.println("}")
    stream.println("}")
    stream.println("/*****************************************\n"+
                   "  End of Scala Generated Code                  \n"+
                   "*******************************************/")

    stream.flush
  }

}