package ppl.delite.framework.codegen.c

import java.io.PrintWriter
import collection.mutable.ListBuffer
import ppl.delite.framework.codegen.{CodeGenerator, Target}

abstract class TargetC extends Target {
  import intermediate._

  val name = "C"

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter, generators: ListBuffer[CodeGenerator{val intermediate: TargetC.this.intermediate.type}])(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {

    def println(s:String) = stream.println(s)
    val x = intermediate.fresh
    //todo note that I am reifying effects, I don't think we still need a pure version of emitSource
    val y =  reifyEffects(f(x))

    val sA = mA.toString
    val sB = mB.toString

    println("/*****************************************\n"+
            "  Emitting C Generated Code                  \n"+
            "*******************************************/\n" +
            "#include <stdio.h>\n" +
            "#include <stdlib.h>"

    )
    //println("class "+className+" extends (("+sA+")=>("+sB+")) {")
    println("int main(int argc, char** argv) {")

    emitBlock(y, generators)(stream)
    //println(quote(getBlockResult(y)))

    //println("}")
    println("}")
    println("/*****************************************\n"+
            "  End of C Generated Code                  \n"+
            "*******************************************/")

    stream.flush


  }


}