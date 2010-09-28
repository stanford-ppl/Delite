package ppl.delite.framework.codegen.c

import ppl.delite.framework.codegen.CodeGenerator
import scala.virtualization.lms.internal.GenericNestedCodegen
import scala.virtualization.lms.ppl.ScalaOpsPkgExp
import java.io.PrintWriter


trait CodeGeneratorCBase extends CodeGenerator {

  //val intermediate: GenericNestedCodegen with ScalaOpsPkgExp
  import intermediate._

  val name = "C Code Generator"

  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {

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

    emitBlock(y)(stream)
    //println(quote(getBlockResult(y)))

    //println("}")
    println("}")
    println("/*****************************************\n"+
            "  End of C Generated Code                  \n"+
            "*******************************************/")

    stream.flush


  }


  def emitValDef(tp: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.print("const ")
    emitVarDef(tp, sym, rhs)
  }
  def emitVarDef(tp: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(tp + " " + quote(sym) + " = " + rhs + ";")
  }

  
}
