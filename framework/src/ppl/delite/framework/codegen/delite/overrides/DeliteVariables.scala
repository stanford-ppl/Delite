package ppl.delite.framework.codegen.delite.overrides

import java.io.PrintWriter
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures.CLikeGenDeliteStruct
import scala.virtualization.lms.internal.{CLikeCodegen, GenerationFailedException}
import scala.virtualization.lms.common._

trait DeliteScalaGenVariables extends ScalaGenEffect {
  val IR: VariablesExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    val symIsResult = !deliteResult.isEmpty && (deliteResult.get contains sym)
    var gen = false
    
    if (symIsResult) {
      rhs match {
        case NewVar(init) => emitValDef(sym, "new generated.scala.Ref(" + quote(init) + ")"); gen = true
        case _ => // pass
      }
    }

    rhs match {
      case ReadVar(Variable(a)) if deliteInputs.contains(a) => emitValDef(sym, quote(a) + ".get"); gen = true
      case Assign(Variable(a), b) if deliteInputs.contains(a) => emitValDef(sym, quote(a) + ".set(" + quote(b) + ")"); gen = true
      case VarPlusEquals(Variable(a), b) if deliteInputs.contains(a) => emitValDef(sym, quote(a) + ".set(" + quote(a) + ".get + " + quote(b) + ")"); gen = true
      case VarMinusEquals(Variable(a), b) if deliteInputs.contains(a) => emitValDef(sym, quote(a) + ".set(" + quote(a) + ".get - " + quote(b) + ")"); gen = true
      case _ => // pass
    }

    if (!gen) {
      super.emitNode(sym, rhs)
    }
  }
}

trait DeliteCLikeGenVariables extends CLikeGenEffect {
  val IR: VariablesExp with DeliteOpsExp
  import IR._
 
  // Remove this when GPU targets also use reference types
  protected def reference: String = "->"

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    val symIsResult = !deliteResult.isEmpty && (deliteResult.get contains sym)
    var gen = false
    if (symIsResult) {
      rhs match {
        case NewVar(init) => stream.println("%sRef< %s%s > *%s = new %sRef< %s%s >(%s);\n".format(deviceTarget,remap(sym.tp),addRef(init.tp),quote(sym),deviceTarget,remap(sym.tp),addRef(init.tp),quote(init))); gen = true
        case _ => // pass
      }
    }

    if (!(deliteInputs intersect syms(rhs)).isEmpty) {
      rhs match {
        case ReadVar(Variable(a)) => emitValDef(sym, quote(a) + reference + "get()"); gen = true
        case Assign(Variable(a), b) => stream.println(quote(a) + reference + "set(" + quote(b) + ");"); gen = true
        case VarPlusEquals(Variable(a), b) => stream.println(quote(a) + reference + "set(" + quote(a) + reference + "get() + " + quote(b) + ");"); gen = true
        case VarMinusEquals(Variable(a), b) => stream.println(quote(a) + reference + "set(" + quote(a) + reference + "get() - " + quote(b) + ");"); gen = true
        case _ => // pass
      }
    }

    if (!gen) {
      super.emitNode(sym, rhs)
    }
  }

  override def getDataStructureHeaders(): String = {
    val out = new StringBuilder
    out.append("#include \"" + deviceTarget + "Ref.h\"\n")
    if(isAcceleratorTarget) out.append("#include \"Host" + deviceTarget + "Ref.h\"\n")
    super.getDataStructureHeaders() + out.toString
  }

}

trait DeliteCudaGenVariables extends CudaGenEffect with DeliteCLikeGenVariables {
  override protected def reference: String = "."
}
trait DeliteOpenCLGenVariables extends OpenCLGenEffect with DeliteCLikeGenVariables {
  override protected def reference: String = "."
}

trait DeliteCGenVariables extends CGenEffect with DeliteCLikeGenVariables with CLikeGenDeliteStruct {
  val IR: VariablesExp with DeliteOpsExp
  import IR._
  
  /*
  override def emitDataStructures(path: String) {
    super.emitDataStructures(path)
    val stream = new PrintWriter(path + deviceTarget + "RefRelease.h")
    for(tp <- dsTypesList.map(baseType) if(!isPrimitiveType(tp) && !isVoidType(tp))) {
      try {
        stream.println("template void " + deviceTarget + "Ref< " + remap(tp) + "* >::release(void);\n")
      }
      catch {
        case e: GenerationFailedException => //
        case e: Exception => throw(e)
      }
    }
    stream.close()
  }
  */
}