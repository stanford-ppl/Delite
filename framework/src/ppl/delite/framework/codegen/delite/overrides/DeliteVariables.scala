package ppl.delite.framework.codegen.delite.overrides

import java.io.PrintWriter
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures.CLikeGenDeliteStruct
import scala.virtualization.lms.internal.{CLikeCodegen, GenerationFailedException}
import scala.virtualization.lms.common._
import scala.collection.mutable.HashSet

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

trait DeliteCLikeGenVariables extends CLikeGenEffect with CLikeGenDeliteStruct {
  val IR: VariablesExp with DeliteOpsExp
  import IR._

  protected val deliteVariableString: String
  protected def typeString(m: Manifest[_]): String
  private val generatedDeliteVariable = HashSet[String]()

  private def shouldGenerate(m: Manifest[_]): Boolean = m match {
    case _ if (isPrimitiveType(m)) => true
    case _ if (isArrayType(m)) => true
    case _ if (encounteredStructs.contains(structName(baseType(m)))) => true
    case _ => false
  }

  override def emitDataStructures(path: String) {
    super.emitDataStructures(path)
    val stream = new PrintWriter(path + deviceTarget + "DeliteVariables.h")
    stream.println("#include \"" + deviceTarget + "DeliteStructs.h\"")
    stream.println("#include \"" + deviceTarget + "DeliteArrays.h\"")
    stream.println("#include \"" + deviceTarget + "HashMap.h\"")
    for((tp,name) <- dsTypesList if shouldGenerate(tp)) {
      emitDeliteVariable(tp, path, stream)
    }
    stream.close()
  }

  private def emitDeliteVariable(m: Manifest[_], path: String, header: PrintWriter) {
    try {
      val mString = typeString(m)
      if(!generatedDeliteVariable.contains(mString)) {
        val stream = new PrintWriter(path + mString + ".h")
        stream.println("#ifndef __" + mString + "__")
        stream.println("#define __" + mString + "__")
        stream.println(deliteVariableString.replaceAll("__T__",mString).replaceAll("__TARG__",remap(m)+addRef(baseType(m))))
        stream.println("#endif")
        stream.close()
        header.println("#include \"" + mString + ".h\"")
        generatedDeliteVariable.add(mString)
      }
    }
    catch {
      case e: GenerationFailedException => //
      case e: Exception => throw(e)
    }
  }

  override def getDataStructureHeaders(): String = {
    val out = new StringBuilder
    out.append("#include \"" + deviceTarget + "DeliteVariables.h\"\n")
    if (isAcceleratorTarget) out.append("#include \"" + hostTarget + "DeliteVariables.h\"\n")
    super.getDataStructureHeaders() + out.toString
  }

}

trait DeliteCudaGenVariables extends CudaGenEffect with DeliteCLikeGenVariables {
  val IR: VariablesExp with DeliteOpsExp
  import IR._

  protected def typeString(m: Manifest[_]) = deviceTarget + "Ref" + remap(m)
  protected val deliteVariableString: String = """
class __T__ {
public:
  __TARG__ data;

  __host__ __device__ __T__(void) {
      data = NULL;
  }

  __host__ __device__ __T__(__TARG__ _data) {
    data = _data;
  }

  __host__ __device__ __TARG__ get(void) {
    return data;
  }

  __host__ __device__ void set(__TARG__ newVal) {
      data = newVal;
  }
};
"""

  def reference: String = "."

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    var gen = false
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
}

trait DeliteOpenCLGenVariables extends OpenCLGenEffect with DeliteCLikeGenVariables {
  val IR: VariablesExp with DeliteOpsExp
  import IR._

  def reference: String = "."

  protected def typeString(m: Manifest[_]) = deviceTarget + "Ref" + remap(m)

  protected val deliteVariableString: String = """
//TODO: fill in
"""
}

trait DeliteCGenVariables extends CGenEffect with DeliteCLikeGenVariables {
  val IR: VariablesExp with DeliteOpsExp
  import IR._

  def reference: String = "->"

  protected def typeString(m: Manifest[_]) = {
    if (cppMemMgr == "refcnt") deviceTarget + "Ref" + unwrapSharedPtr(remap(m))
    else deviceTarget + "Ref" + remap(m)
  }

  protected val deliteVariableString: String = """
#include "DeliteNamespaces.h"
class __T__ {
public:
  __TARG__ data;

  __T__(__TARG__ _data) {
    data = _data;
  }

  __TARG__ get(void) {
    return data;
  }

  void set(__TARG__ newVal) {
      data = newVal;
  }
};

struct __T__D {
  void operator()(__T__ *p) {
    //printf("__T__: deleting %p\n",p);
  }
};
"""

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    val symIsResult = !deliteResult.isEmpty && (deliteResult.get contains sym)
    var gen = false
    if (symIsResult) {
      rhs match {
        case NewVar(init) =>
          if(cppMemMgr == "refcnt") {
            val tpeString = deviceTarget+"Ref"+unwrapSharedPtr(remap(sym.tp))
            stream.println("%s %s(new %s(%s),%sD());".format(wrapSharedPtr(tpeString),quote(sym),tpeString,quote(init),tpeString))
          }
          else {
            val tpeString = deviceTarget+"Ref"+remap(sym.tp)
            stream.println("%s *%s = new %s(%s);".format(tpeString,quote(sym),tpeString,quote(init)))
          }
          gen = true
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
}
