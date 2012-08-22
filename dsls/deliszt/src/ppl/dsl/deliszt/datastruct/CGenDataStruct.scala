package ppl.dsl.deliszt.datastruct

import _root_.scala.virtualization.lms.internal.{Expressions, CCodegen}
import _root_.scala.virtualization.lms.internal.GenerationFailedException

/* This trait defines methods for copying datastructures between JVM and GPU */

trait CGenDataStruct extends CCodegen {

  val IR: Expressions
  import IR._

  def refCopyInputHtoD(sym: Sym[Any]): String = { 
    val out = new StringBuilder
    val typeStr = remap(sym.tp.typeArguments(0))

    out.append("\tjclass cls = env->GetObjectClass(obj);\n")
    out.append("\t%s %s;\n".format(typeStr,quote(sym),remap(sym.tp)))
    out.append("\tjmethodID mid_get = env->GetMethodID(cls,\"get$mc%s$sp\",\"()%s\");\n".format(JNITypeDescriptor(sym.tp.typeArguments(0)),JNITypeDescriptor(sym.tp.typeArguments(0))))
    typeStr match {
      case "int" => out.append("\t%s = env->CallIntMethod(obj,mid_get);\n".format(quote(sym)))
      case "long" => out.append("\t%s = env->CallLongMethod(obj,mid_get);\n".format(quote(sym)))
      case "float" => out.append("\t%s = env->CallFloatMethod(obj,mid_get);\n".format(quote(sym)))
      case "bool" => out.append("\t%s = env->CallBooleanMethod(obj,mid_get);\n".format(quote(sym)))
      case "double" => out.append("\t%s = env->CallDoubleMethod(obj,mid_get);\n".format(quote(sym)))
      case _ => throw new GenerationFailedException("CGen: Cannot call JNI method for this type.")
    }
    out.append("\tenv->DeleteLocalRef(cls);\n")
    out.append("\treturn %s;\n".format(quote(sym)))
    out.toString
  }

  def refCopyOutputDtoH(sym: Sym[Any]): String = { 
    //TODO: Implement this!
    "assert(false);\n return NULL;\n"

  }
  def refCopyMutableInputDtoH(sym: Sym[Any]): String = { 
    //TODO: Implement this!
    "assert(false);\n"
  }
}

