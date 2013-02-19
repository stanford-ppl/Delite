package ppl.delite.framework.datastructures

import java.io.{File,FileWriter,PrintWriter}
import scala.virtualization.lms.common._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.extern.lib.ProtoBuf
import scala.reflect.SourceContext

trait DeliteStructsExp extends StructExp { this: DeliteOpsExp =>
	
  abstract class DeliteStruct[T:Manifest] extends AbstractStruct[T] with DeliteOp[T] {
    type OpType <: DeliteStruct[T]
    val tag = classTag[T]

    def copyTransformedElems(e: => Seq[(String, Rep[Any])]): Seq[(String, Rep[Any])] = 
      original.map(p=>(p._2.asInstanceOf[OpType]).elems.map(e=>(e._1,p._1(e._2)))).getOrElse(e)
  }

  //the following is a HACK to make struct inputs appear in delite op kernel input lists while keeping them out of the read effects list
  //the proper solution is to simply override readSyms as done in trait StructExp, but see def freeInScope in BlockTraversal.scala
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case s: AbstractStruct[_] => s.elems.flatMap(e => readSyms(e._2)).toList
    case _ => super.readSyms(e)
  }

  override def reflectEffect[A:Manifest](d: Def[A], u: Summary)(implicit pos: SourceContext): Exp[A] =  d match {
    case s: AbstractStruct[_] => reflectEffectInternal(d, u)
    case _ => super.reflectEffect(d,u)
  }

  case class NestedFieldUpdate[T:Manifest](struct: Exp[Any], fields: List[String], rhs: Exp[T]) extends Def[Unit]

  override def field_update[T:Manifest](struct: Exp[Any], index: String, rhs: Exp[T]) = recurseFields(struct, List(index), rhs)

  private def recurseFields[T:Manifest](struct: Exp[Any], fields: List[String], rhs: Exp[T]): Exp[Unit] = struct match {
    case Def(Reflect(Field(s,name),_,_)) => recurseFields(s, name :: fields, rhs)
    case _ => reflectWrite(struct)(NestedFieldUpdate(struct, fields, rhs))
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case Reflect(NestedFieldUpdate(struct, fields, rhs), u, es) => reflectMirrored(Reflect(NestedFieldUpdate(f(struct), fields, f(rhs)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}


trait ScalaGenDeliteStruct extends BaseGenStruct {
  val IR: DeliteStructsExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    //TODO: generalize primitive struct packing
    case Struct(tag, elems) =>
      registerStruct(structName(sym.tp), elems)
      emitValDef(sym, "new " + structName(sym.tp) + "(" + elems.map{ e => 
        if (isVarType(e._2.tp) && deliteInputs.contains(e._2)) quote(e._2) + ".get"
        else quote(e._2)
      }.mkString(",") + ")")
      printlog("WARNING: emitting " + structName(sym.tp) + " struct " + quote(sym))    
    case FieldApply(struct, index) =>
      emitValDef(sym, quote(struct) + "." + index)
      printlog("WARNING: emitting field access: " + quote(struct) + "." + index)
    case FieldUpdate(struct, index, rhs) =>
      emitValDef(sym, quote(struct) + "." + index + " = " + quote(rhs))
      printlog("WARNING: emitting field update: " + quote(struct) + "." + index)
    case NestedFieldUpdate(struct, fields, rhs) =>
      emitValDef(sym, quote(struct) + "." + fields.reduceLeft(_ + "." + _) + " = " + quote(rhs))
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]) = m match {
    case s if s <:< manifest[Record] => structName(m)
    case _ => super.remap(m)
  }

  private def isVarType[T](m: Manifest[T]) = m.erasure.getSimpleName == "Variable"
  private def isArrayType[T](m: Manifest[T]) = m.erasure.getSimpleName == "DeliteArray"
  private def isStringType[T](m: Manifest[T]) = m.erasure.getSimpleName == "String"
  private def baseType[T](m: Manifest[T]) = if (isVarType(m)) mtype(m.typeArguments(0)) else m
  
  override def emitDataStructures(path: String) {
    val stream = new PrintWriter(path + "DeliteStructs.scala")
    stream.println("package generated.scala")
    for ((name, elems) <- encounteredStructs) {
      stream.println("")
      emitStructDeclaration(name, elems)(stream)
    }
    stream.close()
    super.emitDataStructures(path)

    val protoFile = new File(path + "structs.proto")
    val stream2 = new PrintWriter(protoFile)
    stream2.println("package generated.scala;")
    stream2.println("import \"messages.proto\";")
    for ((name, elems) <- encounteredStructs) {
      stream2.println("")
      emitMessageDeclaration(name, elems)(stream2)
    }
    stream2.close()
    ProtoBuf.compile(protoFile.getAbsolutePath, path)
    super.emitDataStructures(path)
  }

  def emitStructDeclaration(name: String, elems: Seq[(String,Manifest[_])])(stream: PrintWriter) {
    stream.println("object " + name + " {")
    stream.println("def parseFrom(bytes: com.google.protobuf.ByteString) = {")
    stream.println("val mssg = Structs." + name + ".parseFrom(bytes)")
    emitStructDeserialization(name, elems, "mssg")(stream)
    stream.println("\n}")
    stream.print("def combine(lhs: " + name + ", " + "rhs: " + name + ") = ")
    emitStructReduction(name, elems, "lhs", "rhs")(stream)
    stream.println("\n}")

    stream.print("case class " + name + "(")
    stream.print(elems.map{ case (idx,tp) => "var " + idx + ": " + remap(tp) }.mkString(", "))
    stream.println(") {")
    stream.print("def toByteString = ")
    emitStructSerialization(name, elems, "")(stream)
    stream.println(".build.toByteString")
    stream.println("}")
  }

  private def mangle(name: String) = name.split("_").map(toCamelCase).mkString("")
  private def toCamelCase(name: String) = if (name.length > 0) name.substring(0,1).toUpperCase + (if (name.length > 1) name.substring(1) else "") else ""

  def emitStructDeserialization(name: String, elems: Seq[(String,Manifest[_])], prefix: String)(stream: PrintWriter) {
    def insertField(field: String, tp: Manifest[_]) = {
      val name = structName(tp)
      val getField = prefix + ".get" + mangle(field)
      if (encounteredStructs.contains(name)) emitStructDeserialization(name, encounteredStructs(name), getField)(stream) 
      else if (deVar(tp).erasure.getSimpleName == "DeliteArray") stream.print("ppl.delite.runtime.messages.Serialization.deserializeDeliteArray" + arrayRemap(deVar(tp).typeArguments(0)) + "(" + getField + ")")
      else stream.print(getField)
    }

    stream.print("new " + name + "(")
    var first = true
    for ((field,tp) <- elems) {
      if (!first) stream.print(",")
      first = false
      insertField(field,tp)
    }
    stream.print(")")
  }

  def emitStructSerialization(name: String, elems: Seq[(String,Manifest[_])], prefix: String)(stream: PrintWriter) {
    def insertField(field: String, tp: Manifest[_]) = {
      val name = structName(tp)
      val fullField = prefix + field
      if (encounteredStructs.contains(name)) emitStructSerialization(name, encounteredStructs(name), fullField+".")(stream)
      else if (deVar(tp).erasure.getSimpleName == "DeliteArray") stream.print("ppl.delite.runtime.messages.Serialization.serializeDeliteArray(" + fullField + ")")
      else stream.print(fullField)
    }

    stream.print("Structs." + name + ".newBuilder")
    for ((field,tp) <- elems) {
      stream.print(".set" + mangle(field) + "(")
      insertField(field,tp)
      stream.print(")")
    }
  }

  def emitStructReduction(name: String, elems: Seq[(String,Manifest[_])], prefixL: String, prefixR: String)(stream: PrintWriter) {
    def insertField(field: String, tp: Manifest[_]) = {
      val name = structName(tp)
      val lhs = prefixL + "." + field
      val rhs = prefixR + "." + field
      if (encounteredStructs.contains(name)) emitStructReduction(name, encounteredStructs(name), lhs, rhs)(stream)
      else if (deVar(tp).erasure.getSimpleName == "DeliteArray") stream.print("ppl.delite.runtime.data.DeliteArray" + arrayRemap(deVar(tp).typeArguments(0)) + ".combine(" + lhs + "," + rhs + ")")
      else if (deVar(tp).erasure.getSimpleName == "int") stream.print(lhs + " + " + rhs) //TODO: need something extensible / customizable
      else if (deVar(tp).erasure.getSimpleName == "boolean") stream.print(lhs)
      else throw new RuntimeException("don't know how to combine type " + deVar(tp))
    }

    stream.print("new " + name + "(")
    var first = true
    for ((field,tp) <- elems) {
      if (!first) stream.print(",")
      first = false
      insertField(field,tp)
    }
    stream.print(")")
  }

  def emitMessageDeclaration(name: String, elems: Seq[(String,Manifest[_])])(stream: PrintWriter) {
    stream.println("message " + name + " {")
    var idx = 1
    for ((field,tp) <- elems) {
      stream.println("required " + protoRemap(tp) + " " + field + " = " + idx + ";")
      idx += 1
    }
    stream.println("}")
  }

  private def deVar(tp: Manifest[_]) = if (tp.erasure == classOf[Variable[_]]) mtype(tp.typeArguments(0)) else mtype(tp)

  private def arrayRemap(tp: Manifest[_]): String = {
    val default = remap(tp) 
    default match {
      case "Int" | "Long" | "Double" | "Float" | "Char" | "Short" | "Byte" | "Boolean" => default
      case _ => "Object" //[" + default + "]"
    }
  }
 
  private def protoRemap(tp: Manifest[_]): String = deVar(tp).erasure.getSimpleName match {
    case "int" => "sint32"
    case "long" => "sint64"
    case "float" => "float"
    case "double" => "double"
    case "char" => "uint32"
    case "short" => "sint32"
    case "boolean" => "bool"
    case "byte" => "uint32"
    case "String" => "string"
    case _ if encounteredStructs.contains(structName(tp)) => structName(tp)
    case "DeliteArray" => "ppl.delite.runtime.messages.ArrayMessage"
    case other => throw new RuntimeException("don't know how to remap type " + other + " to Protocol Buffers")
  }

}
