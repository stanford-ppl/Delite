package ppl.delite.framework.datastructures

import java.io.{File,FileWriter,PrintWriter}
import scala.reflect.{RefinedManifest, SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{CudaCodegen,OpenCLCodegen,CCodegen,CLikeCodegen,GenerationFailedException}
import scala.virtualization.lms.internal.Targets._
import ppl.delite.framework.ops.{DeliteOpsExp,CudaGenDeliteOps}
import ppl.delite.framework.Config
import ppl.delite.framework.Util._
import ppl.delite.framework.extern.lib.ProtoBuf
import scala.reflect.SourceContext
import scala.collection.mutable.HashSet

trait DeliteStructsExp extends StructExp { this: DeliteOpsExp with PrimitiveOpsExp with OrderingOpsExp => // FIXME: mix in prim somewhere else
	
  abstract class DeliteStruct[T:Manifest] extends AbstractStruct[T] with DeliteOp[T] {
    type OpType <: DeliteStruct[T]
    val tag = classTag[T]

    def copyTransformedElems(e: => Seq[(String, Rep[Any])]): Seq[(String, Rep[Any])] = 
      original.map(p=>(p._2.asInstanceOf[OpType]).elems.map(e=>(e._1,p._1(e._2)))).getOrElse(e)

    override def equals(other: Any) = other match {
      case o: DeliteStruct[T] => this.tag == o.tag && this.elems == o.elems
      case _ => false
    }
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

  //no shortcutting on mutable structs ...

  // TODO: clean up and check everything's safe
  override def field[T:Manifest](struct: Exp[Any], index: String)(implicit pos: SourceContext): Exp[T] = struct match {
    // is this confined to unsafe immutable or should we look at any mutable struct def?
    /*
    case Def(rhs@Reflect(ObjectUnsafeImmutable(orig), u, es)) => 
      println("**** trying to shortcut field access: " + struct.toString + "=" + rhs + "." + index)

      for (e@Def(r) <- es) {
        println("      dep: " + e.toString + "=" + r)
      }

      // find last assignment ... FIXME: should look at *all* mutations of orig
      val writes = es collect {
        case Def(Reflect(NestedFieldUpdate(`orig`,List(`index`),rhs), _, _)) => rhs
      }
      writes.reverse match {
        case rhs::_ => 
          println("      picking write " + rhs.toString)
          rhs.asInstanceOf[Exp[T]] // take last one
        case Nil => 
          orig match {
            case Def(Reflect(SimpleStruct(tag, fields), _, _)) =>
              val Def(Reflect(NewVar(rhs), _,_)) = fields.find(_._1 == index).get._2
              println("      picking alloc " + rhs.toString) 
              rhs.asInstanceOf[Exp[T]] // take field
            case _ =>
              println("      giving up...")
              super.field(struct, index)
          }
      }
    */
    /*
    case Def(rhs@Reflect(SimpleStruct(tag, fields), _, _)) =>
      println("**** trying to shortcut field access: " + struct.toString + "=" + rhs + "." + index)

      // find last assignment ... FIXME: should look at *all* mutations of struct
      /*context foreach {
        case Def(Reflect(NestedFieldUpdate(`struct`,List(`index`),rhs), _, _)) =>  //ok
        case Def(e) => 
          println("      ignoring " + e)
      }*/
      val writes = context collect {
        case Def(Reflect(NestedFieldUpdate(`struct`,List(`index`),rhs), _, _)) => rhs
      }
      writes.reverse match {
        case rhs::_ => 
          println("      picking write " + rhs.toString)
          rhs.asInstanceOf[Exp[T]] // take last one
        case Nil =>
          val Def(Reflect(NewVar(rhs), _,_)) = fields.find(_._1 == index).get._2
          println("      picking alloc " + rhs.toString)
          rhs.asInstanceOf[Exp[T]] // take field
      }
      */
    case _ => super.field(struct, index)
  }


  private def recurseFields[T:Manifest](struct: Exp[Any], fields: List[String], rhs: Exp[T]): Exp[Unit] = struct match {
    case Def(Reflect(Field(s,name),_,_)) => recurseFields(s, name :: fields, rhs)
    case _ => reflectWrite(struct)(NestedFieldUpdate(struct, fields, rhs))
  }

  // TODO: get rid of entirely or just use mirrorDef
  def mirrorDD[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Def[A] = (e match {
    case IntTimes(a,b) => 
      printlog("warning: encountered effectful primitive def during mirror "+e)
      IntTimes(f(a),f(b))
    case IntPlus(a,b) => 
      printlog("warning: encountered effectful primitive def during mirror "+e)
      IntPlus(f(a),f(b))
    case IntMinus(a,b) => 
      printlog("warning: encountered effectful primitive def during mirror "+e)
      IntMinus(f(a),f(b))
    case IntMod(a,b) => 
      printlog("warning: encountered effectful primitive def during mirror "+e)
      IntMod(f(a),f(b))
    case IntDivide(a,b) => 
      printlog("warning: encountered effectful primitive def during mirror "+e)
      IntDivide(f(a),f(b)) //xx
    case e@OrderingLT(a,b) =>
      printlog("warning: encountered effectful primitive def during mirror "+e)
      OrderingLT(f(a),f(b))(null.asInstanceOf[Ordering[Any]],manifest[Any]) //HACK
    case e@Reflect(a,u,es) => Reflect(mirrorDD(a,f),mapOver(f,u),f(es))
    case _ => 
      printerr("FAIL: "+e)
      e
  }).asInstanceOf[Def[A]]

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case s: AbstractStruct[_] => Nil
    case NestedFieldUpdate(_,_,_) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case s: AbstractStruct[_] => Nil //ignore nested mutability for Structs: this is only safe because we rewrite mutations to atomic operations
    case NestedFieldUpdate(_,_,_) => Nil
    case _ => super.containSyms(e)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case Reflect(NestedFieldUpdate(struct, fields, rhs), u, es) => reflectMirrored(Reflect(NestedFieldUpdate(f(struct), fields, f(rhs)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(x@IntTimes(a,b), u, es) => reflectMirrored(mirrorDD(e,f).asInstanceOf[Reflect[A]])
    case Reflect(x@IntPlus(a,b), u, es) => reflectMirrored(mirrorDD(e,f).asInstanceOf[Reflect[A]])
    case Reflect(x@IntMinus(a,b), u, es) => reflectMirrored(mirrorDD(e,f).asInstanceOf[Reflect[A]])
    case Reflect(x@IntMod(a,b), u, es) => reflectMirrored(mirrorDD(e,f).asInstanceOf[Reflect[A]])
    case Reflect(x@IntDivide(a,b), u, es) => reflectMirrored(mirrorDD(e,f).asInstanceOf[Reflect[A]])
    case Reflect(x@OrderingLT(a,b), u, es) => reflectMirrored(mirrorDD(e,f).asInstanceOf[Reflect[A]])
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]


  object StructType { //TODO: we should have a unified way of handling this, e.g., TypeTag[T] instead of Manifest[T]
    def unapply[T:Manifest](e: Exp[DeliteArray[T]]) = unapplyStructType[T]
    def unapply[T:Manifest] = unapplyStructType[T]
  }

  def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = manifest[T] match {
    case r: RefinedManifest[T] => Some(AnonTag(r), r.fields)
    case t if t.erasure == classOf[Tuple2[_,_]] => Some((classTag(t), List("_1","_2") zip t.typeArguments))
    case t if t.erasure == classOf[Tuple3[_,_,_]] => Some((classTag(t), List("_1","_2","_3") zip t.typeArguments))
    case t if t.erasure == classOf[Tuple4[_,_,_,_]] => Some((classTag(t), List("_1","_2","_3","_4") zip t.typeArguments))
    case t if t.erasure == classOf[Tuple5[_,_,_,_,_]] => Some((classTag(t), List("_1","_2","_3","_4","_5") zip t.typeArguments))
    case _ => None
  }

  def makeManifest[T](clazz: Class[T], typeArgs: List[Manifest[_]]) = new Manifest[T] {
    override val typeArguments = typeArgs
    val runtimeClass = clazz
  }

  /**
   * Applications can override this to generate custom function names & non-colliding classpaths
   */
  def functionName: String = ""

}


trait ScalaGenDeliteStruct extends BaseGenStruct {
  val IR: DeliteStructsExp with DeliteOpsExp
  import IR._

  def sizeof(m: Manifest[_]) = m.toString match {
    case "Boolean" => 1
    case "Byte" => 8
    case "Char" | "Short" => 16
    case "Int" => 32
    case _ => 64
  }

  def shiftOnString(elems: Seq[(String,Exp[Any])], tp: String) = {
    var shift = 0
    val str = for (e <- elems) yield {
      val str = "(" + quote(e._2) + ".asInstanceOf["+tp+"] << " + shift + ")"  
      shift += sizeof(e._2.tp)
      str
    }
    str.mkString(" + ")
  }

  def shiftOffString(sym: Exp[Any], index: String, size: Int) = sym.tp match {
    case StructType(_,elems) => 
      val e = elems.find(_._1 == index).get
      val shiftBy = size - elems.takeWhile(_._1 != index).map(e => sizeof(e._2)).sum - sizeof(e._2)
      "((" + quote(sym) + " << " + shiftBy + ") >>> " + (size-sizeof(e._2)) + ").asInstanceOf[" + remap(e._2) + "]"
    case _ => throw new RuntimeException("tried to generate unrecognized struct type: " + sym.tp.toString)
  }

  //TODO: we should be able to apply this to any struct, but we need a way of dealing with mutable instances vs. immutable instance (transformer?)
  //for now special case tuples (always immutable)
  def structSize(m: Manifest[_]) = m match {
    case StructType(_,elems) if m.toString.contains("scala.Tuple") => elems.map(e => sizeof(e._2)).sum
    case _ => java.lang.Integer.MAX_VALUE
  }

  def structCount(m: Manifest[_]) = m match {
    case StructType(_,elems) => elems.length
    case _ => java.lang.Integer.MAX_VALUE
  }

  def packageName = {
    val appQualifier = if (functionName == "") "" else "."+functionName.toLowerCase+"p"
    "generated." + this.toString + appQualifier
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Struct(tag, elems) if structSize(sym.tp) <= 32 => //bit packing
      emitValDef(sym, shiftOnString(elems, "Int"))
    // case Struct(tag, elems) if structSize(sym.tp) <= 64 => //FIXME: Longs actually perform worse than case classes in our HashMapImpl
    //   emitValDef(sym, shiftOnString(elems, "Long"))
    case Struct(tag, elems) =>
      registerStruct(structName(sym.tp), elems)
      emitValDef(sym, "new " + remap(sym.tp) + "(" + elems.map{ e => 
        if (isVarType(e._2) && deliteInputs.contains(e._2)) quote(e._2) + ".get"
        else quote(e._2)
      }.mkString(",") + ")")
    case FieldApply(struct, index) if structSize(struct.tp) <= 32 =>
      emitValDef(sym, shiftOffString(struct, index, 32))
    // case FieldApply(struct, index) if structSize(struct.tp) <= 64 => 
    //   emitValDef(sym, shiftOffString(struct, index, 64))
    case FieldApply(struct, index) =>
      emitValDef(sym, quote(struct) + "." + index)
    case FieldUpdate(struct, index, rhs) =>
      emitValDef(sym, quote(struct) + "." + index + " = " + quote(rhs))
    case NestedFieldUpdate(struct, fields, rhs) =>
      emitValDef(sym, quote(struct) + "." + fields.reduceLeft(_ + "." + _) + " = " + quote(rhs))
    case _ => super.emitNode(sym, rhs)
  }

  //because we remapped object types to primitive types above
  override def isPrimitiveType[A](m: Manifest[A]) = remap(m) match {
    case "Boolean" | "Byte" | "Char" | "Short" | "Int" | "Long" | "Float" | "Double" => true
    case _ => false
  }

  override def remap[A](m: Manifest[A]) = m match {
    case StructType(_,_) if structSize(m) <= 32 => "Int"
    // case StructType(_,_) if structSize(m) <= 64 => "Long"
    case StructType(_,_) => packageName + "." + structName(m)
    case s if s <:< manifest[Record] && s != manifest[Nothing] => "generated.scala." + structName(m)
    case _ => super.remap(m)
  }
  
  private def isVarType[T](e: Exp[T]) = e.tp.erasure.getSimpleName == "Variable" && (e match { //TODO: this is fragile, based on behavior of var_new override in Structs.scala
    case Def(Struct(_,_)) => false //Var(Struct) => Struct(Var)
    case _ => true
  })

  private def isArrayType[T](m: Manifest[T]) = m.erasure.getSimpleName == "DeliteArray"
  private def isStringType[T](m: Manifest[T]) = m.erasure.getSimpleName == "String"
  
  override def emitDataStructures(path: String) {
    new File(path) mkdirs // doesn't necessarily exist
    val stream = new PrintWriter(path + "DeliteStructs.scala")
    withStream(stream)(emitFileHeader)
    for ((name, elems) <- encounteredStructs) {
      stream.println("")
      emitStructDeclaration(name, elems)(stream)
    }
    stream.close()
    super.emitDataStructures(path)

    if (Config.generateSerializable) {
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
    }
    super.emitDataStructures(path)
  }

  def emitStructDeclaration(name: String, elems: Seq[(String,Manifest[_])])(stream: PrintWriter) {
    if (Config.generateSerializable) {
      stream.println("object " + name + " {")
      stream.println("def parseFrom(bytes: com.google.protobuf.ByteString) = {")
      stream.println("val mssg = Structs." + name + ".parseFrom(bytes)")
      emitStructDeserialization(name, elems, "mssg")(stream)
      stream.println("\n}")
      stream.print("def combine(lhs: " + name + ", " + "rhs: " + name + ") = ")
      emitStructReduction(name, elems, "lhs", "rhs")(stream)
      //TODO: Get rid of below. Just a hack for GPU cluster execution.
      stream.println("def createLocal(len: Int) = new ppl.delite.runtime.data.LocalDeliteArrayObject[" + name + "](len)")
      stream.println("\n}")
    }

    stream.print("case class " + name + "(")
    stream.print(elems.map{ case (idx,tp) => "var " + idx + ": " + remap(tp) }.mkString(", "))
    stream.println(") {")
    if (Config.generateSerializable) {
      stream.print("def toByteString = ")
      emitStructSerialization(name, elems, "")(stream)
      stream.println(".build.toByteString")
    }
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
    stream.println(")")
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

trait CLikeGenDeliteStruct extends BaseGenStruct with CLikeCodegen {
  val IR: DeliteStructsExp with DeliteOpsExp
  import IR._

  // Set of generated or generation-failed structs for this target
  protected val generatedStructs = HashSet[String]()
  protected val generationFailedStructs = HashSet[String]()

  override def remap[A](m: Manifest[A]) = m match {
    case StructType(_,_) => deviceTarget.toString + structName(m)
    case s if s <:< manifest[Record] && s != manifest[Nothing] => deviceTarget.toString + structName(m)
    case _ => super.remap(m)
  }

  protected def isVarType[T](e: Exp[T]) = e.tp.erasure.getSimpleName == "Variable" && (e match { //TODO: this is fragile, based on behavior of var_new override in Structs.scala
    case Def(Struct(_,_)) => false //Var(Struct) => Struct(Var)
    case _ => true
  })

  protected def isVarType[T](m: Manifest[T]) = m.erasure.getSimpleName == "Variable" // && unapplyStructType(m.typeArguments(0)) == None
  protected def isArrayType[T](m: Manifest[T]) = m.erasure.getSimpleName == "DeliteArray" || m.erasure.isArray
  protected def isStringType[T](m: Manifest[T]) = m.erasure.getSimpleName == "String"
  protected def baseType[T](m: Manifest[T]) = if (isVarType(m)) mtype(m.typeArguments(0)) else m
  protected def unwrapArrayType[T](m: Manifest[T]): Manifest[_] = baseType(m) match {
    case bm if isArrayType(bm) => unwrapArrayType(bm.typeArguments(0))
    case bm => bm 
  }
  protected def isNestedArrayType[T](m: Manifest[T]) = isArrayType(baseType(m)) && !isPrimitiveType(unwrapArrayType(m))

  override def emitDataStructures(path: String) {
    new File(path) mkdirs // doesn't necessarily exist
    val structStream = new PrintWriter(path + deviceTarget.toString + "DeliteStructs.h")
    structStream.println("#ifndef __" + deviceTarget.toString + "_DELITESTRUCTS_H__")
    structStream.println("#define __" + deviceTarget.toString + "_DELITESTRUCTS_H__")
    structStream.println("#include \"" + deviceTarget + "types.h\"")    
    //structStream.println("#include \"" + hostTarget + "DeliteArray.h\"")
    //structStream.println("#include \"" + deviceTarget + "DeliteArray.h\"")
    //println("Cuda Gen is generating " + encounteredStructs.map(_._1).mkString(","))
    for ((name, elems) <- encounteredStructs if !generatedStructs.contains(name)) {
      try {
        emitStructDeclaration(path, name, elems)
        //elems /*filterNot { e => isNestedArrayType(e._2) }*/ foreach { e => dsTypesList.add(baseType(e._2).asInstanceOf[Manifest[Any]]) }
        structStream.println("#include \"" + deviceTarget + name + ".h\"")
      }
      catch {
        case e: GenerationFailedException => generationFailedStructs += name 
        case e: Exception => throw(e)
      }
    }
    structStream.println("#endif")
    structStream.close()
    super.emitDataStructures(path)
  }

  def emitStructDeclaration(path: String, name: String, elems: Seq[(String,Manifest[_])])

  override def getDataStructureHeaders(): String = {
    val out = new StringBuilder
    out.append("#include \"" + deviceTarget + "DeliteStructs.h\"\n")
    super.getDataStructureHeaders() + out.toString
  }  
}

trait CudaGenDeliteStruct extends CLikeGenDeliteStruct with CudaGenDeliteOps {
  val IR: DeliteStructsExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Struct(tag, elems) =>
      registerStruct(structName(sym.tp), elems)
      // Within kernel, place on stack
      if(isNestedNode) {
        stream.println(remap(sym.tp) + " " + quote(sym) + " = " + remap(sym.tp) + "(" + elems.map{ e => 
          if (isVarType(e._2) && deliteInputs.contains(e._2)) quote(e._2) + ".get()"
          else quote(e._2)
        }.mkString(",") + ");")
      }
      else {
        stream.println(remap(sym.tp) + " *" + quote(sym) + "_ptr = new " + remap(sym.tp) + "(" + elems.map{ e => 
          if (isVarType(e._2) && deliteInputs.contains(e._2)) quote(e._2) + "->get()"
          else quote(e._2)
        }.mkString(",") + ");")
        stream.println(remap(sym.tp) + " " + quote(sym) + " = *" + quote(sym) + "_ptr;")
      }
      printlog("WARNING: emitting " + remap(sym.tp) + " struct " + quote(sym))    
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

  def emitStructDeclaration(path: String, name: String, elems: Seq[(String,Manifest[_])]) {
    val stream = new PrintWriter(path + deviceTarget + name + ".h")
    try {
      stream.println("#ifndef __" + deviceTarget + name + "__")
      stream.println("#define __" + deviceTarget + name + "__")

      val dependentStructTypes = elems.map(e => baseType(e._2)).collect{ case m@StructType(_,_) => structName(m) }.distinct

      val dependentArrayTypes = elems.map(e => baseType(e._2)).filter{
        case StructType(_,_) => false
        case m if isArrayType(m) => true
        case _ => false
      }.map(m => remap(m)).distinct
      
      dependentStructTypes foreach { t =>
        if (encounteredStructs.contains(t)) {
          stream.println("#include \"" + deviceTarget + t + ".h\"") 
          if (generationFailedStructs.contains(t)) {
            throw new GenerationFailedException("Cannot generate struct " + name + " because of the failed dependency " + t)
          }
          else {
            emitStructDeclaration(path, t, encounteredStructs(t))
          }
        }
      }   
      
      dependentArrayTypes foreach { t=>
        stream.println("#include \"" + t + ".h\"")
      }
      if(isAcceleratorTarget)
        stream.println("#include \"" + hostTarget + name + ".h\"")

      stream.println("class " + deviceTarget + name + " {")
      // fields
      stream.println("public:")
      stream.print(elems.map{ case (idx,tp) => "\t" + remap(tp) + " " + idx + ";\n" }.mkString(""))
      // constructor
      stream.println("\t__host__ __device__ " + deviceTarget + name + "(void) { }")
      stream.print("\t__host__ __device__ " + deviceTarget + name + "(")
      stream.print(elems.map{ case (idx,tp) => remap(tp) + " _" + idx }.mkString(","))
      stream.println(") {")
      stream.print(elems.map{ case (idx,tp) => "\t\t" + idx + " = _" + idx + ";\n" }.mkString(""))
      stream.println("\t}")

      //TODO: Below should be changed to use IR nodes
      stream.println("\t__device__ void dc_copy(" + deviceTarget + name + " from) {")
      for((idx,tp) <- elems) {
        if(isPrimitiveType(baseType(tp))) stream.println("\t\t" + idx + " = from." + idx + ";")
        else stream.println("\t\t" + idx + ".dc_copy(from." + idx + ");")
      }
      stream.println("\t}")
      stream.println("\t__host__ " + deviceTarget + name + " *dc_alloc() {")
      stream.print("\t\treturn new " + deviceTarget + name + "(")
      stream.print(elems.map{ case (idx,tp) => if(!isPrimitiveType(baseType(tp))) ("*" + idx + ".dc_alloc()") else idx }.mkString(","))
      stream.println(");")
      stream.println("\t}")
      // Only generate dc_apply, dc_update, dc_size when there is only 1 DeliteArray among the fields
      val arrayElems = elems.filter(e => isArrayType(baseType(e._2)))
      val generateDC = arrayElems.size > 0
      val generateAssert = (arrayElems.size > 1) || elems.map(e=>baseType(e._2)).collect{ case m@StructType(_,_) if isArrayType(m) => true }.nonEmpty

      if(generateDC) {
        val (idx,tp) = elems.filter(e => isArrayType(baseType(e._2)))(0)
        val argtp = if(generateAssert) "int" else remap(unwrapArrayType(tp))
        stream.println("\t__host__ __device__ " + remap(argtp) + " dc_apply(int idx) {")
        if(generateAssert) stream.println("\t\tassert(false);")
        else stream.println("\t\treturn " + idx + ".apply(idx);")
        stream.println("\t}")
        stream.println("\t__host__ __device__ void dc_update(int idx," + argtp + " newVal) {")
        if(generateAssert) stream.println("\t\tassert(false);")
        else stream.println("\t\t" + idx + ".update(idx,newVal);")  
        stream.println("\t}")
        stream.println("\t__host__ __device__ int dc_size(void) {")
        if(generateAssert) stream.println("\t\tassert(false);")
        else stream.println("\t\treturn " + idx + ".length;")
        stream.println("\t}")
      }
      stream.println("};")
    
      // Wrapper class that holds both the device type and host type
      if(isAcceleratorTarget) {
        stream.println("class Host" + deviceTarget + name + " {")
        stream.println("public:")
        stream.println(hostTarget + name + " *host;")
        stream.println(deviceTarget + name + " *dev;")
        stream.println("};")
      }

      stream.println("#endif")
      generatedStructs += name
      stream.close()
      elems foreach { e => val t = baseType(e._2); dsTypesList.add((t.asInstanceOf[Manifest[Any]],remap(t))) }
      elems foreach { e => val t = unwrapArrayType(e._2); dsTypesList.add((t.asInstanceOf[Manifest[Any]],remap(t))) }
    }
    catch {
      case e: GenerationFailedException => generationFailedStructs += name; (new File(path + deviceTarget + name + ".h")).delete; throw(e)
      case e: Exception => throw(e)
    }
  }
}

trait OpenCLGenDeliteStruct extends CLikeGenDeliteStruct with OpenCLCodegen {
  val IR: DeliteStructsExp with DeliteOpsExp
  import IR._

  def emitStructDeclaration(path: String, name: String, elems: Seq[(String,Manifest[_])]) { }
}

trait CGenDeliteStruct extends CLikeGenDeliteStruct with CCodegen {
  val IR: DeliteStructsExp with DeliteOpsExp
  import IR._

  override def remap[A](m: Manifest[A]) = m match {
    case StructType(_,_) if cppMemMgr == "refcnt" => wrapSharedPtr(deviceTarget.toString + structName(m))
    case StructType(_,_) => deviceTarget.toString + structName(m)
    case s if s <:< manifest[Record] && s != manifest[Nothing] && cppMemMgr =="refcnt" => wrapSharedPtr(deviceTarget.toString + structName(m))
    case s if s <:< manifest[Record] && s != manifest[Nothing] => deviceTarget.toString + structName(m)
    case _ => super.remap(m)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Struct(tag, elems) =>
      registerStruct(structName(sym.tp), elems)
      if (cppMemMgr == "refcnt") {      
        stream.println(remap(sym.tp) + " " + quote(sym) + "(new " + unwrapSharedPtr(remap(sym.tp)) + "(" + elems.map{ e => 
           if (isVarType(e._2) && deliteInputs.contains(e._2)) quote(e._2) + "->get()"
           else quote(e._2)
        }.mkString(",") + ")," + unwrapSharedPtr(remap(sym.tp)) + "D());")
      }
      else {
        stream.println(remapWithRef(sym.tp) + quote(sym) + " = new (" + resourceInfoSym + ".thread_id) " + remap(sym.tp) + "(" + elems.map{ e =>
          if (isVarType(e._2) && deliteInputs.contains(e._2)) quote(e._2) + "->get()"
          else quote(e._2)
        }.mkString(",") + ");")
      }
      printlog("WARNING: emitting " + remap(sym.tp) + " struct " + quote(sym))    
    case FieldApply(struct, index) =>
      emitValDef(sym, quote(struct) + "->" + index)
      printlog("WARNING: emitting field access: " + quote(struct) + "." + index)
    case FieldUpdate(struct, index, rhs) =>
      stream.println(quote(struct) + "->" + index + " = " + quote(rhs) + ";")
      printlog("WARNING: emitting field update: " + quote(struct) + "." + index)
    case NestedFieldUpdate(struct, fields, rhs) =>
      stream.println(quote(struct) + "->" + fields.reduceLeft(_ + "->" + _) + " = " + quote(rhs) + ";")
    case _ => super.emitNode(sym, rhs)
  }

  private val destructorString = """
struct __T__D {
  void operator()(__T__ *p) {
    //printf("__T__: deleting %p\n",p);
    __DESTRUCT_ELEMS__
  }
};
"""

  //TODO: Merge with other C-like codegen (when merged with wip-gpuref)
  // currently CUDA codegen uses stack for struct type datastructure.
  override def emitStructDeclaration(path: String, name: String, elems: Seq[(String,Manifest[_])]) {
    val stream = new PrintWriter(path + deviceTarget + name + ".h")
    try {
      stream.println("#ifndef __" + deviceTarget + name + "__")
      stream.println("#define __" + deviceTarget + name + "__")

      val dependentStructTypes = elems.map(e => 
        if(encounteredStructs.contains(structName(baseType(e._2)))) structName(baseType(e._2))
        else if(encounteredStructs.contains(structName(unwrapArrayType(e._2)))) structName(unwrapArrayType(e._2))
        else remap(baseType(e._2)).replaceAll(deviceTarget,"")  // SoA transfromed types
      ).distinct
        
      val dependentArrayTypes = if (cppMemMgr == "refcnt") elems.filter(e => isArrayType(baseType(e._2))).map(e => unwrapSharedPtr(remap(e._2))).distinct
                                else elems.filter(e => isArrayType(baseType(e._2))).map(e => remap(e._2)).distinct
      dependentStructTypes foreach { t =>
        if (encounteredStructs.contains(t)) {
          stream.println("#include \"" + deviceTarget + t + ".h\"") 
          if (generationFailedStructs.contains(t)) {
            throw new GenerationFailedException("Cannot generate struct " + name + " because of the failed dependency " + t)
          }
          else {
            emitStructDeclaration(path, t, encounteredStructs(t))
          }
        }
      }

      stream.println("#include \"DeliteCpp.h\"")
      stream.println("#include \"DeliteMemory.h\"")

      dependentArrayTypes foreach { t=>
        stream.println("#include \"" + t + ".h\"")
      }

      val dependentMapTypes = elems.filter(e => e._2.erasure.getSimpleName == "DeliteIndex")
      if (dependentMapTypes.nonEmpty) stream.println("#include \"" + deviceTarget + "HashMap.h\"")
      
      if(isAcceleratorTarget)
        stream.println("#include \"" + hostTarget + name + ".h\"")

      stream.println("class " + deviceTarget + name + " : public DeliteMemory {")

      // fields
      stream.println("public:")
      stream.print(elems.map{ case (idx,tp) => "\t" + remap(tp) + " " + addRef(baseType(tp)) + idx + ";\n" }.mkString(""))
      // constructor
      stream.println("\t" + deviceTarget + name + "(void) { }")
      stream.print("\t" + deviceTarget + name + "(")
      //NOTE: for some reason, having the name __real or __imag produces compile error. reserverd keywords?
      stream.print(elems.map{ case (idx,tp) => remap(tp) + addRef(baseType(tp)) + " arg_" + idx }.mkString(","))
      stream.println(") {")
      //stream.println("\t\tprintf(\"allocated %s\\n\");".format(name))
      stream.print(elems.map{ case (idx,tp) => "\t\t" + idx + " = arg_" + idx + ";\n" }.mkString(""))
      stream.println("\t}")

      // equals
      val elemEquals = elems.map { e =>
        if (encounteredStructs.contains(structName(baseType(e._2)))) {
          e._1 + "->equals(to->" + e._1 + ")"
        }
        else {
          e._1 + " == to->" + e._1
        }
      }.mkString("(",") && (",")")
      if (cppMemMgr == "refcnt")
        stream.println("\tbool equals(" + wrapSharedPtr(deviceTarget.toString + name) + " to) {")
      else
        stream.println("\tbool equals(" + deviceTarget + name + addRef() + " to) {")
      stream.println("\t\treturn " + elemEquals + ";")
      stream.println("\t}")

      // hashcode
      val elemHashcode = elems.map { e =>
        if (isPrimitiveType(baseType(e._2))) "delite_hashcode(" + e._1 + ")"
        else e._1 + "->hashcode()"
      }
      stream.println("\tuint32_t hashcode(void) {")
      stream.println("\t\tint32_t hc;")
      stream.println("\t\tint32_t ret = 0;")
      for(hc <- elemHashcode) {
        stream.println("\t\thc = " + hc + ";")
        stream.println("\t\tret = ret * 41 + hc;")
      }
      stream.println("\t\treturn ret;")
      stream.println("\t}")

      // free
      //stream.println("\tvoid release(void) {")
      //stream.print(elems.filter(e => !isPrimitiveType(baseType(e._2)) && remap(baseType(e._2))!="string").map(e => e._1 + "->release();\n").mkString(""))
      //stream.println("\tfree(this);")
      //stream.println("\t}")
      stream.println("};")
      
      //destructor
      val elemD = if (cppMemMgr == "refcnt") elems.filter(e => !isPrimitiveType(baseType(e._2))).map{ case (idx,tp) => "\t(p->" + idx + ").reset();\n" }.mkString("")
                  else ""
      stream.println(destructorString.replaceAll("__T__",deviceTarget+name).replaceAll("__DESTRUCT_ELEMS__",elemD))
      
      stream.println("#endif")

      generatedStructs += name
      stream.close()
      elems foreach { e => val t = baseType(e._2); dsTypesList.add((t.asInstanceOf[Manifest[Any]],remap(t))) }
      elems foreach { e => val t = unwrapArrayType(e._2); dsTypesList.add((t.asInstanceOf[Manifest[Any]],remap(t))) }
    }
    catch {
      case e: GenerationFailedException => generationFailedStructs += name; (new File(path + deviceTarget + name + ".h")).delete; throw(e)
      case e: Exception => throw(e)
    }
  }
  
}

