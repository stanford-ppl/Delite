package ppl.delite.framework.datastructures

import java.io.{File,FileWriter,PrintWriter}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{CudaCodegen,OpenCLCodegen, CCodegen}
import ppl.delite.framework.ops.DeliteOpsExp
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
  }

  def emitStructDeclaration(name: String, elems: Seq[(String,Manifest[_])])(stream: PrintWriter) {
    stream.print("case class " + name + "(")
    stream.print(elems.map{ case (idx,tp) => "var " + idx + ": " + remap(tp) }.mkString(", "))
    stream.println(")")
  }
}

trait CudaGenDeliteStruct extends BaseGenStruct with CudaCodegen {
  val IR: DeliteStructsExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Struct(tag, elems) =>
      registerStruct(structName(sym.tp), elems)
      // Within kernel, place on stack
      if(isNestedNode) {
        stream.println(structName(sym.tp) + " " + quote(sym) + " = " + structName(sym.tp) + "(" + elems.map{ e => 
          if (isVarType(e._2.tp) && deliteInputs.contains(e._2)) quote(e._2) + ".get()"
          else quote(e._2)
        }.mkString(",") + ");")
      }
      else {
        stream.println(structName(sym.tp) + " *" + quote(sym) + "_ptr = new " + structName(sym.tp) + "(" + elems.map{ e => 
          if (isVarType(e._2.tp) && deliteInputs.contains(e._2)) quote(e._2) + "->get()"
          else quote(e._2)
        }.mkString(",") + ");")
        stream.println(structName(sym.tp) + " " + quote(sym) + " = *" + quote(sym) + "_ptr;")
      }
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
  private def argType[T](m: Manifest[T]): Manifest[_] = baseType(m) match {
    case bm if isArrayType(bm) => argType(bm.typeArguments(0))
    case bm => bm 
  }

  override def emitDataStructures(path: String) {
    val structStream = new PrintWriter(path + "DeliteStructs.h")
    structStream.println("#ifndef __DELITESTRUCTS_H__")
    structStream.println("#define __DELITESTRUCTS_H__")
    structStream.println("#include \"DeliteArray.h\"")
    structStream.println("#include \"HostDeliteArray.h\"")
    //println("Cuda Gen is generating " + structs.toString)
    for ((name, elems) <- encounteredStructs) {
      structStream.println("#include \"" + name + ".h\"")
      val stream = new PrintWriter(path + name + ".h")
      stream.println("#ifndef __" + name + "__")
      stream.println("#define __" + name + "__")
      elems foreach { e => dsTypesList.add(baseType(e._2).asInstanceOf[Manifest[Any]]) }
      elems foreach { e => if (encounteredStructs.contains(remap(e._2))) stream.println("#include \"" + remap(e._2) + ".h\"") }
      elems foreach { e => if (encounteredStructs.contains(remap(argType(e._2)))) stream.println("#include \"" + remap(argType(e._2)) + ".h\"") }  
      emitStructDeclaration(name, elems)(stream)
      stream.println("#endif")
      stream.close()
    }
    structStream.println("#endif")
    structStream.close()
    super.emitDataStructures(path)
  }

  def emitStructDeclaration(name: String, elems: Seq[(String,Manifest[_])])(stream: PrintWriter) {
   
    for(prefix <- List("Host","")) {
      stream.println("class " + prefix + name + " {")
      // fields
      stream.println("public:")
      stream.print(elems.map{ case (idx,tp) => "\t" + (if(!isPrimitiveType(baseType(tp))) prefix else "") + remap(tp) + " " + idx + ";\n" }.mkString(""))
      // constructor
      if(prefix == "Host") {
        stream.println("\t" + prefix + name + "(void) { }")
        stream.print("\t" + prefix + name + "(")
      }
      else {
        stream.println("\t__host__ __device__ " + prefix + name + "(void) { }")
        stream.print("\t__host__ __device__ " + prefix + name + "(")
      }
      stream.print(elems.map{ case (idx,tp) => (if(!isPrimitiveType(baseType(tp))) prefix else "") + remap(tp) + " _" + idx }.mkString(","))
      stream.println(") {")
      stream.print(elems.map{ case (idx,tp) => "\t\t" + idx + " = _" + idx + ";\n" }.mkString(""))
      stream.println("\t}")

      if(prefix == "") {
        stream.println("\t__device__ void dc_copy(" + name + " from) {")
        for((idx,tp) <- elems) {
          if(isPrimitiveType(baseType(tp))) stream.println("\t\t" + idx + " = from." + idx + ";")
          else stream.println("\t\t" + idx + ".dc_copy(from." + idx + ");")
        }
        stream.println("\t}")
        stream.println("\t__host__ " + name + " *dc_alloc() {")
        stream.print("\t\treturn new " + name + "(")
        stream.print(elems.map{ case (idx,tp) => if(!isPrimitiveType(baseType(tp))) ("*" + idx + ".dc_alloc()") else idx }.mkString(","))
        stream.println(");")
        stream.println("\t}")
      }
      stream.println("};")
    }
  }
}

trait OpenCLGenDeliteStruct extends BaseGenStruct with OpenCLCodegen {
  val IR: DeliteStructsExp with DeliteOpsExp
  import IR._
}

trait CGenDeliteStruct extends BaseGenStruct with CCodegen {
  val IR: DeliteStructsExp with DeliteOpsExp
  import IR._
}

