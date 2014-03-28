package ppl.delite.framework.codegen.restage

import java.io.{StringWriter,PrintWriter}
import reflect.{SourceContext, RefinedManifest}
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.{DeliteRestageOps,DeliteRestageOpsExp}
import ppl.delite.framework.ops.{DeliteOpsExp,DeliteCollection,DeliteCollectionOpsExp,ScalaGenDeliteCollectionOps}
import ppl.delite.framework.ScopeCommunication._

trait TargetRestage extends Target {
  import IR._

  val name = "Restage"
}

trait RestageCodegen extends ScalaCodegen with Config {
  val IR: Expressions 
  import IR._

  // should be set by DeliteRestage if there are any transformations to be run before codegen
  var transformers: List[WorklistTransformer{val IR: RestageCodegen.this.IR.type}] = Nil
  
  override def kernelFileExt = "scala"

  override def toString = "restage"
  
  val tpeStreamBody = new StringWriter()
  val tpeStream = new PrintWriter(tpeStreamBody)
  
  def emitHeader(out: PrintWriter, append: Boolean) {
    if (!append) {    
      // restage header
      out.println("import ppl.delite.framework.{DeliteILApplication,DeliteILApplicationRunner}")
      out.println("import ppl.delite.framework.datastructures.{DeliteArray,DeliteArrayBuffer}")
      out.println("import ppl.delite.framework.ops.DeliteCollection")
      out.println("import scala.virtualization.lms.util.OverloadHack")
      out.println("import scala.virtualization.lms.common.Record")
      out.println("import reflect.{RefinedManifest,SourceContext}")
      out.println()
      out.println("object RestageApplicationRunner extends DeliteILApplicationRunner with RestageApplication")
      out.println("trait RestageApplication extends DeliteILApplication with OverloadHack {")      
      out.println("/* Emitting re-stageable code */")
      out.println("val drefGlobals = scala.collection.mutable.HashMap[Int,Any]()") // odd things happen when this is declared inside main
      out.println("def main() {")
      out.println("val x0 = args;")
    }
    // out.println("var " + drefBox(curScopeId) + ": Any = null;")      
    out.println("{")
  }  
}

trait RestageFatCodegen extends GenericFatCodegen with RestageCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._
    
  override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
    val staticData = getFreeDataBlock(body)
    
    println("--RestageCodegen emitSource")
    
    var b = body
    for (t <- transformers) {
      b = t.run(b)
    }

    val implStreamBody = new StringWriter()
    val implStream = new PrintWriter(implStreamBody)
    
    withStream(implStream) {
      emitBlock(b)
    }    

    tpeStream.flush()
    out.println(tpeStreamBody.toString)
    implStream.flush()
    out.println(implStreamBody)
    
    //println("GlobalDefs")
    //globalDefs.foreach(println)

    staticData
  }    

}

// restage generators for LMS common ops
trait LMSCodeGenRestage extends RestageFatCodegen {
  val IR: Expressions with Effects with FatExpressions with DeliteRestageOpsExp 
          with IOOpsExp with PrimitiveOpsExp with MathOpsExp with RangeOpsExp with HashMapOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {    
    // scala
    case m@HashMapNew() => emitValDef(sym, "HashMap[" + remap(m.mK) + "," + remap(m.mV) + "]()")
    case HashMapApply(m,k) => emitValDef(sym, quote(m) + "(" + quote(k) + ")")
    case HashMapUpdate(m,k,v)  => emitValDef(sym, quote(m) + "(" + quote(k) + ") = " + quote(v))
    case HashMapContains(m,i) => emitValDef(sym, quote(m) + ".contains(" + quote(i) + ")")  
    case ObjBrApply(f) => emitValDef(sym, "BufferedReader(" + quote(f) + ")")
    case ObjFrApply(s) => emitValDef(sym, "FileReader(" + quote(s) + ")")    
    case ObjIntegerParseInt(s) => emitValDef(sym, "Integer.parseInt(" + quote(s) + ")")
    case IntFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValueL()")
    case MathMax(x,y) => emitValDef(sym, "Math.max(" + quote(x) + ", " + quote(y) + ")")
    case MathLog(x) => emitValDef(sym, "Math.log(" + quote(x) + ")")
    case MathSqrt(x) => emitValDef(sym, "Math.sqrt(" + quote(x) + ")")
    case MathExp(x) => emitValDef(sym, "Math.exp(" + quote(x) + ")")

    // Range foreach
    // !! this is unfortunate: we need the var to be typed differently, but most of this is copy/paste
    case RangeForeach(start, end, i, body) => {
      stream.println("var " + quote(i) + " = " + quote(start))
      stream.println("val " + quote(sym) + " = " + "while (" + quote(i) + " < " + quote(end) + ") {")
      emitBlock(body)
      stream.println(quote(getBlockResult(body)))
      stream.println(quote(i) + " = " + quote(i) + " + 1")
      stream.println("}")
    }

    case _ => super.emitNode(sym, rhs)
  }       
}

// restage generators for Delite common ops
trait DeliteCodeGenRestage extends RestageFatCodegen 
  with ScalaGenDeliteCollectionOps with ScalaGenDeliteArrayOps with ScalaGenDeliteStruct with DeliteScalaGenAllOverrides {
    
  val IR: Expressions with Effects with FatExpressions with DeliteRestageOpsExp 
          with DeliteCollectionOpsExp with DeliteArrayFatExp with DeliteOpsExp with DeliteAllOverridesExp
  import IR._
  import ppl.delite.framework.Util._

  var inRestageStructName = false
  
  override def remap[A](m: Manifest[A]): String = {
    // unvar used here to avoid having Variable types in restaged code (since we are regenerating the var definitions)
    unvar(m) match {
      case s if s.erasure.getSimpleName == "String" => "String"
      
      // HACK: GIterable should be a Record
      case s if s.erasure.getSimpleName == "DeliteArray" && m.typeArguments(0).erasure.getSimpleName == "GIterable" && !inRestageStructName => "DeliteArray[" + restageStructName(m.typeArguments(0)) + "]" 

      // the following cases are meant to catch all struct types and forward them to restageStructRename for proper naming
      case s if s <:< manifest[Record] && !inRestageStructName => restageStructName(s)
      case s if s <:< manifest[Record] => "Record"

      // DeliteArray arg remapping first goes through restageStructName so we don't end up with DeliteArray[DeliteCollection[Int]] sorts of things
      // special treatment for Delite collections... can we unify this?
      case s if s.erasure.getSimpleName == "DeliteArray" => s.typeArguments(0) match {
        case StructType(_,_) if !inRestageStructName => restageStructName(s)
        case s if s <:< manifest[Record] => restageStructName(s)
        case _ => "DeliteArray[" + remap(s.typeArguments(0)) + "]"
      }
      case s if s.erasure.getSimpleName == "DeliteArrayBuffer" => s.typeArguments(0) match {
        case StructType(_,_) if !inRestageStructName => restageStructName(s)
        case s if s <:< manifest[Record] => restageStructName(s)
        case _ => "DeliteArrayBuffer[" + remap(s.typeArguments(0)) + "]"
      }
      case s if isSubtype(s.erasure,classOf[DeliteCollection[_]]) => s.typeArguments(0) match {
        case StructType(_,_) if !inRestageStructName => restageStructName(s)
        case s if s <:< manifest[Record] => restageStructName(s)
        case _ => "DeliteCollection[" + remap(s.typeArguments(0)) + "]"
      }
      
      case s@StructType(_,_) if !inRestageStructName => restageStructName(s)
      case StructType(_,_) => "Record" 

      case _ => 
        // Predef.println("calling remap on: " + m.toString)
        // Predef.println("m.erasure: " + m.erasure)
        // Predef.println("m.simpleName: " + m.erasure.getSimpleName)
        // for (cls <- m.erasure.getInterfaces()) {
        //   println("  intf: " + cls.getSimpleName)
        // }
        // println("  superclass: " + m.erasure.getSuperlass().getSimpleName)
        // Predef.println("m.getInterfaces: " + m.erasure.getInterfaces())
        super.remap(m)
    }
  }
  
  override def quote(x: Exp[Any]) : String = x match {
    case Const(s: String) => (super.quote(x)).replace("\\", "\\\\") // need extra backslashes since we are going through an extra layer
    case _ => 
      // Predef.println("called super on quote: " + x)
      super.quote(x)
  }

  def quoteTag[T](tag: StructTag[T], tp: Manifest[Any]): String = tag match {
    case ClassTag(name) => "ClassTag(\""+name+"\")"
    case NestClassTag(elem) => quoteTag(elem,tp) //"NestClassTag[Var,"+remap(tp)+"]("+quoteTag(elem,tp)+")" // dropping the var wrapper...
    case AnonTag(fields) => "ClassTag(\"erased\")"
    case SoaTag(base, length) => "SoaTag(" + quoteTag(base,tp) + ", " + quote(length) + ")"
    case MapTag() => "MapTag()"
  } 
       
  def recordFieldLookup[T:Manifest](struct: Exp[T], nextStructTp: Manifest[_], currentStr: String, fieldNames: List[String]): String = {        
    if (fieldNames == Nil) return currentStr

    val structFieldTpes = nextStructTp match {
      case StructType(tag,fields) => fields
    }
    val fieldTp = mtype(structFieldTpes.find(_._1 == fieldNames.head).get._2)
  
    val newStr = if (currentStr == "") {
      "field["+remap(fieldTp)+"](" + quote(struct) + ", \"" + fieldNames.head + "\")"
      }
      else {
        "field["+remap(fieldTp)+"](" + currentStr + ", \"" + fieldNames.head + "\")"
      }
    recordFieldLookup(struct, fieldTp, newStr, fieldNames.tail)
  }    
  
  // variable manifests wrap our refined manifests..
  def unvar[T](m: Manifest[T]) = {
    if (m.erasure.getSimpleName == "Variable") {
      m.typeArguments(0).asInstanceOf[Manifest[Any]]
    } 
    else {
      m.asInstanceOf[Manifest[Any]]
    }
  }  
    
  /**
   * This is used to de-duplicate struct names by retaining the DS-name. Without this, different structs with the same field types
   * get mapped to the same name during restaging.
   */

  // our equivalent of encounteredStructs
  val restagedStructs = new scala.collection.mutable.HashSet[String]()
  
  def withoutStructNameRemap[T](tp: Manifest[T]) = {
    inRestageStructName = true
    val x = remap(tp)    
    inRestageStructName = false
    x    
  }

  // this function produces something like:
  //   DenseVectorInt extends DeliteCollection[Int]
  //   Tuple2IntInt extends Record
  def restageStructName[T](tp: Manifest[T]): String = {    
    // Predef.println("restageStructName called on:  " + tp.toString)
    val dsName = structName(tp) // re-use the struct renaming from LMS
    if (!restagedStructs.contains(dsName)) {
      val superCls = tp match {
        // TODO: this unwrapping is just to avoid an infinite loop but is redundant with the unwrapping in remap -- need to find a better mechanism, i.e. toggling or scoping inRestageStructName
        case s if s.erasure.getSimpleName == "DeliteArray" => "DeliteArray[" + remap(s.typeArguments(0)) + "]"
        case s if s.erasure.getSimpleName == "DeliteArrayBuffer" => "DeliteArrayBuffer[" + remap(s.typeArguments(0)) + "]"      
        case s if isSubtype(s.erasure,classOf[DeliteCollection[_]]) => "DeliteCollection[" + remap(s.typeArguments(0)) + "]" 
        case _ => withoutStructNameRemap(tp)
      }
      tpeStream.println("abstract class " + dsName + " extends " + superCls)
      restagedStructs += dsName
    }
    dsName
  }
  

  /**
   * Actual restaging code gen definitions
   */

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {    
    // data exchange
    case ReturnScopeResult(n) => emitValDef(sym, "setScopeResult(" + quote(n) + ")")
    case LastScopeResult() => emitValDef(sym, "getScopeResult") 
    
    case SetDRefOutput(s,id) => 
      stream.println("drefGlobals += " + id + " -> " + quote(s))
      // stream.println(drefBox(id) + " = " + quote(s))
    case e@WrapDRefAsSym(id) => 
      emitValDef(sym, "drefGlobals("+id+").asInstanceOf[Rep["+remap(sym.tp)+"]]")
      // emitValDef(sym, drefBox(id)+".asInstanceOf[Rep["+remap(sym.tp)+"]]")
    
    // LMS stuff pulled in by Delite
    case NewVar(init) => stream.println("var " + quote(sym) + " = " + quote(init))    
    case ThrowException(m) => emitValDef(sym, "fatal(" + quote(m) + ")")
    case RepIsInstanceOf(x,mA,mB) => emitValDef(sym, quote(x) + ".isInstanceOf[Rep[" + remap(mB) + "]]")
    case RepAsInstanceOf(x,mA,mB) => emitValDef(sym, quote(x) + ".asInstanceOf[Rep[" + remap(mB) + "]]")    
    case ObjectUnsafeImmutable(x) => emitValDef(sym, quote(x) + ".unsafeImmutable()")      

    // profiling
    case DeliteProfileStart(x,deps) if deps == Nil =>  emitValDef(sym, "tic(" + quote(x) + ")") 
    case DeliteProfileStart(x,deps) => emitValDef(sym, "tic(" + quote(x) + ", " + deps.map(quote(_)).mkString(",") + ")") 
    case DeliteProfileStop(x,deps) if deps == Nil =>  emitValDef(sym, "toc(" + quote(x) + ")")
    case DeliteProfileStop(x,deps) => emitValDef(sym, "toc(" + quote(x) + ", " + deps.map(quote(_)).mkString(",") + ")")
    
    // if then else
    // !! redundant - copy paste of LMS if/then/else just to avoid DeliteIfThenElse getting a hold of it, which is put in scope by the individual DSLs
    case DeliteIfThenElse(c,a,b,h) =>
      stream.println("val " + quote(sym) + " = if (" + quote(c) + ") {")
      emitBlock(a)
      stream.println(quote(getBlockResult(a)))
      stream.println("} else {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

    // delite array
    // HACK: GIterable should be a Record
    case a@DeliteArrayNew(n,m) if sym.tp.typeArguments(0).erasure.getSimpleName == "GIterable" => emitValDef(sym, "DeliteArray[" + restageStructName(m) + "](" + quote(n) + ")")
    case a@DeliteArrayNew(n,m) => emitValDef(sym, "DeliteArray[" + remap(m) + "](" + quote(n) + ")")
    case DeliteArrayCopy(src,srcPos,dest,destPos,len) => emitValDef(sym, "darray_copy(" + quote(src) + "," + quote(srcPos) + "," + quote(dest) + "," + quote(destPos) + "," + quote(len) + ")")
    // new in wip-develop
    case StructCopy(src,srcPos,struct,fields,destPos,len) => 
      assert(fields.length == 1) // nested fields not supported yet
      assert(destPos.length == 1)
      emitValDef(sym, "darray_copy(" + quote(src) + "," + quote(srcPos) + ", field["+remap(src.tp)+"](" + quote(struct) + ",\"" + fields(0) + "\")," + quote(destPos.head) + "," + quote(len) + ")")
    case VarCopy(src,srcPos,Variable(a),destPos,len) =>
      val dest = quote(a) + (if (deliteInputs contains a) ".get" else "")
      emitValDef(sym, "darray_copy(" + quote(src) + "," + quote(srcPos) + "," + dest + "," + quote(destPos) + "," + quote(len) + ")")
    
    case DeliteArrayGetActSize() => emitValDef(sym, "darray_unsafe_get_act_size()")
    case DeliteArraySetActBuffer(da) => emitValDef(sym, "darray_set_act_buf(" + quote(da) + ")")
    
    // structs
    case Struct(tag, elems) =>
      // oops.. internal scalac error
      // emitValDef(sym, "anonStruct(" + elems.asInstanceOf[Seq[(String,Rep[Any])]].map{case (k,v) => "(\"" + k + "\", " + quote(v) + ")" }.mkString(",") + ")")
       
      val isVar = elems(0)._2 match {
        case Def(Reflect(NewVar(x),u,es)) => true
        case x: Exp[Var[Any]] if x.tp.toString.contains("Variable") => true
        case _ => false
      }
      val structMethod = if (isVar) "mstruct" else "struct"      
      emitValDef(sym, structMethod + "[" + restageStructName(sym.tp) + "](" + quoteTag(tag,sym.tp) + ", " + elems.asInstanceOf[Seq[(String,Rep[Any])]].map{t => "(\"" + t._1 + "\", " + quote(t._2) + ")" }.mkString(",") + ")")
    
    case FieldApply(struct, index) => 
       emitValDef(sym, "field["+remap(sym.tp)+"](" + quote(struct) + ",\"" + index + "\")")
      
    case FieldUpdate(struct, index, rhs) => emitValDef(sym, "field_update[" + remap(sym.tp) + "](" + quote(struct) + ",\"" + index + "\"," + quote(rhs) + ")")
    case NestedFieldUpdate(struct, fields, rhs) => 
      assert(fields.length > 0)      
      // x34.data.id(x66)
      // field[T](field[Record](x34, "data"), "id")
      if (fields.length == 1) { // not nested
        emitValDef(sym, "field_update[" + remap(rhs.tp) + "](" + quote(struct) + ",\"" + fields(0) + "\"," + quote(rhs) + ")")
      }
      else {
        emitValDef(sym, "field_update(" + recordFieldLookup(struct, struct.tp, "", fields) + ", " + quote(rhs) + ")")        
      }
   
    case StructUpdate(struct, fields, idx, x) =>
      assert(fields.length > 0)
      assert(idx.length == 1)
      if (fields.length == 1) { // not nested
        emitValDef(sym, "darray_update(field[DeliteArray["+remap(x.tp)+"]](" + quote(struct) + ", \"" + fields.head + "\"), " + quote(idx.head) + ", " + quote(x) + ")")
      }
      else {
        emitValDef(sym, "darray_update(" + recordFieldLookup(struct, struct.tp, "", fields) + ", " + quote(idx.head) + ", " + quote(x) + ")")
      }
    
    // delite ops
    case s:DeliteOpSingleTask[_] => 
      stream.println("// " + sym.toString + "=" + s + " / " + Def.unapply(sym))
      // each stm inside the block must be restageable..
      stream.print("val " + quote(sym) + " = single({")
      emitBlock(s.block)
      //stream.print("val " + quote(sym) + " = ({")
      stream.println(quote(getBlockResult(s.block)))
      stream.println("})")
      // SingleTasks are sometimes used to construct an immutable alias, but they really ought to explicitly call unsafeImmutable
      // other SingleTasks actually intentionally allocate mutable outputs and reflectMutable, so the following is unsafe
      //stream.println(quote(getBlockResult(s.block)) + ".unsafeImmutable()")
      
    case e:DeliteOpExternal[_] => 
      // DeliteOpExternals are broken right now - we can't generate the JNI stuff from the external node alone... what to do?
      // use --nb? hack the JNI methods in (e.g. by copying?)
      assert(e.inputs != Nil) // basically just makes sure we are using a hacked version
      
      // the proper solution is to store everything we need to generate the external call inside DeliteOpExternal, instead of having it be
      // specified in another DSL trait like we do now...
      stream.print("val " + quote(sym) + " = ")
      stream.println("extern(\"" + e.funcName + "\", {")
      emitBlock(e.allocVal)
      stream.println(quote(getBlockResult(e.allocVal)))
      stream.println("},")      
      stream.println("scala.List(" + e.inputs.map(quote).mkString(",") + "))")
      
    case op: AbstractLoop[_] => 
      stream.println("// a *thin* loop follows: " + quote(sym))
      emitFatNode(List(sym), SimpleFatLoop(op.size, op.v, List(op.body)))        
    
    case _ => 
      // Predef.println("calling super.emitNode on: " + rhs.toString)
      super.emitNode(sym, rhs)
  }
  
  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case op: AbstractFatLoop => emitRestageableLoop(op, symList)
    case _ => super.emitFatNode(symList, rhs)
  }
  
  def makeBoundVarArgs(args: Exp[Any]*) = {
    "(" + args.map(a => quote(a) + ": Rep[" + remap(a.tp) + "]").mkString(",") + ") => "
  }
  
  def emitBufferElem(op: AbstractFatLoop, elem: DeliteCollectElem[_,_,_]) {
    // appendable
    stream.println("{ // appendable")
    stream.println(makeBoundVarArgs(elem.buf.allocVal,elem.buf.eV,op.v))
    emitBlock(elem.buf.appendable)
    stream.println(quote(getBlockResult(elem.buf.appendable)))
    stream.println("},")
    // append
    stream.println("{ // append")
    stream.println(makeBoundVarArgs(elem.buf.allocVal,elem.buf.eV,op.v))
    emitBlock(elem.buf.append)
    stream.println(quote(getBlockResult(elem.buf.append)))
    stream.println("},")
    // setSize
    stream.println("{ // setSize")
    stream.println(makeBoundVarArgs(elem.buf.allocVal,elem.buf.sV))
    emitBlock(elem.buf.setSize)
    stream.println(quote(getBlockResult(elem.buf.setSize)))
    stream.println("},")
    // allocRaw
    stream.println("{ // allocRaw")
    stream.println(makeBoundVarArgs(elem.buf.allocVal,elem.buf.sV))  
    emitBlock(elem.buf.allocRaw)
    stream.println(quote(getBlockResult(elem.buf.allocRaw)))
    stream.println("},")
    // copyRaw
    stream.println("{ // copyRaw")
    stream.println(makeBoundVarArgs(elem.buf.aV2,elem.buf.iV,elem.buf.allocVal,elem.buf.iV2,elem.buf.sV))  
    emitBlock(elem.buf.copyRaw)
    stream.println(quote(getBlockResult(elem.buf.copyRaw)))    
    stream.println("}")
  }
  
  
  def emitRestageableLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    // break the multiloops apart, they'll get fused again anyways
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) => 
        stream.println("// " + sym.toString + "=" + elem + " / " + Def.unapply(sym))
        stream.println("val " + quote(sym) + " = collect[" + remap(elem.mA) + "," + remap(elem.mI) + "," + remap(elem.mCA) + "](")
        // stream.println("val " + quote(sym) + " = collect(")
        // loop size
        stream.println(quote(op.size) + ",")
        // alloc func
        stream.println("{")
        stream.println(makeBoundVarArgs(elem.buf.sV))
        emitBlock(elem.buf.alloc)
        stream.println(quote(getBlockResult(elem.buf.alloc)))
        stream.println("},")
        // func
        stream.println("{")
        stream.println(makeBoundVarArgs(elem.buf.eV,op.v))
        emitBlock(elem.func)
        stream.println(quote(getBlockResult(elem.func)))
        stream.println("},")
        // update
        stream.println("{")
        stream.println(makeBoundVarArgs(elem.buf.allocVal,elem.buf.eV,op.v))
        emitBlock(elem.buf.update)
        stream.println(quote(getBlockResult(elem.buf.update)))
        stream.println("},")
        // finalizer
        stream.println("{")
        stream.println(makeBoundVarArgs(elem.buf.allocVal))
        emitBlock(elem.buf.finalizer)
        stream.println(quote(getBlockResult(elem.buf.finalizer)))
        // conditions
        stream.println("},")
        stream.print("scala.List(")
        for (i <- 0 until elem.cond.length) {
          stream.println("{")
          stream.println(makeBoundVarArgs(op.v))
          emitBlock(elem.cond(i))
          stream.println(quote(getBlockResult(elem.cond(i))))
          stream.print("}")
          if (i < elem.cond.length - 1) stream.println(",")
        }
        stream.println("),")
        // par
        stream.println("\"" + elem.par.toString + "\",")
        // buffer
        emitBufferElem(op, elem)
        stream.println(")")

        
      case (sym, elem: DeliteForeachElem[_]) => 
        stream.println("// " + sym.toString + "=" + elem + " / " + Def.unapply(sym))
        stream.println("val " + quote(sym) + " = foreach(")
        // loop size
        stream.println(quote(op.size) + ",")
        // alloc func
        stream.println("{")
        stream.println(makeBoundVarArgs(op.v))
        emitBlock(elem.func)
        stream.println(quote(getBlockResult(elem.func)))
        stream.println("})")
        
        
      case (sym, elem: DeliteReduceElem[_]) =>   
        stream.println("// " + sym.toString + "=" + elem + " / " + Def.unapply(sym))
        stream.println("val " + quote(sym) + " = reduce(")
        // loop size
        stream.println(quote(op.size) + ",")
        // func
        stream.println("{")
        stream.println(makeBoundVarArgs(op.v))
        emitBlock(elem.func)
        stream.println(quote(getBlockResult(elem.func)))
        stream.println("},")
        // conditions
        stream.print("scala.List(")
        for (i <- 0 until elem.cond.length) {
          stream.println("{")
          stream.println(makeBoundVarArgs(op.v))
          emitBlock(elem.cond(i))
          stream.println(quote(getBlockResult(elem.cond(i))))
          stream.print("}")
          if (i < elem.cond.length - 1) stream.println(",")
        }
        stream.println("),") 
        // zero
        stream.println("{")
        emitBlock(elem.zero)
        stream.println(quote(getBlockResult(elem.zero)))
        stream.println("},") 
        // accInit
        stream.println("{")
        emitBlock(elem.accInit)
        stream.println(quote(getBlockResult(elem.accInit)))
        stream.println("},") 
        // rFunc
        stream.println("{")
        stream.println(makeBoundVarArgs(elem.rV._1,elem.rV._2))
        emitBlock(elem.rFunc)
        stream.println(quote(getBlockResult(elem.rFunc)))
        stream.println("},")
        // stripFirst
        stream.println(elem.stripFirst)
        stream.println(")")
    }
  }
}
