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
      out.println("import reflect.{RefinedManifest,SourceContext}")
      out.println()
      out.println("object RestageApplicationRunner extends DeliteILApplicationRunner with RestageApplication")
      out.println("trait RestageApplication extends DeliteILApplication with OverloadHack {")      
      out.println("/* Emitting re-stageable code */")
      out.println("def main() {")
      out.println("val x0 = args;")
      out.println("{")
    }
    else {
      out.println("{")
    }    
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
      // stream.println("setLastScopeResult(" + quote(getBlockResult(body)) + ")")
    }    

    tpeStream.flush()
    out.println(tpeStreamBody.toString)
    implStream.flush()
    out.println(implStreamBody)
    
    staticData
  }    

}

// for now just lumping all the Delite restage generators together in one place..
trait DeliteCodeGenRestage extends RestageFatCodegen 
  with ScalaGenDeliteCollectionOps with ScalaGenDeliteArrayOps with ScalaGenDeliteStruct with DeliteScalaGenAllOverrides {
    
  val IR: Expressions with Effects with FatExpressions with DeliteRestageOpsExp 
          with IOOpsExp with PrimitiveOpsExp with MathOpsExp with RangeOpsExp with HashMapOpsExp
          with DeliteCollectionOpsExp with DeliteArrayFatExp with DeliteOpsExp with DeliteAllOverridesExp
  import IR._
  import ppl.delite.framework.Util._

  var inRestageStructName = false
  
  override def remap[A](m: Manifest[A]): String = {
    val ms = manifest[String]
    m match {
      case `ms` => "String"
      
      // DeliteArray arg remapping first goes through restageStructName so we don't end up with DeliteArray[DeliteCollection[Int]] sorts of things
      // case s if s.erasure.getSimpleName == "Tuple2" && !inRestageStructName => restageStructName(s) //"Record" // this is custom overridden in ScalaGenTupleOps 
      // case s if s.erasure.getSimpleName == "DeliteArray" && m.typeArguments(0) <:< manifest[Record] && !inRestageStructName => "DeliteArray[" + restageStructName(m.typeArguments(0)) + "]" 
      // HACK: GIterable should be a Record
      case s if s.erasure.getSimpleName == "DeliteArray" && m.typeArguments(0).erasure.getSimpleName == "GIterable" && !inRestageStructName => "DeliteArray[" + restageStructName(m.typeArguments(0)) + "]" 

      // the following cases are meant to erase Struct types to generic Records, so they don't hit the fallback remapping to concrete types (structName) that don't exist
      // case s if s.erasure.getSimpleName == "Tuple2" => "Record"
      // case s if s.erasure.getSimpleName == "DeliteArray" => "DeliteArray[" + remap(s.typeArguments(0)) + "]"
      // case s if s.erasure.getSimpleName == "DeliteArrayBuffer" => "DeliteArrayBuffer[" + remap(s.typeArguments(0)) + "]"      
      // case s if (s <:< manifest[Record] && isSubtype(s.erasure,classOf[DeliteCollection[_]])) => 
      //   "DeliteCollection[" + remap(s.typeArguments(0)) + "]"       
      // case s if s <:< manifest[Record] => "Record" // should only be calling 'field' on records at this level            /
      
      // case s if s.erasure.getSimpleName == "DeliteArray" => "DeliteArray[" + remapOrStructRename(s.typeArguments(0)) + "]"
      // case s if s.erasure.getSimpleName == "DeliteArrayBuffer" => "DeliteArrayBuffer[" + remapOrStructRename(s.typeArguments(0)) + "]"            
      // case s if isSubtype(s.erasure,classOf[DeliteCollection[_]]) => "DeliteCollection[" + remapOrStructRename(s.typeArguments(0)) + "]" 
      
      //case s if s.erasure.getSimpleName == "DeliteArray" => remapOrStructRename(s)
      //case s if s.erasure.getSimpleName == "DeliteArrayBuffer" => remapOrStructRename(s)
      //case s if isSubtype(s.erasure,classOf[DeliteCollection[_]]) => remapOrStructRename(s)
      
      //case s if s <:< manifest[Record] => remapOrStructRename(s)
      //case s if s.erasure.getSimpleName == "Tuple2" => remapOrStructRename(s)

      case s if s <:< manifest[Record] => remapOrStructRename(s)
      case s if isSubtype(s.erasure,classOf[DeliteCollection[_]]) => remapOrStructRename(s)
      case StructType(_,_) => remapOrStructRename(m)
            
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
        // remapErase(m)
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
       
  //def recordFieldLookup[T:Manifest](struct: Exp[T], nextStructTp: Manifest[_], fieldNames: List[String]): String = {        
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

    /*
    if (fieldNames.length == 1) 
      "field["+remap(fieldTp)+"](" + quote(struct) + ", \"" + fieldNames.head + "\")"
    else {      
      "field["+remap(fieldTp)+"](" + recordFieldLookup(struct, fieldTp, fieldNames.tail) + ", \"" + fieldNames.head + "\")"
    }
    */
  }    
  
  /**
   * All of the explicitly constructed manifest stuff below was an attempt to explicitly create RefinedManifests for struct
   * nodes that we spawn in order to de-duplicate struct names that have the same fields (since we've thrown away the domain-specific
   * type). However, this appears to cause more problems than it solves, namely needing to thread the new RefinedManifest through everywhere.
   * 
   * question: when we are explicitly passing RefinedManifests, do we need the remaps above? why? are they interfering?
   *  - even if they are, it is probably nicer to eliminate the explicitly passed RefinedManifests if we can.
   */
  
  /*
  def makeRefinedManifestStr[T](tp: Manifest[T], elems: Seq[(String,Manifest[_])]) = {
    // for (e <- elems) println("field: " + e._1 + ", manifest: " + e._2 + ", is refined: " + e._2.isInstanceOf[RefinedManifest[_]])
    // "new RefinedManifest[" + remap(tp) + "]{ def erasure = classOf[" + remap(tp) + "]; override def typeArguments = List("+tp.typeArguments.map(makeManifestStr(_)).mkString(",") + "); def fields = List(" + elems.map(t => ("\""+t._1+"\"",t._2)).mkString(",") + ")}"
    "new RefinedManifest[" + remap(tp) + "]{ def erasure = classOf[" + remap(tp) + "]; override def typeArguments = List(); def fields = List(" + elems.map(t => ("\""+t._1+"\"",makeManifestStr(t._2))).mkString(",") + ")}" 
  }
  def makeManifestStr[T](m: Manifest[T]): String = m match {
    case x if m.typeArguments.length > 0 =>
      "new Manifest[" + remap(x) + "]{ def erasure = classOf[" + remap(x) + "]; override def typeArguments = List("+x.typeArguments.map(a => makeManifestStr(a)).mkString(",") + ")}"  
    case rm: RefinedManifest[_] => makeRefinedManifestStr(rm, rm.fields/*.map(t => (t._1,makeManifestStr(t._2)))*/)
    case _ => "manifest[" + remap(m) + "]"
  }
  // TODO: matching on Def(Struct) with the sym doesn't seem to work.. why?
  // def makeManifestStr(sym: Sym[Any], rhs: Def[Any]): String = rhs match {
  //   // case Struct(tag, elems) => makeRefinedManifestStr(sym.tp, elems.map(t => (t._1,t._2.tp))) // t._2.tp is NOT a refined manifest
  //   case Struct(tag, elems) => 
  //     // Predef.println("making refined manifest str for def " + rhs.toString)
  //     makeRefinedManifestStr(sym.tp, elems.map(t => (t._1,makeManifestStr(t._2.asInstanceOf[Sym[Any]],t._2 match { case Def(x) => x }))))
  //   case _ => 
  //     // Predef.println("did not find struct for def " + rhs.toString)
  //     // println("did not find struct for sym + " + sym + " with def " + rhs.toString)
  //     // "manifest[" + remap(sym.tp) + "]"
  //     makeManifestStr(sym.tp)
  // }
  */
  // variable manifests wrap our refined manifests..
  def unvar[T](m: Manifest[T]) = {
    // hack
    // if (m.toString.contains("Variable")) {
    // if (m <:< manifest[Variable[Any]]) {
    if (m.erasure.getSimpleName == "Variable") {
      // println("found a variable manifest")
      m.typeArguments(0).asInstanceOf[Manifest[Any]]
    } 
    else {
      m.asInstanceOf[Manifest[Any]]
    }
  }  
    
  /**
   * Another method to de-duplicate struct names by retaining the DS-name
   * This is basically a restaging version of the structName functionality in LMS - at some point we should try to consolidate these.
   */   
  val restagedStructs = new scala.collection.mutable.HashSet[String]()
  // hacky way of distinguishing domain-specific type names from unwrapped Delite type names
  // val restageStructNameBlacklist = Set(/*"Tuple2",*/"Record"/*,"Variable","DeliteArray","DeliteArrayBuffer"*/) 
  
  // we need this so we don't call restageStructName on primitives (i.e. non structs)
  def remapOrStructRename(m: Manifest[_]): String = unvar(m) match {
    case s if s <:< manifest[Record] && !inRestageStructName => restageStructName(s)
    case s if s <:< manifest[Record] => "Record"

    // special treatment for Delite collections... can we unify this?
    case s if s.erasure.getSimpleName == "DeliteArray" => s.typeArguments(0) match {
      case StructType(_,_) if !inRestageStructName => restageStructName(s)
      case s if s <:< manifest[Record] => restageStructName(s)
      case _ => "DeliteArray[" + remapOrStructRename(s.typeArguments(0)) + "]"
    }
    case s if s.erasure.getSimpleName == "DeliteArrayBuffer" => s.typeArguments(0) match {
      case StructType(_,_) if !inRestageStructName => restageStructName(s)
      case s if s <:< manifest[Record] => restageStructName(s)
      case _ => "DeliteArrayBuffer[" + remapOrStructRename(s.typeArguments(0)) + "]"
    }
    case s if isSubtype(s.erasure,classOf[DeliteCollection[_]]) => s.typeArguments(0) match {
      case StructType(_,_) if !inRestageStructName => restageStructName(s)
      case s if s <:< manifest[Record] => restageStructName(s)
      case _ => "DeliteCollection[" + remapOrStructRename(s.typeArguments(0)) + "]"
    }
    
    case s@StructType(_,_) if !inRestageStructName => restageStructName(s)
    case StructType(_,_) => "Record"

    case s => remap(s)
  }  
  def withoutStructNameRemap[T](tp: Manifest[T]) = {
    inRestageStructName = true
    val x = remap(tp)    
    inRestageStructName = false
    x    
  }
  def expandName[T](tp: Manifest[T]): String = tp match {
    case rm: RefinedManifest[T] => tp.erasure.getSimpleName + rm.fields.map(t => expandName(t._2)).mkString("")
    case _ => tp.erasure.getSimpleName
  }
  def restageStructName[T](tpIn: Manifest[T]): String = {    
    val tp = unvar(tpIn)
    // DenseVectorInt extends DeliteCollection[Int]
    // Tuple2IntInt extends Record
    // Predef.println("restageStructName called on:  " + tp.toString)
    val cls = tp.erasure
    val dsName = /*cls.getSimpleName*/ expandName(tp) + tp.typeArguments.map(expandName(_)).mkString("") //+ cls.getInterfaces.map(_.getSimpleName).mkString("")
    // if (restageStructNameBlacklist.contains(dsName) /*|| restageStructNameBlacklist.contains(cls.getSimpleName)*/) {
    //   Predef.println("skipping restageStructName for " + tp.toString)
    //   Predef.println("dsName: " + dsName)
    //   Predef.println("simpleName: " + cls.getSimpleName)
    //   return restageStructNameRemap(tp) 
    // }
    if (!restagedStructs.contains(dsName)) {
      val superCls = tp match {
        // TODO: this unwrapping is just to avoid an infinite loop but is redundant with the unwrapping in remapOrStructRename -- need to find a better mechanism, i.e. toggling or scoping inRestageStructName
        case s if s.erasure.getSimpleName == "DeliteArray" => "DeliteArray[" + remapOrStructRename(s.typeArguments(0)) + "]"
        case s if s.erasure.getSimpleName == "DeliteArrayBuffer" => "DeliteArrayBuffer[" + remapOrStructRename(s.typeArguments(0)) + "]"      
        case s if isSubtype(s.erasure,classOf[DeliteCollection[_]]) => "DeliteCollection[" + remapOrStructRename(s.typeArguments(0)) + "]" 
        case _ => withoutStructNameRemap(tp)
      }
      tpeStream.println("abstract class " + dsName + " extends " + superCls)
      restagedStructs += dsName
    }
    dsName
    //remap(sym.tp) + " with " + dsName
  }
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {    
    // bound sym hack
    case Bind(x) => emitValDef(sym, "bind(" + quote(x) + ")")

    // data exchange
    case ReturnScopeResult(n) => emitValDef(sym, "setScopeResult(" + quote(n) + ")")
    case LastScopeResult() => emitValDef(sym, "getScopeResult") 
    
    // scala
    case m@HashMapNew() => emitValDef(sym, "HashMap[" + remap(m.mK) + "," + remap(m.mV) + "]()")
    case HashMapApply(m,k) => emitValDef(sym, quote(m) + "(" + quote(k) + ")")
    case HashMapUpdate(m,k,v)  => emitValDef(sym, quote(m) + "(" + quote(k) + ") = " + quote(v))
    case HashMapContains(m,i) => emitValDef(sym, quote(m) + ".contains(" + quote(i) + ")")  
    case ObjBrApply(f) => emitValDef(sym, "BufferedReader(" + quote(f) + ")")
    case ObjFrApply(s) => emitValDef(sym, "FileReader(" + quote(s) + ")")    
    case ThrowException(m) => emitValDef(sym, "fatal(" + quote(m) + ")")
    case NewVar(init) => stream.println("var " + quote(sym) + " = " + quote(init))
    case ObjIntegerParseInt(s) => emitValDef(sym, "Integer.parseInt(" + quote(s) + ")")
    case IntFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValueL()")
    case RepIsInstanceOf(x,mA,mB) => emitValDef(sym, quote(x) + ".isInstanceOf[Rep[" + remap(mB) + "]]")
    case RepAsInstanceOf(x,mA,mB) => emitValDef(sym, quote(x) + ".asInstanceOf[Rep[" + remap(mB) + "]]")    
    case MathMax(x,y) => emitValDef(sym, "Math.max(" + quote(x) + ", " + quote(y) + ")")
    case MathLog(x) => emitValDef(sym, "Math.log(" + quote(x) + ")")
    case MathSqrt(x) => emitValDef(sym, "Math.sqrt(" + quote(x) + ")")
    case MathExp(x) => emitValDef(sym, "Math.exp(" + quote(x) + ")")
    // TODO: this manifest doesn't appear to be correct if we come from a struct where we explicitly created our own RefinedManifest
    case ObjectUnsafeImmutable(x) => emitValDef(sym, quote(x) + ".unsafeImmutable()")//("+makeManifestStr(unvar(x.tp))+",implicitly[SourceContext])")

    // profiling
    case DeliteProfileStart(x,deps) if deps == Nil =>  emitValDef(sym, "tic(" + quote(x) + ")") 
    case DeliteProfileStart(x,deps) => emitValDef(sym, "tic(" + quote(x) + ", " + deps.map(quote(_)).mkString(",") + ")") 
    case DeliteProfileStop(x,deps) if deps == Nil =>  emitValDef(sym, "toc(" + quote(x) + ")")
    case DeliteProfileStop(x,deps) => emitValDef(sym, "toc(" + quote(x) + ", " + deps.map(quote(_)).mkString(",") + ")")

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
    // case a@DeliteArrayNew(n) if sym.tp.typeArguments(0) <:< manifest[Record] => emitValDef(sym, "DeliteArray[" + restageStructName(a.mA) + "](" + quote(n) + ")")
    // HACK: GIterable should be a Record
    case a@DeliteArrayNew(n) if sym.tp.typeArguments(0).erasure.getSimpleName == "GIterable" => emitValDef(sym, "DeliteArray[" + restageStructName(a.mA) + "](" + quote(n) + ")")
    case a@DeliteArrayNew(n) => emitValDef(sym, "DeliteArray[" + remap(a.mA) + "](" + quote(n) + ")")
    // case a@DeliteArrayNew(n) => emitValDef(sym, "DeliteArray[" + remap(a.mA) + "](" + quote(n) + ")")
    //case DeliteArrayCopy(src,srcPos,dest,destPos,len) => emitValDef(sym, "darray_unsafe_copy(" + quote(src) + "," + quote(srcPos) + "," + quote(dest) + "," + quote(destPos) + "," + quote(len) + ")")
    case DeliteArrayCopy(src,srcPos,dest,destPos,len) => emitValDef(sym, "darray_copy(" + quote(src) + "," + quote(srcPos) + "," + quote(dest) + "," + quote(destPos) + "," + quote(len) + ")")
    case DeliteArrayGetActSize() => emitValDef(sym, "darray_unsafe_get_act_size()")
    case DeliteArraySetActBuffer(da) => emitValDef(sym, "darray_set_act_buf(" + quote(da) + ")")
    case DeliteArraySetActFinal(da) => emitValDef(sym, "darray_set_act_final(" + quote(da) + ")")
    
    // structs
    // case s@SimpleStruct(tag, elems) =>
    case Struct(tag, elems) =>
      // oops.. internal scalac error
      // emitValDef(sym, "anonStruct(" + elems.asInstanceOf[Seq[(String,Rep[Any])]].map{case (k,v) => "(\"" + k + "\", " + quote(v) + ")" }.mkString(",") + ")")
       
      val isVar = elems(0)._2 match {
        case Def(Reflect(NewVar(x),u,es)) => true
        case x: Exp[Var[Any]] if x.tp.toString.contains("Variable") => true
        case _ => false
      }
      val tp = /*if (isVar) "Var["+remap(sym.tp)+"]" else*/ remap(sym.tp)       
      // Predef.println("manifest simple name: " + sym.tp.erasure.getSimpleName)
      // Predef.println("manifest type arguments: " + sym.tp.typeArguments.map(_.erasure.getSimpleName))
      val structMethod = if (isVar) "mstruct" else "struct"      
      emitValDef(sym, structMethod + "[" + restageStructName(sym.tp) + "](" + quoteTag(tag,sym.tp) + ", " + elems.asInstanceOf[Seq[(String,Rep[Any])]].map{t => "(\"" + t._1 + "\", " + quote(t._2) + ")" }.mkString(",") + ")") //+
        // "(" + makeManifestStr(sym,rhs) + "," +
        // "(new Manifest[" + tp + "]{ def erasure = classOf[" + tp + "]; override def typeArguments = List("+sym.tp.typeArguments.map(a => makeManifestStr(a)).mkString(",") + ")}," + 
        // "(new RefinedManifest[" + tp + "]{ def erasure = classOf[" + tp + "]; override def typeArguments = List("+unvar(sym.tp).typeArguments.map(a => makeManifestStr(a)).mkString(",") + "); def fields = List(" + elems.map(t => ("\""+t._1+"\"","manifest["+remap(t._2.tp)+"]")).mkString(",") + ")}," +
        // "implicitly[Overloaded1], implicitly[SourceContext])")
      
    
    case FieldApply(struct, index) => 
       emitValDef(sym, "field["+remap(unvar(sym.tp))+"](" + quote(struct) + ",\"" + index + "\")")
      //emitValDef(sym, "field(" + quote(struct) + ",\"" + index + "\")("+makeManifestStr(unvar(sym.tp))+",implicitly[SourceContext])")    
      
    case FieldUpdate(struct, index, rhs) => emitValDef(sym, "field_update[" + remap(unvar(sym.tp)) + "](" + quote(struct) + ",\"" + index + "\"," + quote(rhs) + ")")
    case NestedFieldUpdate(struct, fields, rhs) => 
      assert(fields.length > 0)      
      // x34.data.id(x66)
      // field[T](field[Record](x34, "data"), "id")
      if (fields.length == 1) { // not nested
        emitValDef(sym, "field_update[" + remap(rhs.tp) + "](" + quote(struct) + ",\"" + fields(0) + "\"," + quote(rhs) + ")")
      }
      else {
        //val f = "field[Record](" + quote(struct) + ", \"" + fields.head + "\")"
        //emitValDef(sym, "field_update(" + recordFieldLookup(f, fields.tail, rhs.tp) + ", " + quote(rhs) + ")")        
        emitValDef(sym, "field_update(" + recordFieldLookup(struct, struct.tp, "", fields) + ", " + quote(rhs) + ")")        
      }
   
    case StructUpdate(struct, fields, idx, x) =>
      assert(fields.length > 0)
      if (fields.length == 1) { // not nested
        emitValDef(sym, "darray_update(field[DeliteArray["+remap(x.tp)+"]](" + quote(struct) + ", \"" + fields.head + "\"), " + quote(idx) + ", " + quote(x) + ")")
      }
      else {
        //val structFieldTpes = struct match {
        //  case StructType(tag,fields) => fields.map(_._2)
        //}
        //val f = "field["+remap(structFieldTpes(0))+"](" + quote(struct) + ", \"" + fields.head + "\")"
        //emitValDef(sym, "darray_update(" + recordFieldLookup(f, fields.tail, struct.tp) + ", " + quote(idx) + ", " + quote(x) + ")")
        emitValDef(sym, "darray_update(" + recordFieldLookup(struct, struct.tp, "", fields) + ", " + quote(idx) + ", " + quote(x) + ")")
      }
    
    // delite ops
    case s:DeliteOpSingleTask[_] => 
      // each stm inside the block must be restageable..
      stream.print("val " + quote(sym) + " = single({")
      emitBlock(s.block)
      //stream.print("val " + quote(sym) + " = ({")
      stream.println(quote(getBlockResult(s.block)))
      stream.println("})")
      //stream.println(quote(getBlockResult(s.block)) + ".unsafeImmutable()")//"("+makeManifestStr(unvar(sym.tp))+",implicitly[SourceContext])")
      
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
    // append
    stream.println("{")
    stream.println(makeBoundVarArgs(elem.allocVal,elem.eV,op.v))
    emitBlock(elem.buf.append)
    stream.println(quote(getBlockResult(elem.buf.append)))
    stream.println("},")
    // setSize
    stream.println("{")
    stream.println(makeBoundVarArgs(elem.allocVal,elem.sV))
    emitBlock(elem.buf.setSize)
    stream.println(quote(getBlockResult(elem.buf.setSize)))
    stream.println("},")
    // allocRaw
    stream.println("{")
    stream.println(makeBoundVarArgs(elem.allocVal,elem.sV))  
    emitBlock(elem.buf.allocRaw)
    stream.println(quote(getBlockResult(elem.buf.allocRaw)))
    stream.println("},")
    // copyRaw
    stream.println("{")
    stream.println(makeBoundVarArgs(elem.buf.aV,elem.buf.iV,elem.allocVal,elem.buf.iV2,elem.sV))  
    emitBlock(elem.buf.copyRaw)
    stream.println(quote(getBlockResult(elem.buf.copyRaw)))    
    stream.println("}")
  }
  
  
  def emitRestageableLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    // break the multiloops apart, they'll get fused again anyways
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) => 
        stream.println("val " + quote(sym) + " = collect[" + remap(elem.mA) + "," + remap(elem.mI) + "," + remap(elem.mCA) + "](")
        // stream.println("val " + quote(sym) + " = collect(")
        // loop size
        stream.println(quote(op.size) + ",")
        // alloc func
        stream.println("{")
        stream.println(makeBoundVarArgs(elem.sV))
        emitBlock(elem.allocN)
        stream.println(quote(getBlockResult(elem.allocN)))
        stream.println("},")
        // func
        stream.println("{")
        stream.println(makeBoundVarArgs(elem.eV,op.v))
        emitBlock(elem.func)
        stream.println(quote(getBlockResult(elem.func)))
        stream.println("},")
        // update
        stream.println("{")
        stream.println(makeBoundVarArgs(elem.allocVal,elem.eV,op.v))
        emitBlock(elem.update)
        stream.println(quote(getBlockResult(elem.update)))
        stream.println("},")
        // finalizer
        stream.println("{")
        stream.println(makeBoundVarArgs(elem.allocVal))
        emitBlock(elem.finalizer)
        stream.println(quote(getBlockResult(elem.finalizer)))
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
