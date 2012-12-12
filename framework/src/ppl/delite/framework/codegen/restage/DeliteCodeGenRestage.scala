package ppl.delite.framework.codegen.restage

import java.io.PrintWriter
import reflect.{SourceContext, RefinedManifest}
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.{DeliteRestageOps,DeliteRestageOpsExp}
import ppl.delite.framework.ops.{DeliteOpsExp,DeliteCollectionOpsExp,ScalaGenDeliteCollectionOps}

trait TargetRestage extends Target {
  import IR._

  val name = "Restage"
}

trait RestageCodegen extends ScalaCodegen with Config {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "scala"

  override def toString = "restage"

  override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
    val staticData = getFreeDataBlock(body)
    
    println("--RestageCodegen emitSource")
    
    withStream(out) {
      emitBlock(body)
      // stream.println("setLastScopeResult(" + quote(getBlockResult(body)) + ")")
    }    
    
    staticData
  }  
}

trait RestageFatCodegen extends GenericFatCodegen with RestageCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._
  

}

// for now just lumping all the Delite restage generators together in one place..
trait DeliteCodeGenRestage extends RestageFatCodegen 
  with ScalaGenDeliteCollectionOps with ScalaGenDeliteArrayOps with ScalaGenDeliteStruct with DeliteScalaGenAllOverrides {
    
  val IR: Expressions with Effects with FatExpressions with DeliteRestageOpsExp 
          with IOOpsExp with PrimitiveOpsExp
          with DeliteCollectionOpsExp with DeliteArrayFatExp with DeliteOpsExp with DeliteAllOverridesExp
  import IR._


  override def remap[A](m: Manifest[A]): String = {
    val ms = manifest[String]
    m match {
      case `ms` => "String"
      case s if s <:< manifest[Record] => "Rep[Record]" // should only be calling 'field' on records at this level
      case s if s.erasure.getSimpleName == "Tuple2" => "Rep[Record]" // this is custom overridden in ScalaGenTupleOps
      case _ => 
        // Predef.println("calling remap on: " + m.toString)
        super.remap(m)
    }
  }
  
  // dropping all non-SoA tags for now
  def quote[T](tag: StructTag[T]): String = tag match {
    case ClassTag(name) => "ClassTag(\"erased\")"
    case NestClassTag(elem) => "ClassTag(\"erased\")"
    case AnonTag(fields) => "ClassTag(\"erased\")"
    case SoaTag(base, length) => "SoaTag(" + quote(base) + ", " + quote(length) + ")"
    case MapTag() => "ClassTag(\"erased\")"
  } 
  
  def remapTag[T](tag: StructTag[T]): String = tag match {
    case SoaTag(b,l) => "[DeliteArray[Any]]"
    case _ => ""
  }
     
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ReturnScopeResult(n) => emitValDef(sym, "setScopeResult(" + quote(n) + ")")
    case LastScopeResult() => emitValDef(sym, "getScopeResult") 
    case a@DeliteArrayNew(n) => emitValDef(sym, "DeliteArray[" + remap(a.mA) + "](" + quote(n) + ")")
    case DeliteArrayCopy(src,srcPos,dest,destPos,len) => emitValDef(sym, "darray_copy(" + quote(src) + "," + quote(srcPos) + "," + quote(dest) + "," + quote(destPos) + "," + quote(len) + ")")
    
    case ObjBrApply(f) => emitValDef(sym, "BufferedReader(" + quote(f) + ")")
    case ObjFrApply(s) => emitValDef(sym, "FileReader(" + quote(s) + ")")    
    case ThrowException(m) => emitValDef(sym, "fatal(" + quote(m) + ")")
    case NewVar(init) => stream.println("var " + quote(sym) + " = " + quote(init))
    case ObjIntegerParseInt(s) => emitValDef(sym, "Integer.parseInt(" + quote(s) + ")")
    
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

    
    // structs
    // case s@SimpleStruct(tag, elems) =>
    case Struct(tag, elems) =>
      // oops.. internal scalac error
      // emitValDef(sym, "anonStruct(" + elems.asInstanceOf[Seq[(String,Rep[Any])]].map{case (k,v) => "(\"" + k + "\", " + quote(v) + ")" }.mkString(",") + ")")
       
       // struct type arg remaps to a generated struct name that doesn't exist... is the struct type important?
      // emitValDef(sym, "struct[" + remap(s.mT) + "](" + quote(tag) + ", Seq(" + elems.asInstanceOf[Seq[(String,Rep[Any])]].map{t => "(\"" + t._1 + "\", " + quote(t._2) + ")" }.mkString(",") + "))")
      emitValDef(sym, "struct" + remapTag(tag) + "(" + quote(tag) + ", " + elems.asInstanceOf[Seq[(String,Rep[Any])]].map{t => "(\"" + t._1 + "\", " + quote(t._2) + ")" }.mkString(",") + ")")
    
    case FieldApply(struct, index) => emitValDef(sym, "field[" + remap(sym.tp) + "](" + quote(struct) + ",\"" + index + "\")")    
    case FieldUpdate(struct, index, rhs) => emitValDef(sym, "field_update[" + remap(sym.tp) + "](" + quote(struct) + ",\"" + index + "\"," + quote(rhs) + ")")
    case NestedFieldUpdate(struct, fields, rhs) => 
      assert(fields.length == 1) // not handling true nesting yet.. need to somehow call field[..] recursively
      emitValDef(sym, "field_update(" + quote(struct) + ",\"" + fields.head + "\"," + quote(rhs) + ")")
    
    
    // delite ops
    case s:DeliteOpSingleTask[_] => 
      // each stm inside the block must be restageable..
      emitBlock(s.block)
      stream.print("val " + quote(sym) + " = ")
      stream.println(quote(getBlockResult(s.block)))
      
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
  
  def emitRestageableLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    // break the multiloops apart, they'll get fused again anyways
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) => 
        stream.println("val " + quote(sym) + " = collect(")
        stream.println(quote(op.size) + ",")
        stream.println("{")
        stream.println(quote(elem.sV) + ": Rep[" + remap(elem.sV.tp) + "] => ")
        emitBlock(elem.allocN)
        stream.println(quote(getBlockResult(elem.allocN)))
        stream.println("},")
        stream.println("{")
        stream.println("(" + quote(elem.eV) + ": Rep[" + remap(elem.eV.tp) + "]," + quote(op.v) + ": Rep[" + remap(op.v.tp) + "]) => ")
        emitBlock(elem.func)
        stream.println(quote(getBlockResult(elem.func)))
        stream.println("},")
        stream.println("{")
        stream.println("(" + quote(elem.allocVal) + ": Rep[" + remap(elem.allocVal.tp) + "]," + quote(elem.eV) + ": Rep[" + remap(elem.eV.tp) + "]," + quote(op.v) + ": Rep[" + remap(op.v.tp) + "]) => ")
        emitBlock(elem.update)
        stream.println(quote(getBlockResult(elem.update)))
        stream.println("},")
        stream.println("{")
        stream.println(quote(elem.allocVal) + ": Rep[" + remap(elem.allocVal.tp) + "] => ")
        emitBlock(elem.finalizer)
        stream.println(quote(getBlockResult(elem.finalizer)))
        stream.println("})")
        // TODO: filter
      case (sym, elem: DeliteForeachElem[_]) => 
        Predef.println("error: tried to restage DeliteForeachElem but no impl yet")
      case (sym, elem: DeliteReduceElem[_]) =>    
        Predef.println("error: tried to restage DeliteReduceElem but no impl yet")
    }
  }
}
