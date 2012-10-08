package ppl.delite.framework.datastructures

import java.io.{File,FileWriter,PrintWriter}
import scala.virtualization.lms.common._
import ppl.delite.framework.ops.DeliteOpsExp

trait DeliteStructsExp extends StructExp { this: DeliteOpsExp =>
	
  abstract class DeliteStruct[T:Manifest] extends AbstractStruct[T] with DeliteOp[T] {
    type OpType <: DeliteStruct[T]
    val tag = ClassTag[T](structName(manifest[T]))

    def copyTransformedElems(e: => Seq[(String, Rep[Any])]): Seq[(String, Rep[Any])] = 
      original.map(p=>(p._2.asInstanceOf[OpType]).elems.map(e=>(e._1,p._1(e._2)))).getOrElse(e)
  }

}

trait ScalaGenDeliteStruct extends BaseGenStruct {
  val IR: DeliteStructsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Struct(tag, elems) =>
      registerStruct(tag, elems)
      emitValDef(sym, "new " + structName(tag) + "(" + elems.map(e => quote(e._2)).mkString(",") + ")")
      printlog("WARNING: emitting " + structName(tag) + " struct " + quote(sym))    
    case FieldApply(struct, index) =>
      emitValDef(sym, quote(struct) + "." + index)
      printlog("WARNING: emitting field access: " + quote(struct) + "." + index)
    case FieldUpdate(struct, index, rhs) =>
      emitValDef(sym, quote(struct) + "." + index + " = " + quote(rhs))
      printlog("WARNING: emitting field update: " + quote(struct) + "." + index)
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]) = m match {
    case s if s <:< manifest[Record] => structName(m)
    case _ => super.remap(m)
  }
  
  override def emitDataStructures(path: String) {
    val stream = new PrintWriter(path + "DeliteStructs.scala")
    stream.println("package generated.scala")
    for ((tag, elems) <- encounteredStructs) {
      stream.println()
      stream.print("case class " + structName(tag) + "(")
      val fields = for ((idx,tp) <- elems) yield {
      	val genType = if (tp.erasure.getSimpleName == "Variable") 
      	  "var "
        else 
      	  "val "
      	genType + idx + ": " + remap(tp)
      }
      stream.print(fields.mkString(", "))
      stream.println(")")
    }
    stream.close()
    super.emitDataStructures(path)
  }
}
