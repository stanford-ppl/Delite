package ppl.delite.framework.datastructures

import java.io.PrintWriter
import scala.virtualization.lms.common._

trait RecordOps extends Base {
  
  class Record extends Struct[Rep]
      
  def __new[T<:Struct[Rep]:Manifest](fields: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = newRecord[T](fields)
    
  def newRecord[T:Manifest](fields: Seq[(String,Boolean,Rep[T] => Rep[_])]): Rep[T]
  
  implicit def RepToRecordOps(r: Rep[Record]) = new RecordOpsCls(r)
  
  class RecordOpsCls(r: Rep[Record]) {
    def selectDynamic[T:Manifest](n: String): Rep[T] = recordFieldAccess[T](r,n)
  }
  
  def recordFieldAccess[T:Manifest](r: Rep[Record], field: String): Rep[T]
  /*
  class ApplyDynamicOps {
    def applyDynamic[T](n: String)(as: AnyRef*): Rep[T] = error(n + as.mkString("(", ",", ")"))
  }
  
  implicit def applyDynamicOps[T <: Record](qual: Rep[T]): ApplyDynamicOps = new ApplyDynamicOps
    
 */
  
}

trait RecordOpsExp extends RecordOps with BaseExp {
    
  case class CreateRecord[T:Manifest](fields: Seq[(String,Rep[_])]) extends Def[T]
  case class RecordFieldAccess[T:Manifest](res: Rep[Record], field: String) extends Def[T]
    
  def newRecord[T:Manifest](fields: Seq[(String,Boolean,Rep[T] => Rep[_])]): Rep[T] = {
    val x: Sym[T] = fresh[T]
    val flatFields: Seq[(String, Rep[_])] = fields map {case (n, _, rhs) => (n, rhs(x))}
    val nDef: Def[T] = CreateRecord(flatFields)
    createDefinition(x, nDef)
    return x
  }
  
  def recordFieldAccess[T:Manifest](r: Rep[Record], field: String): Rep[T] = RecordFieldAccess[T](r,field)
    
  override def syms(e: Any): List[Sym[Any]] = e match { 
    case CreateRecord(fields) => fields flatMap{case (n, rhs) => syms(rhs)} toList
    case _ => super.syms(e)
  }
  
  override def boundSyms(e: Any): List[Sym[Any]] = e match {    
    case CreateRecord(fields) => Nil
    case _ => super.boundSyms(e)
  }  
  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case CreateRecord(fields) => fields flatMap{case (n, rhs) => freqNormal(rhs)} toList
    case _ => super.symsFreq(e)
  }
    
 
}

trait ScalaGenRecordOps extends ScalaGenBase {

    val IR:RecordOpsExp
    import IR._

    override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
        case CreateRecord(fields) => emitValDef(sym, "new {\n" + fields.map{case (n,rhs) => "\tval " + n + " = " + quote(rhs)}.reduceLeft[String]{(acc, n) => acc + "\n" + n} + "\n}")
        case RecordFieldAccess(res, field) => emitValDef(sym, quote(res) + "." + field + " //field access")
        case _ => super.emitNode(sym,rhs)
    }

}
