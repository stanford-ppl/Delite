package ppl.dsl.optiql.ops

import java.io.PrintWriter
import scala.virtualization.lms.common.{ScalaGenBase, ScalaGenEffect, BaseExp, Base}


trait ResultOps extends Base {
  
  class Result extends Struct[Rep]
      
  def __new[T<:Struct[Rep]:Manifest](fields: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = newResult[T](fields)
    
  def newResult[T:Manifest](fields: Seq[(String,Boolean,Rep[T] => Rep[_])]): Rep[T]
  
  implicit def RepToResultOps(r: Rep[Result]) = new ResultOpsCls(r)
  
  class ResultOpsCls(r: Rep[Result]) {
    def selectDynamic[T](n: String): Rep[T] = resultFieldAccess(r,n)
  }
  
  def resultFieldAccess[T:Manifest](r: Rep[Result], field: String): Rep[T]
  /*
  class ApplyDynamicOps {
    def applyDynamic[T](n: String)(as: AnyRef*): Rep[T] = error(n + as.mkString("(", ",", ")"))
  }
  
  implicit def applyDynamicOps[T <: Result](qual: Rep[T]): ApplyDynamicOps = new ApplyDynamicOps
    
 */
  
}

trait ResultOpsExp extends ResultOps with BaseExp {
    
  case class CreateResult[T](fields: Seq[(String,Rep[_])]) extends Def[T]
  case class ResultFieldAccess[T](res: Rep[Result], field: String) extends Def[T]
    
  def newResult[T:Manifest](fields: Seq[(String,Boolean,Rep[T] => Rep[_])]): Rep[T] = {
    val x: Sym[T] = fresh[T]
    val flatFields: Seq[(String, Rep[_])] = fields map {case (n, _, rhs) => (n, rhs(x))}
    val nDef: Def[T] = CreateResult(flatFields)
    createDefinition(x, nDef)
    return x
  }
  
  def resultFieldAccess[T:Manifest](r: Rep[Result], field: String): Rep[T] = ResultFieldAccess[T](r,field)
    
    
  override def syms(e: Any): List[Sym[Any]] = e match { 
    //case QueryableGroupBy(s,v,k) => syms(s) 
    case CreateResult(fields) => fields flatMap{case (n, rhs) => syms(rhs)} toList
    case _ => super.syms(e)
  }
  
  override def boundSyms(e: Any): List[Sym[Any]] = e match {    
    //case QueryableGroupBy(s,v,k) => v::syms(k)
    case CreateResult(fields) => Nil
    case _ => super.boundSyms(e)
  }  
  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case CreateResult(fields) => fields flatMap{case (n, rhs) => freqNormal(rhs)} toList
    case _ => super.symsFreq(e)
  }
    
 
}

trait ScalaGenResultOps extends ScalaGenBase {

    val IR:ResultOpsExp
    import IR._

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
        case CreateResult(fields) => emitValDef(sym, "new {\n" + fields.map{case (n,rhs) => "\tval " + n + " = " + quote(rhs)}.reduceLeft[String]{(acc, n) => acc + "\n" + n} + "\n}")
        case ResultFieldAccess(res, field) => emitValDef(sym, quote(res) + "." + field + " //field access")
        case _ => super.emitNode(sym,rhs)
    }

}
