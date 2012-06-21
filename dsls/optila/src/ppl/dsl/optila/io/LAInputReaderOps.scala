package ppl.dsl.optila.io

import java.io.{PrintWriter}
import scala.virtualization.lms.common.{TupleOpsExp, Base, BaseFatExp}
import scala.reflect.SourceContext
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optila._

trait LAInputReaderOps extends Base {
  this: OptiLA =>
  
  // default file format is m lines with n floats per line, each float separated by whitespaces
  // (same as matlab .dat)    
  def readMatrix(filename: Rep[String])(implicit ctx: SourceContext) = readMatrix[Double](filename, s => s.toDouble)
  def readMatrix(filename: Rep[String], delim: Rep[String])(implicit ctx: SourceContext) = readMatrix[Double](filename, s => s.toDouble, delim)
  def readVector(filename: Rep[String])(implicit ctx: SourceContext) = readVector[Double](filename, v => v(unit(0)).toDouble)  
  
  // generic versions can be used to read files with different contents
  def readMatrix[Elem:Manifest](filename: Rep[String], schemaBldr: Rep[String] => Rep[Elem], delim: Rep[String] = unit("\\\\s+"))(implicit ctx: SourceContext) = obj_lainput_read_matrix(filename, schemaBldr, delim)
  def readVector[Row:Manifest](filename: Rep[String], schemaBldr: Rep[DenseVector[String]] => Rep[Row], delim: Rep[String] = unit("\\\\s+"))(implicit ctx: SourceContext) = obj_lainput_read_vector(filename, schemaBldr, delim)  

  def obj_lainput_read_matrix[Elem:Manifest](filename: Rep[String], schemaBldr: Rep[String] => Rep[Elem], delim: Rep[String])(implicit ctx: SourceContext): Rep[DenseMatrix[Elem]]
  def obj_lainput_read_vector[Row:Manifest](filename: Rep[String], schemaBldr: Rep[DenseVector[String]] => Rep[Row], delim: Rep[String])(implicit ctx: SourceContext): Rep[DenseVector[Row]]  
}

trait LAInputReaderOpsExp extends LAInputReaderOps with BaseFatExp { 
  this: OptiLAExp with LAInputReaderImplOps with TupleOpsExp =>
  
  case class LAInputReadMatrix[Elem:Manifest](filename: Exp[String], schemaBldr: Exp[String] => Exp[Elem], delim: Exp[String])
    extends DeliteOpSingleWithManifest[Elem,DenseMatrix[Elem]](reifyEffects(lainput_read_matrix_impl(filename, schemaBldr, delim)))
  
  case class LAInputReadVector[Row:Manifest](filename: Exp[String], schemaBldr: Exp[DenseVector[String]] => Exp[Row], delim: Exp[String])
    extends DeliteOpSingleWithManifest[Row,DenseVector[Row]](reifyEffects(lainput_read_vector_impl(filename, schemaBldr, delim)))
  
  def obj_lainput_read_matrix[Elem:Manifest](filename: Exp[String], schemaBldr: Exp[String] => Exp[Elem], delim: Exp[String])(implicit ctx: SourceContext) = reflectEffect(LAInputReadMatrix(filename, schemaBldr, delim))
  def obj_lainput_read_vector[Row:Manifest](filename: Exp[String], schemaBldr: Exp[DenseVector[String]] => Exp[Row], delim: Exp[String])(implicit ctx: SourceContext) = reflectEffect(LAInputReadVector(filename, schemaBldr, delim))  
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(d@LAInputReadVector(fn,s,delim), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,d) } with LAInputReadVector(f(fn),f(s),f(delim))(d.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(d@LAInputReadMatrix(fn,s,delim), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,d) } with LAInputReadMatrix(f(fn),f(s),f(delim))(d.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]  
}


//trait ScalaGenLAInputReaderOps extends ScalaGenBase {
//  val IR: LAInputReaderOpsExp
//  import IR._
//
//  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
//    case LAInputRead(filename) => emitValDef(sym, base + ".read(" + quote(filename) + ")")
//    case LAInputReadVector(filename) => emitValDef(sym, base + ".readVector(" + quote(filename) + ")")
//    case _ => super.emitNode(sym, rhs)
//  }
//}
