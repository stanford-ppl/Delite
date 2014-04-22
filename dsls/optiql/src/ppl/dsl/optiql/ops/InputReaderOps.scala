package ppl.dsl.optiql.ops

import scala.virtualization.lms.common.{Base, BaseFatExp, LiftVariables, Record}
import ppl.dsl.optiql._
import ppl.delite.framework.datastructures.{DeliteArray, DeliteArrayBuffer}
import java.io.PrintWriter
import reflect.{RefinedManifest, SourceContext}

trait InputReaderOps extends Base { this: OptiQL =>

  object TableInputReader {
    def apply(path: Rep[String]) = optiql_table_line_reader(path)
    def apply[T<:Record:Manifest](path: Rep[String], separator: Rep[String]): Rep[Table[T]] = optiql_table_input_reader(path, separator)
  }

  def optiql_table_input_reader[T<:Record:Manifest](path: Rep[String], separator: Rep[String]): Rep[Table[T]]
  def optiql_table_line_reader(path: Rep[String]): Rep[Table[String]]
  def optiql_table_from_seq[T:Manifest](elems: Seq[Rep[T]]): Rep[Table[T]]
  def optiql_table_from_string[T<:Record:Manifest](data: Rep[String], rowSep: Rep[String], colSep: Rep[String]): Rep[Table[T]]

}

trait InputReaderOpsExp extends InputReaderOps with BaseFatExp { this: OptiQLExp with InputReaderImplOps =>

  case class OptiQLTableInputReader[T:Manifest](readBlock: Block[Table[T]]) extends DeliteOpSingleWithManifest[T,Table[T]](readBlock)
  case class OptiQLTableFromSeq[T:Manifest](readBlock: Block[Table[T]]) extends DeliteOpSingleWithManifest[T,Table[T]](readBlock)

  def optiql_table_input_reader[T<:Record:Manifest](path: Rep[String], separator: Rep[String]) = {
    Table(DeliteNewFileReader.readLines(path){ line => optiql_table_record_parser_impl[T](line, separator) })
  }

  def optiql_table_line_reader(path: Rep[String]) = {
    Table(DeliteNewFileReader.readLines(path){ line => line })
  }

  def optiql_table_from_seq[T:Manifest](elems: Seq[Rep[T]]) = {
    reflectPure(OptiQLTableFromSeq(reifyEffectsHere(optiql_table_from_seq_impl(elems))))
  }

  //TODO: it's unfortunate that string split returns an Array rather than a DeliteArray, should we change the signature or have a conversion method?
  def optiql_table_from_string[T<:Record:Manifest](data: Rep[String], rowSep: Rep[String], colSep: Rep[String]) = {
    val jarray = data.split(rowSep)
    Table(DeliteArray.fromFunction(jarray.length)(i => optiql_table_record_parser_impl[T](jarray(i), colSep)))
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@OptiQLTableInputReader(block) => reflectPure(new { override val original = Some(f,e) } with OptiQLTableInputReader(f(block))(mtype(e.mA)))(mtype(manifest[A]),implicitly[SourceContext])      
    case Reflect(e@OptiQLTableInputReader(block), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with OptiQLTableInputReader(f(block))(mtype(e.mA)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case e@OptiQLTableFromSeq(block) => reflectPure(new { override val original = Some(f,e) } with OptiQLTableFromSeq(f(block))(mtype(e.mA)))(mtype(manifest[A]),implicitly[SourceContext])      
    case Reflect(e@OptiQLTableFromSeq(block), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with OptiQLTableFromSeq(f(block))(mtype(e.mA)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait InputReaderImplOps { this: OptiQL =>
  def optiql_table_record_parser_impl[T<:Record:Manifest](record: Rep[String], separator: Rep[String]): Rep[T]
  def optiql_table_from_seq[T:Manifest](elems: Seq[Rep[T]]): Rep[Table[T]]
}

trait InputReaderImplOpsStandard extends InputReaderImplOps { this: OptiQLLift with OptiQLExp =>

  def optiql_table_record_parser_impl[T<:Record:Manifest](record: Rep[String], separator: Rep[String]) = {
    val fields = record.split(separator,-1)
    createRecord(fields)
  }

  private def createRecord[T<:Record:Manifest](record: Rep[Array[String]]) = {
    val rm = manifest[T] match {
      case rm: RefinedManifest[T] => rm
      case m => throw new RuntimeException("No RefinedManifest for type " + m.toString)
    }
    val elems = rm.fields

    val fields = Range(0,elems.length) map { i =>
      val (field, tp) = elems(i)
      tp.toString match {
        case s if s.contains("String") => (field, record(i))
        case "Double" => (field, Double.parseDouble(record(i)))
        case "Float" => (field, Float.parseFloat(record(i)))
        case "Boolean" => (field, record(i) == "true")
        case "Int" => (field, Integer.parseInt(record(i)))
        case "Long" => (field, Long.parseLong(record(i)))
        case "Char" => (field, record(i).charAt(0))
        case d if d.contains("Date") => (field, Date(record(i)))
        case _ => throw new RuntimeException("Unsupported record field type: " + tp.toString)
      }
    }
    
    struct[T](AnonTag(rm), fields)
  }

  //TODO: this is a map
  def optiql_table_from_seq_impl[T:Manifest](elems: Seq[Rep[T]]) = {
    val array = DeliteArray[T](elems.length)
    for (i <- (0 until elems.length): Range) {
      array(i) = elems(i)
    }
    Table(array.unsafeImmutable, array.length)
  }

}
