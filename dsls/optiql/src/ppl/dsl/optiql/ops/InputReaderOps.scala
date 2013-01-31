package ppl.dsl.optiql.ops

import scala.virtualization.lms.common.{Base,BaseFatExp}
import ppl.dsl.optiql._
import ppl.delite.framework.datastructures.{DeliteArray,DeliteArrayBuffer}
import java.io.PrintWriter
import reflect.{RefinedManifest,SourceContext}

trait InputReaderOps extends Base { this: OptiQL =>

  object TableInputReader {
    def apply[T<:Record:Manifest](path: Rep[String], shape: Rep[T], separator: Rep[String] = unit("|")) = optiql_table_input_reader(path, shape, separator)
  }

  def optiql_table_input_reader[T<:Record:Manifest](path: Rep[String], shape: Rep[T], separator: Rep[String]): Rep[Table[T]]

}

trait InputReaderOpsExp extends InputReaderOps with BaseFatExp { this: OptiQLExp with InputReaderImplOps =>

  def optiql_table_input_reader[T<:Record:Manifest](path: Rep[String], shape: Rep[T], separator: Rep[String]) = {
    val array = DeliteFileReader.readLines(path){ line => optiql_table_input_reader_impl(line, shape, separator) }
    Table(array, array.length)
  }

}

trait InputReaderImplOps { this: OptiQL =>
  def optiql_table_input_reader_impl[T<:Record:Manifest](line: Rep[String], shape: Rep[T], separator: Rep[String]): Rep[T]
}

trait InputReaderImplOpsStandard extends InputReaderImplOps { this: OptiQLLift with OptiQLExp =>

  def optiql_table_input_reader_impl[T<:Record:Manifest](line: Rep[String], shape: Rep[T], separator: Rep[String]) = {
    val fields = line.split("\\\\Q" + separator + "\\\\E")
    createRecord(fields, shape)
  }

  private def createRecord[T<:Record:Manifest](record: Rep[Array[String]], shape: Rep[T]) = {
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
        //case "Float" => (field, Float.parseFloat(record(i))
        case "Int" => (field, Integer.parseInt(record(i)))
        case "Char" => (field, record(i).charAt(0))
        case d if d.contains("Date") => (field, Date(record(i)))
        case _ => throw new RuntimeException("Unsupported record field type: " + tp.toString)
      }
    }
    
    struct[T](AnonTag(rm), fields)
  }

}
