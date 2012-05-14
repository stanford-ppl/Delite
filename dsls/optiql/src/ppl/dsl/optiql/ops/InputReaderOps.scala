package ppl.dsl.optiql.ops

import scala.virtualization.lms.common.ScalaGenEffect
import ppl.dsl.optiql._
import java.io.PrintWriter
import reflect.RefinedManifest

trait InputReaderOps { this: OptiQL =>

  object TableInputReader {
    def apply[T<:Record:Manifest](path: Rep[String], shape: Rep[T], separator: Rep[String] = unit("|")) = optiql_table_input_reader(path, shape, separator)
  }

  def optiql_table_input_reader[T<:Record:Manifest](path: Rep[String], shape: Rep[T], separator: Rep[String]): Rep[DataTable[T]]

}

trait InputReaderOpsExp extends InputReaderOps { this: OptiQLExp with InputReaderImplOps =>

  case class OptiQLTableInputReader[T:Manifest](readBlock: Block[DataTable[T]]) extends DeliteOpSingleTask(readBlock)

  def optiql_table_input_reader[T<:Record:Manifest](path: Rep[String], shape: Rep[T], separator: Rep[String]) = reflectEffect(OptiQLTableInputReader(reifyEffectsHere(optiql_table_input_reader_impl(path,shape,separator))))

}

trait InputReaderImplOps { this: OptiQL =>
  def optiql_table_input_reader_impl[T<:Record:Manifest](path: Rep[String], shape: Rep[T], separator: Rep[String]): Rep[DataTable[T]]
}

trait InputReaderImplOpsStandard extends InputReaderImplOps { this: OptiQLCompiler with OptiQLLift =>
  def optiql_table_input_reader_impl[T<:Record:Manifest](path: Rep[String], shape: Rep[T], separator: Rep[String]) = {
    val input = BufferedReader(FileReader(path))
    val table = DeliteArrayBuilder[T]()
    var record = input.readLine()
    while (record != unit(null)) {
      val fields = record.split("\\\\Q" + separator + "\\\\E")
      addRecord(table, fields, shape)
      record = input.readLine()
    }
    input.close()
    val data = table.result
    DataTable(data, data.length)
  }

  private def addRecord[T<:Record:Manifest](table: Rep[DeliteArrayBuilder[T]], record: Rep[Array[String]], shape: Rep[T]) {
    val elems = manifest[T] match {
      case rm: RefinedManifest[T] => rm.fields
      case m => throw new RuntimeException("No RefinedManifest for type " + m.toString)
    }
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
    val res = struct[T](fields:_*)
    table += res
  }

}
