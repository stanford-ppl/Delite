package ppl.apps.cidr

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}
import scala.virtualization.lms.common.RecordOps
import scala.virtualization.lms.common._
import org.scala_lang.virtualized.virtualize
import org.scala_lang.virtualized.SourceContext

object ExampleRunner extends OptiQLApplicationRunner with Example
trait Example extends OptiQLApplication with RecordOps {

  type Content = Record {
    val cidr: String
    val startTime: Int
    val hitCount: Long
    val byteCount: Long
    val category: String
    val subCategory: String
    val country: String
    val continent: String
  }

  def Content(_cidr: Rep[String], start_time: Rep[Int], hit_count: Rep[Long], byte_count: Rep[Long], _category: Rep[String], sub_category: Rep[String], _country: Rep[String], _continent: Rep[String]): Rep[Content] = Record (
    cidr = _cidr,
    startTime = start_time,
    hitCount = hit_count,
    byteCount = byte_count,
    category = _category,
    subCategory = sub_category,
    country = _country,
    continent = _continent
  )

  def printUsage() = {
    println("Usage: TestRunner <input Akamai CIDR file or directory>")
    exit(-1)
  }

  @virtualize
  def main() = {
    if (args.length < 1) printUsage()
    val path = args(0)
    val data = Table.fromFile[Content](path, ",")

    val c = data.Count(e => unit(true)) //why?
    println("total records processed: " + c)

    val q = data Where(_.country == "United States") GroupBy(_.category) Select(g => Record (
      category = g.key,
      totalHits = g.Sum(_.hitCount),
      totalBytes = g.Sum(_.byteCount),
      count = g.Count
    ))

    q.printAsTable()
  }

}
