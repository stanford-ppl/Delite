package ppl.apps.cidr

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}
import scala.virtualization.lms.common.Record

object ExampleRunner extends OptiQLApplicationRunner with Example
trait Example extends OptiQLApplication {

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

  def Content(_cidr: Rep[String], start_time: Rep[Int], hit_count: Rep[Long], byte_count: Rep[Long], _category: Rep[String], sub_category: Rep[String], _country: Rep[String], _continent: Rep[String]): Rep[Content] = new Record {
    val cidr = _cidr
    val startTime = start_time
    val hitCount = hit_count
    val byteCount = byte_count
    val category = _category
    val subCategory = sub_category
    val country = _country
    val continent = _continent
  }

  def printUsage() = {
    println("Usage: TestRunner <input Akamai CIDR file or directory>")
    exit(-1)
  }

  def main() = {
    if (args.length < 1) printUsage()
    val path = args(0)
    val data = Table.fromFile[Content](path, ",")

    val c = data.Count(e => unit(true)) //why?
    println("total records processed: " + c)

    val q = data Where(_.country == "United States") GroupBy(_.category) Select(g => new Record {
      val category = g.key
      val totalHits = g.Sum(_.hitCount)
      val totalBytes = g.Sum(_.byteCount)
      val count = g.Count
    })

    q.printAsTable()
  }

}
