package ppl.apps.traceroute

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}
import org.scala_lang.virtualized.Record
import org.scala_lang.virtualized.RecordOps
import org.scala_lang.virtualized.virtualize
import org.scala_lang.virtualized.SourceContext

object TraceRouteGraphRunner extends OptiQLApplicationRunner with TraceRouteGraph
@virtualize
trait TraceRouteGraph extends OptiQLApplication with RecordOps {

  type TraceRoute = Record {
    val traceType: String
    val traceNumber: Int
    val unixTime: Int
    val region: Int
    val destinationIp: String
    val trace: String
  }

  type Hop = Record {
    val id: Int
    val ip: String
    val latency: Float
  }

  def Hop(_id: Rep[Int], _ip: Rep[String], _latency: Rep[Float]): Rep[Hop] = Record (
    id = _id,
    ip = _ip,
    latency = _latency
  )

  type Edge = Record {
    val src: Int
    val dst: Int
  }

  def Edge(source: Rep[Int], destination: Rep[Int]): Rep[Edge] = Record (
    src = source,
    dst = destination
  )

  def printUsage() = {
    println("Usage: TraceRouteGraph <input Akamai Traceroute file or directory> [link filter threshhold]")
    exit(-1)
  }

  def main() = {
    if (args.length < 1) printUsage()
    val path = args(0)
    val threshhold = if (args.length > 1) args(1).toInt else 0

    tic()
    val data = Table.fromFile[TraceRoute](path, ",")

    val allEdges = data SelectMany { r =>
      val hops = Table.fromString[Hop](r.trace, "\\|", ":") //OrderBy(_.id) //in general we should sort by hop id before making edges, but they're already sorted in the dataset
      val ips = hops Select { h => 
        val bytes = h.ip.split("\\.")
        (bytes(0).toInt << 24) + (bytes(1).toInt << 16) + (bytes(2).toInt << 8) + bytes(3).toInt //better to store as Ints than Strings
      }
      Table.range(0, ips.size-1) Select { i =>
        Edge(ips(i), ips(i+1))
      }
    }

    val edgeCounts = allEdges GroupBy(e => e) Select(g => Record (
      edge = g.key,
      count = g.Count
    ))
    
    val edges = edgeCounts Where(_.count > threshhold) Select(_.edge)

    //edges.toGraph
    toc(edges)
    edges.printAsTable()
  }

}
