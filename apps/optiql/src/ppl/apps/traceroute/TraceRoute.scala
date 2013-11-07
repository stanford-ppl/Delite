package ppl.apps.traceroute

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}
import scala.virtualization.lms.common.Record

object TraceRouteGraphRunner extends OptiQLApplicationRunner with TraceRouteGraph
trait TraceRouteGraph extends OptiQLApplication {

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

  def Hop(_id: Rep[Int], _ip: Rep[String], _latency: Rep[Float]): Rep[Hop] = new Record {
    val id = _id
    val ip = _ip
    val latency = _latency
  }

  type Edge = Record {
    val src: String
    val dst: String
  }

  def Edge(source: Rep[String], destination: Rep[String]): Rep[Edge] = new Record {
    val src = source
    val dst = destination
  }

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
      Table.range(0, hops.size-1) Select { i =>
        Edge(hops(i).ip, hops(i+1).ip)
      }
    }

    val edgeCounts = allEdges GroupBy(e => e) Select(g => new Record {
      val edge = g.key
      val count = g.Count
    })
    
    val edges = edgeCounts Where(_.count > threshhold) Select(_.edge)

    //edges.toGraph
    toc(edges)
    edges.printAsTable()
  }

}
