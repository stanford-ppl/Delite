package ppl.dsl.optigraph.ops

import scala.reflect.RefinedManifest
import scala.virtualization.lms.common.{Base,BaseFatExp,StructExp,Variables,Record}
import ppl.dsl.optigraph.{GIterable, GSet, Graph, Node}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph, OptiGraphExp}
import ppl.delite.framework.datastructures.{DeliteArray,DeliteArrayBuffer}

trait GraphImplOps { this: OptiGraph =>
  def graph_load_impl(fileName: Rep[String]): Rep[Graph]
  def graph_load_array_impl(fromArray: Rep[DeliteArray[(Int,Int)]]): Rep[Graph]
  def graph_nodes_impl(g: Rep[Graph]): Rep[GIterable[Node]]
  def optigraph_table_input_reader_impl[T<:Record:Manifest](path: Rep[String], shape: Rep[T], separator: Rep[String]): Rep[DeliteArray[T]]
}

trait GraphImplOpsStandard extends GraphImplOps {
  this: StructExp with OptiGraphCompiler with OptiGraphLift =>

  def graph_load_impl(fileName: Rep[String]): Rep[Graph] = {
    val g = graph_new()
    val nodemap = HashMap[Int, Node]()
    var xfs = BufferedReader(FileReader(fileName))
    var line = xfs.readLine()
    var nodeIdx = 0
    while (line != unit(null)) {
      if(line.charAt(0) != '#') {
        val edges = line.split("\\t").map(_.toInt)
        if(!nodemap.contains(edges(0))) {
          nodemap(edges(0)) = node_new(nodeIdx)
          nodeIdx += 1
        }
        if(!nodemap.contains(edges(1))) {
          nodemap(edges(1)) = node_new(nodeIdx)
          nodeIdx += 1
        }
      }
      line = xfs.readLine()
    }

    xfs = BufferedReader(FileReader(fileName))
    line = xfs.readLine()
    val nodes = DeliteArray[Node](nodemap.size)
    val outneighbors = DeliteArray[GIterable[Node]](nodemap.size)
    val inneighbors = DeliteArray[GIterable[Node]](nodemap.size)
    for(i <- 0 until nodemap.size) {
      nodes(i) = node_new(i)
      outneighbors(i) = new_empty_iterable[Node]()
      inneighbors(i) = new_empty_iterable[Node]()
    }
    while (line != unit(null)) {
      if(line.charAt(0) != '#') {
        val edges = line.split("\\t").map(_.toInt)
        delitearray_giterable_append(outneighbors, nodemap(edges(0)).Id, nodemap(edges(1)))
        delitearray_giterable_append(inneighbors, nodemap(edges(1)).Id, nodemap(edges(0)))
      }
      line = xfs.readLine()
    }
    graph_set_raw_nodes(g,nodes)
    graph_set_raw_outneighbors(g,outneighbors)
    graph_set_raw_inneighbors(g,inneighbors)
    g
  }

  def graph_load_array_impl(fromArray: Rep[DeliteArray[(Int,Int)]]): Rep[Graph] = {
    val g = graph_new()
    val nodemap = HashMap[Int, Node]()
    var nodeIdx = 0
    for(i <- 0 until fromArray.length) {
      if(!nodemap.contains(fromArray(i)._1)) {
        nodemap(fromArray(i)._1) = node_new(nodeIdx)
        nodeIdx += 1
      }
      if(!nodemap.contains(fromArray(i)._2)) {
        nodemap(fromArray(i)._2) = node_new(nodeIdx)
        nodeIdx += 1
      }
    }   
    
    val nodes = DeliteArray[Node](nodemap.size)
    val outneighbors = DeliteArray[GIterable[Node]](nodemap.size)
    val inneighbors = DeliteArray[GIterable[Node]](nodemap.size)
    for(i <- 0 until nodemap.size) {
      nodes(i) = node_new(i)
      outneighbors(i) = new_empty_iterable[Node]()
      inneighbors(i) = new_empty_iterable[Node]()
    }
    for(i <- 0 until fromArray.length) {
      delitearray_giterable_append(outneighbors, nodemap(fromArray(i)._1).Id, nodemap(fromArray(i)._2))
      delitearray_giterable_append(inneighbors, nodemap(fromArray(i)._2).Id, nodemap(fromArray(i)._1))
    }
    graph_set_raw_nodes(g,nodes.unsafeImmutable)
    graph_set_raw_outneighbors(g,outneighbors.unsafeImmutable)
    graph_set_raw_inneighbors(g,inneighbors.unsafeImmutable)
    g.unsafeImmutable
  }

  def graph_nodes_impl(g: Rep[Graph]): Rep[GIterable[Node]] = {
    val node_array = graph_raw_nodes(g)
    new_iterable_imm(node_array, unit(0), node_array.length)
  }

  def optigraph_table_input_reader_impl[T<:Record:Manifest](path: Rep[String], shape: Rep[T], separator: Rep[String]) = {
    implicit def rv[T:Manifest](v: Var[T]): Exp[T] = readVar(v) //TODO: why isn't readVar implicit working?

    val input = BufferedReader(FileReader(path))
    val table = DeliteArrayBuffer[T]()
    var record = input.readLine()
    var i = 0
    while (record != unit(null)) {
      val fields = readVar(record).split("\\Q" + separator + "\\E")
      addRecord(table, fields, shape)
      record = input.readLine()
      i += 1
      if (i % 1000000 == 0) println("processed " + i/1000000 + " million records")
    }
    input.close()
    println("file reading done")
    darray_buffer_result(table)
  }

  private def addRecord[T<:Record:Manifest](table: Rep[DeliteArrayBuffer[T]], record: Rep[Array[String]], shape: Rep[T]) {
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
        case "Boolean" => (field, record(i)=="true")
        case _ => throw new RuntimeException("Unsupported record field type: " + tp.toString)
      }
    }
    
    val res = struct[T](AnonTag(rm), fields)
    table += res
  }
}
