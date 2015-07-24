package ppl.delite.framework.analysis

import scala.virtualization.lms.internal.{AbstractSubstTransformer,Transforming,FatBlockTraversal}
import scala.reflect.SourceContext
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import java.io.{FileWriter, PrintWriter}
import scala.util.Random
import scala.collection.mutable.Set

trait DotPrintAnalysis extends FatBlockTraversal {
  val IR: DeliteOpsExp
  import IR._

  var fileName: String = "deliteIR.dot"
//  var fileWriter = new FileWriter(fileName)
  var dotStream: PrintWriter = null

  val funcBlocks = Set[Block[Any]]()
  val rfuncBlocks = Set[Block[Any]]()
  val zeroBlocks = Set[Block[Any]]()
  val accInitBlocks = Set[Block[Any]]()

  val allocBlocks = Set[Block[Any]]()
  val applyBlocks = Set[Block[Any]]()
  val updateBlocks = Set[Block[Any]]()
  val appendableBlocks = Set[Block[Any]]()
  val appendBlocks = Set[Block[Any]]()
  val setSizeBlocks = Set[Block[Any]]()
  val allocRawBlocks = Set[Block[Any]]()
  val copyRawBlocks = Set[Block[Any]]()
  val finalizerBlocks = Set[Block[Any]]()
  val hwBlocks = Set[Block[Any]]()

  def noshowBlocks = Set[Block[Any]]() ++
//                      allocBlocks ++
                      applyBlocks ++
//                      updateBlocks ++
                      appendableBlocks ++
                      appendBlocks ++
                      setSizeBlocks ++
                      allocRawBlocks ++
                      copyRawBlocks ++
                      finalizerBlocks


  def run[A](s: Block[A], fileName: String): Unit = {
    dotStream = new PrintWriter(fileName)

    dotStream.println("digraph {")
//    for (s <- globalDefs) {
//      printStm(s)
//    }
    val res = traverseBlock(s)
    dotStream.println("}")
    dotStream.flush
  }

  private def getBlockName(b: Block[Any]) = {
    var prefix = ""
    if (funcBlocks.contains(b)) {
      prefix = "funcBlk"
    } else if (rfuncBlocks.contains(b)) {
      prefix = "rfuncBlk"
    } else if (zeroBlocks.contains(b)) {
      prefix = "zeroBlk"
    } else if (accInitBlocks.contains(b)) {
      prefix = "accInitBlk"
    } else if (allocBlocks.contains(b)) {
      prefix = "allocBlk"
    } else if (applyBlocks.contains(b)) {
      prefix = "applyBlk"
    } else if (updateBlocks.contains(b)) {
      prefix = "updateBlk"
    } else if (appendableBlocks.contains(b)) {
      prefix = "appendableBlk"
    } else if (appendBlocks.contains(b)) {
      prefix = "appendBlk"
    } else if (setSizeBlocks.contains(b)) {
      prefix = "setSizeBlk"
    } else if (allocRawBlocks.contains(b)) {
      prefix = "allocRawBlk"
    } else if (copyRawBlocks.contains(b)) {
      prefix = "copyRawBlk"
    } else if (finalizerBlocks.contains(b)) {
      prefix = "finalizerBlk"
    } else if (hwBlocks.contains(b)) {
      prefix = "hwBlk"
    }
    prefix + s""" $b"""
  }
  override def traverseBlock[A](b: Block[A]) = {
    dotStream.println(s"[DotPrintAnalysis] Within transformBlock, block = $b")
    val bname = getBlockName(b)
    val bs = boundSyms(b)

    dotStream.println(s"subgraph cluster_$b {".replaceAll("""\.""",""))
    val color = Math.abs(Random.nextInt) % 0xFFFFFF;
    dotStream.println("style=filled")
    dotStream.println("node [style=filled,color=white]")
    dotStream.println(s"""fillcolor=\"#$color\"""")
    dotStream.println(s"""label=\"$bname ($bs)\"""")
    if (!noshowBlocks.contains(b)) {
      val res = super.traverseBlock(b)
    }
    dotStream.println("}")
  }

  private def printStm(stm: Stm): Unit = stm match {
    case TP(s, d) =>
        val nodeName = s"$s"
        val className = s"$d"
        var fill = false
        var fillcolor = "white"
        var fontcolor = "black"
        d match {
//          case h: HwDef[_] =>
//            fill = true
//            fillcolor = "blue"
//            fontcolor = "white"
          case a: AbstractLoop[_] =>
            fill = true
            fillcolor = "yellow"
            a.body match {
              case c: DeliteCollectElem[_,_,_] =>
                funcBlocks += c.func
                allocBlocks += c.buf.alloc
                applyBlocks += c.buf.apply
                updateBlocks += c.buf.update
                appendableBlocks += c.buf.appendable
                appendBlocks += c.buf.append
                setSizeBlocks += c.buf.setSize
                allocRawBlocks += c.buf.allocRaw
                copyRawBlocks += c.buf.copyRaw
                finalizerBlocks += c.buf.finalizer
              case r: DeliteReduceElem[_] =>
                funcBlocks += r.func
                rfuncBlocks += r.rFunc
                zeroBlocks += r.zero
                accInitBlocks += r.accInit
              case _ =>
            }

          case Reflect(node, _, _) if node.isInstanceOf[AbstractLoop[_]] =>
            traverseStm(TP(s, node.asInstanceOf[AbstractLoop[_]]))
//          case Reflect(node, _, _) if node.isInstanceOf[HwDef[_]] =>
//            traverseStm(TP(s, node.asInstanceOf[HwDef[_]]))

//            fill = true
//            val a = node.asInstanceOf[AbstractLoop[_]]
//            a.body match {
//              case c: DeliteCollectElem[_,_,_] =>
//                noshowBlocks += c.buf.alloc
//                noshowBlocks += c.buf.apply
//                noshowBlocks += c.buf.update
//                noshowBlocks += c.buf.appendable
//                noshowBlocks += c.buf.append
//                noshowBlocks += c.buf.setSize
//                noshowBlocks += c.buf.allocRaw
//                noshowBlocks += c.buf.copyRaw
//                noshowBlocks += c.buf.finalizer
//              case _ =>
//            }

          case _ => fill = false
        }

        val style = if (fill) {
              s"""[shape=record, fillcolor=\"$fillcolor\", fontcolor=\"$fontcolor\", style=\"filled\", label=\"{TP '$s' | '$className'}\"]"""
            } else {
              s"""[shape=record, label=\"{TP '$s' | '$className'}\"]"""
            }
        dotStream.println(s""" \"$nodeName\" $style """)

        val depSyms = syms(d)
        for (depSym <- depSyms) {
          dotStream.println(s""" \"$depSym\" -> \"$nodeName\" """)
        }
    case TTP(s, d, fd) =>
        val nodeName = s"$s"
        val className = fd.getClass().toString
        dotStream.println(s""" \"$nodeName\" [shape=record, fillcolor="red", style="filled", label="{TTP $s | $className}"] """)

        val depSyms = syms(fd)
        for (depSym <- depSyms) {
          dotStream.println(s""" \"$depSym\" -> \"$nodeName\" """)
        }
    case _ => throw new Exception("Not TP or TTP")
  }

  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(s, d) =>
        val nodeName = s"$s"
        val className = s"$d"
        var fill = false
        var fillcolor = "white"
        var fontcolor = "black"
        d match {
//          case h: HwDef[_] =>
//            fill = true
//            fillcolor = "blue"
//            fontcolor = "white"
          case a: AbstractLoop[_] =>
            fill = true
            fillcolor = "yellow"
            a.body match {
              case c: DeliteCollectElem[_,_,_] =>
                funcBlocks += c.func
                allocBlocks += c.buf.alloc
                applyBlocks += c.buf.apply
                updateBlocks += c.buf.update
                appendableBlocks += c.buf.appendable
                appendBlocks += c.buf.append
                setSizeBlocks += c.buf.setSize
                allocRawBlocks += c.buf.allocRaw
                copyRawBlocks += c.buf.copyRaw
                finalizerBlocks += c.buf.finalizer
              case r: DeliteReduceElem[_] =>
                funcBlocks += r.func
                rfuncBlocks += r.rFunc
                zeroBlocks += r.zero
                accInitBlocks += r.accInit
              case _ =>
            }

          case Reflect(node, _, _) if node.isInstanceOf[AbstractLoop[_]] =>
            traverseStm(TP(s, node.asInstanceOf[AbstractLoop[_]]))
//          case Reflect(node, _, _) if node.isInstanceOf[HwDef[_]] =>
//            traverseStm(TP(s, node.asInstanceOf[HwDef[_]]))

//            fill = true
//            val a = node.asInstanceOf[AbstractLoop[_]]
//            a.body match {
//              case c: DeliteCollectElem[_,_,_] =>
//                noshowBlocks += c.buf.alloc
//                noshowBlocks += c.buf.apply
//                noshowBlocks += c.buf.update
//                noshowBlocks += c.buf.appendable
//                noshowBlocks += c.buf.append
//                noshowBlocks += c.buf.setSize
//                noshowBlocks += c.buf.allocRaw
//                noshowBlocks += c.buf.copyRaw
//                noshowBlocks += c.buf.finalizer
//              case _ =>
//            }

          case _ => fill = false
        }

        val style = if (fill) {
              s"""[shape=record, fillcolor=\"$fillcolor\", fontcolor=\"$fontcolor\", style=\"filled\", label=\"{TP '$s' | '$className'}\"]"""
            } else {
              s"""[shape=record, label=\"{TP '$s' | '$className'}\"]"""
            }
        dotStream.println(s""" \"$nodeName\" $style """)

        val depSyms = syms(d)
        for (depSym <- depSyms) {
          dotStream.println(s""" \"$depSym\" -> \"$nodeName\" """)
        }
        super.traverseStm(stm)
    case TTP(s, d, fd) =>
        val nodeName = s"$s"
        val className = fd.getClass().toString
        dotStream.println(s""" \"$nodeName\" [shape=record, fillcolor="red", style="filled", label="{TTP $s | $className}"] """)

        val depSyms = syms(fd)
        for (depSym <- depSyms) {
          dotStream.println(s""" \"$depSym\" -> \"$nodeName\" """)
        }
      super.traverseStm(stm)
    case _ => throw new Exception("Not TP or TTP")
  }
}
