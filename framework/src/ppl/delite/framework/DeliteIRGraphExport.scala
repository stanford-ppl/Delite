package ppl.delite.framework

import ppl.delite.framework.ops.{DeliteOpsExp}
import scala.reflect.{Manifest, SourceContext}
import java.io.PrintWriter
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.virtualization.lms.internal.NestedBlockTraversal

/*
TODO: do recursive call to SourceContexts to get parent, so we get top-level usage info
*/

trait DeliteIRGraphExport extends NestedBlockTraversal {
    val IR: DeliteOpsExp
    import IR._

    case class Node(name: String, id: Int, d: Def[Any]) {
        var dependencies = new ArrayBuffer[Int]()
        var children = new ArrayBuffer[Child]()
        def makeSourceString: String = {
            findDefinition(d) match {
                case Some(TP(sym, rhs)) =>
                    def recurse(ctxs: List[SourceContext]): String = {
                        ctxs match {
                            case head :: rest =>
                                "%s:%s:%s\\n".format(head.fileName.split("/").last, head.line, head.methodName) ++ recurse(rest)
                            case _ => ""
                        }
                    }
                    "\"" + recurse(sym.sourceContexts).stripSuffix("\\n") + "\""
                case _ => "\"\""
            }
        }
        override def toString = "{\"name\": \"%s\",\n\"id\": %d,\n\"description\": %s,\n\"children\": %s,\n\"dependencies\": %s}" format (name, id, this.makeSourceString, children.mkString("[", ",", "]"), dependencies.mkString("[", ",", "]"))
    }

    case class Child(name: String, nodes: Array[Int]) {
        val ns = nodes.mkString("[", ", ", "]")
        override def toString = "{\"name\": \"%s\",\n\"nodes\": %s}" format (name, ns)
    }

    val symbolNodeMappings = Map.empty[Sym[Any], Int]
    val nodes = new ArrayBuffer[Node]()
    val rootNode = new Node(Config.degFilename.stripSuffix(".deg"), 0, null)
    var rootChildren = new ArrayBuffer[Int]()
    nodes += rootNode

    def addSymToNodes(s: Sym[Any], x: Def[Any]): Node = {
        if (symbolNodeMappings.contains(s)) {
            return nodes(symbolNodeMappings(s))
        }

        val index = nodes.length
        val n = new Node(s.toString + ": " + x.toString, index, x)

        nodes += n

        symbolNodeMappings += (s -> index)

        n
    }

    def exportGraph(start: Block[Any], file: String): Unit =
        exportGraph(start, new java.io.PrintWriter(new java.io.FileOutputStream(file)))

    def exportGraph(start: Block[Any], stream: PrintWriter): Unit = {
        // Accumulate all nodes in the IR graph, populating dependencies and
        // children along the way
        for (stm <- buildScheduleForResult(start, false)) {
            stm match {
                case TP(sym, rhs) =>
                    val n = addSymToNodes(sym, rhs)
                    for (Block(blockExp) <- blocks(stm)) {
                        blockExp match {
                            case blockSym @ Sym(_) =>
                                // Recursively accumulate list of dependency node IDs of each sym in ss
                                def accumulateDependencies(ss: List[Sym[Any]], acc: List[Int]): List[Int] = ss match {
                                    case Nil => acc
                                    case s :: rest =>
                                        s match {
                                            case Def(d) =>
                                                val depNode = addSymToNodes(s, d)
                                                if (acc.contains(depNode.id)) {
                                                    // don't recurse if we've already explored this node
                                                    return accumulateDependencies(rest, acc)
                                                } else {
                                                    return accumulateDependencies(rest ::: syms(d), depNode.id :: acc)
                                                }
                                            case _ => acc
                                        }
                                }
                                // For each block of the Node, create a Child and
                                // populate its nodes array by recursively grabbing the
                                // dependencies of each Sym, starting with the Sym the
                                // Block contains. Uses the Sym -> Node mappings.
                                val cnodes: Array[Int] = accumulateDependencies(List(blockSym), List()).toArray
                                val c = new Child("Block(%s)" format blockSym.toString, cnodes)
                                n.children += c
                            case _ => // TODO do we care if a block contains a Const?
                        }
                    }

                    // The immediate dependencies of the Node should also be
                    // added to the node's list of dependencies. This part will
                    // use the Sym -> Node mappings.
                    val deps: List[Sym[Any]] = syms(rhs).filterNot(d => blocks(stm) exists (_.res == d)).distinct
                    for (dep <- deps) {
                        dep match {
                            case Def(d) =>
                                n.dependencies += addSymToNodes(dep, d).id
                            case _ =>
                        }
                    }
                case _ =>
            }
        }

        // Now use nested block traversal to determine the top-level nodes
        traverseBlock(start)

        // Set the root node's children
        rootNode.children += new Child("main", rootChildren.toArray)

        // // TODO transition to JSON encoding library?
        stream.println("{\"root\": 0,")
        stream.println("\"nodes\":")
        stream.println(nodes.mkString("[", ",", "]"))
        stream.println("}")
        stream.flush
    }

    override def traverseStm(stm: Stm): Unit = {
        stm match {
            case TP(sym, _) =>
                rootChildren += symbolNodeMappings(sym)
            case _ =>
        }
        // Note we don't call super.traverseStm(), so that we stay within
        // the top-most block
    }
}
