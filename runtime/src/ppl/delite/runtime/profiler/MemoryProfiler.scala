package ppl.delite.runtime.profiler

import collection.mutable
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import java.util.concurrent.ConcurrentHashMap
import ppl.delite.runtime.Config
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

object MemoryProfiler
{
	var threadCount = 0
  	var stats = new ArrayBuffer[Map[String, List[Int]]]()
  	var threadToCurrKernel = new ArrayBuffer[Stack[String]]()
  	var threadToId: Map[String, Int] = Map()

	def initializeStats(numThreads: Int) = synchronized {
		threadCount = numThreads
		for (i <- List.range(0, numThreads)) {
		  val threadName = "ExecutionThread-" + i
		  threadToId += threadName -> i
		  stats += Map[String, List[Int]]()
		  threadToCurrKernel += new Stack[String]()
		}

		threadToId += "main" -> numThreads
		stats += Map[String, List[Int]]()
		threadToCurrKernel += new Stack[String]()
	}

	def logArrayAllocation(component: String, arrayLength: Int, elemType: String) = {
		var threadName = Thread.currentThread.getName()
		var threadId = threadToId(threadName)
		var stack = threadToCurrKernel(threadId)
		var kernelCurrentlyExecutedByThread = component
		if (stack.length > 0) kernelCurrentlyExecutedByThread = getNameOfCurrKernel(threadName)
	
		if (!stats(threadId).contains(kernelCurrentlyExecutedByThread)) {
        	stats(threadId) += kernelCurrentlyExecutedByThread -> List[Int]()
	    }

	    val arrayMemSize = arrayLength * sizeOf(elemType)
	    var kernelToAlloc = stats(threadId)
	    val current = arrayMemSize :: kernelToAlloc(kernelCurrentlyExecutedByThread)
	    stats(threadId) += kernelCurrentlyExecutedByThread -> current
  	}

	def pushNameOfCurrKernel(thread: String, kernelId: String) = {
		var threadId = threadToId(thread)
		var stack = threadToCurrKernel(threadId)
    	stack.push(kernelId)
  	}

  	def popNameOfCurrKernel(thread: String) = {
  		var threadId = threadToId(thread)
		var stack = threadToCurrKernel(threadId)
    	if (stack.length > 0) {
    		stack.pop()
    	}
  	}

  	def getNameOfCurrKernel(thread: String): String = {
  		var threadId = threadToId(thread)
		var stack = threadToCurrKernel(threadId)
		if (stack.length > 0) {
			return stack(0) // 0 indexes the top-of-stack
  		}

  		return "null"
  	}

  	def dumpProfile(writer: PrintWriter) {
  		emitMemProfileDataArrays(writer, stats)
  	}

	def emitMemProfileDataArrays(writer: PrintWriter, stats: ArrayBuffer[Map[String, List[Int]]]) {
		var aggrStats = aggregateStatsFromAllThreads(stats)
		
		var tabs = "  "
		var twoTabs = tabs + tabs
  		writer.println(tabs + "\"MemProfile\": {")
  		for (kv <- aggrStats) {
  			val totalMemAllocation = sum(kv._2)
  			writer.println(twoTabs + "\"" + kv._1 + "\" : " + totalMemAllocation + ",")
  		}

  		writer.println(twoTabs + "\"dummy\": 0")
  		writer.print(tabs + "}")
  	}

	def aggregateStatsFromAllThreads(stats: ArrayBuffer[Map[String, List[Int]]]): Map[String, List[Int]] = {
		var aggrStats = Map[String, List[Int]]()
		for (m <- stats) {
			for (kv <- m) {
				var kernel = kv._1
				if (!aggrStats.contains(kernel)) {
					aggrStats += kernel -> List[Int]()
				}

				var memAllocation = kv._2
				val current = aggrStats(kernel) ::: memAllocation
				aggrStats += kernel -> current
			}
		}

		return aggrStats
	}

  	def sizeOf(elemType: String) = elemType match {
  		case "boolean" | "byte" => 8
	    case "char" | "short" => 16
	    case "int" | "float" => 32
	    case "double" | "long" => 64
	    case _ => 64
  	}

  	def sum(l:List[Int]) = {
  		var res = 0
  		for (n <- l) {
  			res += n
  		}

  		res
  	}
}