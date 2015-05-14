package ppl.delite.runtime.profiler

import collection.mutable
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import java.util.concurrent.ConcurrentHashMap
import ppl.delite.runtime.Config
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

class MemoryAccessStats(l2HitRatio: Double, l2Misses: Int, l3HitRatio: Double, l3Misses: Int, bytesReadFromMem: Double) {
    var l2CacheHitRatio: Double = l2HitRatio
    var l2CacheMisses: Int = l2Misses
    var l3CacheHitRatio: Double = l3HitRatio
    var l3CacheMisses: Int = l3Misses
    var bytesReadFromMC: Double = bytesReadFromMem
}

object MemoryProfiler
{
	var threadCount = 0
  	var stats = new ArrayBuffer[Map[String, List[Long]]]()
  	var threadToCurrKernel = new ArrayBuffer[Stack[String]]()
  	var threadToId: Map[String, Int] = Map()
	var memoryAccessStatsMaps = new ArrayBuffer[Map[String, ArrayBuffer[MemoryAccessStats]]]()

	var cppTidStart = -1
	var numCppThreads = 0
	var numScalaThreads = 0
	var numCudaThreads = 0
	var numOpenCLThreads = 0

	def initializeStats(numScala: Int, numCpp:Int, numCuda: Int, numOpenCL: Int) = synchronized {
		val numThreads = numScala + numCpp + numCuda + numOpenCL
		threadCount = numThreads

		if (numCpp > 0) {
			cppTidStart = numScala
		}

		numCppThreads = numCpp
		numScalaThreads = numScala
		numCudaThreads = numCuda
		numOpenCLThreads = numOpenCL

		for (i <- List.range(0, numThreads)) {
		  //val threadName = "ExecutionThread-" + i
		  val threadName = "ExecutionThread" + i
		  threadToId += threadName -> i
		  stats += Map[String, List[Long]]()
		  threadToCurrKernel += new Stack[String]()
		  memoryAccessStatsMaps += Map[String, ArrayBuffer[MemoryAccessStats]]() 
		}

		threadToId += "main" -> numThreads
		stats += Map[String, List[Long]]()
		threadToCurrKernel += new Stack[String]()
	}

	def addMemoryAccessStats(
        sourceContext: String, threadId: Int,
        l2CacheHitRatio: Double, l2CacheMisses: Int,
        l3CacheHitRatio: Double, l3CacheMisses: Int,
        bytesReadFromMC: Double): Unit = {
  	  Predef.println("sourceContext: " + sourceContext)
      val stats = new MemoryAccessStats( l2CacheHitRatio, l2CacheMisses, l3CacheHitRatio, l3CacheMisses, bytesReadFromMC );
      //memoryAccessStatsMaps(threadId) += sourceContext -> stats
      if (!memoryAccessStatsMaps(threadId).contains(sourceContext)) {
		memoryAccessStatsMaps(threadId) += sourceContext -> new ArrayBuffer[MemoryAccessStats]()
	  }

	  memoryAccessStatsMaps(threadId)(sourceContext) += stats
    }

	def clearAll() {
		stats.clear()
		threadToCurrKernel.clear()
		threadToId = Map()
		memoryAccessStatsMaps = new ArrayBuffer[Map[String, ArrayBuffer[MemoryAccessStats]]]()
		initializeStats(numScalaThreads, numCppThreads, numCudaThreads, numOpenCLThreads)
	}

  	def logArrayAllocation(component: String, threadId: Int, arrayLength: Int, elemType: String) = {
		var stack = threadToCurrKernel(threadId)
		var kernelCurrentlyExecutedByThread = component

		if (stack.length > 0) kernelCurrentlyExecutedByThread = getNameOfCurrKernel(threadId)
	
		if (!stats(threadId).contains(kernelCurrentlyExecutedByThread)) {
        	stats(threadId) += kernelCurrentlyExecutedByThread -> List[Long]()
	    }

	    val arrayMemSize = arrayLength.toLong * sizeOf(elemType).toLong
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
		return getNameOfCurrKernel(threadId)
  	}

  	def getNameOfCurrKernel(threadId: Int): String = {
  		var stack = threadToCurrKernel(threadId)
		if (stack.length > 0) {
			return stack(0) // 0 indexes the top-of-stack
  		}

  		return "null"
  	}

  	def dumpProfile(writer: PrintWriter) {
  		emitMemProfileDataArrays( writer )
		emitMemAccessStats( writer )
  	}

	def emitMemProfileDataArrays(writer: PrintWriter) {
		var aggrStats = aggregateStatsFromAllThreads()
		
		var tabs = "  "
		var twoTabs = tabs + tabs
  		writer.println(tabs + "\"MemProfile\": {")
  		for (kv <- aggrStats) {
  			val totalMemAllocation = sum(kv._2)
  			writer.println(twoTabs + "\"" + kv._1 + "\" : " + totalMemAllocation + ",")
  		}

  		writer.println(twoTabs + "\"dummy\": 0")
  		writer.println(tabs + "},\n")
  	}

	def emitMemAccessStats( writer: PrintWriter ) {
		if (numCppThreads > 0) {
			val tabs = "  "
			val twoTabs = tabs + tabs
			val cppTids = List.range(cppTidStart, cppTidStart + numCppThreads)
			val kernelToStats = memoryAccessStatsMaps(cppTidStart)

			writer.println( tabs + "\"MemAccessStats\": {" )
			var firstKernel = true
			for (kv <- kernelToStats) {
				val kernel = kv._1
				val statsLst = kv._2

				writer.println(twoTabs + "\"" + kernel  + "\": {")	

				for (i <- List.range(0, statsLst.length)) {
					val s = cppTids.map(tid => {
						val stats = memoryAccessStatsMaps(tid)(kernel)(i)
						"[" + tid + ", " +
							(stats.l2CacheHitRatio * 100).toInt + ", " + stats.l2CacheMisses + ", " + 
							(stats.l3CacheHitRatio * 100).toInt + ", " + stats.l3CacheMisses + ", " +
							stats.bytesReadFromMC + "]"
					}).mkString(",")

					if (i > 0) {
						writer.println(",")
					} 

					writer.print(twoTabs + tabs + "\"" + i + "\"" + ": [" + s + "]")
				}	

				writer.println("\n" + twoTabs + "},")		
			}

			writer.println(twoTabs + "\"dummy\": {}")
			
			writer.println( tabs + "}" )
		}
	}

	def aggregateStatsFromAllThreads(): Map[String, List[Long]] = {
		var aggrStats = Map[String, List[Long]]()
		for (m <- stats) {
			for (kv <- m) {
				var kernel = kv._1
				if (!aggrStats.contains(kernel)) {
					aggrStats += kernel -> List[Long]()
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

  	def sum(l:List[Long]) = {
  		var res = 0L
  		for (n <- l) {
  			res += n
  		}

  		res
  	}
}
