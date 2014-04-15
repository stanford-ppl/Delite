package ppl.delite.runtime.profiler

import collection.mutable
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import java.util.concurrent.ConcurrentHashMap
import ppl.delite.runtime.Config
import scala.collection.mutable.Stack

object MemoryProfiler
{
	var stats: Map[String, List[Int]] = Map()
	var threadToCurrKernel : Map[String, Stack[String]] = Map()

	def logArrayAllocation(component: String, arrayLength: Int, elemType: String) = synchronized {
		var threadName = Thread.currentThread.getName()
		var kernelCurrentlyExecutedByThread = component
		threadToCurrKernel.synchronized {
			if (threadToCurrKernel.contains(threadName)) {
				kernelCurrentlyExecutedByThread = getNameOfCurrKernel(threadName)
			}
		}

		if (!stats.contains(kernelCurrentlyExecutedByThread)) {
	      	stats.synchronized {
	        	stats += kernelCurrentlyExecutedByThread -> List[Int]()
	      	}
	    }
	    
	    val arrayMemSize = arrayLength * sizeOf(elemType)
	    val current = arrayMemSize :: stats(kernelCurrentlyExecutedByThread)
	    stats.synchronized {
	      stats += kernelCurrentlyExecutedByThread -> current
	    }
  	}

  	def sizeOf(elemType: String) = elemType match {
  		case "boolean" | "byte" => 8
	    case "char" | "short" => 16
	    case "int" | "float" => 32
	    case "double" | "long" => 64
	    case _ => 64 // TODO: could be DeliteArray, ie, in the case of nested arrays. This approximation is incorrect
  	}

  	def emitMemProfileDataArrays(writer: PrintWriter, stats: Map[String, List[Int]]) {
  		writer.println("	\"MemProfile\": {")
  		for (id <- stats.keys) {
  			var allocationLengths = stats(id).foldLeft("")((a,b) => a + "," + b).trim()
  			allocationLengths = allocationLengths.substring(1, allocationLengths.length())
  			writer.println("		\"" + id + "\" : [" + allocationLengths + "],")
  		}

  		writer.println("		\"dummy\": []")
  		writer.println("	}")
  	}

  	def dumpProfile(writer: PrintWriter) {
  		emitMemProfileDataArrays(writer, stats)
  	}

  	def pushNameOfCurrKernel(thread: String, kernelId: String) = synchronized {
	    threadToCurrKernel.synchronized {
	    	if (!(threadToCurrKernel.contains(thread))) {
	    		threadToCurrKernel += thread -> (new Stack())
	    	}
	    	
	    	threadToCurrKernel(thread).push(kernelId)
      	}
  	}

  	def popNameOfCurrKernel(thread: String) = synchronized {
	    threadToCurrKernel.synchronized {
	    	if (threadToCurrKernel.contains(thread)) {
		    	var stack = threadToCurrKernel(thread)
		    	if (stack.length > 0) {
		    		stack.pop()
		    	}
		    }
      	}
  	}

  	def getNameOfCurrKernel(thread: String): String = synchronized {
  		threadToCurrKernel.synchronized {
  			if (threadToCurrKernel.contains(thread)) {
  				var stack = threadToCurrKernel(thread)
  				if (stack.length > 0) {
	  				return stack(0) // 0 indexes the top-of-stack
	  			}
	  		}

	  		return "null"
	  	}
  	}
}