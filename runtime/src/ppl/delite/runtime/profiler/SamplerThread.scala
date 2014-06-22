package ppl.delite.runtime.profiler

import scala.collection.JavaConversions._
import java.io.PrintWriter
import java.lang.Runtime
import java.lang.management.ManagementFactory

object SamplerThread extends Thread {
	var interval: Long = 1000
	private var continue: Boolean = true
	private var hitException: Boolean = false
	private var exception: String = "" // If the thread hit an exception, the message would be stored in this string
	private val runtime = Runtime.getRuntime()
	private var samples: List[ProfileData] = List()
	var globalT: Long = 0;

	override def run() {
		while(continue) {
			try {
				profile()
				Thread.sleep(interval)
			} catch {
				case i: InterruptedException => continue = false //another thread threw an exception -> exit silently
		        case e: Exception => {
		          exception = e.toString
		          continue = false
		        }
			}
		}
	}

	def profile() {
		val time = System.nanoTime()
		val maxMemory: Long = runtime.maxMemory()
		val totalMemory: Long = runtime.totalMemory()
		val freeMemory: Long = runtime.freeMemory()
		val usedMemory: Long = totalMemory - freeMemory
		var gcCount: Long = 0; // Total number of garbage collections
	    var gcTime: Long = 0; // Total time spent in garbage collection

	    /*ManagementFactory.getGarbageCollectorMXBeans().foreach(gc => {
	        val count = gc.getCollectionCount();
	        if(count >= 0) {
	            gcCount += count;
	        }

	        val time = gc.getCollectionTime();
	        if(time >= 0) {
	            gcTime += time;
	        }
	    })*/

	    val pd = new ProfileData(time, maxMemory, totalMemory, freeMemory, usedMemory, gcCount, gcTime)
	    samples = pd :: samples 

	    /*val jvmUpTime = ManagementFactory.getRuntimeMXBean().getUptime();
	    val data = List(jvmUpTime, time, gcCount, gcTime)
	    Predef.println(data mkString ",")*/
	}

	def dumpProfile(writer: PrintWriter, globalStartNanos: Long) {
  		emitMemProfileDataArrays(writer, samples, globalStartNanos)
  	}

	def emitMemProfileDataArrays(writer: PrintWriter, samples: List[ProfileData], globalStartNanos: Long) {
		var tabs = "  "
		var twoTabs = tabs + tabs

  		writer.println(tabs + "\"MemUsageSamples\": {")
  		writer.print(twoTabs + "\"0\" : [\"Max Memory\", \"Total Memory\", \"Used Memory\"]")
  		for (s <- samples) {
  			writer.println(",")  			
  			val data = List(s.maxMemory, s.totalMemory, s.usedMemory) mkString ","
  			val timeStamp = (s.timeStamp - globalStartNanos) / 1000
  			writer.print(twoTabs + "\"" + timeStamp + "\" : [" + data + "]")
  		}

  		writer.println("")
  		writer.print(tabs + "}")
  	}
}

class ProfileData(val timeStamp: Long, val maxMemory: Long, val totalMemory: Long, val freeMemory: Long, val usedMemory: Long, val gcCount: Long, val gcTime: Long) {}