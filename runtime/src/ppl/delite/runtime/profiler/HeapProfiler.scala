
package ppl.delite.runtime.profiler

import java.lang.management.ManagementFactory;
import scala.collection.JavaConversions._

object HeapProfiler {
	val mxBeans = ManagementFactory.getMemoryPoolMXBeans().toList;

	def dumpHeapStatistics() {
		mxBeans.foreach(bean => {
		    val name = bean.getName();
		    val tpe = bean.getType();
		    val usage = bean.getUsage();
		    val peak = bean.getPeakUsage();
		    val collections = bean.getCollectionUsage();

		    //val data = List(name, tpe, usage, peak, collections) mkString ","
		    //Predef.println(data)

		    Predef.println("==========================================")
		    Predef.println("Name: " + name)
		    Predef.println("Type: " + tpe)
		    Predef.println("Usage: " + usage)
		    Predef.println("Peak: " + peak)
		    Predef.println("Collections: " + collections)
		    Predef.println("==========================================")
		    Predef.println()
		})
	}
}