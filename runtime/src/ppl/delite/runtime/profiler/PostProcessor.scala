package ppl.delite.runtime
package profiler

import java.io.{File, PrintWriter}
import scala.collection.mutable.{HashSet, HashMap}
import _root_.scala.util.parsing.json.JSON

object PostProcessor {
	
	val TARGET_SCALA = 0
	val TARGET_CPP = 1
	val TARGET_CUDA = 2

	def processDegFile(degFile: String): DependencyGraph = {
    	DependencyGraph.dependencyGraphNew(degFile)
	}

	def processRawProfileDataFile(profileDataFile: String, depGraph: DependencyGraph): ExecutionProfile = {
		val executionProfile: ExecutionProfile = new ExecutionProfile(profileDataFile, depGraph)
		executionProfile.init()

		executionProfile
	}

	def writeProcessedDataToFile(depGraph: DependencyGraph, executionProfile: ExecutionProfile) {
		for ((k,v) <- executionProfile.summaries) {
			Predef.println("Name: " + k + "   Total time: " + v.totalTime)
		}

		for ((level, m) <- executionProfile.timelineData.levelToTNodes) {
			Predef.println("Level " + level + " =>")
			for ((n, lst) <- m) {
				Predef.println("   " + n + " => ")
				for (i <- lst) {
					val parentName = if (i.parentTNode == null) "null" else i.parentTNode.name
					Predef.println("      Parent: " + parentName + " Start: " + i.start + " Duration: " + i.duration + " End: " + i.end + " ThreadId: " + i.threadId)
				}
			}
		}
	}

	def postProcessProfileData(globalStartNanos: Long, degFile: String, rawProfileDataFile: String) {
		val depGraph = processDegFile(degFile)
		depGraph.string()
		val executionProfile = processRawProfileDataFile(rawProfileDataFile, depGraph)

		writeProcessedDataToFile(depGraph, executionProfile)
	}

	private def dumpProcessedData(globalStartNanos, depGraph, executionProfile) {
		val directory = Path(Config.profileOutputDirectory).createDirectory()
		val file = directory / "profileData.js"
		val writer = new PrintWriter(file)

		val tabs = "   "
		val twoTabs = tabs + tabs
	    writer.println("{\"Profile\":{")
	    writer.println(tabs + "\"ExecutionProfile\": {")
	    writer.println(twoTabs + "\"systemNanoTimeAtAppStart\": " + globalStartNanos + ",")
	    writer.println(twoTabs + "\"jvmUpTimeAtAppStart\": " + executionProfile.jvmUpTimeAtAppStart + ",")
	    writer.println(twoTabs + "\"appStartTime\": " + executionProfile.appStartTime)
	    writer.println(twoTabs + "\"appTotalTime\": " + executionProfile.appTotalTime)
	    writer.println(twoTabs + "\"appEndTime\": " + executionProfile.appEndTime)
	    writer.println(twoTabs + "\"threadCount\": " + executionProfile.threadCount)
	    writer.println(twoTabs + "\"threadScalaCount\": " + executionProfile.threadScalaCount)
	    writer.println(twoTabs + "\"threadCppCount\": " + executionProfile.threadCppCount)
	    writer.println(twoTabs + "\"threadCudaCount\": " + executionProfile.threadCudaCount)
	    writer.println(twoTabs + "\"targetsEnabled\": " + "[scala]") // TODO: Fix this
	    
	    writer.println(tabs + "},")
	}
}