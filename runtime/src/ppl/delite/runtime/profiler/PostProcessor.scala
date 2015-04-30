package ppl.delite.runtime
package profiler

import java.io.{File, PrintWriter}
import scala.collection.mutable.{HashSet, HashMap}
import _root_.scala.util.parsing.json.JSON
import tools.nsc.io.Path

object PostProcessor {
	
	val TARGET_SCALA = 1
	val TARGET_CPP = 2
	val TARGET_CUDA = 4
	val tabs = "   "
	val twoTabs = tabs + tabs

	def processDegFile(degFile: String): DependencyGraph = {
    	DependencyGraph.dependencyGraphNew(degFile)
	}

	def processRawProfileDataFile(profileDataFile: String, depGraph: DependencyGraph): ExecutionProfile = {
		val executionProfile: ExecutionProfile = new ExecutionProfile(profileDataFile, depGraph)
		executionProfile.init()

		executionProfile
	}

	def postProcessProfileData(globalStartNanos: Long, degFile: String, rawProfileDataFile: String) {
		val depGraph = processDegFile(degFile)
		depGraph.string()
		val executionProfile = processRawProfileDataFile(rawProfileDataFile, depGraph)

		executionProfile.writeDNodesToDB()
		executionProfile.writeExecutionSummariesToDB()
		executionProfile.close()

		dumpProcessedData(globalStartNanos, depGraph, executionProfile)	
	}

	private def dumpProcessedData(globalStartNanos: Long, depGraph: DependencyGraph, executionProfile: ExecutionProfile) {
		val directory = Path(Config.profileOutputDirectory).createDirectory()
		val file = directory / "profileData_test.js"
		val writer = new PrintWriter(file.jfile)

	    writer.println("{\"Profile\":{")

	    executionProfile.dumpAppDataInJSON( writer, twoTabs )
	    depGraph.dumpDepGraphInJSON( writer, twoTabs )
	    executionProfile.dumpTimelineDataInJSON( writer, twoTabs )

	    writer.println("}")
	    writer.close()
	}
}