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

	val profileDBFileName = "profile.db"
	val uiDataFileName = "profileDataUI.js"
	val gcStatsFileName = "gcStats.txt"
	val attributesFileName = "profile.js"

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
		executionProfile.writeTicTocNodeSummariesToDB()
		executionProfile.writeAppDataToDB()
		executionProfile.close()

		dumpProcessedData(globalStartNanos, depGraph, executionProfile)	
	}

	private def dumpProcessedData(globalStartNanos: Long, depGraph: DependencyGraph, executionProfile: ExecutionProfile) {
		val directory = Path( Config.profileOutputDirectory ).createDirectory()
		//val file = directory / "profileData_test.js"
		val file = directory / uiDataFileName
		var writer = new PrintWriter( file.jfile )

	    writer.println( "{\"Profile\":{" )

	    executionProfile.dumpAppDataInJSON( writer, tabs )
	    depGraph.dumpDepGraphInJSON( writer, tabs )
	    executionProfile.dumpTimelineDataInJSON( writer, tabs )
	    SamplerThread.dumpMemUsageSamplesInJSON( writer, tabs )

	    writer.println( "}}" )
	    writer.close()

	    //dumpAttributesOfOutputFiles()
	}

	/*
	private def dumpAttributesOfOutputFiles() {
		val directory = Config.profileOutputDirectory
		val attributesFile = Path( directory ) / attributesFileName
		//val attributesFile = Path( directory + "/" + attributesFileName ).createFile()
		val writer = new PrintWriter( attributesFile.jfile )
		
		val uiDataFile = new File( directory + "/" + uiDataFileName )
		writer.println( "%s,%d".format( uiDataFileName, uiDataFile.length ) )

		val profileDBFile = new File( directory + "/" + profileDBFileName )
		writer.println( "%s,%d".format( profileDBFileName, profileDBFile.length ) )

		writer.close()
	}
	*/
}