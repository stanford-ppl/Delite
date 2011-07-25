import sbt._
import java.io.File
import eu.henkelmann.sbt.JUnitXmlTestsListener

final class DeliteProject(info: ProjectInfo) extends DefaultProject(info) with MavenStyleScalaPaths {
  // define a root-level environment file delite.properties
  // from which scala.virtualized.home and virtualization_lms_core.home will be read
  // to get the directory of scala virtualized and the virtualization-lms-core
  lazy val local = new BasicEnvironment {
      def log = DeliteProject.this.log
      def envBackingPath = info.projectPath / "delite.properties"
      lazy val scalaVirtualizedHome = property[String]
  }
  
  // use the local scala-virtualized compiler and library
  override def localScala =
    defineScala("2.9.x-virtualized-SNAPSHOT", new File(local.scalaVirtualizedHome.get.getOrElse {
      log.error("scala.virtualized.home needs to be defined in delite.properties and "+
      "must point to a valid scala-virtualized home directory"); "<undefined>"
    }))::Nil

  // Options
  // Parallelism!
  //override def parallelExecution = true // <-- problem with test cases

  override def compileOptions = super.compileOptions ++ compileOptions("-Yno-generic-signatures") // speed up bytecode gen a little
  override def testCompileOptions = super.testCompileOptions ++ compileOptions("-Yno-generic-signatures")
  
  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "resources"
  
  override def testScalaSourcePath = "tests" / "src" / "ppl" / "tests" / "scalatest" 
  override def testResourcesPath = "tests" / "resources"

  val virtualization_lms_core = "scala" % "virtualization-lms-core_2.9.x-virtualized-SNAPSHOT" % "0.1"
  
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" % "1.4-SNAPSHOT"
  //create a listener that writes to the normal output directory
  def junitXmlListener: TestReportListener = new JUnitXmlTestsListener(outputPath.toString)
  //add the new listener to the already configured ones
  override def testListeners: Seq[TestReportListener] = super.testListeners ++ Seq(junitXmlListener)

  lazy val quickTest = testQuickMethod(testCompileConditional.analysis, testOptions)(o =>
      testTask(testFrameworks, testClasspath, testCompileConditional.analysis, o))

  // Define project class with default source tree layout
  class FlatProject(info: ProjectInfo) extends DefaultProject(info) {
    // Source tree layout
    override def mainScalaSourcePath = "src"
    override def mainResourcesPath = "resources"
    
    override def compileOptions = super.compileOptions ++ compileOptions("-Yno-generic-signatures") // speed up bytecode gen a little
    override def testCompileOptions = super.testCompileOptions ++ compileOptions("-Yno-generic-signatures")
    
    val virtualization_lms_core = "scala" % "virtualization-lms-core_2.9.x-virtualized-SNAPSHOT" % "0.1"
    
    override def localScala =
    defineScala("2.9.x-virtualized-SNAPSHOT", new File(local.scalaVirtualizedHome.get.getOrElse {
      log.error("scala.virtualized.home needs to be defined in delite.properties and "+
      "must point to a valid scala-virtualized home directory"); "<undefined>"
    }))::Nil 
  }
  
  // Define projects
  lazy val framework = project("framework", "Delite Framework", new FlatProject(_))  
  //lazy val runtime = project("runtime", "Delite Runtime", new FlatProject(_) {
  //  override def mainClass = Some("ppl.delite.runtime.Delite")
  //})

  class DSLs(info: ProjectInfo) extends DefaultProject(info) {
    lazy val optiml = project("optiml", "OptiML", new FlatProject(_){
      override def mainClass = Some("ppl.dsl.tests.SimpleVectorTest")
    }, framework)
  }

  lazy val dsls = project("dsls", "DSLs", new DSLs(_), framework)

  lazy val apps = project("apps", "Applications", new APPs(_), framework, dsls)
  class APPs(info: ProjectInfo) extends DefaultProject(info) {
	  lazy val scala = project("scala", "Scala Apps", new FlatProject(_), framework, dsls)
  }
  
  //TR is anybody using this? conflict with defining 'tests' as test source path above...
  //aks: i am, but i'm in the process of trying to convert all the remaining tests to be scalatests.
  //in the meantime, i tried to set this up so it wouldn't conflict with the existing scalatests
  //lazy val tests = project("tests", "Delite Tests", new FlatProject(_), framework, dsls, apps)
}
