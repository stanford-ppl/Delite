import sbt._
import java.io.File

final class DeliteProject(info: ProjectInfo) extends DefaultProject(info) with MavenStyleScalaPaths {
  // define a root-level environment file local.properties
  // from which scala.virtualized.home and virtualization_lms_core.home will be read
  // to get the directory of scala virtualized and the virtualization-lms-core
  lazy val local = new BasicEnvironment {
      def log = DeliteProject.this.log
      def envBackingPath = info.projectPath / "local.properties"
      lazy val scalaVirtualizedHome = property[String]
  }
  
  // use the local scala-virtualized compiler and library
  override def localScala =
    defineScala("2.8.x-virtualized-SNAPSHOT", new File(local.scalaVirtualizedHome.get.getOrElse {
      log.error("scala.virtualized.home needs to be defined in local.properties and "+
      "must point to a valid scala-virtualized home directory"); "<undefined>"
    }))::Nil

  // Options
  // Parallelism!
  //override def parallelExecution = true
  
  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "resources"
  
  override def testScalaSourcePath = "tests" / "src"
  override def testResourcesPath = "tests" / "resources"
  
  val virtualization_lms_core = "scala" % "virtualization-lms-core_2.8.x-virtualized-SNAPSHOT" % "0.1"
  
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" % "1.4-SNAPSHOT" % "test"
  
  // Define project class with default source tree layout
  class FlatProject(info: ProjectInfo) extends DefaultProject(info) {
    // Source tree layout
    override def mainScalaSourcePath = "src"
    override def mainResourcesPath = "resources"
    
    override def testScalaSourcePath = "tests" / "src"
    override def testResourcesPath = "tests" / "resources"
    
    val virtualization_lms_core = "scala" % "virtualization-lms-core_2.8.x-virtualized-SNAPSHOT" % "0.1"
    
    val scalaToolsSnapshots = ScalaToolsSnapshots
    val scalatest = "org.scalatest" % "scalatest" % "1.4-SNAPSHOT" % "test"
    
    override def localScala =
    defineScala("2.8.x-virtualized-SNAPSHOT", new File(local.scalaVirtualizedHome.get.getOrElse {
      log.error("scala.virtualized.home needs to be defined in local.properties and "+
      "must point to a valid scala-virtualized home directory"); "<undefined>"
    }))::Nil 
  }
  
  // Define projects
  lazy val framework = project("framework", "Delite Framework", new FlatProject(_))
  lazy val runtime = project("runtime", "Delite Runtime", new FlatProject(_) {
    override def mainClass = Some("ppl.delite.runtime.Delite")
  })
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
}
