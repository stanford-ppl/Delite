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
      lazy val virtualization_lms_coreHome = property[String]
  }
  
  // use the local scala-virtualized compiler and library
  override def localScala =
    defineScala("2.8.x-virtualized-SNAPSHOT", new File(local.scalaVirtualizedHome.get.getOrElse {
      log.error("scala.virtualized.home needs to be defined in local.properties and "+
      "must point to a valid scala-virtualized home directory"); "<undefined>"
    }))::Nil 

  // Options
  // Parallelism!
  override def parallelExecution = true
  
  override def testScalaSourcePath = "tests" / "src"
  
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" %
    "1.0.1-for-scala-2.8.0.RC1-SNAPSHOT"
  
  // Define project class with default source tree layout
  class FlatProject(info: ProjectInfo) extends DefaultProject(info) {
    // Source tree layout
    override def mainScalaSourcePath = "src"
  }
  
  // Define projects
  lazy val virtualization_lms_core = project(Path.fromFile(local.virtualization_lms_coreHome.get.getOrElse {
      log.error("virtualization_lms_core.home needs to be defined in local.properties and "+
      "must point to a valid virtualization lms core home directory"); "<undefined>"
    }))
  
  lazy val framework = project("framework", "Delite Framework", new FlatProject(_), virtualization_lms_core)
  lazy val runtime = project("runtime", "Delite Runtime", new FlatProject(_), framework, virtualization_lms_core)
  
  lazy val dsls = project("dsls", "DSLs", new DSLs(_), framework, virtualization_lms_core)
   
  class DSLs(info: ProjectInfo) extends DefaultProject(info) {
    lazy val optiml = project("optiml", "OptiML", new FlatProject(_), framework, virtualization_lms_core)
  }
  
  lazy val tests = project("tests", "Tests", new Tests(_), framework, runtime, dsls, virtualization_lms_core)
    
  // Unsure, may have to do sub sub projects
  class Tests(info: ProjectInfo) extends ParentProject(info) {
  }
}