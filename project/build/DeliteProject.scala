import sbt._
import java.io.File

final class DeliteProject(info: ProjectInfo) extends ParentProject(info) with MavenStyleScalaPaths {
  // define a root-level environment file local.properties
  // from which scala.virtualized.home will be read
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
  
  override def testScalaSourcePath = "tests"
  
  lazy val virtualization_lms_core = project(Path.fromFile(local.virtualization_lms_coreHome.get.getOrElse {
      log.error("virtualization_lms_core.home needs to be defined in local.properties and "+
      "must point to a valid virtualization lms core home directory"); "<undefined>"
    }))
  
  lazy val framework = project("framework", "Delite Framework", virtualization_lms_core)
  lazy val runtime = project("runtime", "Delite Runtime", framework, virtualization_lms_core)
  
  lazy val dsls = project("dsls", "DSLs", framework)
  
  class DSLs(info: ProjectInfo) extends ParentProject(info) {
    lazy val optiml = project("optiml", "OptiML", framework)
  }
  
  lazy val tests = project("tests", "Tests", framework, runtime, dsls)
  
  class Tests(info: ProjectInfo) extends DefaultProject(info) {
  }
}