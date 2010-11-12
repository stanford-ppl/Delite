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
  override def parallelExecution = true
  
  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "resources"
  
  override def testScalaSourcePath = "tests" / "src"
  override def testResourcesPath = "tests" / "resources"
  
  val virtualization_lms_core = "scala" % "virtualization-lms-core_2.8.x-virtualized-SNAPSHOT" % "0.1"
  
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" % "1.2.1-SNAPSHOT"
  
  /**
    * Hacks to move build directory outside of src directory. should never be checked in.
    */
  val deliteBuildPath = Path.fromFile("/Users/asujeeth/build/sbt/delite")
  val deliteSrcPath = Path.fromFile("/Users/asujeeth/src/workspace/delite2/")
  val root = Path.fromFile("/")
  
  override def outputPath = deliteBuildPath
  override def managedDependencyPath = outputPath / "lib_managed"

  override def compileOptions = super.compileOptions ++ compileOptions("-Xmx2g")

  // Define project class with default source tree layout
  class FlatProject(info: ProjectInfo, moduleName: String, hackhack: String) extends DefaultProject(info) {
    // Source tree layout
    override def mainScalaSourcePath = deliteSrcPath / moduleName / hackhack / "src"
    override def mainResourcesPath = deliteSrcPath / moduleName / hackhack / "resources"
    override def testScalaSourcePath = deliteSrcPath / moduleName / hackhack / "tests" / "src"
    override def testResourcesPath = deliteSrcPath / moduleName / hackhack / "tests" / "resources"
    override def outputPath = deliteBuildPath / moduleName
    override def managedDependencyPath = outputPath / "lib_managed"
    
    val virtualization_lms_core = "scala" % "virtualization-lms-core_2.8.x-virtualized-SNAPSHOT" % "0.1"
    
    val scalaToolsSnapshots = ScalaToolsSnapshots
    val scalatest = "org.scalatest" % "scalatest" % "1.2.1-SNAPSHOT"
    
    override def localScala =
    defineScala("2.8.x-virtualized-SNAPSHOT", new File(local.scalaVirtualizedHome.get.getOrElse {
      log.error("scala.virtualized.home needs to be defined in local.properties and "+
      "must point to a valid scala-virtualized home directory"); "<undefined>"
    }))::Nil 
  }
  
  // Define projects
  lazy val framework = project(root, "Delite Framework", new FlatProject(_, "framework",""))
  lazy val runtime = project(root, "Delite Runtime", new FlatProject(_, "runtime",""), framework)
  
  lazy val dsls = project(root, "DSLs", new DSLs(_, "dsls"), framework)
   
  class DSLs(info: ProjectInfo, moduleName: String) extends DefaultProject(info) {
    override def outputPath = deliteBuildPath / moduleName
    override def managedDependencyPath = outputPath / "lib_managed"
    
    lazy val optiml = project(root, "OptiML", new FlatProject(_, "optiml", moduleName), framework)
  }
}
