import sbt._

class RuntimeProject(info: ProjectInfo) extends DefaultProject(info) {
 
  // source directory layout
  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "resources"

  override def testScalaSourcePath = "test-src"
  override def testResourcesPath = "test-resources"

  val scalaToolsSnapshots = ScalaToolsSnapshots
  val lift_json = "net.liftweb" %% "lift-json" % "2.4-SNAPSHOT"
  
  val apacheMaven = "Apache Maven Repository" at "https://repository.apache.org/content/groups/snapshots-group/"
  val commonsMath = "org.apache.commons" % "commons-math" % "3.0-SNAPSHOT"
  
//  val scalac = "org.scala-lang" % "scala-compiler" % "2.8.1" 
//  val scala = "org.scala-lang" % "scala-library" % "2.8.1" 

}
