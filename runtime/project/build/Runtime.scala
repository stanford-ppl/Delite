import sbt._

class RuntimeProject(info: ProjectInfo) extends DefaultProject(info) {
 
  // source directory layout
  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "resources"

  override def testScalaSourcePath = "test-src"
  override def testResourcesPath = "test-resources"

//  val scalac = "org.scala-lang" % "scala-compiler" % "2.8.1" 
//  val scala = "org.scala-lang" % "scala-library" % "2.8.1" 

}