import sbt._
import java.io.File

class ParallelSortBenchProject(info: ProjectInfo) extends DefaultProject(info) {

  // source directory layout
  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "resources"

  override def testScalaSourcePath = "test" / "src"
  override def testResourcesPath = "test" / "resources"

 // val scalatest = "org.scalatest" % "scalatest" % "1.4-SNAPSHOT"

//  val scalac = "org.scala-lang" % "scala-compiler" % "2.8.1"
//  val scala = "org.scala-lang" % "scala-library" % "2.8.1"

}