import sbt._
import Keys._

object DeliteBuild extends Build {
  val virtualization_lms_core = "EPFL" % "lms_2.10.0-M1-virtualized" % "0.2"
  
  // FIXME: custom-built scalatest
  val dropboxScalaTestRepo = "Dropbox" at "http://dl.dropbox.com/u/12870350/scala-virtualized"
  val scalatest = "org.scalatest" % "scalatest_2.10.0-virtualized-SNAPSHOT" % "1.6.1-SNAPSHOT" //% "test"

  val virtScala = "2.10.0-M1-virtualized"//"2.10.0-virtualized-SNAPSHOT"
  val virtBuildSettingsBase = Defaults.defaultSettings ++ Seq(
    resolvers += ScalaToolsSnapshots, 
    organization := "stanford-ppl",
    resolvers += dropboxScalaTestRepo,
    scalaVersion := virtScala,
    scalaBinaryVersion := virtScala,
    publishArtifact in (Compile, packageDoc) := false,
    libraryDependencies += virtualization_lms_core,
    // needed for scala.tools, which is apparently not included in sbt's built in version
    libraryDependencies += "org.scala-lang" % "scala-library" % virtScala,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % virtScala,
    // used in delitec to access jars
    retrieveManaged := true,
    scalacOptions += "-Yno-generic-signatures",
    scalacOptions += "-Yvirtualize"
  )

  val virtBuildSettings = virtBuildSettingsBase ++ Seq(
    scalaSource in Compile <<= baseDirectory(_ / "src"),
    scalaSource in Test <<= baseDirectory(_ / "test-src")
  )


  /*
  val vanillaScala = "2.9.1"
  val vanillaBuildSettings = Defaults.defaultSettings ++ Seq(
    //scalaSource in Compile <<= baseDirectory(_ / "src"),
    //scalaVersion := vanillaScala,
    // needed for scala.tools, which is apparently not included in sbt's built in version
    libraryDependencies += "org.scala-lang" % "scala-library" % vanillaScala,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % vanillaScala
  )
  */

  /*
  lazy val getJars = TaskKey[Unit]("get-jars")
  lazy val getJarsTask = getJars <<= (target, fullClasspath in Runtime) map { (target, cp) =>
    println("Target path is: "+target)
    println("Full classpath is: "+cp.map(_.data).mkString(":"))
  }
  */

  // build targets

  // _ forces sbt to choose it as default
  // useless base directory is to avoid compiling leftover .scala files in the project root directory
  lazy val _delite = Project("delite", file("project/boot")) aggregate(framework, dsls, runtime, apps, tests)

  lazy val framework = Project("framework", file("framework"), settings = virtBuildSettings) dependsOn(runtime) // dependency to runtime because of Scopes

  lazy val dsls = Project("dsls", file("dsls"), settings = virtBuildSettings) aggregate(optila, optiml, optiql) 
  lazy val optila = Project("optila", file("dsls/optila"), settings = virtBuildSettings) dependsOn(framework)
  lazy val optiml = Project("optiml", file("dsls/optiml"), settings = virtBuildSettings) dependsOn(optila)
  lazy val optiql = Project("optiql", file("dsls/optiql"), settings = virtBuildSettings) dependsOn(framework)

  lazy val apps = Project("apps", file("apps/scala"), settings = virtBuildSettings) dependsOn(optiml, optiql)

  lazy val runtime = Project("runtime", file("runtime"), settings = virtBuildSettings)

  lazy val tests = Project("tests", file("tests"), settings = virtBuildSettingsBase ++ Seq(
    scalaSource := file("tests/main-src"),
    scalaSource in Test := file("tests/src"),
    libraryDependencies += scalatest,
    parallelExecution in Test := false
    // don't appear to be able to depend on a different scala version simultaneously, so just using scala-virtualized for everything
  )) dependsOn(framework, runtime, optiml, apps)
  
  //dependsOn(framework % "test->compile;compile->compile", optiml % "test->compile;compile->compile", optiql % "test", optimlApps % "test->compile;compile->compile", runtime % "test->compile;compile->compile")
}
