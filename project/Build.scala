import sbt._
import Keys._

object DeliteBuild extends Build {
  val virtualization_lms_core = "EPFL" % "lms_2.10" % "0.3-SNAPSHOT"
  
  System.setProperty("showSuppressedErrors", "false")

  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.2")
  val virtBuildSettingsBase = Defaults.defaultSettings ++ Seq(
    organization := "stanford-ppl",
    scalaOrganization := "org.scala-lang.virtualized",
    scalaVersion := virtScala,
    publishArtifact in (Compile, packageDoc) := false,
    libraryDependencies += virtualization_lms_core,
    libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % virtScala,
    libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % virtScala,
    libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.1.2",

    libraryDependencies += "org.apache.commons" % "commons-math" % "2.2",
    libraryDependencies += "com.google.protobuf" % "protobuf-java" % "2.4.1",
    libraryDependencies += "org.apache.mesos" % "mesos" % "0.9.0-incubating",    
    libraryDependencies += "org.apache.hadoop" % "hadoop-core" % "1.2.0",

    retrieveManaged := true,
    scalacOptions += "-Yno-generic-signatures",
    scalacOptions += "-Yvirtualize"
  )

  val virtBuildSettings = virtBuildSettingsBase ++ Seq(
    scalaSource in Compile <<= baseDirectory(_ / "src"),
    scalaSource in Test <<= baseDirectory(_ / "tests"),
    parallelExecution in Test := false,
    concurrentRestrictions in Global += Tags.limitAll(1) //we need tests to run in isolation across all projects
  )

  // build targets
  
  //default project: just the dependencies needed to export Delite to others (e.g., Forge)
  lazy val delite = Project("delite", file("."), //root directory required to be default
    settings = virtBuildSettings) aggregate(framework, runtime, deliteTest)

  lazy val framework = Project("framework", file("framework"), settings = virtBuildSettings) dependsOn(runtime) // dependency to runtime because of Scopes

  lazy val deliteTest = Project("delite-test", file("framework/delite-test"), settings = virtBuildSettings) dependsOn(framework, runtime)

  lazy val dsls = Project("dsls", file("dsls"), settings = virtBuildSettings) aggregate(optila, optiml, optiql, optimesh, optigraph, opticvx) 
  lazy val optila = Project("optila", file("dsls/optila"), settings = virtBuildSettings) dependsOn(framework, deliteTest)
  lazy val optiml = Project("optiml", file("dsls/optiml"), settings = virtBuildSettings) dependsOn(optila, deliteTest)
  lazy val optiql = Project("optiql", file("dsls/optiql"), settings = virtBuildSettings) dependsOn(framework, deliteTest)
  lazy val optimesh = Project("optimesh", file("dsls/deliszt"), settings = virtBuildSettings) dependsOn(framework, deliteTest)
  lazy val optigraph = Project("optigraph", file("dsls/optigraph"), settings = virtBuildSettings) dependsOn(framework, deliteTest)
  lazy val opticvx = Project("opticvx", file("dsls/opticvx"), settings = virtBuildSettings) dependsOn(framework, deliteTest)

  lazy val apps = Project("apps", file("apps"), settings = virtBuildSettings) aggregate(optimlApps, optiqlApps, optimeshApps, optigraphApps, opticvxApps, interopApps, deliteApps)
  lazy val optimlApps = Project("optiml-apps", file("apps/optiml"), settings = virtBuildSettings) dependsOn(optiml)
  lazy val optiqlApps = Project("optiql-apps", file("apps/optiql"), settings = virtBuildSettings) dependsOn(optiql)
  lazy val optimeshApps = Project("optimesh-apps", file("apps/deliszt"), settings = virtBuildSettings) dependsOn(optimesh)
  lazy val optigraphApps = Project("optigraph-apps", file("apps/optigraph"), settings = virtBuildSettings) dependsOn(optigraph)
  lazy val opticvxApps = Project("opticvx-apps", file("apps/opticvx"), settings = virtBuildSettings) dependsOn(opticvx)
  lazy val interopApps = Project("interop-apps", file("apps/multi-dsl"), settings = virtBuildSettings) dependsOn(optiml, optiql, optigraph) // dependsOn(dsls) not working
  lazy val deliteApps = Project("delite-apps", file("apps/delite"), settings = virtBuildSettings) dependsOn(deliteTest) // dependsOn(dsls) not working

  lazy val runtime = Project("runtime", file("runtime"), settings = virtBuildSettings)

  //include all projects that should be built (dependsOn) and tested (aggregate)
  lazy val tests = (Project("tests", file("project/boot"), settings = virtBuildSettings)
    dependsOn(optimlApps, optiqlApps, optigraphApps, interopApps, deliteApps) aggregate(framework, deliteTest, optiml, optiql, optigraph))
}
