package ppl.delite.framework

object Config {
  val degFilename = System.getProperty("delite.deg.filename", "")
  val deliteHome = System.getProperty("delite.home", System.getProperty("user.dir"))
  val buildDir = System.getProperty("build.dir", deliteHome + java.io.File.separator + "generated")
  val blasHome = System.getProperty("blas.home")
  val useBlas = if (blasHome == null) false else true
}
