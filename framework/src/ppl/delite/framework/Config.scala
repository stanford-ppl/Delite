package ppl.delite.framework

object Config {
  //val degFilename = System.getProperty("delite.deg.filename", "")
  val degFilename = System.getProperty("delite.deg.filename", "out.deg")
  val opfusionEnabled = System.getProperty("delite.opfusion.enabled", "false") != "false"
  val homeDir = System.getProperty("delite.home.dir", System.getProperty("user.dir"))
  //val buildDir = System.getProperty("delite.build.dir", homeDir + java.io.File.separator + "generated")
  val buildDir = System.getProperty("delite.build.dir", "generated")
  val blasHome = System.getProperty("blas.home")
  val useBlas = if (blasHome == null) false else true
  val nestedVariantsLevel = System.getProperty("nested.variants.level", "1").toInt
}
