package ppl.delite.framework

object Config {
  //var degFilename = System.getProperty("delite.deg.filename", "")
  var degFilename = System.getProperty("delite.deg.filename", "out.deg")
  var opfusionEnabled = System.getProperty("delite.opfusion.enabled", "false") != "false"
  var homeDir = System.getProperty("delite.home.dir", System.getProperty("user.dir"))
  //var buildDir = System.getProperty("delite.build.dir", homeDir + java.io.File.separator + "generated")
  var buildDir = System.getProperty("delite.build.dir", "generated")
  var blasHome = System.getProperty("blas.home")
  var blasInit = System.getProperty("blas.init")
  var useBlas = if (blasHome == null) false else true
  var nestedVariantsLevel = System.getProperty("nested.variants.level", "0").toInt
}
