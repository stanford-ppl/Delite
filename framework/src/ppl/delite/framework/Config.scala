package ppl.delite.framework

object Config {
  //var degFilename = System.getProperty("delite.deg.filename", "")
  var degFilename = System.getProperty("delite.deg.filename", "out.deg")
  var opfusionEnabled = System.getProperty("delite.opfusion.enabled", "false") != "false"
  var homeDir = System.getProperty("delite.home.dir", System.getProperty("user.dir"))
  //var buildDir = System.getProperty("delite.build.dir", homeDir + java.io.File.separator + "generated")
  var buildDir = System.getProperty("delite.build.dir", "generated")
  var printGlobals = System.getProperty("delite.print_globals.enabled", "false") == "true"
  var useBlas = System.getProperty("blas.enabled", "false") != "false"
  var collectStencil = System.getProperty("liszt.stencil.enabled", "false") == "true"
  var nestedVariantsLevel = System.getProperty("nested.variants.level", "0").toInt
}
