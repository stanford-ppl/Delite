package ppl.delite.framework

object Config {
  val build_dir = System.getProperty("delite-build-dir", "delite-gen/")
  val deg_filename = System.getProperty("delite-deg-filename", "")
  val gen_kernels : Boolean = java.lang.Boolean.parseBoolean(System.getProperty("delite-gen-kernels", "false"))
}