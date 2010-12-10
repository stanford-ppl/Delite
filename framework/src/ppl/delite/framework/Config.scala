package ppl.delite.framework

object Config {
  val build_dir = System.getProperty("delite-build-dir", "generated/")
  val deg_filename = System.getProperty("delite-deg-filename", "")
}