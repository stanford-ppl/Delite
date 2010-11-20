package ppl.delite.framework

object Config {
  val kernel_path = System.getProperty("delite-kernelpath", "generated/delite-gen/")
  val deg_filename = System.getProperty("delite-deg-filename", "")
  val gen_kernels : Boolean = java.lang.Boolean.parseBoolean(System.getProperty("delite-gen-kernels", "false"))
}