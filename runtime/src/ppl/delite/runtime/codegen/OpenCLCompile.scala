package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.Config
import java.lang.StringBuilder
import tools.nsc.io.{Directory, Path}
import java.io.{File, FileReader, FileWriter}
import ppl.delite.runtime.graph.targets.OS

object OpenCLCompile extends CCompile {

  // List of kernel symbols of type OP_External
  // (used to exclude those kernel files from the OpenCL kernel list)
  val externList = ArrayBuffer[String]()

  def target = "opencl"
  override def ext = "cpp"

  protected def configFile = "OpenCL.xml"
  protected def compileFlags = Array("-w", "-O3", "-shared", "-fPIC", "-lOpenCL")
  protected def outputSwitch = "-o"

  override def compile() {
    super.compile()
    collectKernels()
  }

  private val globalKernels = ArrayBuffer[String]()
  def addGlobalKernel(str: String) {
    globalKernels.append(str)
  }

  // Generate the clKernels.cl file to be read by the GPU execution host thread
  // Basically read all the files with .cl extension
  private def collectKernels() {
    val kernelStr = new StringBuilder
    val kernelWriter = new FileWriter(binCacheHome + "clKernels.cl")

    //TODO: How to figure out whether target supports double precision? Maybe by calling device properties API
    kernelStr.append("#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n")
    kernelStr.append("#pragma OPENCL EXTENSION cl_khr_byte_addressable_store: enable\n")
    kernelStr.append("#define MAX_GROUP 4\n")  //TODO: This will get removed by generelizing the GPU HashReduce
    kernelStr.append("__kernel void dummy_kernel(__global float *dummy, int size) {\n")
    kernelStr.append("int i = get_global_id(0);\n")
    kernelStr.append("if(i < size) dummy[i] = dummy[i] + 1;\n")
    kernelStr.append("}\n")

    val dsLists = Directory(Path(sourceCacheHome + "datastructures")).files
    for (file <- dsLists if(file.extension=="cl")) {
      val srcChars = file.bytes()
      while (srcChars.hasNext) {
        kernelStr.append(srcChars.next().asInstanceOf[Char])
      }
    }
    val devFuncLists = Directory(Path(sourceCacheHome + "kernels")).files
    for (file <- devFuncLists if(file.extension=="cl")) {
      val srcChars = file.bytes()
      while (srcChars.hasNext) {
        kernelStr.append(srcChars.next().asInstanceOf[Char])
      }
    }
    globalKernels.foreach(g => kernelStr.append(g))
    kernelWriter.write(kernelStr.toString)
    kernelWriter.close()
  }

}
