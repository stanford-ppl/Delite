package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.Config
import java.lang.StringBuilder
import tools.nsc.io.{Directory, Path}
import java.io.{File, FileReader, FileWriter}
import ppl.delite.runtime.graph.targets.OS

object OpenCLCompile extends GPUCompile {

  // List of kernel symbols of type OP_External
  // (used to exclude those kernel files from the OpenCL kernel list)
  val externList = ArrayBuffer[String]()

  override def target = "opencl"
  override def ext = "cpp"

  override def compile() {
    super.compile()
    collectKernels()
  }

  //TODO: handle more than one runtime object
  def compile(destination: String, source: String, paths: Array[String]) {
    Directory(Path(destination)).createDirectory()


    println("Compiling OpenCL...")

    //TODO: fix the include & library path issue
    //TODO: Can remove the clblas related options (-I,-l,-L) if the library init routines are included in the framework OP_External
    val cmdString = Array[String](
      "g++",
      "-w", //suppress warnings
      "-I" + Config.clHeaderPath,     // TODO: Remove
      "-I" + Config.clBlasHeaderPath, // TODO: Remove
      "-I" + javaHome + sep + ".." + sep + "include",
      "-I" + javaHome + sep + ".." + sep + "include" + sep + OS.jniMD) ++ //jni
      paths.map("-I"+_) ++
      Array[String](
      "-I" + paths.mkString(" -I"),
      "-I" + deliteHome + sep + "runtime" + sep + "opencl",
      "-O2", //optimized
      "-shared", "-fPIC", //dynamic shared library
      "-lOpenCL",
      "-lclblas",                    // TODO: Remove
      "-L" + Config.clBlasLibPath,   // TODO: Remove
      "-L" + deliteLibs) ++ linkGeneratedLibs(deliteLibs) ++ Array[String](
      "-o", "openclHost.so", //output name
      source //input name
    )
    val process = Runtime.getRuntime.exec(cmdString, null, new File(destination))
    //println("Compilation Command : " + cmdString.mkString(" "))

    process.waitFor //wait for compilation to complete
    checkError(process)
  }

  def compileInit() {
    val cmdString = Array[String](
      "g++",
      "-w", //suppress warnings
      "-I" + javaHome + sep + ".." + sep + "include",
      "-I" + javaHome + sep + ".." + sep + "include" + sep + OS.jniMD, //jni
      "-shared", "-fPIC", //dynamic shared library
      "-o", Config.deliteHome + sep + "runtime" + sep + "opencl" + sep + "openclInit.so", //output name
      Config.deliteHome + sep + "runtime" + sep + "opencl" + sep + "openclInit.cpp"
      )
    val process = Runtime.getRuntime.exec(cmdString, null, new File(Config.deliteHome))
    process.waitFor //wait for compilation to complete
    checkError(process)
  }

  // Generate the clKernels.cl file to be read by the GPU execution host thread
  // Basically read all the files with .cl extension
  private def collectKernels() {
    val kernelStr = new StringBuilder
    val kernelWriter = new FileWriter(binCacheHome + "clKernels.cl")

    //TODO: How to figure out whether target supports double precision? Maybe by calling device properties API
    kernelStr.append("#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n")
    kernelStr.append("#pragma OPENCL EXTENSION cl_khr_byte_addressable_store: enable\n")
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
    val devFuncReader = new FileReader(sourceCacheHome + "kernels" + File.separator + "devFuncs.cl")
    var ch = devFuncReader.read()
    while (ch != -1) {
      kernelStr.append(ch.asInstanceOf[Char])
      ch = devFuncReader.read()
    }
    val kernelLists = Directory(Path(sourceCacheHome + "kernels")).files
    for (file <- kernelLists if((file.extension=="cl")&&(file.name!="devFuncs.cl")&&(!externList.contains(file.name.substring(0,file.name.length-3))))) {
      val srcChars = file.bytes()
      while (srcChars.hasNext) {
        kernelStr.append(srcChars.next().asInstanceOf[Char])
      }
    }
    kernelWriter.write(kernelStr.toString)
    kernelWriter.close()
  }

}
