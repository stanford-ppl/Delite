package ppl.delite.framework.externlib

import ppl.delite.framework._
import java.io._

object ExternLibrary {
  val os = System.getProperty("os.name")
  val windows = os.startsWith("Windows")
  val unix = !windows

  /* Emit and Compile external library */
  def init() {
    if (Config.useBlas) {
      emitLib()
      compileLib()
    }
  }

  /* Emit source files for MKL BLAS library */
  def emitLib() {
    val buildPath = Config.buildDir + File.separator + "scala" + File.separator + "kernels" + File.separator
    val outDir = new File(buildPath); outDir.mkdirs()
    val outCFile = new File(buildPath + "scalaBLAS.c")
    val outScalaFile = new File(buildPath + "scalaBLAS.scala")
    val outSharedLibrary = if (windows) new File(buildPath + "scalaBLAS.dll") else new File(buildPath + "scalaBLAS.so")
    val cstream = new PrintWriter(outCFile)
    val scalastream = new PrintWriter(outScalaFile)
    
    val packageName = "generated.scala"
    val jniPackageName = "generated_scala"
    
    scalastream.println("""
package %s
object scalaBLAS {
  System.load(%s)
  @native
  def matMult[@specialized(Double,Float) T](mat1:Array[T], mat2:Array[T], mat3:Array[T], mat1_r:Int, mat1_c:Int, mat2_c:Int)
  @native
  def matVMult[@specialized(Double,Float) T](mat1:Array[T], vec2:Array[T], vec3:Array[T], mat_row:Int, mat_col:Int, vec_offset:Int, vec_stride:Int)
  @native
  def sigmoid[@specialized(Double,Float) T](vec1:Array[T], vec2:Array[T], start:Int, end:Int)
}
""".format(packageName, "\"\"\"" + outSharedLibrary.getAbsolutePath + "\"\"\""))
    scalastream.flush()

    cstream.println("""
#include <stdlib.h>
#include <stdio.h>

#include <jni.h>
#include "mkl.h"

JNIEXPORT void JNICALL Java_%s_scalaBLAS_00024_matMult_00024mDc_00024sp
(JNIEnv *env, jobject obj, jdoubleArray mat1, jdoubleArray mat2, jdoubleArray mat3, jint mat1_r, jint mat1_c, jint mat2_c)
{
    jboolean copy;
    jdouble *mat1_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy);
    jdouble *mat2_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat2, &copy);
    jdouble *mat3_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat3, &copy);

    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, mat1_r, mat2_c, mat1_c, 1.0, mat1_ptr, mat1_c, mat2_ptr, mat2_c, 0.0, mat3_ptr, mat2_c);

    (*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
    (*env)->ReleasePrimitiveArrayCritical(env, mat2, mat2_ptr, 0);
    (*env)->ReleasePrimitiveArrayCritical(env, mat3, mat3_ptr, 0);
}

JNIEXPORT void JNICALL Java_%s_scalaBLAS_00024_matMult_00024mFc_00024sp
(JNIEnv *env, jobject obj, jfloatArray mat1, jfloatArray mat2, jfloatArray mat3, jint mat1_r, jint mat1_c, jint mat2_c)
{
    jboolean copy;

    jfloat *mat1_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy));
    jfloat *mat2_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat2, &copy));
    jfloat *mat3_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat3, &copy));

    cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, mat1_r, mat2_c, mat1_c, 1.0, mat1_ptr, mat1_c, mat2_ptr, mat2_c, 0.0, mat3_ptr, mat2_c);

    (*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
    (*env)->ReleasePrimitiveArrayCritical(env, mat2, mat2_ptr, 0);
    (*env)->ReleasePrimitiveArrayCritical(env, mat3, mat3_ptr, 0);
}

JNIEXPORT void JNICALL Java_%s_scalaBLAS_00024_matVMult_00024mFc_00024sp
(JNIEnv *env, jobject obj, jfloatArray mat1, jfloatArray vec2, jfloatArray vec3, jint mat_row, jint mat_col, jint vec_offset, jint vec_stride)
{
    jboolean copy;

    jfloat *mat1_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy));
    jfloat *vec2_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));
    jfloat *vec3_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec3, &copy));

    vec2_ptr += vec_offset;

    cblas_sgemv(CblasRowMajor, CblasNoTrans, mat_row, mat_col, 1.0, mat1_ptr, mat_row, vec2_ptr, vec_stride, 0.0, vec3_ptr, 1);

    (*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
    (*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
    (*env)->ReleasePrimitiveArrayCritical(env, vec3, vec3_ptr, 0);
}

JNIEXPORT void JNICALL Java_%s_scalaBLAS_00024_matVMult_00024mDc_00024sp
(JNIEnv *env, jobject obj, jdoubleArray mat1, jdoubleArray vec2, jdoubleArray vec3, jint mat_row, jint mat_col, jint vec_offset, jint vec_stride)
{
    jboolean copy;

    jdouble *mat1_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy));
    jdouble *vec2_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));
    jdouble *vec3_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec3, &copy));

    vec2_ptr += vec_offset;

    cblas_dgemv(CblasRowMajor, CblasNoTrans, mat_row, mat_col, 1.0, mat1_ptr, mat_col, vec2_ptr, vec_stride, 0.0, vec3_ptr, 1);

    (*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
    (*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
    (*env)->ReleasePrimitiveArrayCritical(env, vec3, vec3_ptr, 0);
}

JNIEXPORT void JNICALL Java_%s_scalaBLAS_00024_sigmoid_00024mFc_00024sp
(JNIEnv *env, jobject obj, jfloatArray vec1, jfloatArray vec2, jint start, jint end)
{
    int i = 0;
    jboolean copy;
    
    jfloat *vec1_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec1, &copy));
    jfloat *vec2_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));
    
    for(i=start; i<end; i++) {
        vec2_ptr[i] = 1.0 / (1.0+expf(-1.0*vec1_ptr[i]));
    }

    (*env)->ReleasePrimitiveArrayCritical(env, vec1, vec1_ptr, 0);
    (*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
}

JNIEXPORT void JNICALL Java_%s_scalaBLAS_00024_sigmoid_00024mDc_00024sp
(JNIEnv *env, jobject obj, jdoubleArray vec1, jdoubleArray vec2, jint start, jint end)
{
    int i = 0;
    jboolean copy;
    
    jdouble *vec1_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec1, &copy));
    jdouble *vec2_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));

    for(i=start; i<end; i++) {
        vec2_ptr[i] = 1.0 / (1.0+exp(-1.0*vec1_ptr[i]));
    }

    (*env)->ReleasePrimitiveArrayCritical(env, vec1, vec1_ptr, 0);
    (*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
}
""".format(jniPackageName,jniPackageName,jniPackageName,jniPackageName,jniPackageName,jniPackageName))
  cstream.flush()

  }

  /* Compiles generated library files */
  def compileLib() {
    val javaHome = System.getProperty("java.home")
    val blasHome = Config.blasHome
    val buildPath = Config.buildDir + File.separator + "scala" + File.separator + "kernels" + File.separator

    val windows_script = File.createTempFile("delite-blas-compiler.", ".bat")
    val unix_script = File.createTempFile("delite-blas-compiler.", "")
    val script = if (windows) windows_script else unix_script
    val stream = new PrintWriter(script)
    if (unix) stream.println("#!/usr/bin/env bash")
    
    if (Config.blasInit != null) stream.println(Config.blasInit)
    
    if (windows) {
        val compiler = "icl"
        var args = List[String]()
        args :+= """/nologo"""
        args :+= """/w"""
        args :+= """/O3"""
        args :+= """/I"%s\..\include"""".format(javaHome)
        args :+= """/I"%s\..\include\win32"""".format(javaHome)
        args :+= """/I"%s\mkl\include"""".format(blasHome)
        args :+= """/LIBPATH:"%s\mkl\lib\em64t"""".format(blasHome)
        args :+= """/LIBPATH:"%s\mkl\lib\intel64"""".format(blasHome)
        args :+= """/LIBPATH:"%s\compiler\lib\em64t"""".format(blasHome)
        args :+= """/LIBPATH:"%s\compiler\lib\intel64"""".format(blasHome)
        args ++= List("mkl_intel_lp64.lib", "mkl_intel_thread.lib", "mkl_core.lib", "libiomp5mt.lib")
        args :+= "/LD"
        args :+= "/Fe:scalaBLAS.dll"
        args :+= "scalaBLAS.c"
        stream.println("%s %s".format(compiler, args.mkString(" ")))
    } else {
        val compiler = "icc"
        var args = List[String]()
        args :+= """-w"""
        args :+= """-O3"""
        args :+= """-I"%s/../include"""".format(javaHome)
        args :+= """-I"%s/../include/linux"""".format(javaHome)
        args :+= """-I"%s/mkl/include"""".format(blasHome)
        // unfortunately, intel libs cannot be linked statically, since the distribution provides neither libmkl_mc3.a nor libmkl_def.a
        // that's why, unlike on Windows, we've got to either hardcode the libpath in LD_LIBRARY_PATH or provide it every time when running delite
        // args ++= List("-Bstatic", "-static-intel")
        args :+= """-L"%s/mkl/lib/em64t"""".format(blasHome)
        args :+= """-L"%s/mkl/lib/intel64"""".format(blasHome)
        args :+= """-L"%s/lib/em64t"""".format(blasHome)
        args :+= """-L"%s/lib/intel64"""".format(blasHome)
        args ++= List("-lmkl_intel_lp64", "-lmkl_intel_thread", "-lmkl_core", "-liomp5", "-lmkl_mc3", "-lmkl_def", "-lgfortran")
        args :+= "-shared -fPIC"
        args :+= "-o scalaBLAS.so"
        args :+= "scalaBLAS.c"
        stream.println("%s %s".format(compiler, args.mkString(" ")))
    }
    
    stream.close
    script.setExecutable(true)
    val process = Runtime.getRuntime().exec(script.getAbsolutePath, null, new File(buildPath))
    process.waitFor
    checkError(process)
  }

  private def checkError(process: Process) {
    val errorStream = process.getErrorStream
    var err = errorStream.read()
    if (err != -1) {
      while (err != -1) {
        print(err.asInstanceOf[Char])
        err = errorStream.read()
      }
      println()
      sys.error("MKL BLAS compilation failed")
    }
  }
}
