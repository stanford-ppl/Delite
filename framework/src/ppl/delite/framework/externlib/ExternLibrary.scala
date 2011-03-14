package ppl.delite.framework.externlib

import ppl.delite.framework._
import java.io._

object ExternLibrary {

  /* Emit and Compile external library */
  def init {
    if (Config.useBlas) {
      emitLib
      compileLib
    }
  }

  /* Emit source files for MKL BLAS library */
  def emitLib {
    
    val sep = java.io.File.separator
    val buildPath = Config.buildDir + sep + "scala" + sep
    val outDir = new File(buildPath); outDir.mkdirs()
    val outCFile = new File(buildPath + "scalaBLAS.c")
    val outScalaFile = new File(buildPath + "scalaBLAS.scala")
    val cstream = new PrintWriter(outCFile)
    val scalastream = new PrintWriter(outScalaFile)
    
    val packageName = "generated.scala"
    val jniPackageName = "generated_scala"
    
    scalastream.println("""
package %s
object scalaBLAS {
  System.load("%sscalaBLAS.so")
  @native
  def matMult[@specialized(Double,Float) T](mat1:Array[T], mat2:Array[T], mat3:Array[T], mat1_r:Int, mat1_c:Int, mat2_c:Int)
  @native
  def matVMult[@specialized(Double,Float) T](mat1:Array[T], vec2:Array[T], vec3:Array[T], mat_row:Int, mat_col:Int, vec_offset:Int, vec_stride:Int)
  @native
  def sigmoid[@specialized(Double,Float) T](vec1:Array[T], vec2:Array[T], start:Int, end:Int)
}
""".format(packageName, buildPath))
    scalastream.flush

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
  cstream.flush

  }

  /* Compiles generated library files */
  def compileLib {
    val javaHome = System.getProperty("java.home")
    val buildPath = Config.buildDir + java.io.File.separator + "scala" + java.io.File.separator

    /* Compile JNI Implementation */
    val process = Runtime.getRuntime.exec(Array[String](
      "icc",
      "-w",
      "-O3",
      "-I" + javaHome + "/../include", "-I" + javaHome + "/../include/linux",
      "-I" + Config.blasHome + "/mkl/include",
      "-L" + Config.blasHome + "/mkl/lib/em64t",
      "-L" + Config.blasHome + "/lib/intel64",
      "-lmkl_intel_lp64", "-lmkl_intel_thread", "-lmkl_core", "-liomp5", "-lmkl_mc3", "-lmkl_def", "-lgfortran",
      "-shared", "-fPIC", //dynamic shared library
      "-o", "scalaBLAS.so", //output name
      "scalaBLAS.c" //input name
      ), null, new File(buildPath))
    process.waitFor
	  checkError(process)
  }

  def checkError(process:Process) {
    val first = process.getErrorStream.read
    if (first != -1) { //compilation failed
      val errorBuffer = new Array[Byte](1000)
      val num = process.getErrorStream.read(errorBuffer)
      print(first.asInstanceOf[Char])
      for (i <- 0 until num) print(errorBuffer(i).asInstanceOf[Char])
      println()
      sys.error("MKL BLAS compilation failed")
    }
  }
}
