#include <jni.h>
#include "ppl_dsl_deliszt_datastruct_scala_MeshLoader__.h"
#include "MeshLoader.h"

JNIEXPORT jobject JNICALL Java_ppl_dsl_deliszt_datastruct_scala_MeshLoader_loadMesh (JNIEnv* env, jobject obj, jstring str) {
  MeshLoader ml(env);
  jobject jmesh = ml.loadMesh(str);
  
  return jmesh;
}