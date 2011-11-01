#include <jni.h>
#include "ppl_dsl_deliszt_datastruct_scala_MeshLoader.h"
#include "MeshLoader.h"

System::MeshLoader* ml;

JNIEXPORT jobject JNICALL Java_ppl_dsl_deliszt_datastruct_scala_MeshLoader_loadMesh (JNIEnv* env, jobject obj, jstring str) {
  ml = new System::MeshLoader();

  jobject jmesh = ml->loadMesh(env, str);

  return jmesh;
}

JNIEXPORT jobject JNICALL Java_generated_scala_MeshLoader_loadMesh (JNIEnv* env, jobject obj, jstring str) {
  ml = new System::MeshLoader(true);

  jobject jmesh = ml->loadMesh(env, str);

  return jmesh;
}

JNIEXPORT jobject JNICALL Java_ppl_dsl_deliszt_datastruct_scala_MeshLoader__1loadBoundarySet
  (JNIEnv * env, jobject obj, jstring str, jint type) {
  string name(env->GetStringUTFChars(str, 0));
  
  if(type == 0) {
    return ml->loadBoundarySet<LisztPrivate::ElemTypes::VertexType>(env, name.c_str());
  }
  else if(type == 1) {
    return ml->loadBoundarySet<LisztPrivate::ElemTypes::EdgeType>(env, name.c_str());
  }
  else if(type == 2) {
    return ml->loadBoundarySet<LisztPrivate::ElemTypes::FaceType>(env, name.c_str());
  }
  else if(type == 3) {
    return ml->loadBoundarySet<LisztPrivate::ElemTypes::CellType>(env, name.c_str());
  }
  
  return NULL;
}

JNIEXPORT jobject JNICALL Java_generated_scala_MeshLoader__1loadBoundarySet
  (JNIEnv * env, jobject obj, jstring str, jint type) {
  return Java_ppl_dsl_deliszt_datastruct_scala_MeshLoader__1loadBoundarySet(env, obj, str, type);
}
