#include <jni.h>
#include "ppl_dsl_deliszt_datastruct_scala_MeshLoader__.h"
#include "MeshLoader.h"

System::MeshLoader ml;

JNIEXPORT jobject JNICALL Java_ppl_dsl_deliszt_datastruct_scala_MeshLoader_loadMesh (JNIEnv* env, jobject obj, jstring str) {
  ml.init(env);
  jobject jmesh = ml.loadMesh(str);
  
  return jmesh;
}

JNIEXPORT jobject JNICALL Java_ppl_dsl_deliszt_datastruct_scala_MeshLoader__1loadBoundaries
  (JNIEnv * env, jobject obj, jstring str, jobject moc) {
  int type = ml.callIntMethod(moc,
                    "ppl/dsl/deliszt/datastruct/scala/MeshObjConstruct", "_type",
                    "()I");
  
  string name(env->GetStringUTFChars(str, 0));
  
  if(type == 1) {
    return ml.loadBoundarySet<CRSMeshPrivate::Cell>(name.c_str());
  }
  else if(type == 2) {
    return ml.loadBoundarySet<CRSMeshPrivate::Edge>(name.c_str());
  }
  else if(type == 3) {
    return ml.loadBoundarySet<CRSMeshPrivate::Face>(name.c_str());
  }
  else if(type == 4) {
    return ml.loadBoundarySet<CRSMeshPrivate::Vertex>(name.c_str());
  }
  
  return NULL;
}
