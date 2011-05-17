#include <jni.h>
#include <string>
#include "JNICache.h"
#include "CRSMesh/CRSMesh.h"
#include "MeshIO/LisztFileReader.h"
#include "Layout/BoundarySetBuilder.h"

#ifndef MESHLOADER_H_
#define MESHLOADER_H_

using namespace std;

class MeshLoadException : public std::runtime_error {
public:
    MeshLoadException(const string& message) 
        : std::runtime_error(message) { };
};

class MeshLoader {
public:
	typedef CRSMesh::Mesh parent_mesh;
	typedef SizedMeshSet<parent_mesh::vertex_set::iterator> vertex_set;
	typedef SizedMeshSet<parent_mesh::edge_set::iterator> edge_set;
	typedef SizedMeshSet<parent_mesh::face_set::iterator> face_set;
	typedef SizedMeshSet<parent_mesh::cell_set::iterator> cell_set;

  /*
  env: JNI Environment pointer
  */
  MeshLoader(JNIEnv* env);
  ~MeshLoader();
  
  /*
  Load mesh from file.
  str: Filename
  returns: Java Mesh object
  */
  jobject loadMesh(jstring str);
  
private:
  /*
  Construct a Java object with the specific arguments.
  cls: Java class
  type: Java parameter type string
  ...: Constructor arguments
  returns: New Java object
  */
  jobject createObject(jclass cls, string type, ...);
  jobject createObjectV(jclass cls, string type, va_list args);
  jobject createObject(string clsStr, string type, ...);
  
  /*
  Call Scala methods
  */
  jobject callObjectMethod(jobject obj, string clsStr, string method, string sig, ...);
  jobject callObjectMethodV(jobject obj, string clsStr, string method, string sig, va_list args);
  /*jobject callObjMethod(jobject obj, jclass cls, string method, string sig, ...);
  jobject callObjMethodV(jobject obj, jclass cls, string method, string sig, va_list args);*/
  
  jobject callVoidMethod(jobject obj, string clsStr, string method, string sig, ...);
  jobject callVoidMethodV(jobject obj, string clsStr, string method, va_list args);
  /*jobject callVoidMethod(jobject obj, jclass cls, string method, string sig, ...);
  jobject callVoidMethodV(jobject obj, jclass cls, string method, va_list args); */
  
  /*
  Set a Scala field
  cls: Java class
  jobj: Scala object
  field: Name of field
  type: Java parameter type string
  ...: Value to set
  */
  void setScalaField(jclass cls, jobject jobj, string field, string type, ...);
  
  /*
  Set a CRS field
  jmesh: Mesh object
  field: Name of field
  crs: CRS
  from: Size of CRS
  */
  void setCRSField(jobject jmesh, string field, CRSMeshPrivate::CRS& crs, size_t from);
  
  /*
  Set a CRSConst field
  jmesh: Mesh object
  field: Name of field
  crs: CRS
  from: Size of CRS
  mult: Size of each item
  */
  void setCRSConstField(jobject jmesh, string field, CRSMeshPrivate::CRSConst& crs, size_t from, size_t mult);
  
  jintArray copyIdxArray(CRSMeshPrivate::idx_type* array, size_t len);
  jintArray copyIdArray(CRSMeshPrivate::id_type* array, size_t len);
  jintArray copyIdPairArray(CRSMeshPrivate::IDPair* array, size_t len);
  
  void loadPositions(jobject jmesh, CRSMesh::Mesh mesh, MeshIO::LisztFileReader reader);
  void loadBoundaries(jobject jmesh, MeshIO::LisztFileReader reader);
  
  template<typename MO, typename MeshSet>
  void loadBoundarySet(jobject& jmesh, CRSMesh::Mesh& mesh, const char* name, string field) {
    BoundarySet<MeshSet>* bs = new BoundarySet<MeshSet>();
    if(!bs) {
      throw new MeshLoadException("Could not create boundary set");
    }
       
    if(boundary_builder<CRSMesh::Mesh, MO, BoundarySet<MeshSet> >.load(&mesh, name, bs)) {
      jobject boundMap = getScalaObjField(meshClass, jmesh, field, "Lscala/collection/mutable/Map");
    
      // Copy bs over I guess
      jmethodID cid = cache.getMethod("scala/collection/mutable/Map", "put", "(Ljava/lang/Object,Ljava/lang/Object)Lscala/Option");
      if (cid == NULL) {
        throw new MeshLoadException("Failed to find put method"); /* exception thrown */
      }
      
      jobject bounds = createObject("ppl/dsl/deliszt/datastruct/scala/BoundSetImpl", "");

      // For ranges in bs
      for(MeshSet::iterator it = bs->iter(); it.hasNext();) {
        callVoidMethod(bounds, "ppl/dsl/deliszt/datastruct/scala/BoundSetImpl", "add", "(I)V", it.next().ID());
      }
      
      callVoidMethod(bounds, "ppl/dsl/deliszt/datastruct/scala/BoundSetImpl", "freeze", "()V");
      
      callObjectMethod(boundMap, "scala/collection/mutable/Map", "put", "(Ljava/lang/Object,Ljava/lang/Object)Lscala/Option", env->NewStringUTF(name), bounds);
    }
    
    delete bs;
  }

  jclass meshClass;
  JNIEnv* env;
  JNICache cache;
};

#endif
