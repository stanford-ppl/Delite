#include <jni.h>
#include <pthread.h>
#include <string>
#include <stdexcept>
#include <stdarg.h>
#include "JNICache.h"
#include "CRSMesh/CRSMesh.h"
#include "MeshIO/LisztFileReader.h"
#include "Liszt/BoundarySet.h"
#include "Layout/BoundarySetBuilder.h"

#ifndef MESHLOADER_H_
#define MESHLOADER_H_

using namespace std;

namespace System {
class MeshLoader {
public:
    typedef CRSMesh::Mesh parent_mesh;
    typedef SizedMeshSet<parent_mesh::vertex_set::iterator> vertex_set;
    typedef SizedMeshSet<parent_mesh::edge_set::iterator> edge_set;
    typedef SizedMeshSet<parent_mesh::face_set::iterator> face_set;
    typedef SizedMeshSet<parent_mesh::cell_set::iterator> cell_set;

    MeshLoader(bool generated=false);
    ~MeshLoader();

    /*
     Load mesh from file.
     str: Filename
     returns: Java Mesh object
     */
    jobject loadMesh(JNIEnv* env, jstring str);

    template<typename MO>
    jobject loadBoundarySet(JNIEnv* env, const char* name)
    {
      std::cout << "Loading boundary set " << name << std::endl;
  
      LisztPrivate::BoundarySet *bs = new LisztPrivate::BoundarySet();
      if (!bs) {
        throw MeshIO::MeshLoadException("Could not create boundary set");
      }

      std::cout << "Created boundary set " << name << std::endl;
      
      jobject bounds = NULL;
      
      pthread_mutex_lock(&lock);
      try {
        std::cout << "calling loader " << name << std::endl;
      
        if (boundary_builder.load<MO, LisztPrivate::BoundarySet>(name, bs)) {
          std::cout << "loaded " << name << std::endl;
          bounds = createObject(env,
                  prefix + "/BoundarySetImpl", "");
          std::cout << "created " << name << std::endl;

          const LisztPrivate::BoundarySet::ranges_t& ranges = bs->getRanges();
                  
          // For ranges in bs
          for (LisztPrivate::BoundarySet::range_it it = ranges.begin(), end = ranges.end(); it != end; it++) {
              std::cout << "iter " << name << std::endl;
              callVoidMethod(env, bounds,
                      prefix + "/BoundarySetImpl", "add",
                      "(II)V", it->first, it->second);
          }

          std::cout << "done iter " << name << std::endl;
          callVoidMethod(env, bounds, prefix + "/BoundarySetImpl",
                  "freeze", "()V");
          std::cout << "freeze " << name << std::endl;
        }
        else {
          std::cerr << "Failed to load boundary set: " << name << std::endl;
        }
      }
      catch(...) {
        std::cerr << "Failed to load boundary set: " << name << ": Exception thrown" << std::endl;
        bounds = NULL;
      }
      pthread_mutex_unlock(&lock);
      
      std::cout << "return " << name << std::endl;

      delete bs;
      
      return bounds;
    }

    /*
     Construct a Java object with the specific arguments.
     cls: Java class
     type: Java parameter type string
     ...: Constructor arguments
     returns: New Java object
     */
    jobject createObject(JNIEnv* env, jclass& cls, string type, ...);
    jobject createObjectV(JNIEnv* env, jclass& cls, string type, va_list args);
    jobject createObject(JNIEnv* env, string clsStr, string type, ...);

    /*
     Call Scala methods
     */
    jobject callObjectMethod(JNIEnv* env, jobject& obj, string clsStr, string method,
            string sig, ...);
    jobject callObjectMethodV(JNIEnv* env, jobject& obj, string clsStr, string method,
            string sig, va_list args);
    /*jobject callObjMethod(jobject& obj, jclass cls, string method, string sig, ...);
     jobject callObjMethodV(jobject& obj, jclass cls, string method, string sig, va_list args);*/

    void callVoidMethod(JNIEnv* env, jobject& obj, string clsStr, string method,
            string sig, ...);
    void callVoidMethodV(JNIEnv* env, jobject& obj, string clsStr, string method,
            string sig, va_list args);
    /*jobject callVoidMethod(jobject& obj, jclass cls, string method, string sig, ...);
     jobject callVoidMethodV(jobject& obj, jclass cls, string method, va_list args); */
     
    jint callIntMethod(JNIEnv* env, jobject& obj, string clsStr, string method,
            string sig, ...);
    jint callIntMethodV(JNIEnv* env, jobject& obj, string clsStr, string method,
            string sig, va_list args);

    /*
     Set a Scala field
     cls: Java class
     jobj: Scala object
     field: Name of field
     type: Java parameter type string
     ...: Value to set
     */
    void setScalaField(JNIEnv* env, jclass& cls, jobject& jobj, string field,
            string type, ...);
    string prefix;
    
private:
    /*
     Set a CRS field
     jmesh: Mesh object
     field: Name of field
     crs: CRS
     from: Size of CRS
     */
    void setCRSField(JNIEnv* env, jobject& jmesh, string field, CRSMeshPrivate::CRS& crs,
            size_t from);

    /*
     Set a CRSConst field
     jmesh: Mesh object
     field: Name of field
     crs: CRS
     from: Size of CRS
     mult: Size of each item
     */
    void setCRSPairField(JNIEnv* env, jobject& jmesh, string field,
            CRSMeshPrivate::CRSConst& crs, size_t from);

    jintArray copyIdxArray(JNIEnv* env, CRSMeshPrivate::idx_type* array, size_t len);
    jintArray copyIdArray(JNIEnv* env, CRSMeshPrivate::id_type* array, size_t len);
    jintArray copyIdPairArray(JNIEnv* env, CRSMeshPrivate::IDPair* array, size_t len);

    void loadPositions(JNIEnv* env, jobject& jmesh, CRSMesh::Mesh& mesh,
            MeshIO::LisztFileReader& reader);

    jobject getScalaObjField(JNIEnv* env, string clsStr, jobject& jobj,
            string field, string type);
    jobject getScalaObjField(JNIEnv* env, jclass& cls, jobject& jobj,
            string field, string type);

    JNICache* cache;
    BoundarySetBuilder boundary_builder;
    MeshIO::LisztFileReader reader;
    CRSMesh::Mesh mesh;
    jobject jmesh;
    bool loaded;
    pthread_mutex_t lock;
};
}

#endif
