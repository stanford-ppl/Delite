#include <jni.h>
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

    /*
     env: JNI Environment pointer
     */
    MeshLoader();
    ~MeshLoader();
    
    void init(JNIEnv* env, bool generated=false);

    /*
     Load mesh from file.
     str: Filename
     returns: Java Mesh object
     */
    jobject loadMesh(jstring str);

    template<typename MO>
    jobject loadBoundarySet(const char* name, jobject moc)
    {
        LisztPrivate::BoundarySet *bs = new LisztPrivate::BoundarySet();
        if (!bs) {
            throw MeshIO::MeshLoadException("Could not create boundary set");
        }

        jobject bounds = NULL;
        
        try {
          if (boundary_builder.load<MO, LisztPrivate::BoundarySet>(name, bs)) {
              bounds = createObject(
                      prefix + "/BoundarySetImpl", "L" + prefix + "/MeshObjConstruct;", moc);

              const LisztPrivate::BoundarySet::ranges_t& ranges = bs->getRanges();
                      
              // For ranges in bs
              for (LisztPrivate::BoundarySet::range_it it = ranges.begin(), end = ranges.end(); it != end; it++) {
                  callVoidMethod(bounds,
                          prefix + "/BoundarySetImpl", "add",
                          "(II)V", it->first, it->second);
              }

              callVoidMethod(bounds, prefix + "/BoundarySetImpl",
                      "freeze", "()V");
          }
        }
        catch(...) {
            bounds = NULL;
        }

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
    jobject createObject(jclass& cls, string type, ...);
    jobject createObjectV(jclass& cls, string type, va_list args);
    jobject createObject(string clsStr, string type, ...);

    /*
     Call Scala methods
     */
    jobject callObjectMethod(jobject& obj, string clsStr, string method,
            string sig, ...);
    jobject callObjectMethodV(jobject& obj, string clsStr, string method,
            string sig, va_list args);
    /*jobject callObjMethod(jobject& obj, jclass cls, string method, string sig, ...);
     jobject callObjMethodV(jobject& obj, jclass cls, string method, string sig, va_list args);*/

    void callVoidMethod(jobject& obj, string clsStr, string method,
            string sig, ...);
    void callVoidMethodV(jobject& obj, string clsStr, string method,
            string sig, va_list args);
    /*jobject callVoidMethod(jobject& obj, jclass cls, string method, string sig, ...);
     jobject callVoidMethodV(jobject& obj, jclass cls, string method, va_list args); */
     
    jint callIntMethod(jobject& obj, string clsStr, string method,
            string sig, ...);
    jint callIntMethodV(jobject& obj, string clsStr, string method,
            string sig, va_list args);

    /*
     Set a Scala field
     cls: Java class
     jobj: Scala object
     field: Name of field
     type: Java parameter type string
     ...: Value to set
     */
    void setScalaField(jclass& cls, jobject& jobj, string field,
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
    void setCRSField(jobject& jmesh, string field, CRSMeshPrivate::CRS& crs,
            size_t from);

    /*
     Set a CRSConst field
     jmesh: Mesh object
     field: Name of field
     crs: CRS
     from: Size of CRS
     mult: Size of each item
     */
    void setCRSConstField(jobject& jmesh, string field,
            CRSMeshPrivate::CRSConst& crs, size_t from, size_t mult);

    jintArray copyIdxArray(CRSMeshPrivate::idx_type* array, size_t len);
    jintArray copyIdArray(CRSMeshPrivate::id_type* array, size_t len);
    jintArray copyIdPairArray(CRSMeshPrivate::IDPair* array, size_t len);

    void loadPositions(jobject& jmesh, CRSMesh::Mesh& mesh,
            MeshIO::LisztFileReader& reader);

    jobject getScalaObjField(string clsStr, jobject& jobj,
            string field, string type);
    jobject getScalaObjField(jclass& cls, jobject& jobj,
            string field, string type);

    jclass meshClass;
    JNIEnv* env;
    JNICache* cache;
    BoundarySetBuilder boundary_builder;
    MeshIO::LisztFileReader reader;
    CRSMesh::Mesh mesh;
    jobject jmesh;
};
        }

#endif
