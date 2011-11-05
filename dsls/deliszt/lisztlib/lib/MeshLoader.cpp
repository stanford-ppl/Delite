#include <cstdlib>
#include <stdarg.h>
#include <stdexcept>
#include <string>
#include <jni.h>
#include "MeshLoader.h"
#include "Liszt/BoundarySet.h"

#include <stdio.h>
#include <execinfo.h>
#include <signal.h>
#include <stdlib.h>


void handler(int sig) {
  void *array[10];
  size_t size;

  // get void*'s for all entries on the stack
  size = backtrace(array, 10);

  // print out all the frames to stderr
  std::cerr << "Error: signal " << sig << std::endl;
  backtrace_symbols_fd(array, size, 2);
  exit(1);
}

namespace System {
using namespace CRSMesh;

MeshLoader::MeshLoader(bool generated) : jmesh(NULL), loaded(false) {
  if(generated) {
    prefix = "generated/scala";
  }
  else {
    prefix = "ppl/dsl/deliszt/datastruct/scala";
  }
  
  //signal(SIGSEGV, handler);
  
  DEBUG_PRINT("new cache");
  cache = new JNICache();
  
  lock = PTHREAD_MUTEX_INITIALIZER;
}

jobject MeshLoader::createObject(JNIEnv* env, jclass& cls, string type, ...) {
    va_list args;
    va_start(args, type);
    jobject obj = createObjectV(env, cls, type, args);
    va_end(args);
    return obj;
}

jobject MeshLoader::createObjectV(JNIEnv* env, jclass& cls, string type, va_list args) {
    jmethodID cid;
    string sig = "(" + type + ")V";
    
    /* Get the method ID for the String(char[]) constructor */
    cid = cache->getMethod(env, cls, "<init>", sig);
    if (cid == NULL) {
        throw MeshIO::MeshLoadException("Failed to find <init> method"); /* exception thrown */
    }

    return env->NewObjectV(cls, cid, args);
}

jobject MeshLoader::createObject(JNIEnv* env, string clsStr, string type, ...) {
    va_list args;
    va_start(args, type);
    jclass cls;
    if(!(cls = cache->getClass(env, clsStr))) {
        throw MeshIO::MeshLoadException("Failed to find class " + clsStr); /* exception thrown */
    }

    jobject obj = createObjectV(env, cls, type, args);

    va_end(args);
    return obj;
}

jobject MeshLoader::callObjectMethod(JNIEnv* env, jobject& obj, string clsStr, string method,
        string sig, ...) {
            va_list args;
            va_start(args, sig);

            jobject ret = callObjectMethodV(env, obj, clsStr, method, sig, args);

            va_end(args);
            return ret;
        }

jobject MeshLoader::callObjectMethodV(JNIEnv* env, jobject& obj, string clsStr,
        string method, string sig, va_list args) {
    jclass cls;
    if (!(cls = cache->getClass(env, clsStr))) {
        throw MeshIO::MeshLoadException("Failed to find class " + clsStr); /* exception thrown */
    }

    jmethodID cid = cache->getMethod(env, cls, method, sig);
    if (cid == NULL) {
        throw MeshIO::MeshLoadException(
                "Failed to find method " + method + " with sig " + sig); /* exception thrown */
    }

    return env->CallObjectMethodV(obj, cid, args);
}

void MeshLoader::callVoidMethod(JNIEnv* env, jobject& obj, string clsStr, string method,
        string sig, ...) {
            va_list args;
            va_start(args, sig);

            callVoidMethodV(env, obj, clsStr, method, sig, args);

            va_end(args);
        }

void MeshLoader::callVoidMethodV(JNIEnv* env, jobject& jobj, string clsStr, string method,
        string sig, va_list args) {
    jclass cls;
    if (!(cls = cache->getClass(env, clsStr))) {
        throw MeshIO::MeshLoadException("Failed to find class " + clsStr); /* exception thrown */
    }

    jmethodID cid = cache->getMethod(env, cls, method, sig);
    if (cid == NULL) {
        throw MeshIO::MeshLoadException(
                "Failed to find method " + method + " with sig " + sig); /* exception thrown */
    }

    env->CallVoidMethodV(jobj, cid, args);
}

jint MeshLoader::callIntMethod(JNIEnv* env, jobject& obj, string clsStr, string method,
        string sig, ...) {
            va_list args;
            va_start(args, sig);

            jint ret = callIntMethodV(env, obj, clsStr, method, sig, args);

            va_end(args);
            return ret;
        }

jint MeshLoader::callIntMethodV(JNIEnv* env, jobject& obj, string clsStr,
        string method, string sig, va_list args) {
    jclass cls;
    if (!(cls = cache->getClass(env, clsStr))) {
        throw MeshIO::MeshLoadException("Failed to find class " + clsStr); /* exception thrown */
    }

    jmethodID cid = cache->getMethod(env, cls, method, sig);
    if (cid == NULL) {
        throw MeshIO::MeshLoadException(
                "Failed to find method " + method + " with sig " + sig); /* exception thrown */
    }

    return env->CallIntMethodV(obj, cid, args);
}

jobject MeshLoader::getScalaObjField(JNIEnv* env, string clsStr, jobject& jobj, string field,
        string type) {
    if (jclass cls = cache->getClass(env, clsStr)) {
        return getScalaObjField(env, cls, jobj, field, type);
    }
    else {
        return NULL;
    }
}

jobject MeshLoader::getScalaObjField(JNIEnv* env, jclass& cls, jobject& jobj, string field,
        string type) {
    jmethodID cid;
    string getter = field;
    string sig = "()" + type;

    cid = cache->getMethod(env, cls, getter, sig);
    if (cid == NULL) {
        throw MeshIO::MeshLoadException("Failed to find getter for " + field); /* exception thrown */
    }

    return env->CallObjectMethod(jobj, cid);
}

void MeshLoader::setScalaField(JNIEnv* env, jclass& cls, jobject& jobj, string field,
        string type, ...) {
            va_list args;
            va_start(args, type);
            jmethodID cid;
            string setter = field + "_$eq";
            string sig = "(" + type + ")V";

            cid = cache->getMethod(env, cls, setter, sig);
            if (cid == NULL) {
                throw MeshIO::MeshLoadException("Failed to find setter for " + field); /* exception thrown */
  }
  
  env->CallVoidMethodV(jobj, cid, args);
}

void MeshLoader::setCRSField(JNIEnv* env, jobject& jmesh, string field,
        CRSMeshPrivate::CRS& crs, size_t from) {
    jintArray row_idx = copyIdxArray(env, crs.row_idx, from+1);
    jintArray values = copyIdArray(env, crs.values, crs.row_idx[from]);

    jobject jcrs = createObject(env, prefix + "/CRSImpl", "[I[I",
            row_idx, values);

    jclass meshClass = cache->getClass(env, prefix + "/Mesh");
    setScalaField(env, meshClass, jmesh, field,
            "L" + prefix + "/CRS;", jcrs);
}

void MeshLoader::setCRSPairField(JNIEnv* env, jobject& jmesh, string field,
        CRSMeshPrivate::CRSConst& crs, size_t from) {
    jintArray values = copyIdPairArray(env, crs.values, from);

    jobject jcrs = createObject(env, prefix + "/CRSConst",
            "[II", values, 2);

    jclass meshClass = cache->getClass(env, prefix + "/Mesh");
    setScalaField(env, meshClass, jmesh, field,
            "L" + prefix + "/CRS;", jcrs);
}

jintArray MeshLoader::copyIdxArray(JNIEnv* env, CRSMeshPrivate::idx_type* array,
        size_t len) {
    jintArray jarray = env->NewIntArray(len);

    jint* buffer = new jint[len * 2];
    
    for(size_t i = 0; i < len; i++) {
      buffer[i] = array[i];
    }
    
    env->SetIntArrayRegion(jarray, 0, len, (jint*) array);
    
    delete[] buffer;

    return jarray;
}

jintArray MeshLoader::copyIdArray(JNIEnv* env, CRSMeshPrivate::id_type* array, size_t len) {
    jintArray jarray = env->NewIntArray(len);
    
    jint* buffer = new jint[len * 2];
    
    for(size_t i = 0; i < len; i++) {
      buffer[i] = array[i];
    }

    env->SetIntArrayRegion(jarray, 0, len, buffer);
    
    delete[] buffer;

    return jarray;
}

jintArray MeshLoader::copyIdPairArray(JNIEnv* env, CRSMeshPrivate::IDPair* array,
        size_t len) {
  jintArray jarray = env->NewIntArray(len * 2);
  
  jint* buffer = new jint[len * 2];
  
  for(size_t i = 0; i < len; i++) {
    buffer[2*i] = array[i].data[0];
    buffer[2*i+1] = array[i].data[1];
  }

  env->SetIntArrayRegion(jarray, 0, len * 2, buffer);

  delete[] buffer;

  return jarray;
}

jobject MeshLoader::loadMesh(JNIEnv* env, jstring str) {
  if(!loaded) {
    try {
      jmesh = NULL;
      DEBUG_PRINT("convert filename");
      // Convert file name
      string filename(env->GetStringUTFChars(str, 0));

      DEBUG_PRINT("read in file");
      // Read in mesh
      reader.init(filename);

      DEBUG_PRINT("header");
      MeshIO::LisztHeader h = reader.header();
      
      if(h.magic_number != MeshIO::LISZT_MAGIC_NUMBER) {
        DEBUG_PRINT("wrong magic number");
        throw MeshIO::MeshLoadException("wrong magic number");
      }
      
      MeshIO::FacetEdgeBuilder builder;

      DEBUG_PRINT("builder init");
      builder.init(h.nV, h.nE, h.nF, h.nC, h.nFE);

      DEBUG_PRINT("facet edges");
      MeshIO::FileFacetEdge * fes = reader.facetEdges();

      DEBUG_PRINT("facetedges insert");
      builder.insert(0, h.nFE, fes);

      DEBUG_PRINT("free");
      reader.free(fes);

      // Load mesh
      DEBUG_PRINT("from facet edge builder");
      mesh.initFromFacetEdgeBuilder(&builder);

      // Set fields on mesh
      CRSMeshPrivate::MeshData& data = mesh.data;

      DEBUG_PRINT("create mesh");
      jclass meshClass = cache->getClass(env, prefix + "/Mesh");
      jmesh = createObject(env, meshClass, "");            

      // Set size fields
      setScalaField(env, meshClass, jmesh, "nvertices", "I", data.nvertices);
      setScalaField(env, meshClass, jmesh, "nedges", "I", data.nedges);
      setScalaField(env, meshClass, jmesh, "nfaces", "I", data.nfaces);
      setScalaField(env, meshClass, jmesh, "ncells", "I", data.ncells);
      
      DEBUG_PRINT("nvertices: " << data.nvertices);
      DEBUG_PRINT("nedges: " << data.nedges);
      DEBUG_PRINT("nfaces: " << data.nfaces);
      DEBUG_PRINT("ncells: " << data.ncells);

      // Set vertex relations
      setCRSField(env, jmesh, "vtov", data.vtov, data.nvertices);
      setCRSField(env, jmesh, "vtoe", data.vtoe, data.nvertices);
      setCRSField(env, jmesh, "vtof", data.vtof, data.nvertices);
      setCRSField(env, jmesh, "vtoc", data.vtoc, data.nvertices);

      // Set edge relations
      setCRSPairField(env, jmesh, "etov", data.etov, data.nedges);
      setCRSField(env, jmesh, "etof", data.etof, data.nedges);
      setCRSField(env, jmesh, "etoc", data.etoc, data.nedges);

      // Set face relations
      setCRSField(env, jmesh, "ftov", data.ftov, data.nfaces);
      setCRSField(env, jmesh, "ftoe", data.ftoe, data.nfaces);
      setCRSPairField(env, jmesh, "ftoc", data.ftoc, data.nfaces);

      // Set cell relations
      setCRSField(env, jmesh, "ctov", data.ctov, data.ncells);
      setCRSField(env, jmesh, "ctoe", data.ctoe, data.ncells);
      setCRSField(env, jmesh, "ctof", data.ctof, data.ncells);
      setCRSField(env, jmesh, "ctoc", data.ctoc, data.ncells);

      // Set properties we know about
      // Position for vertices
      loadPositions(env, jmesh, mesh, reader);
      
      boundary_builder.init(h.nBoundaries, reader.boundaries());
      
      loaded = true;
    }
    catch (MeshIO::MeshLoadException e) {
      std::cerr << e.what() << std::endl;
      jmesh = NULL;
    }
    catch (...) {
      std::cerr << "System exception while loading mesh" << std::endl;
      jmesh = NULL;
    }
  }
  else {
    std::cerr << "Mesh already loaded!" << std::endl; 
  }
  
  return jmesh;
}

MeshLoader::~MeshLoader() {
}

void MeshLoader::loadPositions(JNIEnv* env, jobject& jmesh, CRSMesh::Mesh& mesh,
        MeshIO::LisztFileReader& reader) {
    size_t size = mesh.data.nvertices;

    jclass darrCls = cache->getClass(env, "[D");
    if (darrCls == NULL) {
        throw MeshIO::MeshLoadException("Failed to find double array class"); /* exception thrown */
    }

    jobjectArray positions = env->NewObjectArray(size, darrCls, NULL);
    if (positions == NULL) {
        throw MeshIO::MeshLoadException("Failed to create array of double arrays"); /* out of memory error thrown */
    }

    MeshIO::PositionTable* table = reader.positions();

    for (size_t i = 0; i < size; i++) {
        jdouble tmp[3]; /* make sure it is large enough! */
        
        jdoubleArray darr = env->NewDoubleArray(3);
        if (darr == NULL) {
            throw MeshIO::MeshLoadException("Failed to create double array"); /* out of memory error thrown */
        }

        for (int j = 0; j < 3; j++) {
            tmp[j] = table->data[i][j];
        }

        env->SetDoubleArrayRegion(darr, 0, 3, tmp);
        env->SetObjectArrayElement(positions, i, darr);
        env->DeleteLocalRef(darr);
    }

    reader.free(table);

    jclass meshClass = cache->getClass(env, prefix + "/Mesh");
    jobject vertexLabels = getScalaObjField(env, meshClass, jmesh,
            "vertexData",
            "L" + prefix + "/LabelData;");
    jclass labelClass = cache->getClass(env,
            prefix + "/LabelData");
    jobject vertexData = getScalaObjField(env, labelClass, vertexLabels, "data",
            "Lscala/collection/mutable/Map;");

    callObjectMethod(env, vertexData, "scala/collection/mutable/Map", "put",
            "(Ljava/lang/Object;Ljava/lang/Object;)Lscala/Option;",
            env->NewStringUTF("position"), positions);
}

}