#include <cstdlib>
#include <stdarg.h>
#include <stdexcept>
#include <string>
#include <jni.h>
#include "MeshLoader.h"
#include "Liszt/BoundarySet.h"

#ifdef DEBUG
#define DEBUG_PRINT(msg) std::cerr << msg << std::endl;
#else
#define DEBUG_PRINT(msg)
#endif

namespace System {
using namespace CRSMesh;

MeshLoader::MeshLoader() {
}

void MeshLoader::init(JNIEnv* _env, bool generated) {
    if(generated) {
      prefix = "generated/scala";
    }
    else {
      prefix = "ppl/dsl/deliszt/datastruct/scala";
    }

    env = _env;

    DEBUG_PRINT("new cache");
    cache = new JNICache(env);

    DEBUG_PRINT("load class");
    meshClass = cache->getClass(prefix + "/Mesh");
}

jobject MeshLoader::createObject(jclass& cls, string type, ...) {
    va_list args;
    va_start(args, type);
    jobject obj = createObjectV(cls, type, args);
    va_end(args);
    return obj;
}

jobject MeshLoader::createObjectV(jclass& cls, string type, va_list args) {
    jmethodID cid;
    string sig = "(" + type + ")V";
    
    /* Get the method ID for the String(char[]) constructor */
    cid = cache->getMethod(cls, "<init>", sig);
    if (cid == NULL) {
        throw MeshIO::MeshLoadException("Failed to find <init> method"); /* exception thrown */
    }

    return env->NewObjectV(cls, cid, args);
}

jobject MeshLoader::createObject(string clsStr, string type, ...) {
    va_list args;
    va_start(args, type);
    jclass cls;
    if(!(cls = cache->getClass(clsStr))) {
        throw MeshIO::MeshLoadException("Failed to find class " + clsStr); /* exception thrown */
    }

    jobject obj = createObjectV(cls, type, args);

    va_end(args);
    return obj;
}

jobject MeshLoader::callObjectMethod(jobject& obj, string clsStr, string method,
        string sig, ...) {
            va_list args;
            va_start(args, sig);

            jobject ret = callObjectMethodV(obj, clsStr, method, sig, args);

            va_end(args);
            return ret;
        }

jobject MeshLoader::callObjectMethodV(jobject& obj, string clsStr,
        string method, string sig, va_list args) {
    jclass cls;
    if (!(cls = cache->getClass(clsStr))) {
        throw MeshIO::MeshLoadException("Failed to find class " + clsStr); /* exception thrown */
    }

    jmethodID cid = cache->getMethod(cls, method, sig);
    if (cid == NULL) {
        throw MeshIO::MeshLoadException(
                "Failed to find method " + method + " with sig " + sig); /* exception thrown */
    }

    return env->CallObjectMethodV(obj, cid, args);
}

void MeshLoader::callVoidMethod(jobject& obj, string clsStr, string method,
        string sig, ...) {
            va_list args;
            va_start(args, sig);

            callVoidMethodV(obj, clsStr, method, sig, args);

            va_end(args);
        }

void MeshLoader::callVoidMethodV(jobject& jobj, string clsStr, string method,
        string sig, va_list args) {
    jclass cls;
    if (!(cls = cache->getClass(clsStr))) {
        throw MeshIO::MeshLoadException("Failed to find class " + clsStr); /* exception thrown */
    }

    jmethodID cid = cache->getMethod(cls, method, sig);
    if (cid == NULL) {
        throw MeshIO::MeshLoadException(
                "Failed to find method " + method + " with sig " + sig); /* exception thrown */
    }

    env->CallVoidMethodV(jobj, cid, args);
}

jint MeshLoader::callIntMethod(jobject& obj, string clsStr, string method,
        string sig, ...) {
            va_list args;
            va_start(args, sig);

            jint ret = callIntMethodV(obj, clsStr, method, sig, args);

            va_end(args);
            return ret;
        }

jint MeshLoader::callIntMethodV(jobject& obj, string clsStr,
        string method, string sig, va_list args) {
    jclass cls;
    if (!(cls = cache->getClass(clsStr))) {
        throw MeshIO::MeshLoadException("Failed to find class " + clsStr); /* exception thrown */
    }

    jmethodID cid = cache->getMethod(cls, method, sig);
    if (cid == NULL) {
        throw MeshIO::MeshLoadException(
                "Failed to find method " + method + " with sig " + sig); /* exception thrown */
    }

    return env->CallIntMethodV(obj, cid, args);
}

jobject MeshLoader::getScalaObjField(string clsStr, jobject& jobj, string field,
        string type) {
    if (jclass cls = cache->getClass(clsStr)) {
        return getScalaObjField(cls, jobj, field, type);
    }
    else {
        return NULL;
    }
}

jobject MeshLoader::getScalaObjField(jclass& cls, jobject& jobj, string field,
        string type) {
    jmethodID cid;
    string getter = field;
    string sig = "()" + type;

    cid = cache->getMethod(cls, getter, sig);
    if (cid == NULL) {
        throw MeshIO::MeshLoadException("Failed to find getter for " + field); /* exception thrown */
    }

    return env->CallObjectMethod(jobj, cid);
}

void MeshLoader::setScalaField(jclass& cls, jobject& jobj, string field,
        string type, ...) {
            va_list args;
            va_start(args, type);
            jmethodID cid;
            string setter = field + "_$eq";
            string sig = "(" + type + ")V";

            cid = cache->getMethod(cls, setter, sig);
            if (cid == NULL) {
                throw MeshIO::MeshLoadException("Failed to find setter for " + field); /* exception thrown */
  }
  
  env->CallVoidMethodV(jobj, cid, args);
}

void MeshLoader::setCRSField(jobject& jmesh, string field,
        CRSMeshPrivate::CRS& crs, size_t from) {
    jintArray row_idx = copyIdxArray(crs.row_idx, from+1);
    jintArray values = copyIdArray(crs.values, crs.row_idx[from]);

    jobject jcrs = createObject(prefix + "/CRSImpl", "[I[I",
            row_idx, values);

    setScalaField(meshClass, jmesh, field,
            "L" + prefix + "/CRS;", jcrs);
}

void MeshLoader::setCRSConstField(jobject& jmesh, string field,
        CRSMeshPrivate::CRSConst& crs, size_t from, size_t mult) {
    jintArray values = copyIdPairArray(crs.values, from * mult);

    jobject jcrs = createObject(prefix + "/CRSConst",
            "[II", values, mult);

    setScalaField(meshClass, jmesh, field,
            "L" + prefix + "/CRS;", jcrs);
}

jintArray MeshLoader::copyIdxArray(CRSMeshPrivate::idx_type* array,
        size_t len) {
    jintArray jarray = env->NewIntArray(len);

    env->SetIntArrayRegion(jarray, 0, len, (jint*) array);

    return jarray;
}

jintArray MeshLoader::copyIdArray(CRSMeshPrivate::id_type* array, size_t len) {
    jintArray jarray = env->NewIntArray(len);

    env->SetIntArrayRegion(jarray, 0, len, (jint*) array);

    return jarray;
}

jintArray MeshLoader::copyIdPairArray(CRSMeshPrivate::IDPair* array,
        size_t len) {
    jintArray jarray = env->NewIntArray(len * 2);

    env->SetIntArrayRegion(jarray, 0, len * 2, (jint*) array);

    return jarray;
}

jobject MeshLoader::loadMesh(jstring str) {
    try {
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
        jmesh = createObject(meshClass, "");

        // Set size fields
        setScalaField(meshClass, jmesh, "nvertices", "I", data.nvertices);
        setScalaField(meshClass, jmesh, "nedges", "I", data.nedges);
        setScalaField(meshClass, jmesh, "nfaces", "I", data.nfaces);
        setScalaField(meshClass, jmesh, "ncells", "I", data.ncells);
        
        DEBUG_PRINT("nvertices: " << data.nvertices);
        DEBUG_PRINT("nedges: " << data.nedges);
        DEBUG_PRINT("nfaces: " << data.nfaces);
        DEBUG_PRINT("ncells: " << data.ncells);

        // Set vertex relations
        setCRSField(jmesh, "vtov", data.vtov, data.nvertices);
        setCRSField(jmesh, "vtoe", data.vtoe, data.nvertices);
        setCRSField(jmesh, "vtof", data.vtof, data.nvertices);
        setCRSField(jmesh, "vtoc", data.vtoc, data.nvertices);

        // Set edge relations
        setCRSConstField(jmesh, "etov", data.etov, data.nedges, 2);
        setCRSField(jmesh, "etof", data.etof, data.nedges);
        setCRSField(jmesh, "etoc", data.etoc, data.nedges);

        // Set face relations
        setCRSField(jmesh, "ftov", data.ftov, data.nfaces);
        setCRSField(jmesh, "ftoe", data.ftoe, data.nfaces);
        setCRSConstField(jmesh, "ftoc", data.ftoc, data.nfaces, 2);

        // Set cell relations
        setCRSField(jmesh, "ctov", data.ctov, data.ncells);
        setCRSField(jmesh, "ctoe", data.ctoe, data.ncells);
        setCRSField(jmesh, "ctof", data.ctof, data.ncells);
        setCRSField(jmesh, "ctoc", data.ctoc, data.ncells);

        // Set properties we know about
        // Position for vertices
        loadPositions(jmesh, mesh, reader);
    }
    catch (MeshIO::MeshLoadException e) {
        jmesh = NULL;
        std::cerr << e.what() << std::endl;
    }
    
    return jmesh;
}

MeshLoader::~MeshLoader() {
}

void MeshLoader::loadPositions(jobject& jmesh, CRSMesh::Mesh& mesh,
        MeshIO::LisztFileReader& reader) {
    size_t size = mesh.data.nvertices;

    jclass darrCls = cache->getClass("[D");
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

        jdoubleArray darr = env->NewDoubleArray(size);
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

    jobject vertexLabels = getScalaObjField(meshClass, jmesh,
            "vertexData",
            "L" + prefix + "/LabelData;");
    jclass labelClass = cache->getClass(
            prefix + "/LabelData");
    jobject vertexData = getScalaObjField(labelClass, vertexLabels, "data",
            "Lscala/collection/mutable/Map;");

    callObjectMethod(vertexData, "scala/collection/mutable/Map", "put",
            "(Ljava/lang/Object;Ljava/lang/Object;)Lscala/Option;",
            env->NewStringUTF("position"), positions);
}

template<typename MO>
jobject MeshLoader::loadBoundarySet(const char* name) {
    LisztPrivate::BoundarySet *bs = new LisztPrivate::BoundarySet();
    if (!bs) {
        throw MeshIO::MeshLoadException("Could not create boundary set");
    }

    jobject bounds = NULL;
    
    try {
      if (boundary_builder.load<MO, LisztPrivate::BoundarySet>(&mesh, name, bs)) {
          bounds = createObject(
                  prefix + "/BoundarySetImpl", "");

          LisztPrivate::BoundarySet::ranges_t& ranges = bs->getRanges();
                  
          // For ranges in bs
          for (LisztPrivate::BoundarySet::range_it it = ranges.begin(), end = ranges.end(); it != end; it++) {
              callVoidMethod(bounds,
                      prefix + "/BoundarySetImpl", "add",
                      "(II)V", it->first(), it->second());
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

}
