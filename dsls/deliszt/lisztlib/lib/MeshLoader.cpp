#include <cstdlib>
#include <stdarg.h>
#include <stdexcept>
#include <string>
#include <jni.h>
#include "MeshLoader.h"
#include "Liszt/BoundarySet.h"

namespace System {
using namespace CRSMesh;

MeshLoader::MeshLoader(JNIEnv* env) :
        env(env), cache(env) {
    meshClass = cache.getClass("ppl/dsl/deliszt/datastruct/scala/Mesh");
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
    cid = cache.getMethod(cls, "<init>", sig);
    if (cid == NULL) {
        throw new MeshLoadException("Failed to find <init> method"); /* exception thrown */
    }

    return env->NewObjectV(cls, cid, args);
}

jobject MeshLoader::createObject(string clsStr, string type, ...) {
    va_list args;
    va_start(args, type);
    jclass cls;
    if(!(cls = cache.getClass(clsStr))) {
        throw new MeshLoadException("Failed to find class " + clsStr); /* exception thrown */
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
    if (!(cls = cache.getClass(clsStr))) {
        throw new MeshLoadException("Failed to find class " + clsStr); /* exception thrown */
    }

    jmethodID cid = cache.getMethod(cls, method, sig);
    if (cid == NULL) {
        throw new MeshLoadException(
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
    if (!(cls = cache.getClass(clsStr))) {
        throw new MeshLoadException("Failed to find class " + clsStr); /* exception thrown */
    }

    jmethodID cid = cache.getMethod(cls, method, sig);
    if (cid == NULL) {
        throw new MeshLoadException(
                "Failed to find method " + method + " with sig " + sig); /* exception thrown */
    }

    env->CallVoidMethodV(jobj, cid, args);
}

jobject MeshLoader::getScalaObjField(string clsStr, jobject& jobj, string field,
        string type) {
    if (jclass cls = cache.getClass(clsStr)) {
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

    cid = cache.getMethod(cls, getter, sig);
    if (cid == NULL) {
        throw new MeshLoadException("Failed to find getter for " + field); /* exception thrown */
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

            cid = cache.getMethod(cls, setter, sig);
            if (cid == NULL) {
                throw new MeshLoadException("Failed to find setter for " + field); /* exception thrown */
  }
  
  env->CallVoidMethodV(jobj, cid, args);
}

void MeshLoader::setCRSField(jobject& jmesh, string field,
        CRSMeshPrivate::CRS& crs, size_t from) {
    jintArray row_idx = copyIdxArray(crs.row_idx, from);
    jintArray values = copyIdArray(crs.values, crs.row_idx[from]);

    jobject jcrs = createObject("ppl/dsl/deliszt/datastruct/scala/CRS", "[I[I",
            row_idx, values);

    setScalaField(meshClass, jmesh, field,
            "ppl/dsl/deliszt/datastruct/scala/CRS", jcrs);
}

void MeshLoader::setCRSConstField(jobject& jmesh, string field,
        CRSMeshPrivate::CRSConst& crs, size_t from, size_t mult) {
    jintArray values = copyIdPairArray(crs.values, from * mult);

    jobject jcrs = createObject("ppl/dsl/deliszt/datastruct/scala/CRSConst",
            "[II", values, mult);

    setScalaField(meshClass, jmesh, field,
            "ppl/dsl/deliszt/datastruct/scala/CRSConst", jcrs);
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
    MeshIO::LisztFileReader reader;
    CRSMesh::Mesh mesh;

    try {
        // Convert file name
        string filename(env->GetStringUTFChars(str, 0));

        // Read in mesh
        reader.init(filename);

        MeshIO::LisztHeader h = reader.header();
        MeshIO::FacetEdgeBuilder builder;
        builder.init(h.nV, h.nE, h.nF, h.nC, h.nFE);
        MeshIO::FileFacetEdge * fes = reader.facetEdges();
        builder.insert(0, h.nFE, fes);
        reader.free(fes);

        // Load mesh
        mesh.initFromFacetEdgeBuilder(&builder);

        // Set fields on mesh
        CRSMeshPrivate::MeshData& data = mesh.data;

        jobject jmesh = createObject(meshClass, "");

        // Set size fields
        setScalaField(meshClass, jmesh, "nvertices", "I", data.nvertices);
        setScalaField(meshClass, jmesh, "nedges", "I", data.nedges);
        setScalaField(meshClass, jmesh, "nfaces", "I", data.nfaces);
        setScalaField(meshClass, jmesh, "ncells", "I", data.ncells);

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

        // Boundary sets
        loadBoundaries(jmesh, mesh, reader);

        return jmesh;
    }
    catch (MeshLoadException e) {
        return NULL;
    }
}

MeshLoader::~MeshLoader() {
}

void MeshLoader::loadPositions(jobject& jmesh, CRSMesh::Mesh& mesh,
        MeshIO::LisztFileReader& reader) {
    size_t size = mesh.data.nvertices;

    jclass darrCls = cache.getClass("[D");
    if (darrCls == NULL) {
        throw new MeshLoadException("Failed to find double array class"); /* exception thrown */
    }

    jobjectArray positions = env->NewObjectArray(size, darrCls, NULL);
    if (positions == NULL) {
        throw new MeshLoadException("Failed to create array of double arrays"); /* out of memory error thrown */
    }

    MeshIO::PositionTable* table = reader.positions();

    for (size_t i = 0; i < size; i++) {
        jdouble tmp[3]; /* make sure it is large enough! */

        jdoubleArray darr = env->NewDoubleArray(size);
        if (darr == NULL) {
            throw new MeshLoadException("Failed to create double array"); /* out of memory error thrown */
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
            string("vertexData"),
            string("Lppl/dsl/deliszt/datastruct/scala/LabelData"));
    jclass labelClass = cache.getClass(
            "ppl/dsl/deliszt/datastruct/scala/LabelData");
    jobject vertexData = getScalaObjField(labelClass, vertexLabels, "data",
            string("Lscala/collection/mutable/Map"));

    callObjectMethod(vertexData, "scala/collection/mutable/Map", "put",
            "(Ljava/lang/Object,Ljava/lang/Object)Lscala/Option",
            env->NewStringUTF("position"), positions);
}

void MeshLoader::loadBoundaries(jobject& jmesh, CRSMesh::Mesh& mesh,
        MeshIO::LisztFileReader& reader) {
    MeshIO::LisztHeader h = reader.header();
    MeshIO::BoundarySetEntry* bounds = reader.boundaries();
    boundary_builder.init(h.nBoundaries, bounds);

    // Load the meshes we find!
    for (size_t i = 0; i < h.nBoundaries; i++) {
        // If it has a name, load it!
        if (bounds[i].name.size() > 0) {
            MeshIO::IOElemType typ = (MeshIO::IOElemType) (bounds[i].type
                    & ~MeshIO::AGG_FLAG);

            switch (typ) {
            case LisztPrivate::ElemTypes::CELL:
                loadBoundarySet<Cell, cell_set>(jmesh, mesh,
                        bounds[i].name.c_str(), "cellBounds");
                break;
            case LisztPrivate::ElemTypes::EDGE:
                loadBoundarySet<Edge, edge_set>(jmesh, mesh,
                        bounds[i].name.c_str(), "edgeBounds");
                break;
            case LisztPrivate::ElemTypes::FACE:
                loadBoundarySet<Face, face_set>(jmesh, mesh,
                        bounds[i].name.c_str(), "faceBounds");
                break;
            case LisztPrivate::ElemTypes::VERTEX:
                loadBoundarySet<Vertex, vertex_set>(jmesh, mesh,
                        bounds[i].name.c_str(), "vertexBounds");
                break;
            default:
                break;
            }
        }
    }
}

template<typename MO, typename MeshSet, typename BoundarySet>
void MeshLoader::loadBoundarySet(jobject& jmesh, CRSMesh::Mesh& mesh,
        const char* name, string field) {
    BoundarySet *bs = new BoundarySet();
    if (!bs) {
        throw new MeshLoadException("Could not create boundary set");
    }

    if (boundary_builder.load<CRSMesh::Mesh, MO, BoundarySet>(&mesh, name, bs)) {
        jobject boundMap = getScalaObjField(meshClass, jmesh, field,
                "Lscala/collection/mutable/Map");

        // Copy bs over I guess
        jmethodID cid = cache.getMethod("scala/collection/mutable/Map", "put",
                "(Ljava/lang/Object,Ljava/lang/Object)Lscala/Option");
        if (cid == NULL) {
            throw new MeshLoadException("Failed to find put method"); /* exception thrown */
        }

        jobject bounds = createObject(
                "ppl/dsl/deliszt/datastruct/scala/BoundSetImpl", "");

        // For ranges in bs
        for (MeshSet::iterator it = bs->iter(); it.hasNext();) {
            callVoidMethod(bounds,
                    "ppl/dsl/deliszt/datastruct/scala/BoundSetImpl", "add",
                    "(I)V", it.next().ID());
        }

        callVoidMethod(bounds, "ppl/dsl/deliszt/datastruct/scala/BoundSetImpl",
                "freeze", "()V");

        callObjectMethod(boundMap, "scala/collection/mutable/Map", "put",
                "(Ljava/lang/Object,Ljava/lang/Object)Lscala/Option",
                env->NewStringUTF(name), bounds);
    }

    delete bs;
}

}
