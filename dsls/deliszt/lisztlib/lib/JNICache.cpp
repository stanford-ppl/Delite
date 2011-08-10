#include <jni.h>
#include <string>

#include "JNICache.h"

JNICache::JNICache(JNIEnv* env) : env(env) {
}

JNICache::~JNICache() {
  clsMap::const_iterator end = classes.end();
  for (clsMap::const_iterator it = classes.begin(); it != end; ++it)
  {
    env->DeleteWeakGlobalRef(it->second);
  }
}
  
jclass JNICache::getClass(string clsStr) {
  clsMap::iterator it = classes.find(clsStr);
  if(it != classes.end()) {
    return it->second;
  }
  else {
    jclass globalRef = NULL;
    if(jclass cls = env->FindClass(clsStr.c_str())) {
      globalRef = static_cast<jclass>(env->NewWeakGlobalRef(cls));
    }
    classes[clsStr] = globalRef;
    return globalRef;
  }
}

jmethodID JNICache::getMethod(string clsStr, string method, string args) {
  jclass cls = getClass(clsStr);
  return getMethod(cls, method, args);
}

jmethodID JNICache::getMethod(jclass cls, string method, string args) {
  jmethodID cid = (jmethodID) JNI_ERR;
  
  if(cls) {
    JNISignature sign(cls, method, args);
    methodMap::iterator it = methods.find(sign);
    if(it != methods.end()) {
      return it->second;
    }
    else {
      /* Get the method ID */
      cid = env->GetMethodID(cls, method.c_str(), args.c_str());
      methods[sign] = cid;
    }  
  }
  
  return cid;
}