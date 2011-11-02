#include <jni.h>
#include <string>
#include <iostream>

#include "JNICache.h"

JNICache::JNICache() {
}

JNICache::~JNICache() {
}

/*
  clsMap::const_iterator end = classes.end();
  for (clsMap::const_iterator it = classes.begin(); it != end; ++it) {
    env->DeleteGlobalRef(it->second);
  }
*/
  
jclass JNICache::getClass(JNIEnv* env, string clsStr) {
  clsMap::iterator it = classes.find(clsStr);
  if(it != classes.end()) {
    return it->second;
  }
  else {
    jclass globalRef = NULL;
    if(jclass cls = env->FindClass(clsStr.c_str())) {
      globalRef = static_cast<jclass>(env->NewGlobalRef(cls));
    }
    classes[clsStr] = globalRef;
    return globalRef;
  }
}

jmethodID JNICache::getMethod(JNIEnv* env, string clsStr, string method, string args) {
  jclass cls = getClass(env, clsStr);
  return getMethod(env, cls, method, args);
}

jmethodID JNICache::getMethod(JNIEnv* env, jclass cls, string method, string args) {
  jmethodID cid = (jmethodID) JNI_ERR;
  
  if(cls) {
    JNISignature sign(env, cls, method, args);
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
