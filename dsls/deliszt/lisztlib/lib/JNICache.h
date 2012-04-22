#include <jni.h>
#include <string>
#include <map>

#ifndef JNICACHE_H_
#define JNICACHE_H_

using namespace std;

struct JNISignature {
  JNIEnv* env;
  jclass cls;
  string method;
  string sign;
  
  JNISignature(JNIEnv* env, jclass cls, string method, string sign) : env(env), cls(cls), method(method), sign(sign) {}
  
  bool operator<(const JNISignature& other) const
  { 
    if(env < other.env) {
      return true;
    }
    else if(env > other.env) {
      return false;
    }
  
    if(cls < other.cls) {
      return true;
    }
    else if(cls > other.cls) {
      return false;
    }
    
    if(method < other.method) {
      return true;
    }
    else if(method > other.method) {
      return false;
    }
    
    if(sign < other.sign) {
      return true;
    }
    else {
      return false;
    }
  }
};

class JNICache {
public:
  JNICache();
  ~JNICache();
  
  jclass getClass(JNIEnv* env, string desc);
  jmethodID getMethod(JNIEnv* env, string clsStr, string method, string sign);
  jmethodID getMethod(JNIEnv* env, jclass cls, string method, string sign);
private:
  typedef map<string, jclass> clsMap;
  clsMap classes;
  typedef map<JNISignature, jmethodID> methodMap;
  methodMap methods;
};
#endif
