#include <jni.h>
#include <string>
#include <map>

#ifndef JNICACHE_H_
#define JNICACHE_H_

using namespace std;

struct JNISignature {
  jclass cls;
  string method;
  string sign;
  
  JNISignature(jclass cls, string method, string sign) : cls(cls), method(method), sign(sign) {}
  
  bool operator<(const JNISignature& other) const
  {   
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
  JNICache(JNIEnv* env);
  ~JNICache();
  
  jclass getClass(string desc);
  jmethodID getMethod(string clsStr, string method, string sign);
  jmethodID getMethod(jclass cls, string method, string sign);
private:
  JNIEnv* env;
  typedef map<string, jclass> clsMap;
  clsMap classes;
  typedef map<JNISignature, jmethodID> methodMap;
  methodMap methods;
};
#endif
