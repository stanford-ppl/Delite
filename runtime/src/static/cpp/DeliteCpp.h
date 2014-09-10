#ifndef __DELITE_CPP_H__
#define __DELITE_CPP_H__

#include <stdint.h>
#include <cstdarg>
#include <iostream>
#include <fstream>
#include <algorithm> 
#include <functional> 
#include <cctype>
#include <locale>
#include <assert.h>
#include <string>
#include <vector>
#include <map>
#include <sstream>
#include "cppDeliteArraystring.h"
#include "DeliteNamespaces.h"
#include "DeliteDatastructures.h"

/* helper methods and data structures only required for execution with Delite */
#ifndef __DELITE_CPP_STANDALONE__
#include <jni.h>
#endif

#ifdef MEMMGR_REFCNT
std::shared_ptr<cppDeliteArraystring> string_split(const resourceInfo_t &resourceInfo, const string &str, const string &pattern, int32_t lim);
std::shared_ptr<cppDeliteArraystring> cppArgsGet(int num, ...);
#else
cppDeliteArraystring *string_split(const resourceInfo_t &resourceInfo, const string &str, const string &pattern, int32_t lim);
cppDeliteArraystring *cppArgsGet(int num, ...);
#endif
int32_t string_toInt(const string &str);
float string_toFloat(const string &str);
double string_toDouble(const string &str);
bool string_toBoolean(const string &str);
string string_trim(const string &str);
int8_t string_charAt(const string &str, int idx);
bool string_startsWith(const string &str, const string &substr);
string string_plus(const string &str1, const string &str2);
string string_substr(const string &str, int32_t offset, int32_t end);
string string_substr(const string &str, int32_t offset);
int32_t string_length(const string &str);
template<class T> string convert_to_string(T in);
string readFirstLineFile(const string &filename);
template<class K> uint32_t delite_hashcode(K key);
template<class K> bool delite_equals(K key1, K key2);

#ifndef __DELITE_CPP_STANDALONE__
extern std::map<int,jobject> *JNIObjectMap;
jobject JNIObjectMap_find(int key);
void JNIObjectMap_insert(int key, jobject value);
#endif

#endif
