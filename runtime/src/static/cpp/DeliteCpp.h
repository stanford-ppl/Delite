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

/* helper methods and data structures only required for execution with Delite */
#ifndef __DELITE_CPP_STANDALONE__
#include <jni.h>
#endif

using namespace std;

#ifdef MEMMGR_REFCNT
std::shared_ptr<cppDeliteArraystring> string_split(string str, string pattern, int32_t lim);
std::shared_ptr<cppDeliteArraystring> cppArgsGet(int num, ...);
#else
cppDeliteArraystring *string_split(string str, string pattern, int32_t lim);
cppDeliteArraystring *cppArgsGet(int num, ...);
#endif
int32_t string_toInt(string str);
float string_toFloat(string str);
double string_toDouble(string str);
bool string_toBoolean(string str);
string string_trim(string str);
int8_t string_charAt(string str, int idx);
bool string_startsWith(string str, string substr);
string string_plus(string str1, string str2);
template<class T> string convert_to_string(T in);
string readFirstLineFile(string filename);
template<class K> uint32_t delite_hashcode(K key);
template<class K> bool delite_equals(K key1, K key2);

#ifndef __DELITE_CPP_STANDALONE__
extern std::map<int,jobject> *JNIObjectMap;
jobject JNIObjectMap_find(int key);
void JNIObjectMap_insert(int key, jobject value);
#endif

#endif
