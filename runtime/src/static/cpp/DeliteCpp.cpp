#include "DeliteCpp.h"

bool regex_metachar(char c) {
  switch (c) {
    case '\\': case '^': case '$': case '.': case '|': case '?':
    case '*': case '+': case '(': case ')': case '[': case '{':
      return true;
    default:
      return false;
  }
}

char find_delim(string pattern) {
  if (pattern.length()==1 && !regex_metachar(pattern.at(0))) {
    return pattern.at(0);
  }
  else if (pattern.length()==2 && pattern.at(0)=='\\' && regex_metachar(pattern.at(1))) {
    return pattern.at(1);
  }
  else
    return -1;
}

#ifdef MEMMGR_REFCNT
std::shared_ptr<cppDeliteArraystring> string_split(string str, string pattern, int32_t lim) {
#else
cppDeliteArraystring *string_split(string str, string pattern, int32_t lim) {
#endif
  //TODO: current g++ does not fully support c++11 regex, 
  //      so below code does not work.
  /*
  std::string s(str);
  std::regex e(pattern.c_str());
  std::vector<std::string> *elems = new std::vector<std::string>();

  const std::sregex_token_iterator endOfSequence;
  std::sregex_token_iterator token(s.begin(), s.end(), e, -1);
  while(token != endOfSequence) {
    elems->push_back(*token++);
    std::cout << *token++ << std::endl;
  }

  cppDeliteArray<string> *ret = new cppDeliteArray<string>(elems->size());
  for(int i=0; i<elems->size(); i++)
    ret->update(i,elems->at(i));
  return ret;
  */

  //Since above code is not working, we currently only support simple regular expressions
  // http://stackoverflow.com/questions/236129/how-to-split-a-string-in-c
  vector<string> elems;
  int num_tokens = 0;
  string token;
  stringstream ss(str); 
  char delim;
  if (pattern.compare("\\s+")==0) {
    while (ss >> token) {
      num_tokens += 1;
      if (num_tokens == lim) {
        string remainder;
        getline(ss, remainder, (char)NULL);
        elems.push_back(token+remainder);
        break;
      }
      else {
        elems.push_back(token);
      }
    }
  }
  else if ((delim = find_delim(pattern)) != -1) {
    while (getline(ss, token, delim)) {
      num_tokens += 1;
      if (num_tokens == lim) {
        string remainder;
        getline(ss, remainder, (char)NULL);
        elems.push_back(token+remainder);
        break;
      }
      else {
        elems.push_back(token);
      }
    }
  }
  else {
    assert(false && "Given regex is not supported");
  }
  
  // remove the trailing empty strings when the limit is 0
  if (lim == 0) {
    while(elems.back().compare("") == 0) {
      elems.pop_back();
    }
  }

  //construct cppDeliteArray from vector
#ifdef MEMMGR_REFCNT
  std::shared_ptr<cppDeliteArraystring> ret(new cppDeliteArraystring(elems.size()), cppDeliteArraystringD());
#else
  cppDeliteArraystring *ret = new cppDeliteArraystring(elems.size());
#endif
  for(int i=0; i<elems.size(); i++)
    ret->update(i,elems.at(i));
  return ret;
}

int32_t string_toInt(string str) {
  return atoi(str.c_str());
}

float string_toFloat(string str) {
  return strtof(str.c_str(),NULL);
}

double string_toDouble(string str) {
  return strtod(str.c_str(),NULL);
}

bool string_toBoolean(string str) {
  std::transform(str.begin(), str.end(), str.begin(), ::tolower);
  if (str.compare("true") == 0)
    return true;
  else if (str.compare("false") == 0)
    return false;
  else
    assert(false && "Cannot parse boolean string");
}

// Code from http://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring
string &ltrim(string &s) {
  s.erase(s.begin(), std::find_if(s.begin(), s.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
  return s;
}

string &rtrim(string &s) {
  s.erase(std::find_if(s.rbegin(), s.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
  return s;
}

string string_trim(string str) {
  string ret = str;
  return ltrim(rtrim(ret));
}

int8_t string_charAt(string str, int idx) {
  return str.at(idx);
}

bool string_startsWith(string str, string substr) {
  if (str.compare(0,substr.length(),substr) == 0)
    return true;
  else
    return false;
}

string string_plus(string str1, string str2) {
  return str1 + str2;
}


#ifdef MEMMGR_REFCNT
std::shared_ptr<cppDeliteArraystring> cppArgsGet(int num, ...) {
  std::shared_ptr<cppDeliteArraystring> cppArgs(new cppDeliteArraystring(num), cppDeliteArraystringD());
#else
cppDeliteArraystring *cppArgsGet(int num, ...) {
  cppDeliteArraystring *cppArgs = new cppDeliteArraystring(num);
#endif
  va_list arguments;
  va_start(arguments, num);
  for(int i=0; i<num; i++) {
    char *pathname = va_arg(arguments, char *);
    cppArgs->data[i] = string(pathname);
  }
  va_end(arguments);
  return cppArgs;
}

template<class T> string convert_to_string(T in) {
  ostringstream convert;
  convert << in;
  return convert.str();
}

// Explicit instantiation of template functions to enable separate compilation
template string convert_to_string<bool>(bool);
template string convert_to_string<int8_t>(int8_t);
template string convert_to_string<uint16_t>(uint16_t);
template string convert_to_string<int16_t>(int16_t);
template string convert_to_string<int32_t>(int32_t);
template string convert_to_string<int64_t>(int64_t);
template string convert_to_string<float>(float);
template string convert_to_string<double>(double);
template string convert_to_string<string>(string);
template string convert_to_string<void*>(void*);

string readFirstLineFile(string filename) {
  ifstream fs(filename.c_str());
  string line;
  if (fs.good()) {
    getline(fs, line);
  }
  fs.close();
  return line;
}

template <class K>
uint32_t delite_hashcode(K key) {
  return key->hashcode();
}

template<> uint32_t delite_hashcode<bool>(bool key) { return (uint32_t) key; }
template<> uint32_t delite_hashcode<int8_t>(int8_t key) { return (uint32_t) key; }
template<> uint32_t delite_hashcode<uint16_t>(uint16_t key) { return (uint32_t) key; }
template<> uint32_t delite_hashcode<int32_t>(int32_t key) { return (uint32_t) key; }
template<> uint32_t delite_hashcode<int64_t>(int64_t key) { return (uint32_t) key; }
template<> uint32_t delite_hashcode<float>(float key) { return (uint32_t) key; }
template<> uint32_t delite_hashcode<double>(double key) { return (uint32_t) key; }
template<> uint32_t delite_hashcode<string>(string key) {
  //http://docs.oracle.com/javase/1.5.0/docs/api/java/lang/String.html#hashCode%28%29
  int32_t multiplier = 1;
  int32_t hc = 0;
  int n = key.length();
  for(int i=n-1; i>=0; i--) {
    hc += multiplier * key.at(i);
    multiplier *= 31;
  }
  return (uint32_t)hc;
}

template <class K>
bool delite_equals(K key1, K key2) {
  return key1->equals(key2);
}

template<> bool delite_equals<bool>(bool key1, bool key2) { return key1 == key2; }
template<> bool delite_equals<int8_t>(int8_t key1, int8_t key2) { return key1 == key2; }
template<> bool delite_equals<uint16_t>(uint16_t key1, uint16_t key2) { return key1 == key2; }
template<> bool delite_equals<int32_t>(int32_t key1, int32_t key2) { return key1 == key2; }
template<> bool delite_equals<int64_t>(int64_t key1, int64_t key2) { return key1 == key2; }
template<> bool delite_equals<float>(float key1, float key2) { return key1 == key2; }
template<> bool delite_equals<double>(double key1, double key2) { return key1 == key2; }
template<> bool delite_equals<string>(string key1, string key2) { return key1.compare(key2) == 0; }


/* helper methods and data structures only required for execution with Delite */
#ifndef __DELITE_CPP_STANDALONE__
pthread_mutex_t lock_objmap = PTHREAD_MUTEX_INITIALIZER;
std::map<int,jobject> *JNIObjectMap = new std::map<int,jobject>();
jobject JNIObjectMap_find(int key) {
  pthread_mutex_lock (&lock_objmap);
  jobject ret = JNIObjectMap->find(key)->second;
  pthread_mutex_unlock (&lock_objmap);
  return ret;
}
void JNIObjectMap_insert(int key, jobject value) {
  pthread_mutex_lock (&lock_objmap);
  std::map<int,jobject>::iterator it = JNIObjectMap->find(key);
  if(it != JNIObjectMap->end()) 
    it->second = value;
  else
    JNIObjectMap->insert(std::pair<int,jobject>(key,value));
  pthread_mutex_unlock (&lock_objmap);
}
#endif
