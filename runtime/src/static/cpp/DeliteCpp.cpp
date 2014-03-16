#include "DeliteCpp.h"

cppDeliteArray<string> *string_split(string str, string pattern) {
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

  //Since regex is not working, we currently only support 
  assert(strcmp(pattern.c_str(),"\\s+")==0 && "Currently only regex \\s+ is supported for C++ target");
  string token;
  stringstream ss(str); 
  vector<string> elems;
  while (ss >> token)
    elems.push_back(token);
  
  //construct cppDeliteArray from vector
  cppDeliteArray<string> *ret = new cppDeliteArray<string>(elems.size());
  for(int i=0; i<elems.size(); i++)
    ret->update(i,elems.at(i));
  return ret;
}

int string_toInt(string str) {
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

char string_charAt(string str, int idx) {
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


cppDeliteArray< string > *cppArgsGet(int num, ...) {
  cppDeliteArray< string > *cppArgs = new cppDeliteArray< string >(num);
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
template string convert_to_string<char>(char);
template string convert_to_string<short>(short);
template string convert_to_string<int>(int);
template string convert_to_string<long>(long);
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

