#ifndef _COMMON_H
#define _COMMON_H

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string>
#include <ext/hash_map>
#include <assert.h>
namespace std { using namespace __gnu_cxx; }

//ensure we use the 64-bit version of files for >2GB
#ifdef __linux__
#define FOPEN fopen64
#else
#define FOPEN fopen
#endif


#endif
