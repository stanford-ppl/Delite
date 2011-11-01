project(lisztlib)
cmake_minimum_required(VERSION 2.6)

#There are lots of scripts with cmake
#for finding external libraries. 

set(CMAKE_CXX_FLAGS "-Wall -m64 -O3 -DNDEBUG -funroll-loops -Wall -Wno-deprecated -g")
SET(CMAKE_CXX_FLAGS_DEBUG "-O0 -g -ggdb -rdynamic -DDEBUG")

find_package(OpenMP)
find_package(JNI REQUIRED)
IF(EXISTS ${JAVA_INCLUDE_PATH}/jni.h  )
  SET(JNI_REALLY_FOUND 1 CACHE BOOL "Tests whether the header
  actually exists")
message(STATUS "jni.h was found at " ${JAVA_INCLUDE_PATH}/jni.h)
ELSE( )
  SET(JNI_REALLY_FOUND 0 CACHE BOOL "Tests whether the header
  actually exists")
message(STATUS "jni.h was not found at " ${JAVA_INCLUDE_PATH}/jni.h)
ENDIF( )

link_directories(${PROJECT_SOURCE_DIR}/lib /usr/local/lib)
include_directories(${PROJECT_SOURCE_DIR}/include 
                    ${CMAKE_BINARY_DIR}
                    ${JNI_INCLUDE_DIRS}
                    ${JAVA_INCLUDE_PATH})

# NOTE(boulos): If these aren't found (say on int or something) that's okay.
add_subdirectory(lib)
add_subdirectory(client)
