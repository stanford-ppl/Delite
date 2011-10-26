#ifndef _OPENCLLIST_CL_H_
#define _OPENCLLIST_CL_H_

struct OpenCLIntListStruct {
    __global int *data;
    int length;
};

typedef struct OpenCLIntListStruct OpenCLIntList;

int OpenCLIntList_dcSize(OpenCLIntList list) { return list.length; }

#endif
