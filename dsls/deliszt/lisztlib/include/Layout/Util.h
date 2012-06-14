#ifndef _LAYOUT_UTIL_H
#define _LAYOUT_UTIL_H
#include "MeshIO/LisztFormat.h"
#include "Liszt/Util.h"
#include <stdio.h>
#include <string>
#include <sstream>
#include <string.h>
#include "MeshIO/Common.h"

template<typename T>
void writeField(MeshIO::IOElemType domain, const void * data, size_t elem_size, size_t nElems, const char * field_name) {
	MeshIO::LisztType range = GetLisztType< T >::type();
	MeshIO::FileField field = { domain, range, nElems, 0, 0};
	std::ostringstream file_name_ss;
	file_name_ss << field_name << ".lfield";
	std::string file_name = file_name_ss.str();
	
	FILE * file = FOPEN(file_name.c_str(),"w");
	assert(file);
	
	field.name = sizeof(MeshIO::FileField);
	
	fseeko(file,field.name,SEEK_SET);
	fwrite(field_name,strlen(field_name) + 1,1,file);
	
	field.data = ftello(file);
	
	fwrite(data,elem_size,nElems,file);
	
	fseeko(file,0,SEEK_SET);
	fwrite(&field,sizeof(MeshIO::FileField),1,file);
	fclose(file);
}
#endif