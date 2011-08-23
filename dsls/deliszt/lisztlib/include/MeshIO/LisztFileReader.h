#ifndef LISZTFILEREADER_H_
#define LISZTFILEREADER_H_
#include "MeshIO/Common.h"
#include "MeshIO/LisztFormat.h"

namespace MeshIO {

struct BoundarySetEntry {
	IOElemType type;
	id_t start;
	id_t end;
	std::string name;
};

class LisztFileReader {
public:
	LisztFileReader() : file(NULL) {}
	void init(const std::string & filename) {
		file = FOPEN(filename.c_str(),"r");
		if(!file) {
			perror(NULL);
      throw MeshLoadException("file read error");
		}
		if(fread(&head,sizeof(LisztHeader),1,file) != 1) {
			perror(NULL);
      throw MeshLoadException("failed to read header");
		}
		if(head.magic_number != LISZT_MAGIC_NUMBER) {
      throw MeshLoadException("unexpected magic number, is this a liszt mesh?");
		}
	}
	
	const LisztHeader & header() const {
		return head;
	}
	
	//client responsible for freeing BoundarySet * with this.free
	BoundarySetEntry * boundaries() {
		seek(head.boundary_set_table);
		BoundarySet * b = new BoundarySet[head.nBoundaries];
		if(fread(b,sizeof(BoundarySet),head.nBoundaries,file) != head.nBoundaries) {
			perror(NULL);
      throw MeshLoadException("boundary read failed");
		}
		
		BoundarySetEntry * entries = new BoundarySetEntry[head.nBoundaries];
		
		for(unsigned int i = 0; i < head.nBoundaries; i++) {
			entries[i].type = b[i].type;
			entries[i].start = b[i].start;
			entries[i].end = b[i].end;
			
			char buf[2048]; //TODO(zach): buffer overrun for large symbols
			seek(b[i].name_string);
			if(!fgets(buf,2048,file)) {
				perror(NULL);
        throw MeshLoadException("failed to read string");
			}
			entries[i].name = buf;
		}
		delete [] b;
		return entries;
	}
	
	//get all fes, client responsible for freeing result with this.free
	FileFacetEdge * facetEdges() {
		return facetEdges(0,head.nFE);
	}
	
	//get range of facet edges [start,end), client responsible for freeing result with this.free
	FileFacetEdge * facetEdges(id_t start, id_t end) {
		assert(start <= head.nFE);
		assert(end <= head.nFE);
		assert(start <= end);
		lsize_t nfes = end - start;
		seek(head.facet_edge_table + start * sizeof(FileFacetEdge));

std::cout << "allocating " << nfes << " facets of size " << sizeof(FileFacetEdge) << std::endl;
		
		FileFacetEdge * fes = new FileFacetEdge[nfes];
		
		if(fread(fes,sizeof(FileFacetEdge),nfes,file) != nfes) {
			perror(NULL);
      throw MeshLoadException("error reading facet edges");
		}
		
		return fes;
	}
	
	PositionTable * positions() {
		return positions(0,head.nV);
	}
	
	PositionTable * positions(id_t start, id_t end) {
		assert(start <= head.nV);
		assert(end <= head.nV);
		assert(start <= end);
		lsize_t npos = end - start;
		const lsize_t PSIZE = 3 * sizeof(double);
		seek(head.position_table + start * PSIZE);
		
		double * buf = new double[3 * npos];
		
		if(fread(buf,PSIZE,npos,file) != npos) {
			perror(NULL);
      throw MeshLoadException("could not read positions");
		}
		
		return (PositionTable*) buf;
	}
	void free(FileFacetEdge * fe) {
		delete [] fe;
	}
	void free(BoundarySetEntry * bs) {
		delete [] bs;
	}
	void free(PositionTable * pt) {
		delete [] ((double*) pt);
	}
	void close() {
		fclose(file);
		file = NULL;
	}
private:
	void seek(file_ptr loc) {
		if(fseeko(file,loc,SEEK_SET)) {
			perror(NULL);
      throw MeshLoadException("error seeking stream");
		}
	}
	LisztHeader head;
	FILE * file;
};
} // namespace MeshIO
#endif /* LISZTFILEREADER_H_ */
