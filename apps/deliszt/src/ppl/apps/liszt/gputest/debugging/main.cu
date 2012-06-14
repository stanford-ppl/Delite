#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "CRS.h"
#include "Mesh.h"
#include "MeshSet.h"
#include "Field.h"
#include "x26.cu"

#define MAX_BUF 1048576

char buf[MAX_BUF];

/***************************************
*  Print Functions 
***************************************/

void printCRS(const char *name, CRS crs) {
	printf("[%s]: %d, %d\n",name, crs.rows_length, crs.values_length);

	printf("rows : ");
	for(int i=0; i<crs.rows_length; i++) {
		printf("%d,",crs.rows[i]);
	}
	printf("\n");

	printf("values : ");
	for(int i=0; i<crs.values_length; i++) {
		printf("%d,",crs.values[i]);
	}
	printf("\n");
}

void printMesh(Mesh m) {

	printf("Mesh : (%d vertices, %d edges, %d faces, %d cells)\n", m.nvertices, m.nedges, m.nfaces, m.ncells);
	
	printCRS("vtov", m.vtov);
	printCRS("vtoe", m.vtoe);
	printCRS("vtof", m.vtof);
	printCRS("vtoc", m.vtoc);

	printCRS("etov", m.etov);
	printCRS("etof", m.etof);
	printCRS("etoc", m.etoc);
	
	printCRS("ftov", m.ftov);
	printCRS("ftoe", m.ftoe);
	printCRS("ftoc", m.ftoc);
	
	printCRS("ctov", m.ctov);
	printCRS("ctoe", m.ctoe);
	printCRS("ctof", m.ctof);
	printCRS("ctoc", m.ctoc);
}

void printField(Field<int> f) {
	printf("[Field]: ");

	for(int i=0; i<f.size; i++) {
		printf("%d,",f.data[i]);
	}
	printf("\n");
}

void printMeshSet(MeshSet<int> ms) {
	printf("[MeshSet]: ");

	for(int i=0; i<ms.size; i++) {
		printf("%d,",ms.data[i]);
	}
	printf("\n");
}

/***************************************
*  Parsing fucntions 
***************************************/

CRS parseCRS(FILE *fp) {
	CRS crs;
	int idx;
	char *ret;

	ret = fgets(buf,MAX_BUF,fp);
	assert(ret != NULL);
	crs.rows_length = atoi(buf);
	crs.rows = (int *)malloc(sizeof(int)*crs.rows_length);
	ret = fgets(buf,MAX_BUF,fp);
	assert(ret != NULL);
	char *tok = strtok(buf, " \n");
	idx = 0;
	while(tok != NULL) {
		crs.rows[idx++] = atoi(tok);
		tok = strtok(NULL, " \n");
	}
	assert(idx == crs.rows_length);
	
	ret = fgets(buf,MAX_BUF,fp);
	assert(ret != NULL);
	crs.values_length = atoi(buf);
	crs.values = (int *)malloc(sizeof(int)*crs.values_length);
	ret = fgets(buf,MAX_BUF,fp);
	assert(ret != NULL);
	tok = strtok(buf, " \n");
	idx = 0;
	while(tok != NULL) {
		crs.values[idx++] = atoi(tok);
		tok = strtok(NULL, " \n");
	}
	assert(idx == crs.values_length);

	return crs;
}

Mesh parseMesh(char *filename) {

	char *ret;
	Mesh m;

	FILE *fp = fopen(filename, "r");
	assert(fp != NULL);

	/* Get constants */
	ret = fgets(buf,MAX_BUF,fp);
	assert(ret != NULL);
	m.nvertices = atoi(buf);
	
	ret = fgets(buf,MAX_BUF,fp);
	assert(ret != NULL);
	m.nedges = atoi(buf);
	
	ret = fgets(buf,MAX_BUF,fp);
	assert(ret != NULL);
	m.nfaces = atoi(buf);
	
	ret = fgets(buf,MAX_BUF,fp);
	assert(ret != NULL);
	m.ncells = atoi(buf);

	m.vtov = parseCRS(fp); 
	m.vtoe = parseCRS(fp);
	m.vtof = parseCRS(fp);
	m.vtoc = parseCRS(fp);
	
	m.etov = parseCRS(fp);
	m.etof = parseCRS(fp);
	m.etoc = parseCRS(fp);
	
	m.ftov = parseCRS(fp);
	m.ftoe = parseCRS(fp);
	m.ftoc = parseCRS(fp);
	
	m.ctov = parseCRS(fp);
	m.ctoe = parseCRS(fp);
	m.ctof = parseCRS(fp);
	m.ctoc = parseCRS(fp);

	return m;
}

MeshSet<int> parseMeshSet(char *filename) {
	char *ret;
	MeshSet<int> ms;
	int idx;

	FILE *fp = fopen(filename, "r");
	assert(fp != NULL);

	ret = fgets(buf,MAX_BUF,fp);
	assert(ret != NULL);
	ms.dir = atoi(buf);

	ret = fgets(buf,MAX_BUF,fp);
	assert(ret != NULL);
	ms.size = atoi(buf);
	ms.data = (int *)malloc(sizeof(int)*ms.size);

	ret = fgets(buf,MAX_BUF,fp);
	assert(ret != NULL);
	char *tok = strtok(buf, " \n");
	idx = 0;
	while(tok != NULL) {
		ms.data[idx++] = atoi(tok);
		tok = strtok(NULL, " \n");
	}
	assert(idx == ms.size);

	return ms;
}

/*****************************************
*  transfer  functions 
****************************************/
CRS transferCRS(CRS crs) {
	CRS crs_d;

	crs_d.rows_length = crs.rows_length;
	crs_d.values_length = crs.values_length;
	
	int *dptr_rows;
	int *dptr_values;
	cudaMalloc((void**)&dptr_rows,  sizeof(int)*crs.rows_length);
	cudaMalloc((void**)&dptr_values,  sizeof(int)*crs.values_length);
	
	cudaMemcpyAsync(dptr_rows, crs.rows, sizeof(int)*crs.rows_length, cudaMemcpyHostToDevice);
	cudaMemcpyAsync(dptr_values, crs.values, sizeof(int)*crs.values_length, cudaMemcpyHostToDevice);

	crs_d.rows = dptr_rows;
	crs_d.values = dptr_values;

	return crs_d;
}

Mesh transferMesh(Mesh m) {
	Mesh m_d;

	m_d.nvertices = m.nvertices;
	m_d.nedges = m.nedges;
	m_d.nfaces = m.nfaces;
	m_d.ncells = m.ncells;

	m_d.vtov = transferCRS(m.vtov);
	m_d.vtoe = transferCRS(m.vtoe);
	m_d.vtof = transferCRS(m.vtof);
	m_d.vtoc = transferCRS(m.vtoc);
	
	m_d.etov = transferCRS(m.etov);
	m_d.etof = transferCRS(m.etof);
	m_d.etoc = transferCRS(m.etoc);
	
	m_d.ftov = transferCRS(m.ftov);
	m_d.ftoe = transferCRS(m.ftoe);
	m_d.ftoc = transferCRS(m.ftoc);
	
	m_d.ctov = transferCRS(m.ctov);
	m_d.ctoe = transferCRS(m.ctoe);
	m_d.ctof = transferCRS(m.ctof);
	m_d.ctoc = transferCRS(m.ctoc);
	
	return m_d;
}

MeshSet<int> transferMeshSet(MeshSet<int> ms) {
	MeshSet<int> ms_d;

	ms_d.dir = ms.dir;
	ms_d.size = ms.size;

	int *dptr_data;
	cudaMalloc((void**)&dptr_data,  sizeof(int)*ms.size);
	cudaMemcpyAsync(dptr_data, ms.data, sizeof(int)*ms.size, cudaMemcpyHostToDevice);
	ms_d.data = dptr_data;

	return ms_d;
}

Field<int> transferField(Field<int> f) {
	Field<int> f_d;

	f_d.size = f.size;

	int *dptr_data;
	cudaMalloc((void**)&dptr_data,  sizeof(int)*f.size);
	cudaMemcpyAsync(dptr_data, f.data, sizeof(int)*f.size, cudaMemcpyHostToDevice);
	f_d.data = dptr_data;

	return f_d;
}


/****************************************
* Transfer (Back to Host)
***************************************/

Field<int> transferFieldDtoH(Field<int> f_d) {
	Field<int> f;

	f.size = f_d.size;

	int *hptr_data = (int *)malloc(sizeof(int)*f_d.size);
	cudaMemcpyAsync(hptr_data, f_d.data, sizeof(int)*f.size, cudaMemcpyDeviceToHost);
	f.data = hptr_data;

	return f;
}


int main(void) {

	CudaArrayList<Cell> x35;

	Mesh x3 = parseMesh("mesh.txt");
	MeshSet<int> x4 = parseMeshSet("meshset.txt");
	int x5 = x4.size;

	Field<int> x2;
	x2.size = x3.ncells;
	x2.data = (int *)malloc(sizeof(int)*(x2.size));
	for(int i=0; i<x2.size; i++) {
		x2.data[i] = 2;
	}

	Field<int> x2_d = transferField(x2);
	Mesh x3_d = transferMesh(x3);
	MeshSet<int> x4_d = transferMeshSet(x4);

	dim3 threads(512,1,1);
	dim3 blocks(1+(x5-1)/threads.x, 1, 1);
	
	kernel_x26<<<blocks, threads>>>(x5, x35, x4_d, x3_d, x2_d);
	cudaDeviceSynchronize();
	printf("%s\n",cudaGetErrorString(cudaGetLastError()));

	Field<int> out = transferFieldDtoH(x2_d);
	printField(out);
	
	//printMesh(x3);


	return 0;
}
