#include <string>
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <sstream>

std::string DATA_FOLDER ("/home/david/PPL/data/");
std::string CONFIG_FILE ("/home/david/PPL/hyperdsl/delite/asplos/apps/config.txt");

template<class T>
T* read2D(std::string filename, int& nRows, int& nCols) {
  std::string line;
  T value;
  std::vector< std::vector<T> > data;

  std::ifstream fin (filename.c_str());

  while (getline(fin, line)) {
    data.push_back(std::vector<T>());
    std::istringstream ss(line);
    while (ss >> value) { data.back().push_back(value); }
  }
  fin.close();

  nRows = data.size();
  nCols = data[0].size();
  T* matrix = new T[nRows*nCols];
  for (int y = 0; y < nRows; y++) {
    for (int x = 0; x < nCols; x++) {
      matrix[y*nCols + x] = data[y][x];
  }}
  return matrix;
}

template<class T>
T* read1D(std::string filename, int& len) {
  int nRows, nCols;
  T* matrix = read2D<T>(filename, nRows, nCols);
  if (nRows > 1) 
    len = nRows;
  else
    len = nCols;

  return matrix;
}