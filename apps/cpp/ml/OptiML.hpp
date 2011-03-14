#ifndef __OPTIML_HPP__
#define __OPTIML_HPP__

#include <fstream>
#include <iostream>
#include <string>
#include <stdlib.h>
#include <sys/time.h>

#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/tokenizer.hpp>

#include "armadillo"


using std::cout;
using std::cin;
using std::endl;
using std::string;
using std::ios;
using std::fstream;

namespace arma {
  typedef Row<char> browvec;

  mat outer_prod(const vec& v1, const rowvec& v2) {
    assert(v1.n_rows == v2.n_cols);
    uint size = v1.n_rows;
    mat res(size, size);
    for(uint c = 0 ; c < size; c++) {
      for(uint r = 0; r < size; r++) {
	res.at(r,c) = v1.at(r) * v2.at(c);
      }      
    }
    return res;
  }
}

namespace OptiML {
  
  struct timeval start, end;
  long seconds, useconds;    
  
  void tic() {
    gettimeofday(&start, NULL);
  }

  void toc() {
    gettimeofday(&end, NULL);
    seconds  = end.tv_sec  - start.tv_sec;
    useconds = end.tv_usec - start.tv_usec;

    double mtime = ((seconds) + useconds/1000000.0);
    printf("Elapsed time: %.3lf seconds\n", mtime);
  }
  
  void loadFile(std::vector<string>& lines, const char* filename) {
    //load file
    fstream file;
    string line;
    
    file.open(filename);
    while(file.good()) {
      getline(file, line);
      if(line.size() > 1)
	lines.push_back(line);
    }
    file.close();   
  }
  
  typedef boost::tokenizer<boost::char_separator<char> > tokenizer;
  typedef unsigned int uint;
  
  uint getNumTokens(const string& line) {
    boost::char_separator<char> sep(" \t");    
    tokenizer tokens(line, sep);
    uint i = 0;
    for(tokenizer::iterator tok_iter = tokens.begin(); tok_iter != tokens.end(); ++tok_iter) {
      i++;
    }
    return i;
  }
  
  void tokenizeLineIntoDoubleVector(const string& line, std::vector<double>& row) {
    boost::char_separator<char> sep(" \t");    
    tokenizer tokens(line, sep);
    uint i = 0;
    for(tokenizer::iterator tok_iter = tokens.begin(); tok_iter != tokens.end(); ++tok_iter) {
      double num = atof(tok_iter->c_str());
      //cout << num << ":" << i << "|";
      row[i] = num;
      i++;
    }
  }
  
      

  
  namespace ublas {
    namespace Matrix {
      void pprint(const boost::numeric::ublas::matrix<double>& m) {
	cout << "[";
	for( uint r=0; r < m.size1(); r++) {
	  for( uint c=0; c < m.size2(); c++) {
	    cout << m(r,c);
	    if(c == m.size2()-1 && r != m.size1()-1) 
	      cout << "\n ";
	    else if(c != m.size2()-1)
	      cout << " , ";
	  }
	}
	cout << "]\n";
      }
    }
    
    namespace Vector {
      void pprint(const boost::numeric::ublas::vector<double>& v) {
	cout << "[";
	for( uint i=0; i < v.size(); i++) {
	  cout << v(i);
	  if(i != v.size()-1)
	    cout << " , ";
	}
	cout << "]\n";
      }
    }
    
    namespace MLInputReader {
      
      void assignVectorToMatrix(const std::vector<double>& v, boost::numeric::ublas::matrix<double>& m, uint ridx) {
	assert(v.size() == m.size2());
	for(uint c=0; c < v.size(); c++) {
	  m(ridx,c) = v[c];
	}      
      }
      

      boost::numeric::ublas::matrix<double> readDoubleMatrix(char * filename) {
	std::vector<string> lines;
	
	loadFile(lines, filename);
	
	uint numRows = lines.size();
	assert(numRows > 0);
	uint numCols = getNumTokens(lines[0]);
	std::vector<double> rowV(numCols);
	boost::numeric::ublas::matrix<double> res(numRows, rowV.size());
	
	uint ridx = 0;
	while(ridx != numRows) {
	  tokenizeLineIntoDoubleVector(lines[ridx], rowV);
	  assignVectorToMatrix(rowV, res, ridx);
	  ridx++;
	}
	
	//OptiML::Matrix::pprint(res);
	
	return res;
      }
      
      
      
      boost::numeric::ublas::vector<bool> readBooleanVector(char* filename) {
	std::vector<string> lines;
	
	loadFile(lines, filename);
	boost::numeric::ublas::vector<bool> res(lines.size());
	for(uint i=0; i < lines.size();  i++) {
	  double n = atof(lines[i].c_str());
	  if(n <= 0.0)
	    res(i) = false;
	  else
	    res(i) = true;
	}
	
	//OptiML::Vector::pprint(res);
	
	return res;
      }
      
    }    
  }

  namespace armad {
    using namespace arma;
    namespace MLInputReader {
      
      void assignVectorToMatrix(const std::vector<double>& v, mat& m, uint ridx) {
	assert(v.size() == m.n_cols);
	for(uint c=0; c < v.size(); c++) {
	  m(ridx,c) = v[c];
	}      
      }

      mat readDoubleMatrix(char * filename) {
	std::vector<string> lines;
	
	loadFile(lines, filename);
	
	uint numRows = lines.size();
	assert(numRows > 0);
	uint numCols = getNumTokens(lines[0]);
	std::vector<double> rowV(numCols);
	mat res(numRows, rowV.size());
	
	uint ridx = 0;
	while(ridx != numRows) {
	  tokenizeLineIntoDoubleVector(lines[ridx], rowV);
	  assignVectorToMatrix(rowV, res, ridx);
	  ridx++;
	}


	return res;	
      }
      
      browvec readBooleanVector(char* filename) {
	std::vector<string> lines;
	
	loadFile(lines, filename);
	browvec res(lines.size());
	for(uint i=0; i < lines.size();  i++) {
	  double n = atof(lines[i].c_str());
	  if(n <= 0.0)
	    res(i) = false;
	  else
	    res(i) = true;
	}
	return res;
      }
    }
  }
}








#endif 
