#ifndef __OPTIML_HPP__
#define __OPTIML_HPP__

#include <fstream>
#include <iostream>
#include <string>
#include <stdlib.h>

#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/tokenizer.hpp>




using std::cout;
using std::cin;
using std::endl;
using std::string;
using std::ios;
using std::fstream;
using namespace boost::numeric::ublas;

namespace OptiML { namespace ublas {
    namespace Matrix {
      void pprint(const matrix<double>& m) {
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
      void pprint(const vector<double>& v) {
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
      
      void tokenizeLineIntoDoubleVector(const string& line, vector<double>& row) {
	boost::char_separator<char> sep(" \t");    
	tokenizer tokens(line, sep);
	uint i = 0;
	for(tokenizer::iterator tok_iter = tokens.begin(); tok_iter != tokens.end(); ++tok_iter) {
	  double num = atof(tok_iter->c_str());
	  //cout << num << ":" << i << "|";
	  row(i) = num;
	  i++;
	}
      }
      
      void assignVectorToMatrix(const vector<double>& v, matrix<double>& m, uint ridx) {
	assert(v.size() == m.size2());
	for(uint c=0; c < v.size(); c++) {
	  m(ridx,c) = v(c);
	}      
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
      
      matrix<double> readDoubleMatrix(char * filename) {
	std::vector<string> lines;
	
	loadFile(lines, filename);
	
	uint numRows = lines.size();
	assert(numRows > 0);
	uint numCols = getNumTokens(lines[0]);
	vector<double> rowV(numCols);
	matrix<double> res(numRows, rowV.size());
	
	uint ridx = 0;
	while(ridx != numRows) {
	  tokenizeLineIntoDoubleVector(lines[ridx], rowV);
	  assignVectorToMatrix(rowV, res, ridx);
	  ridx++;
	}
	
	//OptiML::Matrix::pprint(res);
	
	return res;
      }
      
      
      
      vector<bool> readBooleanVector(char* filename) {
	std::vector<string> lines;
	
	loadFile(lines, filename);
	vector<bool> res(lines.size());
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
}








#endif 
