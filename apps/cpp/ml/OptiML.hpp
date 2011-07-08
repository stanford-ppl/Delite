#ifndef __OPTIML_HPP__
#define __OPTIML_HPP__

#include <fstream>
#include <iostream>
#include <string>
#include <stdlib.h>
#include <sys/time.h>
#include <math.h>

#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/tokenizer.hpp>

#include "armadillo"


using std::cout;
using std::cin;
using std::endl;
using std::string;
using std::ios;
using std::flush;
using std::fstream;

namespace arma {
  typedef Col<char> bvec;
  typedef Row<char> browvec;

  mat outer_prod(const rowvec& v1, const vec& v2) {
    assert(v1.n_cols == v2.n_rows);
    uint size = v1.n_cols;
    mat res(size, size);


    for(uint c = 0 ; c < size; c++) {
      for(uint r = 0; r < size; r++) {
	res.at(r,c) = v1.at(c) * v2.at(r);
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
    
    file.open(filename, std::ios_base::in);
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
  
  void tokenizeLineIntoIntVector(const string& line, std::vector<int>& row) {
    boost::char_separator<char> sep(" \t");    
    tokenizer tokens(line, sep);
    uint i = 0;
    for(tokenizer::iterator tok_iter = tokens.begin(); tok_iter != tokens.end(); ++tok_iter) {
      int num = atoi(tok_iter->c_str());
      //cout << *tok_iter << "->" << num << ":" << i << "|";
      row[i] = num;
      i++;
    }
  }
  
  
    namespace armad {
    using namespace arma;
    namespace Matrix {
      void zeroOutRow(mat& m, int r) {
	for(uint c = 0; c < m.n_cols; c++) {
	  m(r,c) = 0;
	}
      }
      
      void setMatrixRowToVector(mat& m, int r, const rowvec& v) {
	for(uint c = 0; c < m.n_cols; c++) {
	  m(r,c) = v(c);
	}
      }

      void insertColIntoMatrix(int c, const vec& v, mat& m) {
	assert(v.n_rows == m.n_rows);
	for(uint r = 0; r < v.n_rows; r++) {
	  m.at(r,c) = v.at(r);
	} 
      }

      double sum(const mat &m) {
	double res = 0.0;
	uint size = m.n_cols * m.n_rows;
	for(uint i = 0; i < size; i++) {
	  res += m.at(i);
	}
	return res;
      }
      
      mat abs(const mat &m) {
	mat res(m.n_rows, m.n_cols);
	uint size = m.n_cols * m.n_rows;
	for(uint i = 0; i < size; i++) {
	  res.at(i) = (m.at(i) <= 0.0) ? 0.0 - m.at(i) : m.at(i);
	}
	return res;
      }
      
      mat doubleMatrixDiag(uint w, const vec& v) {
	assert(w == v.n_rows);
	mat res = zeros(w,w);
	for(uint i = 0; i < w; i++) {
	  res.at(i*w + i) = v.at(i);
	}
	return res;
      }
    }

    namespace Vector {

      vec uniformCol(double start, double step_size, double end) {
	assert(((step_size <= 0) || (end < start))==false); 
	int length = ceil((end-start)/step_size);
	vec res(length);
	double cur = start;
	for(int i = 0; i < length; i++) {
	  res(i) = cur;
	  cur += step_size;
	}

	return res;
      }

    }


    namespace MLInputReader {
      
      typedef struct Dataset {
	uint numDocs;
	uint numTokens;
	mat features;
	vec classifications;    
      } Dataset;
      
      
      void readTokenMatrix(Dataset& data, const char* filename) {
	std::vector<string> lines;
	loadFile(lines, filename);

	assert(lines.size() > 1);
	std::vector<int> counts(2);
	tokenizeLineIntoIntVector(lines[1], counts);
	assert(counts.size() == 2);
	//cout << "Found: " << counts[0] << " and " << counts[1] << endl;
	data.numDocs = counts[0];
	data.numTokens = counts[1];

	vec trainClasses(data.numDocs);
	mat features(data.numDocs, data.numTokens);
	rowvec row(data.numTokens);

	for(uint i = 3; i < data.numDocs + 3; i++) {
	  std::vector<double> tokens(getNumTokens(lines[i]));
	  tokenizeLineIntoDoubleVector(lines[i], tokens);
	  trainClasses(i-3) = tokens[0];
	  //cout << "class[" << tokens[0] << "]";
	  row.zeros();
	  int cumsum = 0;
	  for(uint j = 1; j < tokens.size() - 1; j+=2) {
	    cumsum += tokens[j];
	    //cout << "cumsum[" << cumsum << "]token[" << tokens[j+1] << "]" << flush ;
	    row[cumsum] = tokens[j+1];
	  }
	  Matrix::setMatrixRowToVector(features, i-3 ,row);
	}
	
	data.classifications = trainClasses;
	data.features = features;

      }
      
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

      vec readDoubleVectorAsCol(char* filename) {
	std::vector<string> lines;
	
	loadFile(lines, filename);
	vec res(lines.size());
	for(uint i=0; i < lines.size();  i++) {
	  res.at(i) = atof(lines[i].c_str());
	}


	return res;
      }
      
      bvec readBooleanVectorAsCol(char* filename) {
	std::vector<string> lines;
	
	loadFile(lines, filename);
	bvec res(lines.size());
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


}








#endif 
