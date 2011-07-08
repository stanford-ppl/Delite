#include <iostream>
#include <stdlib.h>
#include <math.h>
#include <limits>
#include <ml/OptiML.hpp>
#include "armadillo"

using std::cout;
using namespace arma;

void print_usage() {
  cout << "Usage: LinReg <input vector file> <output vector file>\n";
  exit(-1);
}

int main(int argc,char *argv[]) {

  if(argc != 3) {
    print_usage();
  }

  vec x = OptiML::armad::MLInputReader::readDoubleVectorAsCol(argv[1]);
  vec y = OptiML::armad::MLInputReader::readDoubleVectorAsCol(argv[2]);

  OptiML::tic();
  int tau = 10;
  mat X(x.n_rows,2);
  vec ones(x.n_rows);
  ones.ones();
  OptiML::armad::Matrix::insertColIntoMatrix(0,ones,X);
  OptiML::armad::Matrix::insertColIntoMatrix(1,x,X);

  double xstep = 25.0 / X.n_rows;
  vec xref_pts = OptiML::armad::Vector::uniformCol(-10,xstep,14.99);
  mat xref(xref_pts.n_rows, 2);
  OptiML::armad::Matrix::insertColIntoMatrix(0,ones,xref);
  OptiML::armad::Matrix::insertColIntoMatrix(1,xref_pts,xref);
  mat Xt = trans(X);  

  rowvec guess(xref.n_rows);
  for(uint e = 0; e < xref.n_rows; e++) {
    double x_cur = xref(e,1);
    vec weights(x.n_rows);
    for(uint i = 0; i < x.n_rows; i++) {
      weights(i) = exp(-0.1*pow(x_cur-x(i),2)/(2.0*pow(tau,2)));
    }
    mat W = OptiML::armad::Matrix::doubleMatrixDiag(weights.n_rows, weights);
    mat t1 = Xt*W;
    vec theta = inv(t1*X)*(t1*y);
    guess.at(e) = dot(trans(theta),(trans(xref.row(e))));
  }
  
  OptiML::toc();
  //guess.print("guess = ");
}

