#include <iostream>
#include <stdlib.h>
#include <ml/OptiML.hpp>
#include "armadillo"

using std::cout;
using namespace arma;

void print_usage() {
  cout << "Usage: GDA <input data file> <output label data file>\n";
  exit(-1);
}

int main(int argc,char *argv[]) {

  if(argc != 3) {
    print_usage();
  }

  mat  x_in = OptiML::armad::MLInputReader::readDoubleMatrix(argv[1]);
  //transpose the matrix to get performance
  mat x = trans(x_in);
  bvec y = OptiML::armad::MLInputReader::readBooleanVectorAsCol(argv[2]);

  
  
  

  cout << "Computing GDA using armadillo and MKL\n";

  OptiML::tic();
   
  uint m = y.n_rows;
  uint n = x.n_rows;

  double y_ones = 0.0, y_zeros=0.0;

  vec mu0_num(n);
  vec mu1_num(n);

  for(uint i=0; i<m ; i++) {
    if(y(i)==false) {
      y_zeros++;
      mu0_num += x.col(i);
    } else {
      y_ones++;
      mu1_num += x.col(i);
    }
  }

  double phi = 1.0/m * y_ones;
  vec mu0 = mu0_num / y_zeros;
  vec mu1 = mu1_num / y_ones;

  mat sigma(n,n);
  mat tmp(n,n);

  for(uint i=0; i<m; i++) {
    if(y(i) == false) {
      sigma += outer_prod(trans(x.unsafe_col(i) - mu0), x.unsafe_col(i) - mu0);
    } else {
      sigma += outer_prod(trans(x.unsafe_col(i) - mu1), x.unsafe_col(i) - mu1);
    }
  }
  
  OptiML::toc();
  
  //sigma.print("sigma = ");

  //cout << "phi: " << phi << endl << "sigma: ";

  return 0;

}
