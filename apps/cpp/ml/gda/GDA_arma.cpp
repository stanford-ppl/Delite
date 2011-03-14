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

  mat  x = OptiML::armad::MLInputReader::readDoubleMatrix(argv[1]);
  browvec y = OptiML::armad::MLInputReader::readBooleanVector(argv[2]);
  
  cout << "Computing GDA using armadillo and MKL\n";

  OptiML::tic();
   
  uint m = y.n_cols;
  uint n = x.n_cols;

  double y_ones = 0.0, y_zeros=0.0;

  rowvec mu0_num(n);
  rowvec mu1_num(n);

  for(uint i=0; i<m ; i++) {
    if(y(i)==false) {
      y_zeros++;
      mu0_num += x.row(i);
    } else {
      y_ones++;
      mu1_num += x.row(i);
    }
  }

  double phi = 1.0/m * y_ones;
  rowvec mu0 = mu0_num / y_zeros;
  rowvec mu1 = mu1_num / y_ones;

  mat sigma(n,n);
  for(uint i=0; i<m; i++) {
    if(y(i) == false) {
      sigma += outer_prod(trans(x.row(i) - mu0), x.row(i) - mu0);
    } else {
      sigma += outer_prod(trans(x.row(i) - mu1), x.row(i) - mu1);
    }
  }
  
  OptiML::toc();
  
  //sigma.print("sigma = ");

  //cout << "phi: " << phi << endl << "sigma: ";

  return 0;

}
