#include <iostream>
#include <stdlib.h>
#include <ml/OptiML.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/matrix_proxy.hpp>
#include <boost/numeric/ublas/vector.hpp>

using std::cout;
using namespace boost::numeric::ublas;
using boost::numeric::ublas::vector;

void print_usage() {
  cout << "Usage: GDA <input data file> <output label data file>\n";
  exit(-1);
}

int main(int argc,char *argv[]) {

  if(argc != 3) {
    print_usage();
  }

  matrix<double> x = OptiML::ublas::MLInputReader::readDoubleMatrix(argv[1]);
  vector<bool>   y = OptiML::ublas::MLInputReader::readBooleanVector(argv[2]);
  
  cout << "Computing GDA using ublas\n";
  
  OptiML::tic();
  
  uint m = y.size();
  uint n = x.size2();

  double y_ones = 0.0, y_zeros=0.0;

  vector<double> mu0_num(n);
  vector<double> mu1_num(n);

  for(uint i=0; i<m ; i++) {
    if(y(i)==false) {
      y_zeros++;
      mu0_num += row(x,i);
    } else {
      y_ones++;
      mu1_num += row(x,i);
    }
  }

  double phi = 1.0/m * y_ones;
  vector<double> mu0 = mu0_num / y_zeros;
  vector<double> mu1 = mu1_num / y_ones;

  matrix<double> sigma(n,n);
  for(uint i=0; i<m; i++) {
    if(y(i) == false) {
      matrix<double> out = outer_prod(trans(row(x,i) - mu0), row(x,i)- mu0);
      noalias(sigma) += out;
    } else {
      matrix<double> out = outer_prod(trans(row(x,i) - mu1), row(x,i)- mu1);
      noalias(sigma) += out;
    }
  }
  
  OptiML::toc();
  //  cout << "phi: " << phi << endl << "sigma: ";
  //OptiML::ublas::Matrix::pprint(sigma);

  return 0;
}
