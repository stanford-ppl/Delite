#include <iostream>
#include <stdlib.h>
#include <math.h>
#include <limits>
#include <ml/OptiML.hpp>
#include "armadillo"

using std::cout;
using namespace arma;

double tol = 0.001; // tolerance

void print_usage() {
  cout << "Usage: kmeans <input data file> <initmu data file>\n";
  exit(-1);
}

int findNearestCluster(const rowvec& x_i, const mat& mu ) {
  double min_d = std::numeric_limits<double>::max();
  int min_j = -1;

  for(uint j = 0; j < mu.n_rows; j++) {
    uint l = x_i.n_cols;
    vec  v(l);
    for(uint i = 0; i < l; i++) {
      rowvec r = mu.row(j);
      v.at(i) = pow(x_i.at(i) - r.at(i), 2);
    }
    
    int dist = sum(v);
    if(dist < min_d) {
      min_d = dist;
      min_j = j;
    }
    
  }

  return min_j;

}

void constructNearestClusterVector(ivec &v, const mat& x, const mat& mu) {
  for(uint i = 0; i < v.n_rows; i++) {
    v.at(i) = findNearestCluster(x.row(i), mu);
  }
}

int main(int argc,char *argv[]) {

  if(argc != 3) {
    print_usage();
  }

  mat x = OptiML::armad::MLInputReader::readDoubleMatrix(argv[1]);
  mat mu =  OptiML::armad::MLInputReader::readDoubleMatrix(argv[2]);
  int m = x.n_rows;
  int k = mu.n_rows;
  int iter = 0;

  mat oldmu(mu.n_rows, mu.n_cols);
  
  cout << "kmeans starting computation.\n";
  
  OptiML::tic();
  
  double diff;
  do {
    iter++;
    oldmu = mu;
    
    ivec c(m);
    constructNearestClusterVector(c, x, mu);

    //    irowvec c_t = trans(c);
    //    c_t.print("c = ");
    
    rowvec weightedPoints(x.n_cols);
    for(int j = 0; j < k; j++) {
      weightedPoints.zeros();
      int points = 0;
      for(int i = 0; i < m; i++) {
	if(c(i) == j) {
	  weightedPoints = weightedPoints +  x.row(i);
	  points++;
	}
      }
      if(points == 0) {
	points++;
      }
      //weightedPoints.print("wp = ");
      //cout << "points: " << points << endl;
      OptiML::armad::Matrix::setMatrixRowToVector(mu,j, weightedPoints/points);

    }
    mat mdiff = OptiML::armad::Matrix::abs(mu-oldmu);
    diff = OptiML::armad::Matrix::sum(mdiff);
  } while (diff > tol);

  OptiML::toc();
  cout << "finished in " << iter << " iterations.\n";
  mu.print("mu = ");
  return 0;

}


// //NOTE: this function returns an Int: need to be careful
// def findNearestCluster( x_i: DoubleVector, mu: DoubleMatrix ) : Int = {
//      var min_d = Double.PositiveInfinity
//      var min_j = -1

//      for (j <- 0 until mu.height) {

//        val l = x_i.length
//        val v =  DoubleVector(l)
//        var idx = 0
//        while (idx != l) {
//          v(idx) = Math.pow(x_i(idx)- mu(j)(idx),2)
//          idx += 1
//        }

//        val dist = v.sum

//        if (dist < min_d){
//          min_d = dist
//          min_j = j
//        }
//      }

//      return min_j
// }
