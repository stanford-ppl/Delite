#include <iostream>
#include <stdlib.h>
#include <math.h>
#include <algorithm>
#include <limits>
#include <ml/OptiML.hpp>
#include <ml/Random.cpp>
#include "armadillo"

using std::cout;
using std::max;
using std::min;
using namespace arma;

void print_usage() {
  cout << "Usage: SVM <train data file> <test data file> <model filename> <num tests>\n";
  exit(-1);
}


class SVMModel {
  rowvec weights;
  vec alphas; 
  double b;
  
  Random* rand;

public:
  
  SVMModel() {
    b = 0.0;
    rand = new Random(100);
  }
  
  void smoTrain(mat& X, vec& Y, double C, double tol, uint max_passes) {
    printf("Training SVM using the SMO algorithm\n");
    
    // intermediate training info
    alphas = vec(X.n_rows);
    alphas.zeros();

    //X.print("X=");
    
    uint numSamples = X.n_rows;
    uint passes = 0;
    uint actual = 0;
    
    while (passes < max_passes){
      cout << "." << flush;
      actual++;
      uint num_changed_alphas = 0;
      for (uint i = 0; i < numSamples; i++) {
	//alphas.print("alphas=");
	//Y.print("Y=");
	
	//rowvec x_r = X.row(i);
	//printf("x(%d)", i);
	//x_r.print("="); 
	vec f_itt = X*trans(X.row(i));
	//f_itt.print("f_itt=");
	//vec f_ity = Y%f_itt;
	//f_ity.print("f_ity=");
	vec f_it = alphas%Y%(f_itt);
	//f_it.print("f_it=");
	
	
	double f_i = sum(f_it) + b;
	//cout << "f_i: " << f_i << endl;
	double E_i = f_i - Y.at(i);
	//cout << "E_i: " << E_i << endl;
	
	if (((Y.at(i)*E_i < -1 * tol) && (alphas.at(i) < C)) || ((Y.at(i)*E_i > tol) && (alphas.at(i) > 0))){
	  // select a candidate j from the remaining numSamples-i samples at random
	  //cout << "if1\n";	  
          uint j = floor(rand->nextDouble()*(numSamples-1)) + 1;
	  //cout << "j: " << j << endl;
	  while (j == i){
	    j = floor(rand->nextDouble()*(numSamples-1)) + 1;
	    //cout << "j: " << j << endl;
	  }
	 
	  
	  vec f_jt = alphas%Y%(X*trans(X.row(j)));
	  //f_jt.print("f_jt=");
	  double f_j = sum(f_jt) + b;
	  double E_j = f_j - Y.at(j);
	  //cout << "f_j: " << f_j << endl;
	  //cout << "E_j: " << E_j << endl;
	  
	  double old_aj = alphas.at(j);
	  double old_ai = alphas.at(i);

	  // calculate bounds L and H that must hold in order for a_i, alphas(j) to
	  // satisfy constraints and check
          double L = 0.0;
          double H = 0.0;
	  if (Y(i) != Y(j)){
	    L = max(0.0, alphas.at(j) - alphas.at(i));
	    H = min(C, C + alphas.at(j) - alphas.at(i));
	  }else{
	    L = max(0.0, alphas.at(i) + alphas.at(j) - C);
	    H = min(C, alphas.at(i) + alphas.at(j));
	  }

	  //cout << "L: " << L << " H: " << H << endl;

	  if (L != H){
	    // calculate eta
	    //cout << "if2\n";
	    double eta = dot(X.row(i), X.row(j))*2 - dot(X.row(i),X.row(i)) - dot(X.row(j), X.row(j));
	    //cout << "eta: " << eta << endl;
	    // check eta
	    if (eta < 0){
	      //cout << "if3" << endl;
	      // compute new alphas(j)
	      alphas.at(j) = alphas.at(j) - Y.at(j)*(E_i-E_j)/eta;
	      // clip alphas(j) if necessary
	      if (alphas.at(j) > H) alphas.at(j) = H;
	      else if (alphas.at(j) < L) alphas.at(j) = L;
	      
	      //cout << "alpha(" << j << ") = " << alphas.at(j) << endl;
	      // check alphas(j) convergence
	      //printf("tol?: %f\n", alphas.at(j) - old_aj);
	      if (fabs(alphas.at(j) - old_aj) >  0.00001){
		//cout << "if4" << endl;

		// find a_i to maximize objective function
		old_ai = alphas.at(i);
		alphas.at(i) = alphas.at(i) + Y.at(i)*Y.at(j)*(old_aj-alphas.at(j));
		//cout << "alpha(" << i << ") = " << alphas.at(i) << endl;
		  // compute the new b such that KKT conditions are satisfied
		double old_b = b;
		
		double b1 = b - E_i - dot(X.row(i),X.row(i))*Y.at(i)*(alphas.at(i)-old_ai)- dot(X.row(i),X.row(j))*Y.at(j)*(alphas.at(j)-old_aj);
		double b2 = b - E_j - dot(X.row(i),X.row(j))*Y.at(i)*(alphas.at(i)-old_ai)- dot(X.row(j),X.row(j))*Y.at(j)*(alphas.at(j)-old_aj);
		if ((alphas(i) > 0) && (alphas(i) < C)){
		  //cout << "if5" << endl;
		  b = b1;
		}
		if ((alphas(j) > 0) && (alphas(j) < C)){
		  //cout << "if6" << endl;
		  b = b2;
		}
		if (old_b == b){
		  // neither threshold valid
		  cout << "if7" << endl;
		  //b = ((b1+b2)/2);
		}
		num_changed_alphas += 1;
		//cout << "b= " << b << endl;
	      } // alpha converged?
	      
	    } // negative eta?
	  } // L != H?
	} // main if (select alphas)
      } // for i = 1 to numSamples

      if (num_changed_alphas == 0){
	passes += 1;
      }else{
	passes=0;
      }
    } // while

    // SMO finished        
    printf("\nIterations:%d\n", actual);
  }

  void computeWeights(mat& X, vec& Y) {
    // internal model storage
    weights = rowvec(X.n_cols);
    weights.zeros();
    
    // compute the weights (assuming a linear kernel)
    for (uint i=0; i < X.n_rows; i++){
      weights = weights + X.row(i)*alphas.at(i)*Y.at(i);
    }
  }

  int classify(rowvec& test_pt) {
    // SVM prediction is W'*X + b
    //cout << "weights:" << weights.n_elem << " test_pt" << test_pt.n_elem << endl;
    if ((dot(trans(weights),test_pt) + b) < 0){
      return -1;
    }
    else 
      return 1;
  }
  
};

int main(int argc,char *argv[]) {

  if(argc != 5) {
    print_usage();
  }

  
  
  
  char* trainfile = argv[1];
  char* testfile = argv[2];
  char* modelFile = argv[3];
  uint  numTests = atoi(argv[4]);

  std::vector<double> runErrors;
  
  OptiML::armad::MLInputReader::Dataset traindata;
  OptiML::armad::MLInputReader::Dataset testdata;

  OptiML::armad::MLInputReader::readTokenMatrix(traindata, trainfile);
  OptiML::armad::MLInputReader::readTokenMatrix(testdata, testfile);

  // adjust the classification labels to -1 and +1 for SMO
  vec YTrain(traindata.classifications.n_rows);
  for(uint i=0; i < traindata.classifications.n_rows; i++) {
    YTrain.at(i) = traindata.classifications.at(i) == 0 ? -1 : 1;
  }
  vec YTest(testdata.classifications.n_rows);
  for(uint i=0; i < testdata.classifications.n_rows; i++) {
    YTest.at(i) = testdata.classifications.at(i) == 0 ? -1 : 1;
  }

  // run the SMO training algorithm
  SVMModel svm;
  OptiML::tic();
  svm.smoTrain(traindata.features, YTrain, 1, .001, 10);
  OptiML::toc();

  svm.computeWeights(traindata.features, YTrain);
  // TEST RESULTS
  uint numTestDocs = testdata.features.n_rows;
  vec outputLabels(numTestDocs);
  //SVMModel svm_test(modelFile);
  for (uint i=0; i < numTestDocs; i++){
    rowvec pt = testdata.features.row(i);
    outputLabels.at(i) = svm.classify(pt);
  }
  printf("SVM testing finished. Calculating error..\n");
  uint errors = 0;
  for (uint i = 0; i < numTestDocs; i++){
    if (YTest.at(i) != outputLabels(i)) 
      errors +=1;
  }
  printf("Classification error: %f\n", ((double)errors/(double)numTestDocs));
  runErrors.push_back((double)errors/(double)numTestDocs);


}

