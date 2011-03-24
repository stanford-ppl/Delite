#include <iostream>
#include <stdlib.h>
#include <math.h>
#include <limits>
#include <ml/OptiML.hpp>
#include "armadillo"

using std::cout;
using namespace arma;

void print_usage() {
  cout << "Usage: NaiveBayes <training file> <test file>\n";
  exit(-1);
}


int main(int argc,char *argv[]) {

  if(argc != 3) {
    print_usage();
  }

  
  char* trainingFile = argv[1];
  char* testFile     = argv[2];

  
  OptiML::armad::MLInputReader::Dataset traindata;
  OptiML::armad::MLInputReader::Dataset testdata;

  OptiML::armad::MLInputReader::readTokenMatrix(traindata, trainingFile);
  OptiML::armad::MLInputReader::readTokenMatrix(testdata, testFile);

  uint numTrainDocs = traindata.numDocs;
  uint numTokens = traindata.numTokens;

  OptiML::tic();
  vec words_per_email(traindata.features.n_rows);
  words_per_email.zeros();
  for(uint j = 0; j < traindata.features.n_cols; j++) {
    for(uint i = 0; i < traindata.features.n_rows; i++) {
      words_per_email(i) += traindata.features.at(i,j);
    }
  }
  
  cout << "Training model on " << numTrainDocs << " documents.\n";
  
  //traindata.classifications.print("classes = ");
  uint spamcount = sum(traindata.classifications);
  //  cout << "spamcount: " << spamcount << endl;

  vec phi_y1(numTokens);
  vec phi_y0(numTokens);

  //traindata.features.print("features = ");

  mat featuresT = trans(traindata.features);
  //cout << "featuresT(" << featuresT.n_rows << "," << featuresT.n_cols << ") should be (" << numTokens << "," << numTrainDocs << ")\n";
  
  for(uint j = 0; j < numTokens; j++) {
    double spamwordcount = 0.0;
    double spam_totalwords = 0.0;
    double nonspamwordcount = 0.0;
    double nonspam_totalwords = 0.0;


    for (uint i = 0; i < numTrainDocs; i++) {
      if (traindata.classifications(i) == 1) {
	spamwordcount += featuresT(j,i);
	spam_totalwords += words_per_email(i);
      }
      else {
	nonspamwordcount += featuresT(j,i);
	nonspam_totalwords += words_per_email(i);
      }
    }
    phi_y1(j) = (spamwordcount + 1) / (spam_totalwords + numTokens);
    phi_y0(j) = (nonspamwordcount + 1) / (nonspam_totalwords + numTokens);
  }

  double phi_y = spamcount / (double) numTrainDocs;

  OptiML::toc();

  {
    double numTestDocs = testdata.numDocs;
    double numTokens = testdata.numTokens;

    cout << "Testing model on " << numTestDocs << " documents." << endl;
    
    vec output(numTestDocs);
    for (uint j = 0 ; j < numTestDocs; j++) {
      double p_norm = 0.0;
      double p_spam = 0.0;
      for (uint i = 0; i < numTokens; i++) {
	if (testdata.features(j,i) > 0) {
	  p_norm += (log(phi_y0(i)) + log(1-phi_y)) * testdata.features(j,i);
	  p_spam += (log(phi_y1(i)) + log(phi_y)) * testdata.features(j,i);
	}
      }
      if (p_spam > p_norm) 
	output(j) = 1.0;
      else
	output(j) = 0.0;
    }
    
    // Compute error on test set
    uint incorrect_classifications = 0;
    for (uint i = 0; i < numTestDocs; i++){
      if (testdata.classifications(i) != output(i))
	incorrect_classifications += 1;
    }
      
    rowvec p1t = trans(phi_y1);
    p1t.print("phi_y1 = ");
    rowvec p0t = trans(phi_y0);
    p0t.print("phi_y0 = ");
    printf("phi_y: %f\n", phi_y);
    printf("Test error: %f\n",  ((double)incorrect_classifications)  / ((double)testdata.numDocs));

  }




//     val phi_y = spamcount / numTrainDocs


  


  
    
  
}

