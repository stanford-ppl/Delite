#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <string>
#include <math.h>
#include <limits>
#include <ml/OptiML.hpp>
#include <omp.h>

using namespace std;

double tol = 0.001; // tolerance

void print_usage() {
  cout << "Usage: logreg <input data file> <labels file> <num threads>\n";
  exit(-1);
}

double ** ReadInputMatrix(char * filename, int & p_row, int & p_col) {
  int i, j;
  double **p;
  int row = 0, col = 0;
  char chr, pre_chr = 'a';
  bool start = false;

  ifstream infile(filename);
  infile>>noskipws;

  while(infile>>chr) {
    if(!start && chr != ' ') start = true;
    if(chr == '\n') ++row, start = false;
    if(start && chr == ' ' && pre_chr != ' ' && !row) ++col;
    pre_chr = chr;
  }
  infile.close();
  col++;
  p_row = row, p_col = col;

  if((p = (double**)malloc(row * sizeof(double*))) == NULL) cout<<"Cannot allocate memory";
  for(i = 0; i < row; i++) {
    if((p[i] = (double*)malloc(col * sizeof(double))) == NULL) cout<<"Cannot allocate memory";
  }

  ifstream datafile(filename);
  i = 0, j = 0;
  while(i < row && j < col) {
    datafile >> p[i][j];
    j++;
    if(j == col) i++, j = 0;
  }
  datafile.close();

  return p;
}

double * ReadInputVector(char * filename, int & p_len) {
  int i;
  double* p;
  int row = 0;
  char chr;

  ifstream infile(filename);
  infile>>noskipws;

  while(infile>>chr) {
    if(chr == '\n') ++row;
  }
  infile.close();
  p_len = row;

  if((p = (double*)malloc(row * sizeof(double))) == NULL) cout<<"Cannot allocate memory";

  ifstream datafile(filename);
  i = 0;
  for (i = 0; i < row; i++) datafile >> p[i];

  datafile.close();

  return p;
}


int main(int argc,char *argv[]) {
  double **x, *v;
  int x_row, x_col, v_len;
  int i, j;
  int iter = 0;

  if(argc != 4) {
    print_usage();
  }

  int alpha = 1;
  double tol = .001;
  int maxIter = 100;
  int numThreads = atoi(argv[3]);

  omp_set_num_threads(numThreads);

  x = ReadInputMatrix(argv[1],x_row, x_col);
  v = ReadInputVector(argv[2], v_len);
  assert(x_row == v_len);

  int numSamples = x_row;
  int numFeatures = x_col;
  
  cout << "numSamples: " << numSamples << endl;
  cout << "numFeatures: " << numFeatures << endl;

  cout << "numThreads: " << numThreads << endl;
  cout << "logreg starting computation" << endl;
  
  OptiML::tic();
  
  double diff;
  double* cur;
  if((cur = (double*)malloc(numFeatures * sizeof(double))) == NULL) cout<<"Cannot allocate memory";
  double* gradient;
  if((gradient = (double*)malloc(numFeatures * sizeof(double))) == NULL) cout<<"Cannot allocate memory";
  for (i = 0; i < numFeatures; i++) gradient[i] = 0.0;

  int outer_size;
  if (numThreads < 128) outer_size = 128; else outer_size = numThreads;
  int inner_size;
  if (numFeatures < 128) inner_size = 128; else inner_size = numFeatures;

  double** all_gradients = (double**)malloc(outer_size*sizeof(double*));
  for (i = 0; i < numThreads; i++) {
    all_gradients[i] = (double*)malloc(inner_size*sizeof(double));
  }

  do { //until_converged
    iter++;
    
    for(i = 0; i < numFeatures; i++) cur[i] = gradient[i]; //swap
    for(i = 0; i < numFeatures; i++) gradient[i] = 0.0;

    
    #pragma omp parallel private(i,j) //shared(x,v,numFeatures,numSamples)
    {
     int thread = omp_get_thread_num(); 
     for (j = 0; j < numFeatures; j++) {
        all_gradients[thread][j] = 0.0;
      }

    //sum
    #pragma omp for private(i,j) schedule(static)
    for (i = 0; i < numSamples; i++) {
      double hyp_temp = 0;
      for (j = 0; j < numFeatures; j++) { //hyp(cur, x(i))
        hyp_temp += -1.0 * cur[j] * x[i][j];
      }
      double hyp = v[i] - (1.0 / (1.0 + exp(hyp_temp)));
      for (j = 0; j < numFeatures; j++) { //x(i) * 
        all_gradients[thread][j] += x[i][j] * hyp;
      }
    }
    }

    for (i = 0; i < numThreads; i++) {
      for (j = 0; j < numFeatures; j++) {
        gradient[j] += all_gradients[i][j];
      }
    }

    for (j = 0; j < numFeatures; j++) {
      gradient[j] = cur[j] + alpha * gradient[j];
    }


    diff = 0;
    for(i = 0; i < numFeatures; i++) diff += fabs(gradient[i] - cur[i]); //L1 distance

  } while (diff > tol && iter < maxIter);

  for(i = 0; i < x_row; i++) free(x[i]);
  free(x);
  free(v);
  free(cur);

  for(i = 0; i < numThreads; i++) free(all_gradients[i]);
  free(all_gradients);

  OptiML::toc();
  cout << "finished in " << iter << " iterations.\n";
  for(i = 0; i < numFeatures; i++) {
    cout << fixed << gradient[i] << endl;
  }
  free(gradient);

  return 0;

}

