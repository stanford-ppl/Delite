#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <string>
#include <math.h>
#include <limits>
#include <sys/time.h>
#include "omp.h"
//#include <ml/OptiML.hpp>

#define MAX_ITER 10

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
}

using namespace std;

double tol = 0.001; // tolerance

void print_usage() {
  cout << "Usage: kmeans <input data file> <initmu data file>\n";
  exit(-1);
}

void constructNearestClusterVector(double *v, double **x, int x_row, int x_col, double **mu, int mu_row, int mu_col) {

  #pragma omp parallel for 
  for(int i = 0; i < x_row; i++) {
    double min_d = -1; 
    int min_j = -1;
    for(int j = 0; j < mu_row; j++) {
      double dist = 0;
      for(int k = 0; k < x_col; k++) {
        dist += (x[i][k] - mu[j][k]) * (x[i][k] - mu[j][k]);
      }
      if(min_d < -0.5 || dist < min_d) min_d = dist, min_j = j;
    }
    v[i] = min_j;
  }
}

double ** ReadInputFile(char * filename, int & p_row, int & p_col) {
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

int main(int argc,char *argv[]) {
  double **x, **mu;
  int x_row, x_col, mu_row, mu_col;
  int iter = 0;

  if(argc != 3) {
    print_usage();
  }

  x = ReadInputFile(argv[1],x_row, x_col);
  mu = ReadInputFile(argv[2],mu_row, mu_col);

  double **oldmu;
  if((oldmu = (double**)malloc(mu_row * sizeof(double*))) == NULL) cout<<"Cannot allocate memory for oldmu";
  for(int i = 0; i < mu_row; i++) {
    if((oldmu[i] = (double*)malloc(mu_col * sizeof(double))) == NULL) cout<<"Cannot allocate memory for oldmu";
  }
  
  cout << "kmeans starting computation.\n";
  
  OptiML::tic();
  
  double diff;
  double *c;
  if((c = (double*)malloc(x_row * sizeof(double))) == NULL) cout<<"Cannot allocate memory for c";

  // Moved this allocation to inside the loop for parallel execution
  //double *weightedPoints;
  //if((weightedPoints = (double*)malloc(x_col * sizeof(double))) == NULL) cout<<"Cannot allocate memory for weightedPoints";

  do {
    iter++;
    
    for(int i = 0; i < mu_row; i++) 
      for(int j = 0; j < mu_col; j++)
        oldmu[i][j] = mu[i][j];
    
    constructNearestClusterVector(c, x, x_row, x_col, mu, mu_row, mu_col);
    
    #pragma omp parallel for 
    for(int i = 0; i < mu_row; i++) {
      double *weightedPoints;
      if((weightedPoints = (double*)malloc(x_col * sizeof(double))) == NULL) cout<<"Cannot allocate memory for weightedPoints";
      for(int j = 0; j < x_col; j++) weightedPoints[j] = 0;
      int points = 0;
      for(int j = 0; j < x_row; j++) {
        if(fabs(c[j] - i) < 1e-8) {
          for(int k = 0; k < x_col; k++) weightedPoints[k] += x[j][k];
          points++;
        }
      }
      if(!points) points++;
      for(int j = 0; j < mu_col; j++) mu[i][j] = weightedPoints[j] / points;
      free(weightedPoints);
    }

    diff = 0;  
    for(int i = 0; i < mu_row; i++)
      for(int j = 0; j < mu_col; j++)
        diff += fabs(mu[i][j] - oldmu[i][j]);

  } while (diff > tol && iter < MAX_ITER);

  for(int i = 0; i < x_row; i++) free(x[i]);
  free(x);
  for(int i = 0; i < mu_row; i++) free(oldmu[i]);
  free(oldmu);
  free(c);
  //free(weightedPoints);

  OptiML::toc();
  cout << "finished in " << iter << " iterations.\n";
  for(int i = 0; i < mu_row; i++) {
    for(int j = 0; j < mu_col; j++)
      cout<<mu[i][j]<<"  ";
    cout<<endl;
  }

  for(int i = 0; i < mu_row; i++) free(mu[i]);
  free(mu);

  return 0;

}

