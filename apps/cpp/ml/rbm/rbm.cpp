#include <iostream>
#include <fstream>
#include <cmath>
#include "omp.h" 
#include <mkl.h>
#include <cstdlib>
#include <sys/time.h>

using namespace std;

#define PI 3.14159265358979323846

void print_usage() {
  cout << "Usage: RBM <MNIST data file> <numHiddenUnits><numCases>" << endl;
  exit(-1);
}

float* ReadInputFile(char * filename, int & p_row, int & p_col) {
  int i, j;
  float *p;
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

  if((p = (float*)malloc(row * col * sizeof(float))) == NULL) cout<<"Cannot allocate memory";

  ifstream datafile(filename);
  i = 0, j = 0;
  while(i < row && j < col) {
    datafile >> p[i * col + j];
    j++;
    if(j == col) i++, j = 0;
  }
  datafile.close();

  return p;
}

int main(int argc,char *argv[]) {
  int i, j, k;
  if(argc != 4) {
    print_usage();
  }

  int maxEpoch = 10;
  int numHiddenUnits = atoi(argv[2]);
  int numCases = atoi(argv[3]);
  float epsilonW = 0.1f;
  float epsilonVb = 0.1f;
  float epsilonHb = 0.1f;
  float weightCost = 0.0002f;
  float initialMomentum = 0.5f;
  float finalMomentum = 0.9f;

  float *trainingData;
  int numDims, numBatches;
  float sum, sum_neg;

  cout << "Using " << numHiddenUnits << " hidden units." << endl;

  cout << "Reading MNIST dataset" << endl;

  trainingData = ReadInputFile(argv[1], numBatches, numDims);
  numBatches /= numCases;

  omp_set_num_threads(1); 

  struct timeval start, end;
  long seconds, useconds;
  srand(time(NULL));

  gettimeofday(&start, NULL);

  float *data, *vishid, *hidbiases, *visbiases, *vishidinc, *hidbiasinc, *visbiasinc;
  if((data = (float*)malloc(numCases * numDims * sizeof(float))) == NULL) cout << "Cannot allocate memory for data";
  if((vishid = (float*)malloc(numDims * numHiddenUnits * sizeof(float))) == NULL) cout << "Cannot allocate memory for vishid";
  if((hidbiases = (float*)malloc(numHiddenUnits * sizeof(float))) == NULL) cout << "Cannot allocate memory for hidbiases";
  if((visbiases = (float*)malloc(numDims * sizeof(float))) == NULL) cout << "Cannot allocate memory for visbiases";
  if((vishidinc = (float*)malloc(numDims * numHiddenUnits * sizeof(float))) == NULL) cout << "Cannot allocate memory for vishidinc";
  if((hidbiasinc = (float*)malloc(numHiddenUnits * sizeof(float))) == NULL) cout << "Cannot allocate memory for hidbiasinc";
  if((visbiasinc = (float*)malloc(numDims * sizeof(float))) == NULL) cout << "Cannot allocate memory for visbiasinc";

  for(i = 0; i < numDims * numHiddenUnits; i++) vishidinc[i] = 0;
  for(i = 0; i < numHiddenUnits; i++) hidbiases[i] = 0, hidbiasinc[i] = 0;
  for(i = 0; i < numDims; i++) visbiases[i] = 0, visbiasinc[i] = 0;

  int count = 0;
  ifstream randfile("rand.txt");
  i = 0, j = 0;
  while(i < numDims && j < numHiddenUnits) {
    randfile >> vishid[i * numHiddenUnits + j];
    j++;
    count++;
    if(j == numHiddenUnits) i++, j = 0;
  }
  randfile.close();

  float *poshidprobs, *posprods, *poshidact, *posvisact, *negdata, *neghidprobs, *negprods, *neghidact, *negvisact, *poshidstates;
  if((poshidprobs = (float*)malloc(numCases * numHiddenUnits * sizeof(float))) == NULL) cout << "Cannot allocate memory for poshidprobs";
  if((posprods = (float*)malloc(numDims * numHiddenUnits * sizeof(float))) == NULL) cout << "Cannot allocate memory for posprods";
  if((poshidact = (float*)malloc(numHiddenUnits * sizeof(float))) == NULL) cout << "Cannot allocate memory for poshidact";
  if((posvisact = (float*)malloc(numDims * sizeof(float))) == NULL) cout << "Cannot allocate memory for posvisact";
  if((poshidstates = (float*)malloc(numCases * numHiddenUnits * sizeof(float))) == NULL) cout << "Cannot allocate memory for poshidstates";
  if((negdata = (float*)malloc(numCases * numDims * sizeof(float))) == NULL) cout << "Cannot allocate memory for negdata";
  if((neghidprobs = (float*)malloc(numCases * numHiddenUnits * sizeof(float))) == NULL) cout << "Cannot allocate memory for neghidprobs";
  if((negprods = (float*)malloc(numDims * numHiddenUnits * sizeof(float))) == NULL) cout << "Cannot allocate memory for negprods";
  if((neghidact = (float*)malloc(numHiddenUnits * sizeof(float))) == NULL) cout << "Cannot allocate memory for neghidact";
  if((negvisact = (float*)malloc(numDims * sizeof(float))) == NULL) cout << "Cannot allocate memory for negvisact";

  int epoch;
  for (epoch = 0; epoch < maxEpoch; epoch++) {
    float errsum = 0.0f;
    int batch;
    for (batch = 0; batch < numBatches; batch++) {
      //positive phase
      for(i = 0; i < numCases; i++)
        for(j = 0; j < numDims; j++)
          data[i * numDims + j] = trainingData[batch * numCases * numDims + i * numDims + j];

      cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, numCases, numHiddenUnits, numDims, 1.0, data, numDims, vishid, numHiddenUnits, 0, poshidprobs, numHiddenUnits);
      for(i = 0; i < numCases; i++)
        for(j = 0; j < numHiddenUnits; j++)
          poshidprobs[i * numHiddenUnits + j] = 1.0 / (1.0 + exp( - poshidprobs[i * numHiddenUnits + j] - hidbiases[j]));

      cblas_sgemm(CblasRowMajor, CblasTrans, CblasNoTrans, numDims, numHiddenUnits, numCases, 1.0, data, numDims, poshidprobs, numHiddenUnits, 0, posprods, numHiddenUnits);
      for(i = 0; i < numHiddenUnits; i++) {
        sum = 0;
        for(j = 0; j < numCases; j++)
          sum += poshidprobs[j * numHiddenUnits + i];
        poshidact[i] = sum;
      }

      for(i = 0; i < numDims; i++) {
        sum = 0;
        for(j = 0; j < numCases; j++)
          sum += data[j * numDims + i];
        posvisact[i] = sum;
      }

      for(i = 0; i < numCases; i++)
        for(j = 0; j < numHiddenUnits; j++) 
          if(poshidprobs[i * numHiddenUnits + j] > float(rand()) / float(RAND_MAX))
            poshidstates[i * numHiddenUnits + j] = 1;
          else
            poshidstates[i * numHiddenUnits + j] = 0;

      //negative phase
      cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasTrans, numCases, numDims, numHiddenUnits, 1.0, poshidstates, numHiddenUnits, vishid, numHiddenUnits, 0, negdata, numDims);
      for(i = 0; i < numCases; i++)
        for(j = 0; j < numDims; j++)
          negdata[i * numDims + j] = 1.0 / (1.0 + exp( - negdata[i * numDims + j] - visbiases[j]));
      
      cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, numCases, numHiddenUnits, numDims, 1.0, negdata, numDims, vishid, numHiddenUnits, 0, neghidprobs, numHiddenUnits);
      for(i = 0; i < numCases; i++)
        for(j = 0; j < numHiddenUnits; j++)
          neghidprobs[i * numHiddenUnits + j] = 1.0 / (1.0 + exp( - neghidprobs[i * numHiddenUnits + j] - hidbiases[j]));

      cblas_sgemm(CblasRowMajor, CblasTrans, CblasNoTrans, numDims, numHiddenUnits, numCases, 1.0, negdata, numDims, neghidprobs, numHiddenUnits, 0, negprods, numHiddenUnits);
      for(i = 0; i < numHiddenUnits; i++) {
        sum = 0;
        for(j = 0; j < numCases; j++)
          sum += neghidprobs[j * numHiddenUnits + i];
        neghidact[i] = sum;
      }
      
      for(i = 0; i < numDims; i++) {
        sum = 0;
        for(j = 0; j < numCases; j++)
          sum += negdata[j * numDims + i];
        negvisact[i] = sum;
      }
     
      float err = 0; 
      for(i = 0; i < numCases; i++)
        for(j = 0; j < numDims; j++)
          err += (data[i * numDims + j] - negdata[i * numDims + j]) * (data[i * numDims + j] - negdata[i * numDims + j]);
      errsum += err;

      //update weights and biases
      int momentum;
      if(epoch > 5) momentum = finalMomentum;
      else momentum = initialMomentum;

      for(i = 0; i < numDims; i++)
        for(j = 0; j < numHiddenUnits; j++) {
          vishidinc[i * numHiddenUnits + j] = vishidinc[i * numHiddenUnits + j] * momentum + epsilonW * ((posprods[i * numHiddenUnits + j] - negprods[i * numHiddenUnits + j]) / float(numCases) - (vishid[i * numHiddenUnits + j] * weightCost));
          vishid[i * numHiddenUnits + j] += vishidinc[i * numHiddenUnits + j];
        }

      for(i = 0; i < numDims; i++) {
        visbiasinc[i] = visbiasinc[i] * momentum + (posvisact[i] - negvisact[i]) * (epsilonVb / float(numCases));
        visbiases[i] += visbiasinc[i];
      }

      for(i = 0; i < numHiddenUnits; i++) {
        hidbiasinc[i] = hidbiasinc[i] * momentum + (poshidact[i] - neghidact[i]) * (epsilonHb / float(numCases));
        hidbiases[i] += hidbiasinc[i];
      }
    }
    cout << "--> Epoch " << epoch << endl;
    cout << " error = " << errsum << endl;
  }

  gettimeofday(&end, NULL);
  seconds = end.tv_sec - start.tv_sec;
  useconds = end.tv_usec - start.tv_usec;

  double mtime = ((seconds) + useconds / 1000000.0);
  cout << "Elapsed time: " << mtime << " seconds" << endl;
}


