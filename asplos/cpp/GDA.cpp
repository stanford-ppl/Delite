#include "asplos.h"
using std::cout;
using std::endl;

int main(int argc, char *argv[]) {
  int M, DC;
  float* data = read2D<float>(DATA_FOLDER + "gda/eye_eeg.dat", M, DC);
  int N = DC - 1;

  cout << "M = " << M << endl;
  cout << "N = " << N << endl;

  float* x = new float[M*N];
  bool* y = new bool[M];
  for (int i = 0; i < M; i++) {
    for (int j = 0; j < DC; j++) {
      if (j < N) x[i*N + j] = data[i*DC + j];
      else       y[i] = data[i*DC + j] > 0.0f;
    }
  }
  delete data;

  int y_zeros = 0, y_ones = 0;
  float* mu0 = new float[N];
  float* mu1 = new float[N];

  for (int i = 0; i < M; i++) {
    if (y[i]) {
      y_ones++; 
      for (int j = 0; j < N; j++) { mu1[j] += x[i*N + j]; }
    }
    else {
      y_zeros++;
      for (int j = 0; j < N; j++) { mu0[j] += x[i*N + j]; }
    }
  }
  for (int j = 0; j < N; j++) {
    mu0[j] /= y_zeros;
    mu1[j] /= y_ones;
  }

  float* sigma = new float[N*N];
  for (int i = 0; i < N*N; i++) { sigma[i] = 0.0f; }

  // --- Insert MaxJ call here!
  // gda(x, y, mu0, mu1, sigma, M, N);

  float phi = 1.0f / M * y_ones;
  cout << "phi = " << phi << endl;
  cout << "mu0 = " << endl;
  for (int j = 0; j < N; j++) { cout << mu0[j] << " "; }
  cout << endl << "mu1 = " << endl;
  for (int j = 0; j < N; j++) { cout << mu1[j] << " "; }

  cout << endl << "sigma = " << endl;
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      cout << sigma[i*N + j] << " ";
    }
    cout << endl;
  }
}