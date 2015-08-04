#include "asplos.h"
using std::cout;
using std::endl;

int main(int argc, char *argv[]) {
  int R, C;
  float* x = read2D<float>(DATA_FOLDER + "logreg/x1m10.dat", R, C);
  float* y = read1D<float>(DATA_FOLDER + "logreg/y1m.dat", R);

  float alpha = 1.0f;
  float* theta = new float[C];
  float* thetaNew = new float[C];
  for (int j = 0; j < C; j++) { theta[j] = 0.0f; thetaNew[j] = 0.0f; }

  // --- Insert MaxJ call here!
  // logreg(x, y, theta, thetaNew, R, C, alpha);

  for (int j = 0; j < C; j++) {
    cout << thetaNew[j] << " ";
  }
  cout << endl;
}
