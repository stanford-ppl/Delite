#include "asplos.h"
using std::cout;
using std::endl;

int main(int argc, char *argv[]) {
  int DR, DC;
  float* data = read2D<float>(DATA_FOLDER + "knn/letter-data.dat", DR, DC);

  float heldOutRatio = 0.2f;
  int N = heldOutRatio * DR;  // Number of training samples
  int R = DR - N;             // Number of testing samples
  int D = DC - 1;             // Feature dimensions
  //int K = 3;                // (Compile constant for now)

  float* test = new float[R*D];   int* testLabels = new int[R];
  float* train = new float[N*D];  int* trainLabels = new int[N];

  for (int i = 0; i < R; i++) {
    for (int j = 0; j < DC; j++) {
      if (j < D)
        test[i*D + j] = data[i*DC + j];
      else
        testLabels[i] = (int) data[i*DC + j];
    }
  }
  int y = 0;
  for (int i = R; i < DR; y++, i++) {
    for (int j = 0; j < DC; j++) {
      if (j < D)
        train[y*D + j] = data[i*DC + j];
      else
        trainLabels[y] = data[i*DC + j];
    }
  }
  delete data;

  int* labelsOut = new int[R];
  for (int i = 0; i < R; i++) { labelsOut[i] = 0; }

  // --- Insert MaxJ call here!
  // knn(test, testLabels, train, labelsOut, N, R, D);

  int nWrong = 0;

  for (int i = 0; i < R; i++) {
    cout << "#" << i << ": ";
    if (labelsOut[i] != testLabels[i]) { cout << "Incorrect";  nWrong++; }else cout << "Correct";
    cout << " (expected " << testLabels[i] << ", found " << labelsOut[i] << ")" << endl;
  }
  float percent = 100.0f * float(nWrong) / R; 
  cout << "Number incorrect: " << nWrong << "/" << R << " (" << percent << ")" << endl;
}
