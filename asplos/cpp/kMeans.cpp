#include "asplos.h"
using std::cout;
using std::endl;

int main(int argc, char *argv[]) {
  int M, D, K;
  float* x = read2D<float>(DATA_FOLDER + "kmeans/mandrill-large.dat", M, D);
  float* mu = read2D<float>(DATA_FOLDER + "kmeans/initmu.dat", K, D);

  float* muNew = new float[K*D];
  for (int i = 0; i < K*D; i++) { muNew[i] = 0.0f; }

  // --- Insert MaxJ call here!
  // kmeans(x, mu, muNew, M, D, K);

  for (int i = 0; i < K; i++) {
    for (int j = 0; j < D; j++) {
      cout << muNew[i*D + j] << "  ";
    }
    cout << endl;
  }
}
