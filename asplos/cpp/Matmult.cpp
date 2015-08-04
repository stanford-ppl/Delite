#include "asplos.h"
#include <stdlib.h>
using std::cout;

int main(int argc, char *argv[]) {
  std::ifstream fin;
  fin.open(CONFIG_FILE.c_str());
  if (!fin) exit(1);

  int M, P, N;
  fin >> M >> P >> N;

  fin.close();

  float* a = new float[M*P];
  float* b = new float[P*N];
  float* c = new float[M*N]; // Preallocate output

  for (int i = 0; i < M; i++) {
    for (int j = 0; j < P; j++) {
      a[i*P + j] = float(i + j);
  }}
  for (int i = 0; i < P; i++) {
    for (int j = 0; j < N; j++) {
      b[i*N + j] = float(i - j);
  }}
  for (int i = 0; i < M*N; i++) { c[i] = 0.0f; }

  // --- Insert call to MaxJ here!! --- 
  //matmult(a, b, c, M, N, P)

  for (int i = 0; i < M; i++) {
    for (int j = 0; j < N; j++) {
      cout << c[i*N + j] << " ";
    }
    cout << std::endl;
  }

}