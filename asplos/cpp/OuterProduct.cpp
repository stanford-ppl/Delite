#include "asplos.h"
#include <stdlib.h>
using std::cout;

int main(int argc, char *argv[]) {
  std::ifstream fin;
  fin.open(CONFIG_FILE.c_str());
  if (!fin) exit(1);

  int M, N;
  fin >> M >> N;

  fin.close();

  float* a = new float[M];
  float* b = new float[N];
  float* c = new float[M*N]; // Preallocate output

  for (int i = 0; i < M; i++) { a[i] = float(i + 3); }
  for (int j = 0; j < N; j++) { b[j] = float(j * 5); }
  for (int i = 0; i < M*N; i++) { c[i] = 0.0f; }

  // --- Insert call to MaxJ here!! --- 
  //vectorOuterProduct(a, b, c, M, N)

  for (int i = 0; i < M; i++) {
    for (int j = 0; j < N; j++) {
      cout << c[i*N + j] << " ";
    }
    cout << std::endl;
  }
}