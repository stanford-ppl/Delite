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

  float* a = new float[M*N];
  float* col = new float[M]; // Preallocate result

  for (int i = 0; i < M; i++) {
    col[i] = 0.0f;
    for (int j = 0; j < P; j++) {
      a[i*P + j] = float(N*i + j);
  }}

  // --- Insert call to MaxJ here!! --- 
  //float*col = sumRows(a, M, N)

  for (int i = 0; i < M; i++) {
    cout << col[i] << std::endl;
  }
}