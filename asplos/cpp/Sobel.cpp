#include "asplos.h"
using std::cout;
using std::endl;

int main(int argc, char *argv[]) {
  int H, W;
  int* imgIn = read2D<int>(DATA_FOLDER + "conv/test_img.dat", H, W);

  int* imgPad = new int[(H+2)*(W+2)];
  for (int i = 0; i < H+2; i++) {
    for (int j = 0; j < W+2; j++) {
      if (i > 0 && j > 0 && i <= H && j <= W) {
        imgPad[i*(W+2) + j] = imgIn[(i-1)*W + (j-1)];
        imgIn[(i-1)*W + (j-1)] = 0;
      }
      else 
        imgPad[i*(W+2) + j] = 0;
    }
  }
  int* imgOut = imgIn; // Reuse memory from image input now that we've copied mem

  // --- Insert MaxJ call here!
  // gda(img, imgOut, H, W);

  for (int i = 0; i < H; i++) {
    for (int j = 0; j < W; j++) {
      cout << imgOut[i*W + j] << " ";
    }
    cout << endl;
  }
}