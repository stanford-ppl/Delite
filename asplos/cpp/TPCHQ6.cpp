#include "asplos.h"
using std::cout;
using std::endl;

int main(int argc, char *argv[]) {
  int N;
  int* shipdates = read1D<int>(DATA_FOLDER + "tpch/dates.dat", N);
  float* discounts = read1D<float>(DATA_FOLDER + "tpch/discounts.dat", N);
  int* quantities = read1D<int>(DATA_FOLDER + "tpch/quantities.dat", N);
  float* prices = read1D<float>(DATA_FOLDER + "tpch/prices.dat", N);

  float revenue;  // Result
  // --- Insert MaxJ call here!
  // query6(shipdates, discounts, quantities, prices, revenue, N);

  cout << "Revenue: " << revenue << endl;
}
