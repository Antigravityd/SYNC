#include <fstream>

using namespace std;

int main() {
  ofstream outfile;
  outfile.open("output.txt");
  float A[5][5];

  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < 5; j++) {
      A[i][j] = (j >= i) ? -i + j + 1: 0;
    }
  }

  float b[5] = {3, 2.03, 1.16, 0.44, 0.02};

  float x[5];
  for (int i = 4; i >=0; i--) {
    x[i] = b[i];
    for (int j = i + 1; j < 5; j++) {
      x[i] -= A[i][j] * x[j];
    }
  }

  for (int i = 0; i < 5; i++) {
    outfile << x[i] << " ";
  }

  outfile << endl;

  return 0;

}
