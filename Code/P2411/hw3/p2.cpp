#include <iostream>
#include <fstream>
#include <cmath>

using namespace std;

float V(float r) {
  if (r <= 0.80) {
    return -5.625;
  } else {
    return -4.5 / r;
  }
}

float E(float r) {
  if (r <= 0.80) {
    return 0.0;
  } else {
    return 4.5 / pow(r, 2);
  }
}

int main() {
  ofstream outfile;
  float a = 0.001, b = 4.0, h = 0.02, space = (b - a) / 22, x = a + space;
  outfile.open("output.txt");

  outfile << "x\tV\tNumerical E\tTheoretical E" << endl;

  for (int i = 0 ; i < 21; i++) {
    outfile << x << " " << V(x) << " " << (V(x + h) - V(x)) / h << " " << E(x) << endl;
    x += space;
  }

  return 0;

}
