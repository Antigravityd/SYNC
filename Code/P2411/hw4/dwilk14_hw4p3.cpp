#include <cmath>
#include <fstream>
#include <iostream>

using namespace std;

double ed(double (*f)(double), double x, double h) {
  return 1 / (3 * h) * (8 * (f(x + h/4) - f(x - h/4)) - (f(x + h/2)-f(x - h/2)));
}

double V(double r) {
  return (r > 0.8) ? 4.5 / r : 5.625;
}

double E(double r) {
  return (r > 0.8) ? 4.5 / pow(r, 2) : 0;
}

int main() {
  ofstream outfile;
  outfile.open("output3.txt");
  double h = 0.02, r = 0.01, b = 4, step = (b - r) / 26;

  outfile << "r V Enum Etheory" << endl;
  for (int i = 0; i < 26; i++) {
    outfile << r << " " <<  V(r) << " " <<  -ed(E, r, h) << " " << E(r) << endl;
    r += step;
  }

  return 0;
  
}
