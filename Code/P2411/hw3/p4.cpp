#include <iostream>
#include <fstream>
#include <cmath>

using namespace std;

double p4(double x) {
  return (3 - 30 * pow(x, 2) + 35 * pow(x, 4)) / 8;
}

double FD(double (*f)(double), double x, double h) {
  return (f(x + h) - f(x)) / h;
}

double CD(double (*f)(double), double x, double h) {
  return (f(x + h/2) - f(x-h/2)) / h;
}

double ED(double (*f)(double), double x, double h) {
  return 1 / (3 * h) * (8 * (f( x + h/4) - f(x - h/4)) - f(x + h/2) + f(x - h/2));
}

double exact(double x) {
  return 35 / 2 * pow(x, 3) - 7.5 * x;
}

int main() {
  ofstream outfile;
  outfile.open("output4.txt");
  double h = 0.1, a = -1, b = 1, step = (b - a) / 22, x = a + step;

  outfile << "x\tP_4(x)\tFD err\tCD err\tED err" << endl;

  for (int i = 0; i < 21; i++) {
    outfile << x << " " << p4(x) << " " << abs(FD(p4, x, h) - exact(x)) << " " \
	    << abs(CD(p4, x, h) - exact(x)) << " " << abs(ED(p4, x, h) - exact(x)) << endl;

    x += step;
  }

  return 0;

}
