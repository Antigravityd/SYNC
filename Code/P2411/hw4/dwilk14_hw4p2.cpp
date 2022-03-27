#include <cmath>
#include <fstream>
#include <iostream>

using namespace std;

double cd(double (*f)(double), double x, double h) {
  return (f(x + h / 2) - f(x - h / 2)) / h;
}

double c2d(double (*f)(double), double x, double h) {
  return (cd(f, x + h / 2, h) - cd(f, x - h / 2, h)) / h;
}

int main() {
  ofstream outfile;
  outfile.open("output2");
  double x = M_PI, end = 8 * M_PI, h = M_PI / 18;

  outfile << "x[rad] x[deg] sin(x) sin(x)' sin(x)''" << endl;

  while (x < end) {
    outfile << x << " " << x * 180 / M_PI << " " << sin(x) << " " \
	 << cd(sin, x, h) << " " << c2d(sin, x, h) << endl;
    x += h;
  }

  return 0;

}
