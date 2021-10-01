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
  double x = M_PI, h = M_PI / 9, end = 8 * M_PI;
  int n = (end - x) / h;

  cout << "x[rad] x[deg] sin(x) sin(x)' sin(x)''" << endl;

  for (int i = 0; i < n + 1; i++) {
    cout << x << " " << x * 180 / M_PI << " " << sin(x) << " " \
	 << cd(sin, x, h) << " " << c2d(sin, x, h) << endl;
    x += h;
  }

  return 0;

}
