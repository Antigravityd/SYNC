#include <cmath>
#include <iostream>
#include <fstream>

using namespace std;

struct array {double *vals; int size;};

array euler(double (*f)(double), double a, double b, double y_0, double h) { // implementation for dy/dt = f(y)
  double y = y_0;
  int n = (b - a) / h;
  double *result = (double*)malloc(n * sizeof(double));

  for (int i = 0; i < n; i++) {
    y += h * f(y);
    result[i] = y;
  }

  array r = {result, n};
  return r;

}

double f(double V) {
  return -V / 2;
}

double exact(double t) {
  return 5 * exp(-t / 2);
}
int main() {
  array result = euler(f, 0, 4, 5, 0.1);
  double t = 0.1;
  for (int i = 0; i < result.size; i++) {
    t += 0.1;
    cout << t << " " << result.vals[i] << " " << exact(0.1 + i * 0.1)<< endl;
  }

  return 0;

}
