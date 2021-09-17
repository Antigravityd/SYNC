#include <iostream>
#include <cmath>

using namespace std;
double f(double x) {
  double k = 9.E9;
  double lambda = 2.E-10;
  double d = 0.1;

  return k * lambda / sqrt(pow(x,2) + pow(d,2));

}

int main() {
  double k = 9.E9;
  double lambda = 2.E-10;
  double d = 0.1;

  double L = 0.5;
  double step = L / 514;
  double a = 0.;
  
  double result;
  for (int i = 0; i < 514; i++) {
    double b = a + step;

    result += (b - a) / 6 * (f(a) + 4 * f((a + b) / 2) + f(b));

    a = b;
  }

  cout.precision(15);
  cout << "Integral estimate: " << result << " V" << endl;
  cout << "Exact: " << k * lambda * log((L + sqrt(pow(L, 2)+pow(d, 2))) / d) << " V" << endl;
  
  return 0;
}
