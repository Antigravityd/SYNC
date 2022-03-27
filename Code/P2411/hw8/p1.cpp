#include <fstream>
#include <functional>
#include <cmath>

using namespace std;

double f(double tht, double n) {
  return 2 * 9.81 * cos(tht) - n;
}

double ftht(double tht) {
  return -2 * 9.81 * sin(tht);
}

double h(double tht, double n) {
  return 2 * 9.81 * sin(tht) - 0.4 * n;
}

double htht(double tht) {
  return 2 * 9.81 * cos(tht);
}

int main() {
  ofstream outfile;
  outfile.open("output1.txt");
  double tht = 0.1, N = 19.6, tol = 1e-8;
  int n = 0;

  outfile << "n theta N f h" << endl;

  while ((abs(f(tht, N)) >= tol || abs(h(tht, N)) >= tol) && n < 15) {
    outfile << n << " " << tht << " " << N << " " << f(tht, N) << " " << h(tht, N) << endl;
    tht -= (f(tht, N) * -0.4 - h(tht, N) * -1) / (ftht(tht) * -0.4 - htht(tht) * -1);
    N   -= (h(tht, N) * ftht(tht) - f(tht, N) * htht(tht)) / (ftht(tht) * -0.4 - htht(tht)* -1);
    ++n;
  }

  return 0;

}
