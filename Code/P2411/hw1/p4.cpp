#include <fstream>
#include <iostream>
#include <cmath>

using namespace std;

int main() {
  ofstream outfile("p4_out.txt");
  double pi = 3.141592653589793;

  int m1 = 20;
  int m2 = 40;

  double step1 = 12 * pi / m1;
  double step2 = 12 * pi / m2;

  outfile << "m = 20:" << endl;
  outfile << "x" << "\t" << "x/2" << "\t" << "-x/2" << "\t" << "0.5xsin(x)" << endl;
  for (int i = 0; i < m1; i++) {
    double x = -6 * pi + step1 * i;
    outfile << x << "\t" << x/2 << "\t" << -1 * x/2 << "\t" << 0.5 * x * sin(x) << endl;
  }

  outfile << endl << "m = 40:" << endl;
  outfile << "x" << "\t" << "x/2" << "\t" << "-x/2" << "\t" << "0.5xsin(x)" << endl;
  for (int i = 0; i < m2; i++) {
    double x = -6 * pi + step2 * i;
    outfile << x << "\t" << x/2 << "\t" << -1 * x/2 << "\t" << 0.5 * x * sin(x) << endl;
  }

  return 0;

}
