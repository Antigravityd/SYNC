#include <iostream>
#include <cmath>

using namespace std;

int main() {

  int y[7] = {18, 33, 23, 18, 5, 2, 1};

  int ctotal = 0, itotal = 0;
  for (int i = 0; i < 7; i++) {
    ctotal += i*y[i];
    itotal += y[i];
  }

  double mean = ctotal / itotal;

  double stdv = 0;
  for (int i = 0; i < 7; i++) {
    stdv = sqrt(pow(stdv, 2) + pow(y[i]-mean, 2));
  }

  stdv /= sqrt(itotal);

  cout << "The estimated population mean is " << mean << endl;
  cout << "The standard deviation is " << stdv << endl;

  return 0;

}
