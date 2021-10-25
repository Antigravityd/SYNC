#include <fstream>
#include <iostream>
#include <cmath>

using namespace std;

double f(double x) {
    return tan(x) - x;
}

double deriv(double x) {
  double h = 0.01;
  return (f(x + h) - f(x)) / h;
}

int main() {
  ofstream outfile;
  outfile.open("output2.txt");
  outfile.precision(10);
  double guess1 = 4.4,  guess2 = 7.6, fx1 = f(guess1), fx2 = f(guess2);


  outfile << "n, x1, fx1" << endl;

  for (int i = 0; i < 15; i++) {
    outfile << i << ", " << guess1 << ", " << fx1 << endl;

    if (abs(fx1) < 1e-8) {
      break;
    }

    guess1 -= fx1 / deriv(guess1);
    fx1 = f(guess1);

  }

  outfile << "n, x2, fx2" << endl;


  for (int i = 0; i < 15; i++) {
    outfile << i << ", " << guess2 << ", " << fx2 << endl;

    if (abs(fx2) < 1e-8) {
      break;
    }

    guess2 -= fx2 / deriv(guess2);
    fx2 = f(guess2);

  }

  return 0;

}
