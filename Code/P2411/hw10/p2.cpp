#include <fstream>
#include <cstdlib>
#include <cmath>

using namespace std;


double moment48(int n, int k) {
  double sum = 0;
  for (int i = 0; i < n; i++) {
    sum += pow(drand48(), k);
  }

  return sum / n;

}

double momentCustom(int n, int k, int last) {
  double sum = 0;
  for (int i = 0; i < n; i++) {
    last = (57 * last + 1) % 256;
    sum += pow((double)last / 256., k);
  }

  return sum / n;

}




int main() {
  ofstream out;
  out.open("output2.txt");
  srand48(12);
  int last = 10;

  out << "k n moment" << endl;

  for (int k = 1; k < 4; k++) {
    for (int ex = 2; ex < 6; ex++) {
      int n = pow(10, ex);
      out << k << " " << n << " " << momentCustom(n, k, last) << endl;
    }
  }

  out << "k n moment" << endl;

  for (int k = 1; k < 4; k++) {
    for (int ex = 2; ex < 6; ex++) {
      int n = pow(10, ex);
      out << k << " " << n << " " << moment48(n, k) << endl;
    }
  }


  return 0;

}
