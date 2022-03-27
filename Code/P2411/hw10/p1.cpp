#include <cstdlib>
#include <fstream>

using namespace std;

double piMC(int n) {
  int inside = 0;
  for (int i = 0; i < n; i++) {
    double x = 2 * drand48() - 1, y = 2 * drand48() - 1;
    if (x*x + y*y <= 1)
      ++inside;
  }

  return 4 * (double)inside / (double)n;

}


int main() {
  ofstream out;
  out.open("output1.txt");
  srand48(12);


  out << "n " << "approx pi" << endl;
  out << "100 " << piMC(100) << endl;
  out << "10000 " << piMC(10000) << endl;
  out << "100000 " << piMC(100000) << endl;

  return 0;

}
