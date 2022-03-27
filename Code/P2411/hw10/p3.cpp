#include <iostream>
#include <fstream>
#include <cstdlib>

using namespace std;

int main() {
  srand48(12);
  int x = 10;
  ofstream out;
  out.open("output3.txt");

  out << "drand custom" << endl;

  for (int i = 0; i < 10000; i++) {
    x = (57 * x + 1) % 256;
    out << drand48() << " " << (double)x / 256. << endl;
  }


  return 0;

}
