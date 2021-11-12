#include <iostream>
#include <fstream>

using namespace std;

int main() {
  ofstream out;
  out.open("output2.txt");
  int x = 3;

  for (int i = 0; i < 12; i++) {
    out << (float)x / 8 << endl;
    x = (4 * x + 1) % 9;
  }

  return 0;

}
