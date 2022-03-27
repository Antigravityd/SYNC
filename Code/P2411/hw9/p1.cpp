#include <iostream>
#include <fstream>

using namespace std;

int main() {
  ofstream out;
  out.open("output1.txt");
  int x = 3;

  for (int i = 0; i < 12; i++) {
    out << x << endl;
    x = (4 * x + 1) % 9;
  }

  return 0;

}
