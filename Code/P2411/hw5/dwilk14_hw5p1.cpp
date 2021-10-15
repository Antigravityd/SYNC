#include <cmath>
#include <iostream>
#include <fstream>

using namespace std;

float f(float x) {
  return 5 / 2 * pow(x, 3) - 3 / 2 * x'
    }

int main() {
  float h = 0.1, xm = -1, xp = xm + h;
  int n = 0;

  outfile << "x+ x- f(xmid)" << endl;
 origin: n++;
  fxmid = f((xp + xm) / 2);
  cout <<  xp << " " << xm << " " << fxmid << endl;
  fxmid < 1E-6 + n >= 20 ? goto end : xp += h + xm += h + goto origin;
  
 end: 
  
  return 0;
  
}
