#define _USE_MATH_DEFINES
#include <fstream>
#include <cmath>
#include <iostream>

using namespace std;

int main() {
  float x0 = 1.4;
  float y0 = 2.0;
  float v0 = 25.6;
  float theta = 35.0 * M_PI / 180.0;
  float g = 9.81;

  float x = x0;
  float y = y0;

  ofstream outfile("p3_out.txt");
  outfile << "t       x       y" << endl;
  
  float t = 0.0;
  while (y > 0.0) {
    x = x0 + v0 * cos(theta) * t;
    y = y0 + v0 * sin(theta) * t - g * pow(t, 2) / 2;
    outfile << t << "\t" << x << "\t" << y << endl;
    t += 0.1;
  }

  return 0;

}
