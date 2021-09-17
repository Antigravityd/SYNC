#include <fstream>
#include <iostream>

using namespace std;

int main() {
  double x[9] = {-1., -0.75, -0.50, -0.25, 0, 0.25, 0.5, 0.75, 1.};
  double fx[9] = {-24.0000, -16.9063, -11.5000, -7.5938, -5.0000, -3.5313, -3.0000, -3.2188, -4.0000};

  // we are unable to apply Simpson's rule in a satisfactory manner, as there is no way to calculate
  // f((a+b)/2) for most of the points.
  
  double result = 0.;
  for (int i = 0; i < 8; i++) {
    double a = x[i];
    double b = x[i+1];

    result += (b-a) * 0.5 * (fx[i] + fx[i+1]);
  }

  cout << "Integral estimate: " << result << endl;
  
  return 0;
 
}
