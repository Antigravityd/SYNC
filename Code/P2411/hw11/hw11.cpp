#include <fstream>
#include <cmath>

using namespace std;

int main() {
  ofstream out;
  out.open("output.txt");
  double beta = 10, better = 10;

  out << "beta_i better_i beta_i+1 better_i+1" << endl;

  for (int i = 0; i < 1000; i+=2) {
    out << beta << " " << better << " ";
    beta = fmod((57. * beta + 1.), 256.);
    better = fmod((25214903917. * better + 11.), 3.0E14);
    out << beta << " " << better << endl;
    beta = fmod((57. * beta + 1.), 256.);
    better = fmod((25214903917. * better + 11.), 3.0E14);
  }


  return 0;

}
