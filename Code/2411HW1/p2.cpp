#include <iostream> 
#include <iomanip> 
using namespace std; 
 
int main (){ 
  const float c=3.e8;  // speed of light, m/s 
  double fs,fo,deltaf,vrel;  // fs, source frequency, Hz        // fo, frequency detected by object, Hz 
       // deltaf, frequency shift, Hz        
       // vrel, object velocity, m/s 
  fs=103.3e6; 
  deltaf=9.44;    
 
  cout << "Algorithm (i):" << endl; 
  fo=fs+deltaf; 
  vrel=fo*c-fs*c; 
  vrel=vrel/fs; 
  cout << "v=" << setprecision (15) << vrel << " m/s" << endl; 
 
  cout << "Algorithm (ii):" << endl; 
  vrel=deltaf*c/fs; 
  cout << "v=" << setprecision (15) << vrel << " m/s" << endl; 
 
  cout << "Just a check:" << endl; 
  cout << "v=" << setprecision (15)  
     << 9.44*(3.e8)/( (double) fs) << " m/s" << endl; 
 
  return 0; 
}
