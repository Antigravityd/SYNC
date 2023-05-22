pkg load symbolic
syms m n t e B;
x = sym([m/(nte**2) -B/(ne) 0; B/(ne) m/(nte**2) 0; 0 0 nte**2/m]);
inv(x)
