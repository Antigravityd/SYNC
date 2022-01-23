from numpy import arange
from math import exp
from itertools import product

def f(a, b, x, y, z):
    return 4*x*y*z**2*(a+b)*(exp(-z*(b-a))-exp(z*(a-b))) \
           +4*z**2*(a-b)*(x**2*exp(z*(a+b))-y**2*exp(-z*(a+b))) \
           +exp(z*(b-a))-exp(z*(a-b))

for a, b, x, y, z in product(*[arange(-5, 5, 0.01) for i in range(0,5)]):
    res = f(a, b, x, y, z)
    if abs(res) < 1e-5 and abs(z) > 0.0001 and abs(a-b) > 0.0001:
        print("zero at ", *[round(a, 4) for a in [a,b,x,y,z]])
