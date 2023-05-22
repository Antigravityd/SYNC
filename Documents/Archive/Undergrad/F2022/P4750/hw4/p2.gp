set terminal tikz
set output 'p2.tex'

c0 = 500e9 # ct / m
fc = 350   # Hz
fa = 0.7   # Hz
a0 = 1e-12 # m / ct
D  = 42500 
i = {0, 1}

C(f) = c0 / (1 + i * (f / fc))
A(f) = a0 / (1 - (f / fa)**2 - i * (f / fa))

G(f) = abs(A(f) * C(f) * D)
H(f) = abs(1 / (1 + G(f)))

set title 'Constant Feedback Loop Gains'
set xlabel 'Frequency (Hz)'
set ylabel 'Gain'
set logscale xy
plot [f=0.1:1e3] G(f) title 'Open-loop', H(f) title 'Closed-loop'
