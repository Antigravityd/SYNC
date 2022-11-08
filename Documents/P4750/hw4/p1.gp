set terminal tikz

set output "p1.tex"

c = 3e8
h = 6.63e-34
m = 40
l = 1064e-9
L = 4e3

shot(f, P) =  (1 / L) * sqrt(h * c * l / P)
radp(f, P) = 1 / (m * f**2 * L) * sqrt(h * P / (pi**2 * c * l))
qnoise(f, P) = sqrt(shot(f, P)**2 + radp(f, P)**2)

set logscale xy
set yrange [1e-23:1e-19]
set label "\\SI{".gprintf("%.3e", qnoise(1000, 40))."}{}" at 1000,qnoise(1000, 40)
set label "\\SI{".gprintf("%.3e", qnoise(1000, 40e3))."}{}" at 1000,qnoise(1000, 40e3)
set label "\\SI{".gprintf("%.3e", qnoise(1000, 40e6))."}{}" at 1000,qnoise(1000, 40e6)
set xlabel "$\\log(\\textrm{Frequency (Hz)})$"
set ylabel "$\\log(h)$" offset -2,0
set title "Standard Quantum Limit for Different LIGO Laser Powers"
plot [f=0.1:1e3] qnoise(f, 40) title '\SI{40}{W}', \
     qnoise(f, 40e3) title '\SI{40}{kW}', \
     qnoise(f, 40e6) title '\SI{40}{MW}'

