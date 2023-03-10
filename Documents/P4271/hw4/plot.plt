set terminal tikz
set output "plt.tex"

f(x) = m*x + b
g(x) = n*x + c

fit f(x) 'b1.dat' using 1:2 via m, b
fit g(x) 'b1.dat' using 1:3 via n, c

set xlabel "$J(J+1)$"
set ylabel "E(J)"
set key top left
set title "Moment of Inertia of ${^{174}}$Hf"
plot 'b1.dat' using 1:2 title "Band 1", 'b1.dat' using 1:3 title "Band 2", f(x) title "Band 1 Fit", g(x) title "Band 2 Fit"
