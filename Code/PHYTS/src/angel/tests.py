from angel import *


print(inps.parse("INPS: { file.eps} X( 10 ) Y( 11 ) IX( 12.3 ) IY( 55.3 ) S(1.2) A(30 )"))

print(inps.parse("INPS: { file.eps} X( 10 ) Y( 11 ) IX( 12.3 ) IY( 55.3 ) SX(1.) SY( 2.5 ) A(30 )"))

print(set_stmt.parse(" SET: c1[ 10*2 ]"))

print(x.parse("X: Title X"))

print(y.parse("Y: Title Y :)"))

print(normal_h.parse("""'Graph'
X: Xtitle
Y: Ytitle
H: X  Y1*10+12 NY2(Error) D1
   1  2        3   4
   5  6        7   8
   9  10       11  12
"""))

print(normal_h.parse("""'Graph'
X: Xtitle
Y: Ytitle
H: X  Y1*10+12 NY2 D1**3
   1  2        3   4
   5  6        7   8
   9  10       11  12
"""))
