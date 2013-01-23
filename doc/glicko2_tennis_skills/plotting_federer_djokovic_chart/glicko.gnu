set xdata time
set timefmt "%d/%m/%y"
set xrange ["01/01/06":"31/12/11"]
set format x "%m/%y"
set yrange [-0.4:1.5]

set title 'History of skills on serve/return for Roger Federer and Rafael Nadal on the HARD surface' font '1,12'
set xlabel 'Time' font '1,12'
set ylabel 'Tennis Skill [Glicko2 interval scale]' font '1,12'
plot \
"playerARatingOnServe.dat" using 1:2 with lines t 'Roger Federer on serve', \
"playerBRatingOnServe.dat" using 1:2 with lines t 'Novak Djokovic on serve', \
"playerARatingOnReturn.dat" using 1:2 with lines t 'Roger Federer on return', \
"playerBRatingOnReturn.dat" using 1:2 with lines t 'Novak Djokovic on return', \
"playerAWon.dat" using 1:2 lt 5 lc 1 t 'Federer vs Djokovic match - WON', \
"playerBWon.dat" using 1:2 lt 5 lc 2 t 'Federer vs Djokovic match - LOST'
   