# Plotting accuracy of predicted probability of winning a tennis point on serve

set yrange [-0.3:1.3]
set grid

set title "Correlation between predicted probability of winning a match and average ratio of matches won \
\n ATP tennis data, HARD surface, 2008-2011, 3256 tennis matches \n  Pearson correlation=0.963"

set xlabel "Average ratio of matches won" font '1,12'
set ylabel "Predicted probability of winning a match" font '1,12'

plot "match_prob_accuracy.dat" lt 6 lc 3 notitle, x notitle  lc rgb 'grey'