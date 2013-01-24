# Plotting accuracy of predicted probability of winning a tennis point on serve

set yrange [0.3:1]
set grid

set title "Correlation between predicted probability of winning a point on serve and average ratio of points won on serve. \
\n ATP tennis data, HARD surface, 2008-2011, 6021 tennis matches \n  Pearson correlation=0.99277"
set xlabel "Average ratio of points won on serve" font '1,12'
set ylabel "Predicted probability of winning a point on serve " font '1,12'

plot "point_prob_accuracy.dat" notitle, x notitle lc rgb 'grey'