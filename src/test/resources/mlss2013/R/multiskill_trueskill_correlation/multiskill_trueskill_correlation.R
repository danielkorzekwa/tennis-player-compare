multiskill_data <- read.csv("trueskill_point_servereturn_2007.csv",header=T)
multiskill_data$predicted_prob <- (multiskill_data$predicted_prob +1)*0.01
multiskill_data$true_prob <- (multiskill_data$true_prob +1)*0.01

trueskill_data <- read.csv("trueskill_2007.csv",header=T)
trueskill_data$predicted_prob <- (trueskill_data$predicted_prob +1)*0.01
trueskill_data$true_prob <- (trueskill_data$true_prob +1)*0.01

symbols(multiskill_data$true_prob ~ multiskill_data$predicted_prob,
circles=multiskill_data$sample_size/sum(multiskill_data$sample_size),inches = "false",add=FALSE,bg=rgb(0, 1, 0, alpha=0.5),xlab="",ylab="")

symbols(trueskill_data$true_prob ~ trueskill_data$predicted_prob,
circles=trueskill_data$sample_size/sum(trueskill_data$sample_size),inches = "false",add=TRUE,bg=rgb(0, 0, 1, alpha=0.5))

abline(0,1)

par(mar=c(4.2, 3.8, 0.2, 0.2))

abline(h = seq(0,1,by=0.1),  col = "lightgray", lty = 3)
grid(,NA)

title(xlab="Predicted win probability of a tennis match (100 bins)")
title(ylab="Percentagate of tennis matches won")

legend("topleft",c("MultiSkill, c = 0.993, p < 2.2e-16","TrueSkill, c = 0.991, p < 2.2e-16"),inset=0.03, cex=1,
col=c(rgb(0, 1, 0, alpha=0.5),rgb(0, 0, 1, alpha=0.5)),pch=c(20,20), lty=c(0,0))

