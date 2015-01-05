# Analysis of tennis probabilities. Input data example:
#  predicted_prob  true_prob sample_size
#1           0.02 0.02522843         394
#2           0.03 0.02668653         839
#3           0.04 0.03702703         740
#4           0.05 0.04712575         835
#5           0.06 0.06099778        1353
#6           0.07 0.07106265        1261

data <- read.csv("trueskill_point_servereturn_2007.csv",header=T)
data$predicted_prob <- (data$predicted_prob +1)*0.01
data$true_prob <- (data$true_prob +1)*0.01
#data <- subset(data,predicted_prob>0.2 & predicted_prob<0.8)

#Calculate log likelihood
winLlh <- log(data$predicted_prob)*(data$true_prob * data$sample_size)
loseLlh <- log(1-pmin(pmax(data$predicted_prob,0.00001),0.99999))*((1-data$true_prob)*data$sample_size)
llh <- sum(winLlh  + loseLlh ) / sum(data$sample_size)
sprintf("llh=%f",llh)

#Calculate correlation
sprintf("Correlation predicted_prob vs true_prob=%f",cor(data$predicted_prob,data$true_prob))

#Linear regression fit
#summary(lm(data$true_prob~data$predicted_prob))

#Draw analysis charts
layout(matrix(c(1,1)))
barplot(data$sample_size,names.arg=data$predicted_prob,xlab="predicted_prob",ylab="sample_size")

plot(data$true_prob ~ data$predicted_prob,xlab="",ylab="")
abline(0,1)

title(xlab="Predicted win probability of tennis match (100 bins)")
title(ylab="Percentagate of tennis matches won")
title(main="Correlation between predicted and actually probabilities  
of winning a tennis match,
14768 ATP tennis matches, 2007-2011")

