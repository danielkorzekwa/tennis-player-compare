multiskill <- read.csv("multiskill_federer_djokovic_2010_2011.csv")
trueskill <- read.csv("trueskill_federer_djokovic_2010_2011.csv")

plot(multiskill $t,type="l",ylim=c(0,1),col="green",xlab="",ylab="")
lines(trueskill $t,type="l", col="blue")
points(multiskill $e,pch=15, col="red")

par(mar=c(4.2, 3.8, 0.2, 0.2))

abline(h = seq(0,1,by=0.1),  col = "lightgray", lty = 3)
grid(,NA)

title(xlab="Tennis matches in time order")
title(ylab="Win probability for Roger Federer")

legend("topright",c("MultiSkill","TrueSkill","Betfair Exchange, direct matches"),
inset=0.03, cex=1,col=c("green","blue","red"),pch=c(-1,-1,015), lty=c(1,1,0))


