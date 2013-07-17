skills <- read.csv("federer_multiskill_trueskill_2006_2007.csv")

plot(skills$multiskill_serve,type="l",ylim=c(0,8),col="green",xlab="",ylab="")
lines(skills$multiskill_return,type="l",lty=2,col="green")
lines(skills$trueskill,type="l",,col="blue",xlab="",ylab="")

par(mar=c(4.2, 3.8, 0.2, 0.2))

abline(h = seq(0,10,by=1),  col = "lightgray", lty = 3)
grid()

title(xlab="Tennis matches in time order")
title(ylab="Skill value")

legend("topleft",c("MultiSkill on serve","MultiSkill on return",
"TrueSkill"), inset=0.03, cex=1, col=c("green","green","blue"),lty=c(1,2,1),
bg="white")
