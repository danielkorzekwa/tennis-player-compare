multiskill <- read.csv("multiskill_federer_djokovic_skills_2010_2011.csv")
trueskill <- read.csv("trueskill_federer_djokovic_skills_2010_2011.csv")

plot(multiskill$federer_serve,type="l",ylim=c(0,7),col="green",xlab="",ylab="")
lines(multiskill$federer_return,type="l",lty=2,col="green")

lines(multiskill$djokovic_serve,type="l",,col="blue")
lines(multiskill$djokovic_return,type="l",lty=2,col="blue")

par(mar=c(4.2, 3.8, 0.2, 0.2))

abline(h = seq(0,10,by=1),  col = "lightgray", lty = 3)
grid()

title(xlab="Tennis matches in a time order")
title(ylab="Skills on serve and return")

legend("topright",c("Federer on serve","Federer on return",
"Djokovic on serve","Djokovic on return"),
inset=0.03, cex=1,
col=c("green","green","blue", "blue"),lty=c(1,2,1,2))

