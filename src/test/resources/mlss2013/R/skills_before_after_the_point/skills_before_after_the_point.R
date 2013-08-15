x <- seq(-30, 60, length=1000)
p1_serve_before <- dnorm(x,mean=4,sd=sqrt(81))
p1_serve_after <- dnorm(x,mean=27.1743,sd=sqrt(37.5013))

p2_serve_before <- dnorm(x,mean=41,sd=sqrt(25))
p2_serve_after <- dnorm(x,mean=33.8460,sd=sqrt(20.8610))

plot(x, p1_serve_before, type="l", ylim=c(0,0.1), col="green", xlab="",ylab="")
lines(x,p1_serve_after,col="green", lty=2)
lines(x,p2_serve_before,col="blue")
lines(x,p2_serve_after,col="blue", lty=2)

par(mar=c(4.2, 3.8, 0.2, 0.2))

abline(h = seq(0,0.1,by=0.01),  col = "lightgray", lty = 3)
grid()

title(xlab="Tennis skill")
title(ylab="pdf (skill)")

legend("topleft",c("Player 1 on serve before the point",
"Player 1 on serve after the point","Player 2 on return before the point",
"Player 2 after the point"), inset=0.03, cex=1, col=c("green","green","blue","blue"),
lty=c(1,2,1,2),bg="white")
