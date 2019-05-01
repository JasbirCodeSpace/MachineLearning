X = seq(from=-100,to=100,by=1)
Y1 = X^3
Y2 = -X^3
Y3 = (2*X-1)^3
Y4 = 2*X^3-1

par(mfrow=c(2,2))
plot(X,Y1,xlab = "X",ylab = "Y = X^3",main = "a")
plot(X,Y2,xlab = "X",ylab = "Y2 = -X^3",main = "b")
plot(X,Y3,xlab = "X",ylab = "Y3 = (2*X-1)^3",main = "c")
plot(X,Y4,xlab = "X",ylab = "Y4 = 2*X^3-1",main = "d")
