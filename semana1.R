## Repaso General



library(quantmod)


getSymbols("MSFT",src="yahoo",from="2020-01-01",to="2021-01-15")


plot(MSFT$MSFT.Close,main="MSFT")
hist(MSFT$MSFT.Close,col="blue",breaks = 50, main="MSFT",xlab = "Price")
d=density(MSFT$MSFT.Close)
plot(d,col="red",main="MSFT Density",xlab = "Price")

msft=Delt(MSFT$MSFT.Close)[-1]
names(msft)="msft"
hist(msft,col="blue",breaks = 50,xlab = "Returns")
d=density(msft)
plot(d,col="red",main="MSFT Density",xlab = "Returns")

c=ecdf(msft)
plot(c,col="red",main="MSFT Cumulative Density",xlab = "Returns")

media=mean(msft)
sde=sd(msft)
X = rnorm(10000) # X is a sample of 100 normally distributed random variables
P = ecdf(X)    # P is a function giving the empirical CDF of X
P(0.0)         # This returns the empirical CDF at zero (should be close to 0.5)
plot(P) 

p=density(X)
plot(p,col="red",main="Density")

smsft=rnorm(length(msft),media,sde)
dsmsft=density(smsft)
plot(d,col="red",main="MSFT Simulated  Density",xlab = "Returns")
lines(dsmsft,col="green",main="MSFT Density",xlab = "Returns")
