## PS 2

## clear memory
rm(list=ls())

## set seed
set.seed(123456)

#Question 1
sim2<- read.csv("~/Downloads/1608.csv") # use read.csv if reading a csv file
sim2.ts = ts(data=sim2$Y1)
plot(sim2.ts, col="blue", main="Simulated AR(1) Process")
acf(sim2.ts, main="ACF for Simulated AR(1) Process")
pacf(sim2.ts, main="PACF for Simulated AR(1) Process")

g.sim2.ts = ts(data=sim2.ts, start=2, end=100)
plot(g.sim2.ts,col="blue", main="Log sim2", xlab="Time")

ar1.sim2 <- Arima(sim2.ts, order=c(1,0,0), method = "CSS")
ar1.sim2.r2 <- (cor(fitted(ar1.sim2), sim2.ts)^2)*(99/98)
ar1.sim2.aic <- -2*ar1.sim2$loglik + 2*(3)
ar1.sim2.bic <- -2*ar1.sim2$loglik + 3*log(ar1.sim2$nobs)
ar2.sim2 <- Arima(g.sim2.ts, order=c(2,0,0), method = "CSS")
ar2.sim2.r2 <- cor(fitted(ar2.sim2), sim2.ts[-1])^2*(99/97)
ar2.sim2.aic <- -2*ar2.sim2$loglik + 2*(4)
ar2.sim2.bic <- -2*ar2.sim2$loglik + 4*log(ar2.sim2$nobs)
arma11.sim2 <- Arima(g.sim2.ts, order=c(1,0,1), method = "CSS")
arma11.sim2.r2 <- cor(fitted(arma11.sim2), sim2.ts[-1])^2*(99/97)
arma11.sim2.aic <- -2*arma11.sim2$loglik + 2*(4)
arma11.sim2.bic <- -2*arma11.sim2$loglik + 4*log(arma11.sim2$nobs)
arma14.sim2 <- Arima(g.sim2.ts, order=c(1,0,4), method = "CSS")
arma14.sim2.r2 <- cor(fitted(arma14.sim2), sim2.ts[-4])^2*(99/94)
arma14.sim2.aic <- -2*arma14.sim2$loglik + 2*(7)
arma14.sim2.bic <- -2*arma14.sim2$loglik + 7*log(arma14.sim2$nobs)
arma21.sim2 <- Arima(g.sim2.ts, order=c(2,0,1), method = "CSS")
arma21.sim2.r2 <- cor(fitted(arma21.sim2), sim2.ts[-1])^2*(99/96)
arma21.sim2.aic <- -2*arma21.sim2$loglik + 2*(5)
arma21.sim2.bic <- -2*arma21.sim2$loglik + 5*log(arma21.sim2$nobs)
ar1.sim2 #-.559 0.754
ar2.sim2  #-.522 0.694 0.087
arma11.sim2   #-0.549 0.805 -0.116
arma14.sim2   #-0.520 +0.919 -0.245 -0.089 -0.084 -0.09
arma21.sim2   #-0.539 - 0.031 0.617 0.762

ar2.sim2.ni <- Arima(g.sim2.ts, order=c(2,0,0), method = "CSS", include.mean = FALSE)
ar2.sim2.ni.r2 <- cor(fitted(ar2.sim2.ni), sim2.ts[-1])^2*(99/97)
ar2.sim2.ni.aic <- -2*ar2.sim2.ni$loglik + 2*(3)
ar2.sim2.ni.bic <- -2*ar2.sim2.ni$loglik + 3*log(ar2.sim2.ni$nobs)
ar2.sim2.ni #.710 .105
arma11.sim2.ni <- Arima(g.sim2.ts, order=c(1,0,1), method = "CSS", include.mean = FALSE)
arma11.sim2.ni.r2 <- cor(fitted(arma11.sim2.ni), sim2.ts[-1])^2*(99/97)
arma11.sim2.ni.aic <- -2*arma11.sim2.ni$loglik + 2*(3)
arma11.sim2.ni.bic <- -2*arma11.sim2.ni$loglik + 3*log(arma11.sim2.ni$nobs)
arma11.sim2.ni #846 147

table <- matrix(c(ar1.sim2.r2, ar1.sim2.aic, ar1.sim2.bic,ar2.sim2.r2, ar2.sim2.aic, ar2.sim2.bic,
                  arma11.sim2.r2, arma11.sim2.aic, arma11.sim2.bic,arma14.sim2.r2, arma14.sim2.aic, 
                  arma14.sim2.bic,arma21.sim2.r2,arma21.sim2.aic, arma21.sim2.bic, ar2.sim2.ni.r2,
                  ar2.sim2.ni.aic, ar2.sim2.ni.bic,arma11.sim2.ni.r2,arma11.sim2.ni.aic,arma11.sim2.ni.bic), 
                ncol=3, byrow =TRUE)
colnames(table) <- c("r2", "AIC", "BIC")
rownames(table) <- c("AR(1)", "AR(2)", "ARMA(1,1)", "ARMA(1,4)", "ARMA(2,1)", "AR(2) [mean 0]", "ARMA(1,1) [mean 0]")


table

acf(ar1.sim2$residuals)
pacf(ar1.sim2$residuals)

#Question2
quarterly<- read.csv("~/Downloads/1607.csv") # use read.csv if reading a csv file
names(quarterly)                   # lis the variables in mydata
cpinsa <- quarterly$CPINSA
#a  
cpinsa.ts = ts(data=cpinsa, frequency = 4, start=c(1960,1), end=c(2008,1))
plot(cpinsa.ts,col="blue", main="Consumer Price Index", xlab="Time")
#b
acf(cpinsa.ts, main="ACF for Consumer Price Index")
pacf(cpinsa.ts, main="PACF for Consumer Price Index")

## the code below cuts off the first observation off of one series 
## and the last off of the other - this works for a first difference, but is not elegant
#c
g.cpinsa<-log(cpinsa.ts[-1]/cpinsa.ts[-193])
g.cpinsa.ts = ts(data=g.cpinsa, frequency = 4, start=c(1960,2), end=c(2008,1))
plot(g.cpinsa.ts,col="blue", main="Consumer Price Index", xlab="Time")
#d
acf(g.cpinsa.ts, main="ACF for Growth rate of Consumer Price Index")
pacf(g.cpinsa.ts, main="PACF for Growth rate of Consumer Price Index")
#e
g.cpinsa2<-log(cpinsa.ts[5:193]/cpinsa.ts[1:189])
g.cpinsa2.ts = ts(data=g.cpinsa2, frequency = 4, start=c(1961,1), end=c(2008,1))
plot(g.cpinsa2.ts, main="Logged, Seasonally Lagged CPI")
#f
acf(g.cpinsa2.ts, main="ACF for Growth rate of Consumer Price Index")
pacf(g.cpinsa2.ts, main="PACF for Growth rate of Consumer Price Index")
#g
ar1.gcpinsa2 <-  Arima(g.cpinsa2.ts, order=c(1,0,0), method="CSS")
ar1.gcpinsa2.r2 <- cor(fitted(ar1.gcpinsa2), g.cpinsa2.ts)^2
arma25.gcpinsa2 <-  Arima(g.cpinsa2.ts, order=c(2,0,5), method="CSS")
arma25.gcpinsa2.r2 <- cor(fitted(arma25.gcpinsa2), g.cpinsa2.ts)^2
arma52.gcpinsa2 <-  Arima(g.cpinsa2.ts, order=c(5,0,2), method="CSS")
arma52.gcpinsa2.r2 <- cor(fitted(arma52.gcpinsa2), g.cpinsa2.ts)^2
ar2.gcpinsa2 <-  Arima(g.cpinsa2.ts, order=c(2,0,0), method="CSS")
ar2.gcpinsa2.r2 <- cor(fitted(ar2.gcpinsa2), g.cpinsa2.ts)^2
ar5.gcpinsa2 <-  Arima(g.cpinsa2.ts, order=c(5,0,0), method="CSS")
ar5.gcpinsa2.r2 <- cor(fitted(ar5.gcpinsa2), g.cpinsa2.ts)^2

ar1.gcpinsa2
ar1.gcpinsa2.r2
ar2.gcpinsa2
ar2.gcpinsa2.r2
arma25.gcpinsa2
arma25.gcpinsa2.r2
arma52.gcpinsa2
arma52.gcpinsa2.r2
ar5.gcpinsa2
ar5.gcpinsa2.r2
#h
table <- matrix(c(ar1.gcpinsa$aic, ar1.gcpinsa$bic, ar1.gcpinsa.r2,
                  ar2.gcpinsa$aic, ar2.gcpinsa$bic, ar2.gcpinsa.r2, 
                  ar5.gcpinsa$aic, ar5.gcpinsa$bic, ar5.gcpinsa.r2,
                  arma52.gcpinsa$aic, arma52.gcpinsa$bic, arma52.gcpinsa.r2,
                  arma25.gcpinsa$aic, arma25.gcpinsa$bic, arma25.gcpinsa.r2), 
                ncol=3, byrow =TRUE)
colnames(table) <- c("AIC", "BIC", "r2")
rownames(table) <- c("AR(1)", "AR(2)", "AR(5)", "ARMA(5,2)","ARMA(2,5)")
table
#i
dummies = seasonaldummy(g.cpinsa.ts)
y<- lm(g.cpinsa.ts ~ dummies)
y
#j
plot(ts(y$residuals), main="Seasonal Dummy Residuals")
#k
acf(y$residuals)
pacf(y$residuals)