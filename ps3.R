## PS 3

## clear memory
rm(list=ls())

data.df <- read.csv("~/Downloads/1607.csv",header=TRUE, sep=",")
tb3mo.ts <- ts(data=data.df[c("Tbill")], frequency=4, start=c(1960,1), end=c(2008,1))
r10.ts <- ts(data=data.df[c("r10")], frequency=4, start=c(1960,1), end=c(2008,1))
st.ts <- r10.ts-tb3mo.ts;
#a
plot(st.ts, col="blue", ylab="r10-Tbill", main="st", xlab="Year")
#b
acf(st.ts, main="ACF for st")
pacf(st.ts, main="PACF for st")
#c
ar2.st <- Arima(st.ts, order=c(2,0,0), method = "ML")
ar2.st
#d
ar2.st.res <- st.ts - fitted(ar2.st)
acf(ar2.st.res)
pacf(ar2.st.res)
Box.test(ar2.st.res,type="Ljung")
#e
ar7.st <- Arima(st.ts, order=c(7,0,0), method = "CSS-ML")
ar7.st
#f
ar7.st.res <- st.ts - fitted(ar7.st)
acf(ar7.st.res)
pacf(ar7.st.res)
#g
Box.test(ar7.st.res,type="Ljung")

#Part h
tb3mo.ts2 <- ts(data=data.df[c("Tbill")], frequency=4, start=c(1960,1), end=c(2005,3))
r10.ts2 <- ts(data=data.df[c("r10")], frequency=4, start=c(1960,1), end=c(2005,3))
st.ts2 <- r10.ts2-tb3mo.ts2;
ar2.st2 <- Arima(st.ts2, order=c(2,0,0), method = "CSS-ML")
yt.hat <- 1.3867 + ar2.st2$coef[1]*st.ts2[183] + ar2.st2$coef[2]*st.ts2[182]
e.hat <- st.ts[184]-yt.hat
ar7.st2 <- Arima(st.ts2, order=c(7,0,0), method = "CSS-ML")
ar7.st2
yt.hat2 <- ar7.st2$coef[1]*st.ts2[183] + ar7.st2$coef[2]*st.ts2[182] + ar7.st2$coef[3]*st.ts2[181] + ar7.st2$coef[4]*st.ts2[180] + ar7.st2$coef[5]*st.ts2[179] + ar7.st2$coef[6]*st.ts2[178]  + ar7.st2$coef[7]*st.ts2[177] + ar7.st2$coef[8]
e.hat2 <- st.ts[184]-yt.hat2
e.hat
e.hat2
#Part i
list <- as.numeric(st.ts2)
for (h in 1:10){
  yt.hat2[h] <- 0
  yt.hat2[h] <- 1.1789*list[183+h-1] - 0.471*list[182+h-1] + 0.392*list[181+h-1] - 0.345*list[180+h-1] +0.324*list[179+h-1] - 0.383*list[178+h-1]  + 0.152*list[177+h-1] + 1.389
  #  yt.hat2[i] <- ar7.st2$coef[1]*list[183+i-1] + ar7.st2$coef[2]*list[182+i-1] + ar7.st2$coef[3]*list[181+i-1] + ar7.st2$coef[4]*list[180+i-1] + ar7.st2$coef[5]*list[179+i-1] + ar7.st2$coef[6]*list[178+i-1]  + ar7.st2$coef[7]*list[177+i-1] + ar7.st2$coef[8]
  e.hat2[h] <- st.ts[183+h]-yt.hat2[h]
  list[183+h] <- yt.hat2[h]
}
yt.hat2
e.hat2

list <- as.numeric(st.ts2)
for (h in 1:10){
  yt.hat[h] <- 0
  yt.hat[h] <- 1.3867 + 1.0964*list[183+h-1] - 0.245*list[182+h-1]
  e.hat[h] <- st.ts[183+h]-yt.hat[h]
  list[183+h] <- yt.hat[h]
}
yt.hat
e.hat

mean(e.hat^2)
mean(e.hat2^2)