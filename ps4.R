#PS4
data.df <- read.csv("~/Downloads/1605.csv",header=TRUE, sep=",")
gdp.ts <- ts(data=data.df[c("GDP")], frequency=4, start=c(1960,1), end=c(2002,1))
#a
plot(gdp.ts, col="blue", ylab="$", main="GDP", xlab="Year")
#b
t<-1
for (i in 2:169)
  t[i]<-i
partb <- lm(gdp.ts ~ t + I(t^2) + I(t^3))
partb
#c
y <- fitted(partb)
acf(gdp.ts-y)
pacf(gdp.ts-y)
#d
adf.test(gdp.ts)
#e
dlrgdp <- 0
for (i in 2:169)
  dlrgdp[i] <- log(gdp.ts[i]/gdp.ts[i-1])
dlrgdp.ts <- ts(dlrgdp, frequency=4, start=c(1960,1), end=c(2002,1))
plot(dlrgdp.ts, col="blue", ylab="$", main="dlrgdp", xlab="Year")
#f
dlrgdpAR2 <- Arima(dlrgdp.ts, order=c(2,0,0), method = "CSS")
dlrgdpAR2
#g
y<- fitted(dlrgdpAR2)
res <- gdp.ts-y
acf(res)
pacf(res)
#h
adf.test(dlrgdp.ts, k =2)
#i
total <- lm(dlrgdp.ts ~ t)
half1 <- lm(dlrgdp.ts[1:52] ~ ts(t[1:52]))
half2 <- lm(dlrgdp.ts[52:169] ~ ts(t[52:169]))
RSSt <- sum((dlrgdp.ts-fitted(total))^2)
RSS1 <- sum((dlrgdp.ts[1:52]-fitted(half1))^2)
RSS2 <- sum((dlrgdp.ts[52:169]-fitted(half2))^2)
F <- 165*(RSSt-(RSS1+RSS2))/((RSS1+RSS2)*2)
F > qf(.36, 2, 165)
F#j
dummy<-0
for (i in 2:52)
  dummy[i]<-0
for (i in 53:169)
  dummy[i]<-1
partj <- Arima(dlrgdp.ts, xreg=dummy, order=c(2,0,0), method = "CSS")
partj
#k
acf(gdp.ts-fitted(partj))
pacf(gdp.ts-fitted(partj))