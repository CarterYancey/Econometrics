#PS5
#Q1
data.df <- read.csv("~/Downloads/1607.csv",header=TRUE, sep=",")
indpro.ts <- ts(data=data.df[c("indpro")], frequency=4, start=c(1960,1), end=c(2008,1))
m1.ts <- ts(data=data.df[c("M1NSA")], frequency=4, start=c(1960,1), end=c(2008,1))
urate.ts <- ts(data=data.df[c("urate")], frequency=4, start=c(1960,1), end=c(2008,1))
#a
plot(indpro.ts, col="blue", main="indpro.ts", xlab="Year")
plot(m1.ts, col="blue", main="m1.ts", xlab="Year")
plot(urate.ts, col="blue", main="urate.ts", xlab="Year")
#b
adf.test(indpro.ts)
#c
adf.test(m1.ts)
#d
y <- lm(indpro.ts ~ m1.ts)
y
#e
res <- (indpro.ts-fitted(y))
acf(res)
pacf(res)
#f
adf.test(res)
#g
plot.zoo(indpro.ts, m1.ts)


#Q2
data.df <- read.csv("~/Downloads/1604.csv",header=TRUE, sep=",")
pUS.ts <- ts(data=data.df[c("p_us")], frequency=4, start=c(1973,1), end=c(2008,2))
pCA.ts <- ts(data=data.df[c("p_ca")], frequency=4, start=c(1973,1), end=c(2008,2))
exCA.ts <- ts(data=data.df[c("ex_ca")], frequency=4, start=c(1973,1), end=c(2008,2))
#a
plot(log(pUS.ts), col="blue", main="pUS", xlab="Year")
plot(log(pCA.ts), col="blue", main="pCA", xlab="Year")
plot(log(exCA.ts), col="blue", main="exCA", xlab="Year")
#b
adf.test(log(exCA.ts))
adf.test(log(pCA.ts),k=0)
adf.test(log(pUS.ts),k=1)
#c
y <- lm(log(exCA.ts) ~ log(pUS.ts) + log(pCA.ts))
y
#d
#e
res <- (log(exCA.ts) - fitted(y))
acf(res)
pacf(res)