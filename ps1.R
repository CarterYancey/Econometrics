 #Load the data and create variables
 data.df <- read.csv("~/Downloads/1606.csv",header=TRUE, sep="\t")
 tb3mo.ts <- ts(data=data.df[c("TB3mo")], frequency=4, start=c(1959,3), end=c(2001,1))
 tb1yr.ts <- ts(data=data.df[c("TB1yr")], frequency=4, start=c(1959,3), end=c(2001,1))

 #Individual Plots
 plot(tb3mo.ts, col="blue", ylab="Bill Rate", main="3 Month Bill", xlab="Year")
 plot(tb1yr.ts, col="blue", ylab="Bill Rate", main="1 Year Bill", xlab="Year")

 # Aggregate Plot
 plot(tb1yr.ts, col="blue", ann=FALSE, ylim=range(2,16))
 lines(tb3mo.ts, col="green")
 box()
 title(main="Aggregate")
 title(xlab="Year")
 title(ylab="Bill Rate")
 legend("topleft", legend=c("3Mo Bill", "1Yr Bill"), lty=c(1,1),col=c("green","blue"))

 #OLS
 ols <- lm(tb1yr.ts ~ tb3mo.ts, data=data.df)
 ols
 y <- fitted(ols)

 residuals.ts <- tb1yr.ts - 0.9167*tb3mo.ts - 0.6982
 u2 <- (residuals.ts)^2
 Ru2<- summary(lm(u2 ~ y + y^2))$r.squared
 LM <- nrow(data.df)*Ru2
 p.value <- 1-pchisq(LM, 2)
 p.value

 par(mar = c(5, 5, 3, 5))
 plot(residuals.ts, col="red", ylab="Residuals")
 mtext("3 Month Bill", side = 4, line=3)
 par(new=TRUE)
 plot(tb3mo.ts, col="green", xaxt="n", yaxt="n", ylab="", xlab="")
 axis(side=4)
 box()
 title(main="Residuals vs tb3mo")
 legend("topleft", c("Residuals", "tb3mo"), col = c("red", "green"), lty=c(1,1))

 #OLS w/ Dummy Var
 dummy <- tb3mo.ts
 for (i in 1:nrow(dummy)){
	   for (j in 1:ncol(dummy)){
		       if (dummy[i,j] > 10) dummy[i] <- 1
     else dummy[i,j] <- 0
       }
 }
 ols2 <- lm(tb1yr.ts ~ tb3mo.ts + dummy, data=data.df)
 ols2
 y2 <- fitted(ols2)


 #Compare ols with ols2
 ols.r2 <- summary(ols)$r.squared
 ols2.r2 <- summary(ols2)$r.squared
 #ols.X2 <- chisq.test(y, tb1yr.ts)
 #ols2.X2 <- chisq.test(y2, tb1yr.ts)
 ols.aic <- AIC(ols)
 ols2.aic <- AIC(ols2)
 table <- matrix(c(ols.r2, ols2.r2, ols2.r2-ols.r2,ols.aic, ols2.aic, ols2.aic-ols.aic), ncol=3, byrow =TRUE)
 colnames(table) <- c("ols", "ols2", "Difference")
 rownames(table) <- c("r-squared", "AIC")
 table <- as.table(table)
 table
