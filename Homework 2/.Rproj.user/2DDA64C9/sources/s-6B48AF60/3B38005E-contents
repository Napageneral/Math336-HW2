mydata <- read.table("global.txt")
summary(mydata)
boxplot(mydata)

year = mydata[,1]
Annual_Anomaly = mydata[,14]
hist(Annual_Anomaly)
YearVsAnnual_Anomaly = mydata[,c(1,14)]
plot(YearVsAnnual_Anomaly,type="o", xlab = "Year", ylab = "Annual Temperature Anomaly")

#Linear Regressions in order (i),(ii),(iii),(iv),(v)
ablinepiece(lm(Annual_Anomaly ~ year), lwd=2.0, col = "yellow")
legend(1850,.8, col=c("yellow"), lty = 1,lwd=2.0, legend=c("1850-2014"), bty="n", text.font=2, cex=1)
abline(lm(Annual_Anomaly[1:61] ~ year[1:61]),lwd=2.0, col = "blue")
legend(1850,.7, col=c("blue"), lty = 1,lwd=2.0, legend=c("1850-1910"), bty="n", text.font=2, cex=1)
abline(lm(Annual_Anomaly[1:101] ~ year[1:101]), lwd=2.0, col = "green")
legend(1850,.6, col=c("green"), lty = 1,lwd=2.0, legend=c("1850-1950"), bty="n", text.font=2, cex=1)
abline(lm(Annual_Anomaly[1:126] ~ year[1:126]), lwd=2.0, col = "red")
legend(1850,.5, col=c("red"), lty = 1,lwd=2.0, legend=c("1850-1975"), bty="n", text.font=2, cex=1)
abline(lm(Annual_Anomaly[1:151] ~ year[1:151]),lwd=2.0, col = "orange")
legend(1850,.4, col=c("orange"), lty = 1,lwd=2.0, legend=c("1850-2000"), bty="n", text.font=2, cex=1)


#Table of linear regression slope intercepts in order
slope_int= matrix(c(-9.5489, 0.0048, 2.8800, -0.0017, -5.1043 , 0.0025, -5.6973, 0.0028, -7.5377, 0.0038), ncol = 2, byrow=TRUE)
colnames(slope_int) = c("intercept","slope")
rownames(slope_int) = c("1850-2014","1850-1910","1850-1950","1850-1975","1850-2000")
slope_int = as.table(slope_int)
slope_int
