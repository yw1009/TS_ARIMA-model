at<-read.csv(file.choose(),header = T,sep=",")
at1 <- as.vector(at)
MonthYear<-at$Month.Year
TSold<-at$Number.of.Tractor.Sold
data<-ts(TSold,start = c(2003,1),frequency = 12)
View(data)
plot(data,xlab="Years",ylab="No. of tractors sold",main="Company's Sale Over The Decade",col="red")

a <- sma(data,order=12,h=12,silent = FALSE)
seasonplot(data,12,col=rainbow(12))
a1 <- a$fitted
seasonplot(a1,12,col=rainbow(12))

plot(diff(data),main="First difference",xlab="Years",ylab="Differences")
plot(log10(data),main="Log transformed",xlab="Years",ylab="Log")
plot(diff(log10(data)),main="First difference of log transform",xlab="Years",ylab="Differences")
par(mfrow=c(1,2))
acf(ts(diff(log10(data))),main="ACF of diff log")
pacf(ts(diff(log10(data))),main="PACF of diff log")
fore <- require(forecast)
ARIMAfit = auto.arima(log10(data),approximation = FALSE,trace=FALSE)
summary(ARIMAfit)
par(mfrow=c(1,1))
pred <- predict(ARIMAfit,n.ahead = 36)

pred$pred
plot(data,type='l',xlim=c(2004,2018),ylim=c(1,1600),xlab = 'Year',ylab = 'Tractor Sales')
lines(10^(pred$pred),col='red')
lines(10^(pred$pred+2*pred$se),col='green')
lines(10^(pred$pred-2*pred$se),col='green')

par(mfrow=c(1,2))
acf(ts(ARIMAfit$residuals))
pacf(ts(ARIMAfit$residuals))
