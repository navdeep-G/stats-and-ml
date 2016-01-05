#ARIMA Methods

library(forecast)

#setwd(to/the/repo/location)

#Little function to plot acf and pacf in one plot window with
#the same y-axis for comparison purposes
acf.pacf = function(x) {
  par(mfrow=c(1,2))
  acf(x,ylim=c(-1,1))
  pacf(x,ylim=c(-1,1))
  par(mfrow=c(1,1))
}
#==============================================================================================================================

#ARIMA modelling with the tooth dataset
tooth = scan("../data/tooth.txt")
tooth.ts = ts(tooth)
plot(tooth.ts)
acf.pacf(tooth.ts)

#Previous examination of the ACF shows very slow lags. This indicates taking the first difference and checking again:

tooth.dif = diff(tooth.ts)
plot(tooth.dif)
acf.pacf(tooth.dif)

#After taking the first difference, I notice that the variability is still changing with time. This indicates I should 
#take a second difference:

tooth.dif2 = diff(tooth.dif)
plot(tooth.dif2)

acf.pacf(tooth.dif2)

#The lags are a lot better than the first difference. Also, the second difference ACF plot suggests a MA(2) model while the
#PACF plot suggests a AR(2) model.

#============================================================================================================================
#Evaluate AR(2) model:
tooth.arima.220 = arima(tooth.ts,order=c(2,2,0))
acf.pacf(tooth.arima.220$resid)
tooth.arima.220

#Testing significance of coefficients
theta.hat = tooth.arima.220$coef
se.theta.hat = sqrt(diag(tooth.arima.220$var.coef))
t.stat = abs(theta.hat/se.theta.hat)
t.stat #Only 2nd term is greater than 2 with a value of 3.3573730. The first time has a value of .5242378.

#Checking residuals
x11()
par(mfrow=c(1,2))
hist(tooth.arima.220$resid)
qqnorm(tooth.arima.220$resid)
qqline(tooth.arima.220$resid)
par(mfrow=c(1,1))

#Check normality
shapiro.test(tooth.arima.220$resid) #P-value of .0443, which suggests not normal.
#The Ljung Box test statistic. Checks adequuacy of the model. If we reject, then model does not fit well.
Box.test(tooth.arima.220$resid,type="Ljung") 
#The model is a good fit since we fail to reject with a p-value of .67. So, the residuals are independent, white noise.

#Predicting additional 5 time points for AR(2,2,0)
y.hat = predict(tooth.arima.220,n.ahead=5)$pred
se.y.hat = predict(tooth.arima.220,n.ahead=5)$se
n = length(tooth.ts)
n.p = length(theta.hat)
pred.width = 2*qt(.975,n-n.p)*se.y.hat

#Plot of predictions
x11()
plot(tooth.ts,main="ARIMA(2,2,0)",xlim = c(0,length(tooth)+length(y.hat)),
     ylim = c(min(tooth,y.hat-0.5*pred.width),max(tooth,y.hat+0.5*pred.width)))
points(length(tooth)+1:length(y.hat),y.hat,col="blue")
lines(length(tooth)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(tooth)+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(length(tooth)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(tooth)+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#Plot with modified xlim to see predictions better
x11()
plot(tooth.ts,main="ARIMA(2,2,0)",xlim = c(70,length(tooth)+length(y.hat)),
     ylim = c(min(tooth[70:length(tooth)],y.hat-0.5*pred.width),max(tooth,y.hat+0.5*pred.width)))
points(length(tooth)+1:length(y.hat),y.hat,col="blue")
lines(length(tooth)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(tooth)+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(length(tooth)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(tooth)+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")
#===========================================================================================================================
#Evaluate MA(2) model:
tooth.arima.022 = arima(tooth.ts,order=c(0,2,2))
acf.pacf(tooth.arima.022$resid)
tooth.arima.022

#Testing significance of coefficients
theta.hat = tooth.arima.022$coef
se.theta.hat = sqrt(diag(tooth.arima.022$var.coef))
t.stat = abs(theta.hat/se.theta.hat)
t.stat #Both terms are greater than 2,which suggests they're significant.

#Checking residuals
x11()
par(mfrow=c(1,2))
hist(tooth.arima.022$resid)
qqnorm(tooth.arima.022$resid)
qqline(tooth.arima.022$resid)
par(mfrow=c(1,1))

#Check normality
shapiro.test(tooth.arima.022$resid) #P-value of .3091, which suggests normal.
#The Ljung Box test statistic. Checks adequuacy of the model. If we reject, then model does not fit well.
Box.test(tooth.arima.022$resid,type="Ljung") 
#The model is a good fit since we fail to reject with a p-value of .39. So, the residuals are independent, white noise.

#Predicting additional 5 time points for AR(0,2,2)
y.hat = predict(tooth.arima.022,n.ahead=5)$pred
se.y.hat = predict(tooth.arima.022,n.ahead=5)$se
n = length(tooth.ts)
n.p = length(theta.hat)
pred.width2 = 2*qt(.975,n-n.p)*se.y.hat

#Plot of predictions
x11()
plot(tooth.ts,main="ARIMA(0,2,2)",xlim = c(0,length(tooth)+length(y.hat)),
     ylim = c(min(tooth,y.hat-0.5*pred.width),max(tooth,y.hat+0.5*pred.width)))
points(length(tooth)+1:length(y.hat),y.hat,col="blue")
lines(length(tooth)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(tooth)+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(length(tooth)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(tooth)+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#Plot with modified xlim to see predictions better
x11()
plot(tooth.ts,main="ARIMA(0,2,2)",xlim = c(70,length(tooth)+length(y.hat)),
     ylim = c(min(tooth[70:length(tooth)],y.hat-0.5*pred.width),max(tooth,y.hat+0.5*pred.width)))
points(length(tooth)+1:length(y.hat),y.hat,col="blue")
lines(length(tooth)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(tooth)+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(length(tooth)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(tooth)+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#Compare AIC for final model selection:
tooth.arima.220
tooth.arima.022
cbind(pred.width,pred.width2)
#The best model out of the two is ARIMA(0,2,2) with an AIC criterion of 433.65, which is less than the ARIMA(2,2,0) score of 
#439.65. Also, this model displays smaller prediction intervals.
#############################################################################################################################

#ARIMA modelling with the dvd dataset
dvd = scan("../data/dvd.txt")
dvd.ts = ts(dvd)
plot(dvd.ts)
acf.pacf(dvd.ts)

#Previous examination of the ACF shows very slow lags. This indicates taking the first difference and checking again:
dvd.dif = diff(dvd.ts)
plot(dvd.dif)
#After taking the first difference, I notice that the variability is relatively constant.
acf.pacf(dvd.dif)
#After looking at the acf.pacf plot, I notice it suggests a MA(6) model and a AR(5) model.
#============================================================================================================================
#Evaluate AR(5) model:
dvd.arima.510 = arima(dvd.ts,order=c(5,1,0))
acf.pacf(dvd.arima.510$resid)
dvd.arima.510

#Testing significance of coefficients
theta.hat = dvd.arima.510$coef
se.theta.hat = sqrt(diag(dvd.arima.510$var.coef))
t.stat = abs(theta.hat/se.theta.hat)
t.stat #

#Checking residuals
x11()
par(mfrow=c(1,2))
hist(dvd.arima.510$resid)
qqnorm(dvd.arima.510$resid)
qqline(dvd.arima.510$resid)
par(mfrow=c(1,1))

#Check normality
shapiro.test(dvd.arima.510$resid) #P-value of .8514, which suggests normal.
#The Ljung Box test statistic. Checks adequuacy of the model. If we reject, then model does not fit well.
Box.test(dvd.arima.510$resid,type="Ljung") 
#The model is a good fit since we fail to reject with a p-value of .58. So, the residuals are independent, white noise.

#Predicting additional 5 time points for AR(5,1,0)
y.hat = predict(dvd.arima.510,n.ahead=5)$pred
se.y.hat = predict(dvd.arima.510,n.ahead=5)$se
n = length(dvd.ts)
n.p = length(theta.hat)
pred.width = 2*qt(.975,n-n.p)*se.y.hat

#Plot of predictions
x11()
plot(dvd.ts,main="ARIMA(5,1,0)",xlim = c(0,length(dvd)+length(y.hat)),
     ylim = c(min(dvd,y.hat-0.5*pred.width),max(dvd,y.hat+0.5*pred.width)))
points(length(dvd)+1:length(y.hat),y.hat,col="blue")
lines(length(dvd)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(dvd)+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(length(dvd)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(dvd)+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#Plot with modified xlim to see predictions better
x11()
plot(dvd.ts,main="ARIMA(5,1,0)",xlim = c(70,length(dvd)+length(y.hat)),
     ylim = c(min(dvd[70:length(dvd)],y.hat-0.5*pred.width),max(dvd,y.hat+0.5*pred.width)))
points(length(dvd)+1:length(y.hat),y.hat,col="blue")
lines(length(dvd)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(dvd)+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(length(dvd)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(dvd)+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")
#===========================================================================================================================
#Evaluate MA(6) model:
dvd.arima.016 = arima(dvd.ts,order=c(0,1,6))
acf.pacf(dvd.arima.016$resid)
dvd.arima.016

#Testing significance of coefficients
theta.hat = dvd.arima.016$coef
se.theta.hat = sqrt(diag(dvd.arima.016$var.coef))
t.stat = abs(theta.hat/se.theta.hat)
t.stat #

#Checking residuals
x11()
par(mfrow=c(1,2))
hist(dvd.arima.016$resid)
qqnorm(dvd.arima.016$resid)
qqline(dvd.arima.016$resid)
par(mfrow=c(1,1))

#Check normality
shapiro.test(dvd.arima.016$resid) #P-value of .86, which suggests normal.
#The Ljung Box test statistic. Checks adequuacy of the model. If we reject, then model does not fit well.
Box.test(dvd.arima.016$resid,type="Ljung") 
#The model is a good fit since we fail to reject with a p-value of .98. So, the residuals are independent, white noise.

#Predicting additional 5 time points for AR(0,1,6)
y.hat = predict(dvd.arima.016,n.ahead=5)$pred
se.y.hat = predict(dvd.arima.016,n.ahead=5)$se
n = length(dvd.ts)
n.p = length(theta.hat)
pred.width2 = 2*qt(.975,n-n.p)*se.y.hat

#Plot of predictions
x11()
plot(dvd.ts,main="ARIMA(0,1,6)",xlim = c(0,length(dvd)+length(y.hat)),
     ylim = c(min(dvd,y.hat-0.5*pred.width),max(dvd,y.hat+0.5*pred.width)))
points(length(dvd)+1:length(y.hat),y.hat,col="blue")
lines(length(dvd)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(dvd)+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(length(dvd)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(dvd)+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#Plot with modified xlim to see predictions better
x11()
plot(dvd.ts,main="ARIMA(0,1,6)",xlim = c(70,length(dvd)+length(y.hat)),
     ylim = c(min(dvd[70:length(dvd)],y.hat-0.5*pred.width),max(dvd,y.hat+0.5*pred.width)))
points(length(dvd)+1:length(y.hat),y.hat,col="blue")
lines(length(dvd)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(dvd)+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(length(dvd)+1:length(y.hat),y.hat,lty=3,col="blue")
lines(length(dvd)+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#Compare AIC for final model selection:
dvd.arima.510
dvd.arima.016
cbind(pred.width,pred.width2)
#The best model out of the two is ARIMA(0,1,6) with an AIC criterion of 726.65, which is less than the ARIMA(5,1,0) score of 
#737.7. Also, this model displays smaller prediction intervals. 
############################################################################################################################

#Redefine acf.pacf function with additional features lag and season
acf.pacf = function(x,lag,season) {
  par(mfrow=c(1,2))
  acf(x,ylim=c(-1,1),lag.max=lag,lwd=2)
  abline(v=season,col="red",lty=3)
  abline(v=2*season,col="red",lty=3)
  abline(v=3*season,col="red",lty=3)
  abline(v=4*season,col="red",lty=3)
  pacf(x,ylim=c(-1,1),lag.max=lag,lwd=2)
  abline(v=season,col="red",lty=3)
  abline(v=2*season,col="red",lty=3)
  abline(v=3*season,col="red",lty=3)
  abline(v=4*season,col="red",lty=3)
  par(mfrow=c(1,1))
}

#ARIMA modelling with the hotel dataset
hotel = scan("../data/hotel.txt")
hotel.ts = ts(hotel)
plot(hotel.ts)

#Plotting first difference to check for changing variability
plot(diff(hotel.ts,1))

#Stabilizing the variance since previous plot showed changing variance over time.
log.hotel = log(hotel.ts)
plot(diff(log.hotel,1))

acf.pacf(log.hotel,48,12)

#Take the first difference and first seasonal difference
arima.010.010 = arima(log.hotel,order = c(0, 1, 0),
                      seasonal = list(order = c(0, 1, 0), period = 12))
acf.pacf(arima.010.010$resid,48,12)

#The previous acf.pacf plot suggests an MA(1) seasonal model:

#MA(1) seasonal model
arima.010.011 = arima(log.hotel,order = c(0, 1, 0),
                      seasonal = list(order = c(0, 1, 1), period = 12))
acf.pacf(arima.010.011$resid,48,12)

#This seems to take care of the seasonal autocorrelation.

#Previous suggests adding a non seasonal MA(3) nonseasonal term or a AR(5) term:

#MA(3) nonseasonal term:
arima.013.011 = arima(log.hotel,order = c(0, 1, 3),
                      seasonal = list(order = c(0, 1, 1), period = 12))
acf.pacf(arima.013.011$resid,48,12)

#Testing significance of coefficients
theta.hat = arima.013.011$coef
se.theta.hat = sqrt(diag(arima.013.011$var.coef))
t.stat = abs(theta.hat/se.theta.hat)
t.stat #All terms are significant except second term. 

#predicting 12 months for arima(5,1,0)x(1,1,0)x12
y.hat = predict(arima.013.011,n.ahead=12)$pred
se.y.hat = predict(arima.013.011,n.ahead=12)$se
n = length(log.hotel)
n.p = length(theta.hat)
pred.width = 2*qt(.975,n-n.p)*se.y.hat

#Saving the width for comparison later
pred.width.fit1 = pred.width

#Plot predictions
x11()
plot(log.hotel,main="ARIMA(0,1,3)x(0,1,1)x12",xlim = c(0,n+length(y.hat)),
     ylim = c(min(log.hotel,y.hat-0.5*pred.width),max(log.hotel,y.hat+0.5*pred.width)))
points(n+1:length(y.hat),y.hat,col="blue")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#Plot predictions
x11()
plot(log.hotel,main="ARIMA(0,1,3)x(0,1,1)x12",xlim = c(120,n+length(y.hat)),
     ylim = c(min(log.hotel[120:n],y.hat-0.5*pred.width),max(log.hotel,y.hat+0.5*pred.width)))
points(n+1:length(y.hat),y.hat,col="blue")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")
#===========================================================================================================================
#Recall, the original arima.010.011 also hinted at a AR(5) nonseasonal term:
arima.010.011 = arima(log.hotel,order = c(0, 1, 0),
                      seasonal = list(order = c(0, 1, 1), period = 12))
acf.pacf(arima.010.011$resid,48,12)

#AR(5) nonseasonal term:
arima.510.011 = arima(log.hotel,order = c(5, 1, 0),
                      seasonal = list(order = c(0, 1, 1), period = 12))
acf.pacf(arima.510.011$resid,48,12)

#Testing significance of coefficients
theta.hat = arima.510.011$coef
se.theta.hat = sqrt(diag(arima.510.011$var.coef))
t.stat = abs(theta.hat/se.theta.hat)
t.stat #All are significant

#predicting 12 months for arima(5,1,0)x(1,1,0)x12
y.hat = predict(arima.510.011,n.ahead=12)$pred
se.y.hat = predict(arima.510.011,n.ahead=12)$se
n = length(log.hotel)
n.p = length(theta.hat)
pred.width = 2*qt(.975,n-n.p)*se.y.hat

#Saving the width for comparison later
pred.width.fit2 = pred.width

#Plot predictions
x11()
plot(log.hotel,main="ARIMA(5,1,0)x(0,1,0)x12",xlim = c(0,n+length(y.hat)),
     ylim = c(min(log.hotel,y.hat-0.5*pred.width),max(log.hotel,y.hat+0.5*pred.width)))
points(n+1:length(y.hat),y.hat,col="blue")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#Plot predictions
x11()
plot(log.hotel,main="ARIMA(5,1,0)x(0,1,1)x12",xlim = c(120,n+length(y.hat)),
     ylim = c(min(log.hotel[120:n],y.hat-0.5*pred.width),max(log.hotel,y.hat+0.5*pred.width)))
points(n+1:length(y.hat),y.hat,col="blue")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#Check AIC criterion for final model:
arima.013.011
arima.510.011
#Check prediction widths between models:
cbind(pred.width.fit1,pred.width.fit2)
#After analyzing both models, I will select hotel.arima.013.011 as the final model since the AlC is smaller than the 
#second model. Also, the prediction widths for the first model are smaller, which provided more evidence for it being better
#than the second model. 
############################################################################################################################

#ARIMA modelling with the johnson and johnson dataset
jj = scan("../data/jj.txt")
jj.ts = ts(jj)
plot(jj.ts)

#Plotting first difference to check for changing variability
plot(diff(jj.ts,1))

#There is an obvious change in variance over time and a transformation is needed. I will apply a log transformation:
log.jj = log(jj.ts) 
plot(diff(log.jj,1))

#This seems to take care of changing variance over time

#Using the log transformation:
acf.pacf(log.jj,20,4)

#Take first difference
arima.010.000 = arima(log.jj,order = c(0, 1, 0),
                      seasonal = list(order = c(0, 0, 0), period = 4))
acf.pacf(arima.010.000$resid,20,4)

#Take the first difference and first seasonal difference
arima.010.010 = arima(log.jj,order = c(0, 1, 0),
                      seasonal = list(order = c(0, 1, 0), period = 4))
acf.pacf(arima.010.010$resid,20,4)

#The previous plot seems to suggest taking a AR(1) seasonal model:
arima.010.110 = arima(log.jj,order = c(0, 1, 0),
                      seasonal = list(order = c(1, 1, 0), period = 4))
acf.pacf(arima.010.110$resid,20,4)

#This seems to take care of the seasonal term. Also, the previous acf.pacf plot suggests a MA(1) nonseasonal term and
# a AR(1) nonseasonal term. 

#MA(1) nonseasonal term:
arima.011.110 = arima(log.jj,order = c(0, 1, 1),
                      seasonal = list(order = c(1, 1, 0), period = 4))
acf.pacf(arima.011.110$resid,20,4)

#Testing significance of coefficients
theta.hat = arima.011.110$coef
se.theta.hat = sqrt(diag(arima.011.110$var.coef))
t.stat = abs(theta.hat/se.theta.hat)
t.stat #All terms are significant except second term. 

#predicting 12 months for arima(5,1,0)x(1,1,0)x12
y.hat = predict(arima.011.110,n.ahead=4)$pred
se.y.hat = predict(arima.011.110,n.ahead=4)$se
n = length(log.jj)
n.p = length(theta.hat)
pred.width = 2*qt(.975,n-n.p)*se.y.hat

#Saving the width for comparison later
pred.width.fit1 = pred.width

#Plot predictions
x11()
plot(log.jj,main="ARIMA(0,1,1)x(1,1,0)x4",xlim = c(0,n+length(y.hat)),
     ylim = c(min(log.jj,y.hat-0.5*pred.width),max(log.jj,y.hat+0.5*pred.width)))
points(n+1:length(y.hat),y.hat,col="blue")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#Plot predictions
x11()
plot(log.jj,main="ARIMA(0,1,1)x(1,1,0)x4",xlim = c(60,n+length(y.hat)),
     ylim = c(min(log.jj[60:n],y.hat-0.5*pred.width),max(log.jj,y.hat+0.5*pred.width)))
points(n+1:length(y.hat),y.hat,col="blue")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")
#===========================================================================================================================
#AR(1) nonseasonal term:
arima.110.110 = arima(log.jj,order = c(1, 1, 0),
                      seasonal = list(order = c(1, 1, 0), period = 4))
acf.pacf(arima.110.110$resid,20,4)

#Testing significance of coefficients
theta.hat = arima.110.110$coef
se.theta.hat = sqrt(diag(arima.110.110$var.coef))
t.stat = abs(theta.hat/se.theta.hat)
t.stat #All terms are significant except second term. 

#predicting 12 months for arima(5,1,0)x(1,1,0)x12
y.hat = predict(arima.110.110,n.ahead=4)$pred
se.y.hat = predict(arima.110.110,n.ahead=4)$se
n = length(log.jj)
n.p = length(theta.hat)
pred.width = 2*qt(.975,n-n.p)*se.y.hat

#Saving the width for comparison later
pred.width.fit2 = pred.width

#Plot predictions
x11()
plot(log.jj,main="ARIMA(1,1,0)x(1,1,0)x4",xlim = c(0,n+length(y.hat)),
     ylim = c(min(log.jj,y.hat-0.5*pred.width),max(log.jj,y.hat+0.5*pred.width)))
points(n+1:length(y.hat),y.hat,col="blue")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#Plot predictions
x11()
plot(log.jj,main="ARIMA(1,1,0)x(1,1,0)x4",xlim = c(60,n+length(y.hat)),
     ylim = c(min(log.jj[60:n],y.hat-0.5*pred.width),max(log.jj,y.hat+0.5*pred.width)))
points(n+1:length(y.hat),y.hat,col="blue")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#Check AIC criterion for final model:
arima.011.110
arima.110.110
#Check prediction widths between models:
cbind(pred.width.fit1,pred.width.fit2)
#After analyzing both models, I will select arima.011.110 as the final model since the AlC is smaller than the 
#second model. Also, the prediction widths for the first model are smaller, which provided more evidence for it being better
#than the second model. 
