#Time Series Regression

#Below are 4 applications of time series regression

#setwd(to/the/repo/location)

#Quarterly Sales of the TRK-50 Mountain Bike.
y = c(10, 31, 43, 16, 11, 33, 45, 17, 13, 34, 48, 19, 15, 37, 51, 21)

#Convert sample data to time series object
y.ts = ts(y)

#Get a vector of time at which the time series was sampled 
t = time(y.ts)

#Create quarterly variables
q = factor(rep(1:4,4))

#Build time series at the quarterly level
y.ts.1 = ts(y, freq = 4)

#Fit a regression model to time series data
fit = lm(y.ts~t+q, data = y.ts)

#Get a summary of the fit
summary(fit) #Note all terms are significant.

#Plot time series and look at residuals
plot(y.ts)
plot(fit$residuals)
plot(fit)

#Checking for autocorrelation
acf(fit$resid)
durbinWatsonTest(fit$resid)

#Test for normality
shapiro.test(fit$residuals) 
#Seems to be normally distributed. 
#We can take a look at plots below
qqnorm(fit$residuals)
qqline(fit$residuals)

#Four quarter aadditional forecast
new.data = data.frame(t = 17:20, q = as.factor(1:4))
y.pred = predict(fit, new.data, interval="prediction")
plot(y.ts,xlim=c(0,20),ylim = c(1,60),ylab = "Sales", main="Quarterly Sales of the TRK-50 Mountain Bike")
points(17:20,y.pred[,1],col="red")
points(17:20,y.pred[,2],col="blue")
points(17:20,y.pred[,3],col="blue")

#============================================================================================================================================
#Some transformations to data

y = scan("../data/hotel.txt", skip = 1)
y.ts = ts(y)

#Plot time series
plot(y.ts) #Seems to show variability with time. Lets take a look at some transformations.

#Comparing transformations effects on variability
par(mfrow = c(2,2))
plot(diff(y.ts), xlim = c(0,160), ylab = "Y", main = "Raw data")
plot(diff((y.ts)^.5), xlim = c(0,160), ylab = "Y", main = "Square Root ")
plot(diff((y.ts)^.25), xlim = c(0,160), ylab = "Y", main = "Fourth Root ")
plot(diff(log(y.ts)), xlim = c(0,160), ylab = "Y", main = "Natural Log ")
par(mfrow = c(1,1))

#Setting up time and month parameters
t = time(y.ts)
month=as.factor(rep(1:12,14))

#Fitting non-transformed time series
fit = lm(y.ts~t + month,data=y.ts)
summary(fit)

#Fitting square root transformation
fit.5 = lm(((y.ts)^.5)~t + month,data=y.ts)
summary(fit.5)

#Fitting fourth root transformation
fit.25 = lm(((y.ts)^.25)~t + month,data=y.ts)
summary(fit.25)

#Fitting natural log transformation
fit.ln = lm(log(y.ts)~t + month,data=y.ts)
summary(fit.ln)

par(mfrow = c(2,2))
#Forecasting 12 months for non-transformed data
new.data=data.frame(t=169:180,month=as.factor(1:12))
y.pred = predict(fit, new.data, interval="prediction")
plot(y.ts,xlim=c(0,180),ylim = c(300,1200), ylab = "Y", main = "Non-Transformed Prediction")
points(169:180,y.pred[,1],col="red")
points(169:180,y.pred[,2],col="blue")
points(169:180,y.pred[,3],col="blue")
lines(169:180,y.pred[,1],col="red")
lines(169:180,y.pred[,2],col="blue")
lines(169:180,y.pred[,3],col="blue")

#Forecasting 12 months for square root transformation
new.data.5=data.frame(t=169:180,month=as.factor(1:12))
y.pred.5 = predict(fit.5, new.data.5, interval="prediction")
plot(((y.ts)^.5),xlim=c(0,180),ylim = c(20,35), ylab = "Y", main = "Square Root Transformed Prediction")
points(169:180,y.pred.5[,1],col="red")
points(169:180,y.pred.5[,2],col="blue")
points(169:180,y.pred.5[,3],col="blue")
lines(169:180,y.pred.5[,1],col="red")
lines(169:180,y.pred.5[,2],col="blue")
lines(169:180,y.pred.5[,3],col="blue")

#Forecasting 12 months for fourth root transformation
new.data.25=data.frame(t=169:180,month=as.factor(1:12))
y.pred.25 = predict(fit.25, new.data.25, interval="prediction")
plot(((y.ts)^.25),xlim=c(0,180),ylim = c(4.5,6), ylab = "Y", main = "Fourth Root Transformed Prediction")
points(169:180,y.pred.25[,1],col="red")
points(169:180,y.pred.25[,2],col="blue")
points(169:180,y.pred.25[,3],col="blue")
lines(169:180,y.pred.25[,1],col="red")
lines(169:180,y.pred.25[,2],col="blue")
lines(169:180,y.pred.25[,3],col="blue")

#Forecasting 12 months for natural log transformation
new.data.ln=data.frame(t=169:180,month=as.factor(1:12))
y.pred.ln = predict(fit.ln, new.data.ln, interval="prediction")
plot(log(y.ts),xlim=c(0,180),ylim = c(6,7.2), ylab = "Y", main = "Natural Log Transformed Prediction")
points(169:180,y.pred.ln[,1],col="red")
points(169:180,y.pred.ln[,2],col="blue")
points(169:180,y.pred.ln[,3],col="blue")
lines(169:180,y.pred.ln[,1],col="red")
lines(169:180,y.pred.ln[,2],col="blue")
lines(169:180,y.pred.ln[,3],col="blue")
par(mfrow = c(1,1))

#Comparing prediction interval widths between transformations
cbind(y.pred[,3] - y.pred[,2], y.pred.5[,3] - y.pred.5[,2], y.pred.25[,3] - y.pred.25[,2], y.pred.ln[,3] - y.pred.ln[,2])
#Natural log transformation results in the smallest prediction interval

#============================================================================================================================================

#Johnson & Johnson Data
jj = scan("../data/jj.txt")
jj.ts = ts(jj)
t = time(jj.ts)

#Plot of origianl and diff show's increasing variability
plot(diff(jj.ts), type = "l", xlab = "Quarter", ylab = "")
plot(jj.ts,type = "l", xlab="Quarter",ylab="")

#Address by taking the log of the data, resulting graph shows greatly equalizes variance
plot(diff(log(jj.ts)), ylim = c(-1,1), type = "l", xlab = "Quarter", ylab = "")

fit = lm(jj.ts~t, data = jj.ts)
summary(fit)
fit.1 = lm(log(jj.ts)~t, data = jj.ts)
summary(fit.1)
#Evaluating model assumptions
plot(fit.1$residuals)
#Checking for autocorrelation
acf(fit.1$resid)
#Homoscedasticity 
durbinWatsonTest(fit.1$resid)
#Test for normality
shapiro.test(fit.1$residuals)
qqnorm(fit.1$residuals)
qqline(fit.1$residuals)
#Assumptions met
new.data = data.frame(t = 85:88)
y.pred = predict(fit.1, new.data, interval="prediction")
plot(log(jj.ts),xlim = c(0,100), ylim = c(-1,4))
points(85:88,y.pred[,1],col="red")
points(85:88,y.pred[,2],col="blue")
points(85:88,y.pred[,3],col="blue")

#============================================================================================================================================

#global temperature data
tmp = scan("../data/globtemp.txt")
years = seq(1856,1997)
tmp = tmp[years >= 1900] #only looking at 1900-1997
years = seq(1900,1997)
tmp.ts=ts(tmp)
t=time(tmp.ts)
plot(tmp.ts)
fit=lm(tmp.ts~t,data=tmp.ts)
summary(fit)
shapiro.test(fit$residuals)
new.data = data.frame(t = 99:103)
y.pred = predict(fit, new.data, interval="prediction")
plot(tmp.ts,xlim = c(0,100), ylim = c(-1,1))
points(99:103,y.pred[,1],col="red")
points(99:103,y.pred[,2],col="blue")
points(99:103,y.pred[,3],col="blue")

