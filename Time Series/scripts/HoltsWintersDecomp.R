#More on Additive and Mutliplicative Decomposition and some Holts Winters

library(lmtest)
library(car)

#Time series plot of tiger dataset. Checked log,square root, and raw data
tiger = scan("../data/tiger.txt")
tiger.ts = ts(tiger, frequency=4)
plot(tiger.ts)
plot(diff(tiger.ts))
tiger.log = log(tiger.ts)
plot(diff(tiger.log))
plot(tiger.log)
tiger.square = sqrt(tiger.ts)
plot(diff(tiger.square))
plot(tiger.square)

#Comments:
#After reviewing the initial time series, an increasing seasonal variation is present. 
#This would suggest that a multiplicative model is most adequate as it pertains to the decomposition method available. 
#Also, a log transformation was conducted on the data to stabilize and will be used for the linear model. Note, the log
#tranformation seemed to be the best stabilizer in comparison to square root based on the difference plots previously
#shown. 

#Multiplicative Decomposition
tiger.decomp = decompose(tiger.ts, type="multiplicative")
plot(tiger.decomp)
#Comments:
#The mulitplicative model seems to show linearity as it pertains to trend.
d = tiger.ts/tiger.decomp$seasonal
plot(d)
t = time(d)
tr.lm = lm(d~t)
summary(tr.lm)

#Predictions for multiplicative decomposition

new.data=data.frame(t=9+(0:3)/4)
t.pred = predict(tr.lm,new.data,interval="prediction")
t.pred
tiger.pred=t.pred[,1]*tiger.decomp$figure
tiger.pred
B2=0.5*(t.pred[,3]-t.pred[,2])
B2

#Plot of Multiplicative decomposition prediction
plot(tiger.ts, xlim = c(8,10), ylim=c(80,250))
lines(9+(0:3)/4, tiger.pred,lty=3)
points(9+(0:3)/4,tiger.pred, col="red")
points(9+(0:3)/4, tiger.pred+B2, col="blue")
points(9+(0:3)/4, tiger.pred-B2, col="blue")
lines(9+(0:3)/4,tiger.pred, col="red")
lines(9+(0:3)/4, tiger.pred+B2, col="blue")
lines(9+(0:3)/4, tiger.pred-B2, col="blue")

#Trend and Seasonal components:
tr.lm$coeff
tiger.decomp$figure

#Residual Analysis:

#extracting the residuals
trend.estimate = predict(tr.lm,t)
seasonal.estimate = tiger.decomp$seasonal
fitted = trend.estimate*seasonal.estimate
residuals = as.numeric(tiger.ts - fitted)

#Evaluate residuals
plot(residuals)
#Checking for autocorrelation
acf(residuals)
durbinWatsonTest(residuals)
#Test for normality
shapiro.test(residuals)
qqnorm(residuals)
qqline(residuals)

#Linear Model with trend and seasonal estimates

t = seq(1:32)

quarter=rep(1:4,8)
model = lm(tiger.log ~ t+relevel(as.factor(quarter),ref=1))
summary(model)
model$coeff

#Forecasting Model

new.quarter = as.factor(quarter)
forecasting.data = data.frame (t = c(33:36), quarter=1:4)
forecast = predict(model,forecasting.data, interval ="prediction")
forecast

e=2.718282
actual.fit=e^(forecast[,1])
actual.lwr=e^(forecast[,2])
actual.upr=e^(forecast[,3])
linear.width = 1/2*(actual.upr-actual.lwr)
actual.pred=cbind(actual.fit,actual.lwr,actual.upr)
actual.pred

cbind(B2, linear.width)


#Plot of linear model predictions
plot(tiger.ts,main="tiger drink t.s. prediction 1 year",xlab="years", ylab="sales", type="b",xlim=c(8,10),ylim=c(80,250))
points(9+(0:3)/4,actual.pred[,1],col="red")
points(9+(0:3)/4,actual.pred[,2],col="blue")
points(9+(0:3)/4,actual.pred[,3],col="blue")
lines(9+(0:3)/4,actual.pred[,1],col="red")
lines(9+(0:3)/4,actual.pred[,2],col="blue")
lines(9+(0:3)/4,actual.pred[,3],col="blue")

#Residual Analysis:

#checking for autocorrelation
acf(model$resid)
#durbin Watson test statistic. Seems to show positive autocorrelation
durbinWatsonTest(model$resid)
#Test for normality. Residuals are normal.
shapiro.test(model$resid)
qqnorm(model$resid)
qqline(model$resid)

#Compare interval lenghts:
#Linear:
actual.pred[,3]-actual.pred[,2]
#Multiplicative:
t.pred[,3]-t.pred[,2]

#Explanation and final model choice:
#After comparing the two models, and checking for prediction widths, 
#I can conclude that the multiplicative model is best for this data set 
#since it appears to show the smallest prediction width (an average 
#of 8 for each quarter) in comparison to the linear model, which would 
#suggest it possesses a set of more accurate predictions. 
#===========================================================================================================================

#Time series plot of sport dataset. Checked log,square root, and raw data
sport = scan("../data/sporting.txt")
sport.ts = ts(sport,frequency=12)
plot(diff(sport.ts))
plot(sport.ts)
sport.log = log(sport.ts)
plot(diff(sport.log))
plot(sport.log)
sport.square = sqrt(sport.ts)
plot(diff(sport.square))
plot(sport.square)

#Comments:
#After looking at the original time series plot for the sports data, 
#the multiplicative model seems most adequate in terms of decomposition. 
#Also, a log transformation was conducted to stabilize the data and will be
#use for the linear model later. Note, once again, a log transformation was 
#shown to be the best stabilizer of the data based on the previous difference 
#plots. 

#Multiplicative decompostion
sport.decomp = decompose(sport.ts, type="multiplicative")
plot(sport.decomp)
#Comments:
#Shows a linear trend after decomposition

d = sport.ts/sport.decomp$seasonal
plot(d)
t = time(d)
tr.lm2 = lm(d~t)
summary(tr.lm2)

#Prediction for multiplicative decomposition

new.data=data.frame(t=12+(0:11)/12)
t.pred = predict(tr.lm2,new.data,interval="prediction")
t.pred
sport.pred=t.pred[,1]*sport.decomp$figure
sport.pred
B3=0.5*(t.pred[,3]-t.pred[,2])
B3
plot(sport.ts, xlim = c(11,14), ylim=c(1200,3500))
line(12+(0:11)/12, sport.pred)
lines(12+(0:11)/12,sport.pred, col="red")
lines(12+(0:11)/12, sport.pred+B3, col="blue")
lines(12+(0:11)/12, sport.pred-B3, col="blue")
points(12+(0:11)/12,sport.pred, col="red")
points(12+(0:11)/12, sport.pred+B3, col="blue")
points(12+(0:11)/12, sport.pred-B3, col="blue")

#Trend and Seasonal components:
tr.lm2$coeff
sport.decomp$figure

#Residual Analysis:

#extracting the residuals
trend.estimate = predict(tr.lm2,t)
seasonal.estimate = sport.decomp$seasonal
fitted = trend.estimate*seasonal.estimate
residuals = as.numeric(sport.ts - fitted)

#Evaluate residuals
plot(residuals)
#Checking for autocorrelation
acf(residuals)
durbinWatsonTest(residuals)
#Test for normality
shapiro.test(residuals)
qqnorm(residuals)
qqline(residuals)

#Linear Model with trend and seasonal estimates

t = seq(1:132)

month=rep(1:12,11)
model2 = lm(sport.log[1:132] ~ t+relevel(as.factor(month),ref=1))
summary(model2)
model2$coeff

#Predictions for linear model

new.month = as.factor(month)
forecasting.data = data.frame (t = 133:144, month=factor(1:12))
forecast = predict(model2,forecasting.data, interval ="prediction")
forecast

e=2.718282
actual.fit=e^(forecast[,1])
actual.lwr=e^(forecast[,2])
actual.upr=e^(forecast[,3])
actual.pred=cbind(actual.fit,actual.lwr,actual.upr)
actual.pred

#Prediction plots
plot(sport.ts,main="sport t.s. prediction 1 year",xlab="years", ylab="sales", type="b",xlim=c(11,14),ylim=c(1200,3500))
points(12+(0:11)/12,actual.pred[,1],col="red")
points(12+(0:11)/12,actual.pred[,2],col="blue")
points(12+(0:11)/12,actual.pred[,3],col="blue")
lines(12+(0:11)/12,actual.pred[,1],col="red")
lines(12+(0:11)/12,actual.pred[,2],col="blue")
lines(12+(0:11)/12,actual.pred[,3],col="blue")

#Residual Analysis:

#checking for autocorrelation
acf(model2$resid)
#durbin Watson test statistic. Seems to show positive autocorrelation
durbinWatsonTest(model2$resid)
#Test for normality. Residuals are normal.
shapiro.test(model2$resid)
qqnorm(model2$resid)
qqline(model2$resid)

#Compare interval lenghts:
#Linear:
actual.pred[,3]-actual.pred[,2]
#Multiplicative:
t.pred[,3]-t.pred[,2]

#Explanation and final model selection:
#There is a clear distinction between widths for each model type. 
#Overall, the multiplicative model shows a smaller prediction width 
#than the general linear model. This would suggest the multiplicative model 
#is the best one when these two are compared since it has the tightest predictive length. 
#===========================================================================================================================

#Decomposition Methods for CA unemployment data
caunemp = scan("../data/CA_Unemployment_Rate.txt")
caunemp.ts = ts(caunemp, frequency=12)
caunemp.diff = diff(caunemp.ts)
plot(caunemp.diff)
plot(caunemp.ts)

#Comments:
#After looking at the data set, a polynomial (3rd) transformation was the best thing 
#possible to my knowledge. Also, the data is not very stable, which led me to pursue
#a multiplicative decomposition model. 


#Multiplicative Decomposition
caunemp.decomp = decompose(caunemp.ts, type="multiplicative")
plot(caunemp.decomp)
#Comments:
#The trend is not excellent for this decomposition, but it is the
#best solution thus far.

d = caunemp.ts/caunemp.decomp$seasonal
plot(d)
t = time(d)

#Use a third degree polynomial
tr.lm3 = lm(d~poly(t,3,raw=TRUE))
summary(tr.lm3)

#Predictions
y = c(13,12.8,12.8,12.2,11.9,12.2,12.8,12.5,12.1,12.1,12.5,12.3)
x = 21+(0:11)/21
new.data=data.frame(t=21+(0:11)/21)
caunemp.pred = predict(tr.lm3,new.data,interval="prediction")
caunemp.pred
caunemp2.pred=caunemp.pred[,1]*caunemp.decomp$figure
caunemp2.pred
B4=0.5*(caunemp.pred[,3]-caunemp.pred[,2])
B4

#Prediction plots
plot(caunemp.ts, xlim = c(20,25), ylim=c(5,20))
lines(21+(0:11)/21, caunemp2.pred)
lines(21+(0:11)/21,caunemp2.pred, col="red")
lines(21+(0:11)/21, caunemp2.pred+B2, col="blue")
lines(21+(0:11)/21, caunemp2.pred-B2, col="blue")
lines(x,y,col="green")
points(21+(0:11)/21,caunemp2.pred, col="red")
points(21+(0:11)/21, caunemp2.pred+B4, col="blue")
points(21+(0:11)/21, caunemp2.pred-B4, col="blue")
points(x,y,col="green")

#Trend and Seasonal components:
tr.lm3$coeff
caunemp.decomp$figure

#Residual Analysis:

#extracting the residuals
trend.estimate = predict(tr.lm3,t)
seasonal.estimate = caunemp.decomp$seasonal
fitted = trend.estimate*seasonal.estimate
residuals = as.numeric(caunemp.ts - fitted)

#Evaluate residuals
plot(residuals)
#Checking for autocorrelation
acf(residuals)
durbinWatsonTest(residuals)
#Test for normality
shapiro.test(residuals)
qqnorm(residuals)
qqline(residuals)

#Linear model with trend and seasonal estimates

t2 = seq(1:240)
month2=rep(1:12,20)

#Use third degree polynomial for linear
model3 = lm(caunemp.ts~poly(t2,3,raw=TRUE)+relevel(as.factor(month2),ref=1))
summary(model3)


#Predictions
new.month2 = as.factor(month2)
forecasting.data2 = data.frame (t2 = c(241:252), month2=1:12)
forecast2 = predict(model3,forecasting.data2, interval ="prediction")
forecast2

actual.fit2=(forecast2[,1])
actual.lwr2=(forecast2[,2])
actual.upr2=(forecast2[,3])
linear.width = 1/2*(actual.upr2-actual.lwr2)
actual.pred2=cbind(actual.fit2,actual.lwr2,actual.upr2)
actual.pred2

#Prediction plots
plot(caunemp.ts,main="caunemp t.s. prediction 1 year",xlab="years", ylab="sales", type="b",xlim=c(20,25),ylim=c(10,20))
points(21+(0:11)/21,actual.pred2[,1],col="red")
points(21+(0:11)/21,actual.pred2[,2],col="blue")
points(21+(0:11)/21,actual.pred2[,3],col="blue")
lines(21+(0:11)/21,actual.pred2[,1],col="red")
lines(21+(0:11)/21,actual.pred2[,2],col="blue")
lines(21+(0:11)/21,actual.pred2[,3],col="blue")
y2 = c(13,12.8,12.8,12.2,11.9,12.2,12.8,12.5,12.1,12.1,12.5,12.3)
x2 = 21+(0:11)/21
points(x2,y2,col="green")
lines(x2,y2,col="green")

#Residual Analysis:

#checking for autocorrelation
acf(model3$resid)
#durbin Watson test statistic. Seems to show positive autocorrelation
durbinWatsonTest(model3$resid)
#Test for normality. Residuals are NOT normal.
shapiro.test(model3$resid)
qqnorm(model3$resid)
qqline(model3$resid)

#Compare interval lenghts:
#Linear:
actual.pred2[,3]-actual.pred2[,2]
#Multiplicative:
caunemp.pred[,3]-caunemp.pred[,2]

#Explanation and final model selection:
#Comparing interval widths between both models indicates a slight advantage for the multplicative decomposition. So,
#by this technical advantage, I would have to say the multiplicative model is best. However, I do believe neither one of
#these models is best when it comes to modeling this dataset. In my opinion, a Holts Winters model could've been used to 
#model this data. It is shown below:

#Set up estimates

t = time(caunemp.ts)
fit = lm(caunemp.ts[1:240]~t[1:240])

#Set up Holt Winters model for seasonality with a multiplicative model

l.0=fit$coeff[1]
b.0=fit$coeff[2]
caunemp.hw=HoltWinters(caunemp.ts, seasonal="multiplicative",l.start=l.0,b.start=b.0)
caunemp.hw
library(forecast)
caunemp.pred=forecast(caunemp.hw,12,level=c(95))
caunemp.pred
plot(caunemp.pred)

#Notice, the prediction points fall much more closer to the actual points. So, if possible, I would actually go with this
#model. However, with the choices available, I would go with multiplicative decomposition because of the small prediction
#widths.


