
#Analyzing the birth dataset using regression, decomposition methods, exponential smoothing, and then comparing each of the previous
#models to find the best fit

#setwd(to/the/repo/location)
library(forecast)
library(lmtest)
library(car)

#Read in file and set up time series using first 30 years:
birth = scan("../data/birth.txt")
birth.ts = ts(birth[1:372])
#==============================================================================================================================

#Analyze using standard regression techniques. I will report trend and
#seasonal estimates, a table with point predictions, 95% interval predictions,
#and the interval widths.

#Plot of original time series
plot(birth.ts)

#After an intial review of the time series, I suspect I will need to transform the data using a 5th degree polynomial.

#Plot of first difference
plot(diff(birth.ts,1))

#The variance seems relatively constant over time. 

#Set up for regression model
t = time(birth.ts)
month=as.factor(rep(1:12,30))

#Use 5th degree polynomial 
birth.poly.5 = lm(birth.ts~poly(t,5,raw=TRUE)+relevel(as.factor(month),ref=1))
#Trend and seasonal estimates
summary(birth.poly.5)

#Forecasting 12 months for 5th degree polynomial model
new.data=data.frame(t=361:372,month=as.factor(1:12))
birth.pred = predict(birth.poly.5, new.data, interval="prediction")
birth.pred
#Prediction widths
pred.width = birth.pred[,3]-birth.pred[,2]
pred.width

plot(birth.ts,xlim=c(0,375),ylim = c(200,450), ylab = "Y", main = "5th Degree Polynomial Fit")
points(361:372,birth.pred[,1],col="red")
points(361:372,birth.pred[,2],col="blue")
points(361:372,birth.pred[,3],col="blue")
lines(361:372,birth.pred[,1],col="red")
lines(361:372,birth.pred[,2],col="blue")
lines(361:372,birth.pred[,3],col="blue")

#Forecasting 12 months for 5th degree polynomial model. Modified xlim to see prediction better
new.data=data.frame(t=361:372,month=as.factor(1:12))
birth.pred = predict(birth.poly.5, new.data, interval="prediction")
plot(birth.ts,xlim=c(350,375),ylim = c(200,450), ylab = "Y", main = "5th Degree Polynomial Fit")
points(361:372,birth.pred[,1],col="red")
points(361:372,birth.pred[,2],col="blue")
points(361:372,birth.pred[,3],col="blue")
lines(361:372,birth.pred[,1],col="red")
lines(361:372,birth.pred[,2],col="blue")
lines(361:372,birth.pred[,3],col="blue")

#Table with point predicitions, 95% confidence intervals, and prediction widths:
birth.forecast = cbind(birth.pred,pred.width)
birth.forecast

#Compare point predictions with actual points:
birth.compare = cbind(birth.pred[,1],birth[361:372])
birth.compare

#Evaluating model assumptions
plot(birth.poly.5$residuals)
#Checking for autocorrelation
acf(birth.poly.5$resid)
#Homoscedasticity 
durbinWatsonTest(birth.poly.5$resid)
#Test for normality
shapiro.test(birth.poly.5$residuals)
qqnorm(birth.poly.5$residuals)
qqline(birth.poly.5$residuals)
#Assumptions are not met. Normality fails with a very low p-value and the data is positively correlated with slow lags.

#The fit of this model is not quite adequate. The prediction points seem to be off by a fair amount when compared to the 
#actual points of the 31st year. This indicated that a regression model (with a 5th degree transformation) is probably not 
#adequate when it comes to capturing the nature of this data. 
#==============================================================================================================================

#Analyze using standard decomposition. I will report trend and
#seasonal estimates, a table with point predictions, 95% interval predictions,
#and the interval widths.

#Read in file and set up time series using first 30 years. Set up specifically for decomposition:
birth = scan("birth.txt")
birth.ts = ts(birth[1:360],frequency = 12)

#Plot of original time series
plot(birth.ts)

#The variation among the seasons is quite constant. So, a decomposition model with additive effects would be best. 

#Additive Decomposition
birth.decomp = decompose(birth.ts,type="additive")
plot(birth.decomp)
d = birth.ts - birth.decomp$seasonal
t = time(d)
#tr=lm(d~poly(t,5,raw=TRUE)) #+relevel(as.factor(month),ref=1)), poly(t,5,raw=TRUE)
tr=lm(d~t)
summary(tr)

#Trend and Seasonal components:
tr$coeff
birth.decomp$figure

#Prediction plot
birth.pred = predict(tr, data.frame(t=31+(0:11)/12), interval="prediction")
birth.pred2 =  birth.pred[,1] + birth.decomp$figure
B = 0.5*(birth.pred[,3]-birth.pred[,2])
plot(birth.ts,xlim=c(0,35),ylim = c(200,450))
lines(31+(0:11)/12, birth.pred2, lty=3)
points(31+(0:11)/12, birth.pred2, col='red')
points(31+(0:11)/12, birth.pred2+B, col='blue')
points(31+(0:11)/12, birth.pred2-B, col='blue')
lines(31+(0:11)/12, birth.pred2, col='red')
lines(31+(0:11)/12, birth.pred2+B, col='blue')
lines(31+(0:11)/12, birth.pred2-B, col='blue')

#Prediction plot with zoomed in xlim
birth.pred = predict(tr, data.frame(t=31+(0:11)/12), interval="prediction")
birth.pred2 =  birth.pred[,1] + birth.decomp$figure
B = 0.5*(birth.pred[,3]-birth.pred[,2])
plot(birth.ts,xlim=c(28,35),ylim = c(200,450))
lines(31+(0:11)/12, birth.pred2, lty=3)
points(31+(0:11)/12, birth.pred2, col='red')
points(31+(0:11)/12, birth.pred2+B, col='blue')
points(31+(0:11)/12, birth.pred2-B, col='blue')
lines(31+(0:11)/12, birth.pred2, col='red')
lines(31+(0:11)/12, birth.pred2+B, col='blue')
lines(31+(0:11)/12, birth.pred2-B, col='blue')

#Table with point predicitions, 95% confidence intervals, and prediction widths:
birth.forecast.additive = cbind(birth.pred2,birth.pred[,2],birth.pred[,3],B)
birth.forecast.additive

#Compare point predictions with actual points:
birth.compare.additive = cbind(birth.pred2[1:12],birth[361:372])
birth.compare.additive

#extracting the residuals
trend.estimate = predict(tr,t)
seasonal.estimate = birth.decomp$seasonal
fitted = trend.estimate + seasonal.estimate
residuals = as.numeric(birth.ts - fitted)

#Evaluating model assumptions
plot(residuals)
#Checking for autocorrelation
acf(residuals)
#Homoscedasticity 
durbinWatsonTest(residuals)
#Test for normality
shapiro.test(residuals)
qqnorm(residuals)
qqline(residuals)
#Assumptions are not met. Normality fails with a very low p-value and the data is positively correlated with slow lags.

#The predicted data points do not fall very close to the actual points but they're a bit closer than the previous
#polynomial model. However, the prediction interval seems to be quite large and can cause room for error. This can be a
#cause for concern in terms of modeling and making predictions. 
#==============================================================================================================================

#Analyze using exponential smoothing methods. I will report the optimized
#smoothing constants, a table with point predictions, 95% interval predictions,
#and the interval widths.

#The birth data set has a global trend with seasonality. This led me to believe that a Holt's Winter Method is 
#most adequate. The seasonal variance is constant, which means an additive aspect of Holt Winters is best. I decided 
#to use the first 5 years for the baseline estimation as it pertains to b.0 and l.0.

birth = scan("birth.txt")
birth.ts = ts(birth[1:360],frequency = 12)

#Set up estimates
t = time(birth.ts)
fit = lm(birth.ts[1:60]~t[1:60])

#Set up Holt Winters model for seasonality with a multiplicative model

l.0=fit$coeff[1]
b.0=fit$coeff[2]
birth.hw=HoltWinters(birth.ts, seasonal="additive",l.start=l.0,b.start=b.0)
birth.hw
birth.pred=forecast(birth.hw,12,level=c(95))
birth.pred
plot(birth.pred)

#prediction widths:
birth.width.hw = birth.pred$up-birth.pred$lo
birth.width.hw
#Compare predicted to actual points:
birth.compare.hw = cbind(birth.pred$mean,birth[361:372])
birth.compare.hw
#Interval and prediction widths. 
birth.compare.forecast = cbind(birth.pred$lo,birth.pred$up,birth.width.hw)
birth.compare.forecast

#This model seems to be quite adequate. It shows a tight prediction interval and seems to capture the actual points in 
#a good manner. I would say it beats out the additive model from part B. However, it does have its faults. For example, the
#second data point is 285, but the predicted value is 239. And, also, the prediction intervals do not capture the actual 
#values, which can cause concern and bad predictions. 
#==============================================================================================================================

#Analyze using an ARIMA(p,d,q)x(P,D,Q)S models in R. I will report model
#estimates, a table with point predictions, 95% interval predictions, and the
#interval widths. In addition I will provide the ACF/PACF of my final models.

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

#Plot data and acf.pacf
birth = scan("birth.txt")
birth.ts = ts(birth)
plot(birth.ts)

acf.pacf(birth.ts,48,12)

#The ACF plot shows that the spikes are dying down slowly, and this indicates that the dataset is not stationary. 
#To fix this, the first difference is taken. 

birth.dif=diff(birth.ts,1)

acf.pacf(birth.dif,48,12)

#According to the previous plot, the non-seasonal plots died down much quicker in the ACF plot. However, the seasonal 
#spikes are still slow. So, the first seasonal difference should also be taken. 

#Take the first difference and first seasonal difference
arima.010.010 = arima(birth.ts,order = c(0, 1, 0),
                      seasonal = list(order = c(0, 1, 0), period = 12))
acf.pacf(arima.010.010$resid,48,12)

#The previous plot shows the first seasonal difference brought the data to a stationary state. Both of the spikes are dying 
#down quickly. The differenced ACF/PACF shows an MA1 seasonal should be added to the model.

#MA(1) seasonal model
arima.010.011 = arima(birth.ts,order = c(0, 1, 0),
                      seasonal = list(order = c(0, 1, 1), period = 12))
acf.pacf(arima.010.011$resid,48,12)

#The previous goes on to suggest a nonseasonal MA(1) term or a nonseasonal AR(4) term.

#MA(1) nonseasonal term:
arima.011.011 = arima(birth.ts,order = c(0, 1, 1),
                      seasonal = list(order = c(0, 1, 1), period = 12))
acf.pacf(arima.011.011$resid,48,12)

#Testing significance of coefficients
theta.hat = arima.011.011$coef
se.theta.hat = sqrt(diag(arima.011.011$var.coef))
t.stat = abs(theta.hat/se.theta.hat)
t.stat #All terms are significant except second term. 

#predicting 12 months for arima(5,1,0)x(1,1,0)x12
y.hat = predict(arima.011.011,n.ahead=12)$pred
se.y.hat = predict(arima.011.011,n.ahead=12)$se
n = length(birth.ts)
n.p = length(theta.hat)
pred.width = 2*qt(.975,n-n.p)*se.y.hat

#Saving the width for comparison later
pred.width.fit1 = pred.width
pred.width.fit1

#Plot predictions
x11()
plot(birth.ts,main="ARIMA(0,1,1)x(0,1,1)x12",xlim = c(0,n+length(y.hat)),
     ylim = c(min(birth.ts,y.hat-0.5*pred.width),max(birth.ts,y.hat+0.5*pred.width)))
points(n+1:length(y.hat),y.hat,col="blue")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#Plot predictions
x11()
plot(birth.ts,main="ARIMA(0,1,1)x(0,1,1)x12",xlim = c(120,n+length(y.hat)),
     ylim = c(min(birth.ts[120:n],y.hat-0.5*pred.width),max(birth.ts,y.hat+0.5*pred.width)))
points(n+1:length(y.hat),y.hat,col="blue")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat-0.5*pred.width,lty=3,col="red")
lines(n+1:length(y.hat),y.hat,lty=3,col="blue")
lines(n+1:length(y.hat),y.hat+0.5*pred.width,lty=3,col="red")

#AR(4) nonseasonal term:
arima.410.011 = arima(birth.ts,order = c(5, 1, 0),
                      seasonal = list(order = c(0, 1, 1), period = 12))
acf.pacf(arima.410.011$resid,48,12)

#Testing significance of coefficients
theta.hat = arima.410.011$coef
se.theta.hat = sqrt(diag(arima.410.011$var.coef))
t.stat = abs(theta.hat/se.theta.hat)
t.stat #All are significant

#predicting 12 months for arima(5,1,0)x(1,1,0)x12
y.hat2 = predict(arima.410.011,n.ahead=12)$pred
se.y.hat2 = predict(arima.410.011,n.ahead=12)$se
n = length(birth.ts)
n.p = length(theta.hat)
pred.width = 2*qt(.975,n-n.p)*se.y.hat

#Saving the width for comparison later
pred.width.fit2 = pred.width
pred.width.fit2

#Plot predictions
x11()
plot(birth.ts,main="ARIMA(4,1,0)x(0,1,0)x12",xlim = c(0,n+length(y.hat2)),
     ylim = c(min(birth.ts,y.hat2-0.5*pred.width),max(birth.ts,y.hat2+0.5*pred.width)))
points(n+1:length(y.hat2),y.hat2,col="blue")
lines(n+1:length(y.hat2),y.hat2,lty=3,col="blue")
lines(n+1:length(y.hat2),y.hat2-0.5*pred.width,lty=3,col="red")
lines(n+1:length(y.hat2),y.hat2,lty=3,col="blue")
lines(n+1:length(y.hat2),y.hat2+0.5*pred.width,lty=3,col="red")

#Plot predictions
x11()
plot(birth.ts,main="ARIMA(4,1,0)x(0,1,1)x12",xlim = c(120,n+length(y.hat2)),
     ylim = c(min(birth.ts[120:n],y.hat2-0.5*pred.width),max(birth.ts,y.hat2+0.5*pred.width)))
points(n+1:length(y.hat2),y.hat2,col="blue")
lines(n+1:length(y.hat2),y.hat2,lty=3,col="blue")
lines(n+1:length(y.hat2),y.hat2-0.5*pred.width,lty=3,col="red")
lines(n+1:length(y.hat2),y.hat2,lty=3,col="blue")
lines(n+1:length(y.hat2),y.hat2+0.5*pred.width,lty=3,col="red")

#Check AIC criterion for final model:
arima.011.011
arima.410.011
#After comparing AIC, arima.410.011 is the best model.

#Check prediction widths between models:
cbind(pred.width.fit1,pred.width.fit2)
#The prediction widths seems to be very close between the two models. 

#Compare arima.410.011 and arima.011.011 to actual points:
birth.compare.arima.011 = cbind(y.hat,birth[361:372])
birth.compare.arima.011
birth.compare.arima.410 = cbind(y.hat2,birth[361:372])
birth.compare.arima.410

#The model, arima.410.011 seems to be closer to the prediction values, which indicates it as being the better model.

#After analyzing both models, I will select hotel.arima.410.011 as the final model since the AlC is smaller than the 
#first model. Also, the prediction points are closer to the actual values in comparison to the prediction values of the 
#first model. 

#E)

#Compare and contrast your final model from each method, select the best model and
#perform a complete residual analysis of your selected best fitting model.

#Comparison of all predicted values from each model with actual values:
birth.compare
birth.compare.additive
birth.compare.hw
birth.compare.arima.410
#By looking at the previous output, we can see that the arima.410.011 model falls closer to the actual points on a 
#consistent basis and is thus performing better.

#Comparison of prediction value widths:
pred.width.overall = cbind(pred.width,B,birth.width.hw,pred.width.fit2)
pred.width.overall
#By looking at the previous output, we can see that the arima.410.011 model(pred.width.fit2) has the tightest prediction
#widths overall and this goes on to push this as being the best model out of the four.

#After the previous analysis, I would have to say the best model is the arima.410.011 because it falls closer to the actual
#points for the 31st year and it has the tightest prediction interval widths. 

#Residual Analysis of arima.410.011

#Checking residuals
x11()
par(mfrow=c(1,2))
hist(arima.410.011$resid)
qqnorm(arima.410.011$resid)
qqline(arima.410.011$resid)
par(mfrow=c(1,1))

#Check normality
shapiro.test(arima.410.011$resid) #P-value of 8.965e-07, which suggests not normal.
#The Ljung Box test statistic. Checks adequuacy of the model. If we reject, then model does not fit well.
Box.test(arima.410.011$resid,type="Ljung") 
#Model seems adequate fit with a p-value of .9859.


