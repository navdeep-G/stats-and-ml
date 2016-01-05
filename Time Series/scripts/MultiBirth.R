#Analyze birth data using decomposition methods 
#I will report the trend and seasonal estimates, a table with your point predictions,
#95% interval predictions, and the interval widths.

#Read in file and set up time series using first 30 years. Set up specifically for decomposition:
birth = scan("../data/birth.txt")
birth.ts = ts(birth[1:360],frequency = 12)

#Plot of original time series
plot(birth.ts)

#The variation among the seasons is quite constant. So, a decomposition model with additive effects would be best. 

#Additive Decomposition
birth.decomp = decompose(birth.ts,type="multiplicative")
plot(birth.decomp)
d = birth.ts/birth.decomp$seasonal
t = time(d)
#tr=lm(d~poly(t,5,raw=TRUE)) #+relevel(as.factor(month),ref=1)), poly(t,5,raw=TRUE)
tr=lm(d~t)
summary(tr)

#Trend and Seasonal components:
tr$coeff
birth.decomp$figure

#Prediction plot
birth.pred = predict(tr, data.frame(t=31+(0:11)/12), interval="prediction")
birth.pred2 =  birth.pred[,1]*birth.decomp$figure
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

#Compare point predictions with actual points:
birth.compare.additive = cbind(birth.pred2[1:12],birth[361:372])

#extracting the residuals
trend.estimate = predict(tr,t)
seasonal.estimate = birth.decomp$seasonal
fitted = trend.estimate + seasonal.estimate
residuals = as.numeric(birth.ts - fitted)