#Additive and Multiplicative models

#Below are 4 applications of time series regression

#setwd(to/the/repo/location)

#============================================================================================================================================
#Additive and Multiplicative decomposition for oligopoly dataset

#Additive Model for oligopoly dataset (Manual process, i.e., not using R's blackbox method decompose())

#Read in data and set up time series object
oligopoly = scan("../data/oligopoly.txt",skip = 1)
oligopoly.ts = ts(oligopoly,frequency=4)

#Take a look at the time series
plot(oligopoly.ts)

#Decompose dataset and plot decomposition
oligopoly.decomp = decompose(oligopoly.ts,type="additive")
plot(oligopoly.decomp)

#Remove seasonal trend
d = oligopoly.ts - oligopoly.decomp$seasonal

#Set up time series with new additive decomposition
t = time(d)
tr=lm(d~t)

#Prediction
#Set up new data, predict on new data, and plot predictions
newdata=data.frame(t=4+(0:3)/4) 
d.pred = predict(tr,newdata, interval= "prediction")
oligopoly.pred = d.pred[,3] + oligopoly.decomp$figure
B = 0.5*(d.pred[,3]-d.pred[,2])
plot(oligopoly.ts,xlim = c(1,5), ylim=c(20,80),main="Oligopoly Add", xlab = "", ylab= "")
lines(4+(0:3)/4, oligopoly.pred, lty=3)
points(4+(0:3)/4, oligopoly.pred, col='red')
points(4+(0:3)/4, oligopoly.pred+B, col='blue')
points(4+(0:3)/4, oligopoly.pred-B, col='blue')

#Multiplicative model

#Blackbox method using decompose()
oligopoly.decomp = decompose(oligopoly.ts,type="multiplicative")
plot(oligopoly.decomp)

#Deseasonalized obs and set up time series model
d = oligopoly.ts/oligopoly.decomp$seasonal
plot(d)
t = time(d)
tr.lm = lm(d~t)

#Prediction
#Set up new data, predict on new data, and plot predictions
new.data=data.frame(t=4+(0:3)/4)
d.pred = predict(tr.lm,new.data, interval = "prediction")
oligopoly.pred = d.pred[,1]*oligopoly.decomp$figure
B2 = 0.5*(d.pred[,3]-d.pred[,2])
plot(oligopoly.ts, xlim = c(1,5), ylim=c(15,60))
lines(4+(0:3)/4, oligopoly.pred, lty= 3)
points(4+(0:3)/4, oligopoly.pred, col='red')
points(4+(0:3)/4, oligopoly.pred+B2, col='blue')
points(4+(0:3)/4, oligopoly.pred-B2, col='blue')

#============================================================================================================================================
#Additive and Multiplicative decomposition for hotel dataset

#Read in data
hotel = scan(file = "C:/Users/navdeepgill/Desktop/hotel.txt")
hotel

#Set up time series object
hotel.ts = ts(hotel,frequency =12)
hotel.ts
plot(hotel.ts,type = "b",xlab = "Year",ylab = "",
main = "Hotel Data Time Series Plot")

#Multiplicative decomposition
hotel.decomp.mul = decompose(hotel.ts, type = "multiplicative")
hotel.decomp.mul
plot(hotel.decomp.mul)

#Deseanalized obs and set up time series model
d = hotel.ts /hotel.decomp.mul$seasonal
plot(d, type = "l")
t=time(d)
t
hotel.trend.mul = lm(d ~ t) #Hotel Trend LM for multiplicative
summary(hotel.trend.mul)

#Prediction
#First, use trend to predict
#Set up new data, predict on new data, and plot predictions
new.data = data.frame(t=15+(0:11)/12)
d.pred = predict(hotel.trend.mul,new.data, interval="prediction")
hotel.pred = d.pred[,1]*hotel.decomp.mul$figure #Actual Fitted Value 
B = .5*(d.pred[,3]-d.pred[,2])
plot(hotel.ts, xlim = c(12,16), ylim = c(500,1200),
main = "Hotel TS Plot with Prediction Multiplicative",
xlab = "Years",ylab = "")
lines(15+(0:11)/12, hotel.pred, lty = 5)
points(15+(0:11)/12, hotel.pred, col = "red")
points(15+(0:11)/12, hotel.pred + B, col = "blue")
points(15+(0:11)/12, hotel.pred - B, col = " blue ")

#Additive Decomposition
hotel.decomp.add = decompose(hotel.ts, type = "additive")
plot(hotel.decomp.add)
hotel.decomp.add 

#Deseasonalized obs and set up time series model
d = hotel.ts - hotel.decomp.add$seasonal
d
t = time(d)
t
hotel.trend.add = lm(d ~ t)
summary(hotel.trend.add)

#Prediction
#Set up new data, predict on new data, and plot predictions
new.data = data.frame(t=15+(0:11)/12)
d.pred.add = predict(hotel.trend.add,new.data, interval = "prediction")
hotel.predict = d.pred.add[,1]+hotel.decomp.add$figure
hotel.predict
B1 = .5*(d.pred.add[,3]-d.pred.add[,2])
B1

plot(hotel.ts, xlim = c(12,16), ylim = c(500,1200),
main = "Hotel TS Plot with Prediction Additive",
xlab = "Years",ylab = "")
lines(15+(0:11)/12, hotel.predict, lty = 3)
points(15+(0:11)/12, hotel.predict, col = "red")
points(15+(0:11)/12, hotel.predict + B1, col = "blue")
points(15+(0:11)/12, hotel.predict - B1, col = " blue ")

cbind(B, B1)

#============================================================================================================================================
#Additive and Multiplicative decomposition for johnson and johnson dataset

#Read in data
jjdata = scan("/Users/navdeepgill/Desktop/jj.txt")
jjdata

#Set up time series and plot
jjdata.ts = ts(jjdata,frequency = 4)
jjdata.ts
plot(jjdata.ts,type = "b",xlab = "quarters",ylab = "",
main = "JJ data Time Series Plot")

#Multiplicative Decomposition
jjdata.decomp.mult = decompose(jjdata.ts, type = "multiplicative")
jjdata.decomp.mult
plot(jjdata.decomp.mult)

#Deseanalized obs and set up time series models(Will also take a look at square transformation)
d = jjdata.ts /jjdata.decomp.mult$seasonal
plot(d, type = "b")
t=time(d)
t
tsq = t^2
tr1.mul = lm(d ~ t)
summary(tr1.mul)
tr2.mul = lm(d~(tsq +t)) # 
summary(tr2.mul)

#First, use trend to predict
#Set up new data, predict on new data, and plot predictions
new.data = data.frame(tsq = (22+(0:3)/4)^2,t=22+(0:3)/4) # 
d.pred = predict(tr2.mul,new.data, interval="prediction")
jjdata.pred = d.pred[,1]*jjdata.decomp.mult$figure
B = .5*(d.pred[,3]-d.pred[,2])
plot(jjdata.ts, xlim = c(20,24), ylim = c(0, 30),
main = "jj TS Plot with Prediction Multiplicative",
xlab = "Quarters",ylab = "")
lines(22+(0:3)/4, jjdata.pred, lty = 3)
points(22+(0:3)/4, jjdata.pred, col = "red")
points(22+(0:3)/4, jjdata.pred+ B, col = "blue")
points(22+(0:3)/4, jjdata.pred - B, col = " blue ")


#Additive Decomposition
jjdata.decomp.add = decompose(jjdata.ts, type = "additive")
plot(jjdata.decomp.add)
jjdata.decomp.add

#Deseasonalized obs and set up time series model(Will also look at square transform on time)
d = jjdata.ts - jjdata.decomp.add$seasonal
d
t = time(d)
t
tsq =t^2
jjdata.trend.add = lm(d ~ (tsq + t)) #
summary(jjdata.trend.add)

#Prediction
#Set up new data, predict on new data, and plot predictions
new.data = data.frame(tsq = (22+(0:3)/4)^2,t=22+(0:3)/4) # 
d.pred.add = predict(jjdata.trend.add,new.data, interval = "prediction")
jjdata.predict = d.pred.add[,1]+jjdata.decomp.add$figure
jjdata.predict
B1 = .5*(d.pred.add[,3]-d.pred.add[,2])
B1

plot(jjdata.ts, xlim = c(20,24), ylim = c(0,30),
main = "JJDATA TS Plot with Prediction Additive",
xlab = "Quarters",ylab = "")
lines(22+(0:3)/4, jjdata.predict, lty = 3)
points(22+(0:3)/4, jjdata.predict, col = "red")
points(22+(0:3)/4, jjdata.predict + B1, col = "blue")
points(22+(0:3)/4, jjdata.predict - B1, col = " blue ")

cbind(B, B1)

#============================================================================================================================================
#Additive and Multiplicative decomposition for bike sales dataset
bike = c(10,31,43,16,11,33,45,17,13,34,48,19,15,37,51,21)
bike.ts = ts(bike, frequency = 4)
bike.ts
plot(bike.ts,type = "b",xlab = "quarters",ylab = "",
main = "Bike data Time Series Plot")

#Multiplicatie Decomposition
bike.decomp.mult = decompose(bike.ts, type = "multiplicative")
bike.decomp.mult
plot(bike.decomp.mult)

#Deseanalized obs and set up time series model
d = bike.ts / bike.decomp.mult$seasonal
plot(d)
t=time(d)
t
tr1.mul = lm(d ~ t)
summary(tr1.mul)

#First, use trend to predict
#Set up new data, predict on new data, and plot predictions
new.data = data.frame(t=5+(0:3)/4)
d.pred = predict(tr1.mul,new.data, interval="prediction")
bike.pred = d.pred[,1]*bike.decomp.mult$figure
B = .5*(d.pred[,3]-d.pred[,2])
plot(bike.ts, xlim = c(1,6), ylim = c(0, 80),
main = "Bike TS Plot with Prediction Multiplicative",
xlab = "Quarters",ylab = "")
lines(5+(0:3)/4, bike.pred, lty = 3)
points(5+(0:3)/4, bike.pred, col = "red")
points(5+(0:3)/4, bike.pred + B, col = "blue")
points(5+(0:3)/4, bike.pred - B, col = " blue ")

#Additive Decomposition
bike.decomp.add = decompose(bike.ts, type = "additive")
plot(bike.decomp.add)
bike.decomp.add

#Deseasonalized obs and set up time series mdoel
d = bike.ts - bike.decomp.add$seasonal
d
t = time(d)
t
bike.tr.add = lm(d ~ t)
summary(bike.tr.add)

#Prediction
#Set up new data, predict on new data, and plot predictions
new.data = data.frame(t=5+(0:3)/4)
d.pred.add = predict(bike.tr.add,new.data, interval = "prediction")
bike.predict = d.pred[,1]+bike.decomp.add$figure
bike.predict
B1 = .5*(d.pred.add[,3]-d.pred.add[,2])
B1

plot(bike.ts, xlim = c(1,6), ylim = c(0, 80),
main = "Bike TS Plot with Prediction Additive",
xlab = "Quarters",ylab = "")
lines(5+(0:3)/4, bike.predict, lty = 3)
points(5+(0:3)/4, bike.predict, col = "red")
points(5+(0:3)/4, bike.predict + B1, col = "blue")
points(5+(0:3)/4, bike.predict - B1, col = " blue ")

cbind(B, B1)

