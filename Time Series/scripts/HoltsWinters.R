#Holt Winters Methods

library(forecast)

#setwd(to/the/repo/location)

#============================================================================================================================================
#Plot time series for hotel data
hotel = scan("../data/hotel.txt")
hotel.ts = ts(hotel,frequency=12,start=c(1,1))
plot(hotel.ts)

#After initial review of the time series, I see an increase in seasonal variation
#over time, which leads me to believe a Holts Winters multiplicative model is best.

#Set up estimates. 
t = time(hotel.ts)
fit = lm(hotel.ts[1:12]~t[1:12])

#Set up Holt Winters multiplicative model
l.0=fit$coeff[1]
b.0=fit$coeff[2]
hotel.hw=HoltWinters(hotel.ts, seasonal="multiplicative",l.start=l.0,b.start=b.0)
hotel.hw

#Smoothing constants alpha and beta
hotel.hw$alpha
hotel.hw$beta

#95% prediction intervals and plot of data with predictions.
hotel.pred=forecast(hotel.hw,12,level=c(95))
hotel.pred
plot(hotel.pred)

#============================================================================================================================================
#Holt Winters with loans dataset

#Plot time series data
loan = scan("../data/loans.txt")
loan.ts=ts(loan,frequency=12)
plot(loan.ts)

#After initial review of the time series, I see a change in the mean level and growth rate
#over time, which leads me to believe a Holts trend model is best.

#Set up estimates. 
t = time(loan.ts)
fit = lm(loan.ts[1:12]~t[1:12])

#No seasonality. So, Holts trend should be suitable
l.0 = fit$coeff[1]
b.0 = fit$coeff[2]
loan.holts=HoltWinters(loan.ts,gamma=FALSE,l.start=l.0,b.start=b.0)
loan.holts

#Smoothing constants alpha and beta
loan.holts$alpha
loan.holts$beta

#95% prediction intervals and plot of data with predictions.
loan.pred=forecast(loan.holts,6,level=c(95))
loan.pred
plot(loan.pred)

#============================================================================================================================================
#Holts Winters with Johnson and Johnson dataset
#Plot time series data

jj = scan("../data/jj.txt")
jj.ts = ts(jj,frequency=4, start(1,1))
plot(jj.ts)

#After initial review of the time series, I see an increase in seasonal variation
#over time, which leads me to believe a Holts Winters multiplicative model is best.

#Set up estimates. 
t = time(jj.ts)
fit = lm(jj.ts[1:12]~t[1:12])

#Set up Holt Winters multiplicative model
l.0 = fit$coeff[1]
b.0 = fit$coeff[2]
jj.hw=HoltWinters(jj.ts, seasonal = "multiplicative",l.start=l.0,b.start=b.0)
jj.hw

#Smoothing constants alpha and beta
jj.hw$alpha
jj.hw$beta

#95% prediction intervals and plot of data with predictions.
jj.pred=forecast(jj.hw,4,level=c(95))
jj.pred
plot(jj.pred)

#============================================================================================================================================
#Holts Winters with global temperature dataset
#Plot time series data

glob = scan("../dataglobtemp.txt")
glob.ts = ts(glob,frequency=1, start(1,1))
plot(glob.ts)

#After initial review of the time series, I see a change in the mean level and growth rate
#over time, which leads me to believe a Holts trend model is best.

#Set up estimates. 
t = time(glob.ts)
fit = lm(glob.ts[1:12]~t[1:12])

#No seasonality is apparent. So, Holts trend is suitable.  
l.0 = fit$coeff[1]
b.0 = fit$coeff[2]
glob.holts=HoltWinters(glob,gamma=FALSE,l.start=l.0,b.start=b.0)
glob.holts

#Smoothing constants alpha and beta
glob.holts$alpha
glob.holts$beta

#95% prediction intervals and plot of data with predictions.
glob.pred=forecast(glob.holts,5,level=c(95))
glob.pred
plot(glob.pred)

