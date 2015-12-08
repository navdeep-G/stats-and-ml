#Looking at Beta Priors

#1: Three different priors (a,b)
#Beta priors a = 5, b = 25
binwidth = 0.005
theta = seq(from = binwidth/2, to =1-(binwidth/2), by = binwidth)
a = 1
b = 1
ptheta = dbeta(theta, a, b)

z=51 #Number of successes (infection|vaccination)
N=8197 #Number of total data points
pdatagiventheta=theta^z*(1-theta)^(N-z) #P(Data|Theta)
pthetagivendata = dbeta(theta, a+z, N+b-z) #P(Theta|Data)

windows(10,10)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = FALSE))
maxy=max(c(ptheta, pthetagivendata))
plot(theta, ptheta, type = "l", lwd = 3, main="Prior")
plot(theta, pdatagiventheta, type = "l", lwd = 3, main="Likelihood")
plot(theta, pthetagivendata, type = "l", lwd = 3, main="Posterior")

#Beta priors a = 6, b = 15
binwidth = 0.005
theta = seq(from = binwidth/2, to =1-(binwidth/2), by = binwidth)
a = 6
b = 15
ptheta = dbeta(theta, a, b)
datavec = c(1,1,1,1, 1, 1, 1, 1, 1, 1, 1, 0, 0 , 0 )
z=sum(datavec==1)
N=length(datavec)
pdatagiventheta=theta^z*(1-theta)^(N-z)
pthetagivendata = dbeta(theta, a+z, N+b-z)
windows(10,10)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = FALSE))
maxy=max(c(ptheta, pthetagivendata))
plot(theta, ptheta, type = "l", lwd = 3, main="Prior")
plot(theta, pdatagiventheta, type = "l", lwd = 3, main="Likelihood")
plot(theta, pthetagivendata, type = "l", lwd = 3, main="Posterior")

#Beta priors a = 20, b = 5
binwidth = 0.005
theta = seq(from = binwidth/2, to =1-(binwidth/2), by = binwidth)
a = 20
b = 5
ptheta = dbeta(theta, a, b)
datavec = c(1,1,1,1, 1, 1, 1, 1, 1, 1, 1, 0, 0 , 0 )
z=sum(datavec==1)
N=length(datavec)
pdatagiventheta=theta^z*(1-theta)^(N-z)
pthetagivendata = dbeta(theta, a+z, N+b-z)
windows(10,10)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = FALSE))
maxy=max(c(ptheta, pthetagivendata))
plot(theta, ptheta, type = "l", lwd = 3, main="Prior")
plot(theta, pdatagiventheta, type = "l", lwd = 3, main="Likelihood")
plot(theta, pthetagivendata, type = "l", lwd = 3, main="Posterior")

#2: 3 different data vectors

#Beta priors a = 20, b = 5. Data vector has half of its values as "1"
binwidth = 0.005
theta = seq(from = binwidth/2, to =1-(binwidth/2), by = binwidth)
a = 20
b = 5
ptheta = dbeta(theta, a, b)
datavec = c(1,1,1,1, 1, 1, 1, 0, 0, 0, 0, 0, 0 , 0 )
z=sum(datavec==1)
N=length(datavec)
pdatagiventheta=theta^z*(1-theta)^(N-z)
pthetagivendata = dbeta(theta, a+z, N+b-z)
windows(10,10)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = FALSE))
maxy=max(c(ptheta, pthetagivendata))
plot(theta, ptheta, type = "l", lwd = 3, main="Prior")
plot(theta, pdatagiventheta, type = "l", lwd = 3, main="Likelihood")
plot(theta, pthetagivendata, type = "l", lwd = 3, main="Posterior")

#Beta priors a = 20, b = 5. Data vector has 5 of its values as "1"
binwidth = 0.005
theta = seq(from = binwidth/2, to =1-(binwidth/2), by = binwidth)
a = 20
b = 5
ptheta = dbeta(theta, a, b)
datavec = c(1,1,1,1, 1, 0, 0, 0, 0, 0, 0, 0, 0 , 0 )
z=sum(datavec==1)
N=length(datavec)
pdatagiventheta=theta^z*(1-theta)^(N-z)
pthetagivendata = dbeta(theta, a+z, N+b-z)
windows(10,10)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = FALSE))
maxy=max(c(ptheta, pthetagivendata))
plot(theta, ptheta, type = "l", lwd = 3, main="Prior")
plot(theta, pdatagiventheta, type = "l", lwd = 3, main="Likelihood")
plot(theta, pthetagivendata, type = "l", lwd = 3, main="Posterior")

#Beta priors a = 20, b = 5. Data vector has 12 of its values as "1"
binwidth = 0.005
theta = seq(from = binwidth/2, to =1-(binwidth/2), by = binwidth)
a = 20
b = 5
ptheta = dbeta(theta, a, b)
datavec = c(1,1,1,1, 1, 1, 1, 1, 1, 1, 1, 1, 0 , 0 )
z=sum(datavec==1)
N=length(datavec)
pdatagiventheta=theta^z*(1-theta)^(N-z)
pthetagivendata = dbeta(theta, a+z, N+b-z)
windows(10,10)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = FALSE))
maxy=max(c(ptheta, pthetagivendata))
plot(theta, ptheta, type = "l", lwd = 3, main="Prior")
plot(theta, pdatagiventheta, type = "l", lwd = 3, main="Likelihood")
plot(theta, pthetagivendata, type = "l", lwd = 3, main="Posterior")

