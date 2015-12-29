### This is the script file for simulating the LLN, CLT, and
### independence of the sample mean and sample variance.

#################################################################################
# cummean function: calculates the cummulative mean of a vactor.

cummean = function(x){
	n = length(x)
	y = numeric(n)
	z = c(1:n)
	y = cumsum(x)
	y = y/z
	return(y)
}

# LLN

n = 10000

z = rnorm(n)

hist(z, main= 'Standard Normal Random Deviates')

x = seq(1,n,1)

y = cummean(z)

plot(x,y,type= 'l',main= 'Convergence Plot')

epsilon = 0.1

Count = 0

for(i in 1:n){
	if(abs(y[i]) > epsilon) Count = Count + 1
}

Count/n 	# approximate probability that abs(y[i]-mu) > epsilon

#################################################################################
# CLT

# Normal

n = 30		# sample size
k = 1000		# number of samples

mu = 5
sigma = 2
SEM = sigma/sqrt(n)

x = matrix(rnorm(n*k,mu,sigma),n,k)		# This gives a matrix with the samples 
							# down the columns.

x.mean = apply(x,2,mean)

x.down = mu - 4*SEM
x.up = mu + 4*SEM

y.up = 1.5

hist(x.mean,prob= T,xlim= c(x.down,x.up),ylim= c(0,y.up),main= 'Sampling 
	distribution of the sample mean, Normal case')

par(new= T)

x = seq(x.down,x.up,0.01)

y = dnorm(x,mu,SEM)

plot(x,y,type= 'l',xlim= c(x.down,x.up),ylim= c(0,y.up))

# Exponential

n = 30		# sample size
k = 1000		# number of samples

theta = 3

lambda = 1/theta

mu = theta
sigma = theta
SEM = sigma/sqrt(n)


x = matrix(rexp(n*k,lambda),n,k)		# This gives a matrix with the 
							# samples down the columns.

x.mean = apply(x,2,mean)

x.down = 0
x.up = 6

y.up = 1

hist(x.mean,prob= T,xlim= c(x.down,x.up),ylim= c(0,y.up),main= 'Sampling 
	distribution of the sample mean, Normal case')

par(new= T)

x = seq(x.down,x.up,0.01)

y = dnorm(x,mu,SEM)

plot(x,y,type= 'l',xlim= c(x.down,x.up),ylim= c(0,y.up))

#################################################################################
# Independence of x.bar and s.

# Normal

n = 30		# sample size
k = 1000		# number of samples

x = matrix(rnorm(n*k,mu,sigma),n,k)		# This gives a matrix with the samples 
							# down the columns.

x.mean = apply(x,2,mean)
x.sd = sqrt(apply(x,2,var))

plot(x.mean,x.sd)

cor(x.mean,x.sd)

# Exponential

n = 30		# sample size
k = 1000		# number of samples

theta = 3

lambda = 1/theta

mu = theta
sigma = theta
SEM = sigma/sqrt(n)

x = matrix(rexp(n*k,lambda),n,k)		# This gives a matrix with the samples 
							# down the columns.

x.mean = apply(x,2,mean)
x.sd = sqrt(apply(x,2,var))

plot(x.mean,x.sd)

cor(x.mean,x.sd)
