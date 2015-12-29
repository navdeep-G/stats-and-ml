### This R program plots the binomial probability mass function four 
### times with different parameter values on the same scales.

### Plot the Binomial

x.max = 50

par(mfrow=c(2,2))

# plot 1

n = 20
p = 0.5

x = seq(0,x.max)
y = dbinom(x,n,p)
plot(x,y,type='h', main="Bin(20,0.5)")

# plot 2

n = 20
p = 0.75

x = seq(0,x.max)
y = dbinom(x,n,p)
plot(x,y,type='h', main="Bin(20,0.75)")

# plot 3

n = 40
p = 0.75

x = seq(0,x.max)
y = dbinom(x,n,p)
plot(x,y,type='h', main="Bin(40,0.75)")

### Plot the Exponential

y.max = 2

par(mfrow=c(2,2))

# plot 4

lambda = 0.5		# note that this is the rate paramter = 1/scale paramter

x = seq(0.01,10,0.01)
y = dexp(x,lambda)

plot(x,y,type='l', main="Exp(0.5)", ylim=c(0,y.max))

# plot 5

lambda = 1

x = seq(0.01,10,0.01)
y = dexp(x,lambda)

plot(x,y,type='l', main="Exp(1)", ylim=c(0,y.max))

# plot 6

lambda = 2

x = seq(0.01,10,0.01)
y = dexp(x,lambda)

plot(x,y,type='l', main="Exp(2)", ylim=c(0,y.max))

### Plot the Gamma

y.max = 3

par(mfrow=c(2,2))

# plot 7

alpha = 0.5		# shape parameter
lambda = 1		# note that this is the rate paramter = 1/scale paramter

x = seq(0.01,20,0.01)
y = dgamma(x,alpha,lambda)

plot(x,y,type='l', main="Gamma(0.5,1)", ylim=c(0,y.max))

# plot 8

alpha = 1		# shape parameter
lambda = 1		# note that this is the rate paramter = 1/scale paramter

x = seq(0.01,20,0.01)
y = dgamma(x,alpha,lambda)

plot(x,y,type='l', main="Gamma(1,1)", ylim=c(0,y.max))

y.max = 0.3

# plot 9

alpha = 5			# shape parameter
lambda = 1		# note that this is the rate paramter = 1/scale paramter

x = seq(0.01,20,0.01)
y = dgamma(x,alpha,lambda)

plot(x,y,type='l', main="Gamma(5,1)", ylim=c(0,y.max))

# plot 10

alpha = 10		# shape parameter
lambda = 1		# note that this is the rate paramter = 1/scale paramter

x = seq(0.01,20,0.01)
y = dgamma(x,alpha,lambda)

plot(x,y,type='l', main="Gamma(5,1)", ylim=c(0,y.max))

y.max = 2

par(mfrow=c(2,2))

# plot 11

alpha = 2			# shape parameter
lambda = 0.5		# note that this is the rate paramter = 1/scale paramter

x = seq(0.01,10,0.01)
y = dgamma(x,alpha,lambda)

plot(x,y,type='l', main="Gamma(2,0.5)", ylim=c(0,y.max))

# plot 12

alpha = 2			# shape parameter
lambda = 1		# note that this is the rate paramter = 1/scale paramter

x = seq(0.01,10,0.01)
y = dgamma(x,alpha,lambda)

plot(x,y,type='l', main="Gamma(2,1)", ylim=c(0,y.max))

# plot 13

alpha = 2		# shape parameter
lambda = 2		# note that this is the rate paramter = 1/scale paramter

x = seq(0.01,10,0.01)
y = dgamma(x,alpha,lambda)

plot(x,y,type='l', main="Gamma(2,2)", ylim=c(0,y.max))

# plot 14

alpha = 2			# shape parameter
lambda = 3		# note that this is the rate paramter = 1/scale paramter

x = seq(0.01,10,0.01)
y = dgamma(x,alpha,lambda)

plot(x,y,type='l', main="Gamma(2,3)", ylim=c(0,y.max))
