### This R script simulates flipping a fair coin 1000 times.
### Three examples of using simulation are given using various 
### R commands.  Finally, the CLT is demonstrated.

# Example 1. Simulate flipping a fair coin n times.

n = 1000
p = 0.50

h = 0 	# counter for heads

for (i in 1:n){	
	x = runif(1)
	if (x < p) h = h + 1
}
h	# total number of heads simulated

ph = h/n
ph	# simulated estimate of the probability of heads

# Example 2. Simulate flipping a fair coin n times.
# Simulate using vectors.

x = runif(n)	# creates a vector x of random uniform values

length(x)

x[1:10] # prints the first 10 values of the vector x

C = x[x < p]	# puts all values of x that are less than p into a vector C
			
h = length(C)	# gives the number of heads
h

ph = h/n
ph

# Example 3. Recall that flipping a fair coin n times is a binomial experiment
 
h = rbinom(1,n,p)
h

ph = h/n
ph

# Example 4. Now simulate flipping a fair coin n times, but do it 100,000 
# times and then plot plot the sampling distribution of the estimated 
# probability of heads.  What does the CLT say about the distribution?  
# Ans: approximately N( p, sqrt((p*(1-p))/n) )

Reps = 100000

h = rbinom(Reps,n,p) # this produces a vector of length Reps

ph = h/n	# this uses the vector division elementwise

ph.min = min(ph)		# these commands find the range of values of ph
ph.max = max(ph)

hist(ph, prob=T, xlim=c(ph.min,ph.max), main="Sampling distribution of the probability of heads")

par(new=T) # resets the plot so another plot can be made of the same graph sheet

x = seq(ph.min, ph.max, 0.001)

p.mean = p
p.sd = sqrt((p*(1-p))/n)

y = dnorm(x, p.mean, p.sd)

plot(x, y, type="l")

####################################################################

### This is an R program that simulates a system of 
### n components connected in series.  The probability that
### a component fails is p.  The probability that
### the system fails is approximated using simulation.

p = 0.25 
n = 4

Reps = 10000

Count2 = 0	# Counter for number of failing systems in the overall simulation
for (j in 1:Reps){
	Count1 = 0	# Counter for number of failures in a simulated system
	for (i in 1:n){
		ci = runif(1)
		if(ci < p) Count1 = Count1 + 1
	}
	if (Count1 > 0) Count2 = Count2 + 1
}

psf = Count2/Reps
psf

psf.truth = 1-(1-p)**n
psf.truth

### Alternative solution, using R

c = rbinom(Reps,n,p)
Count = c[c>0]
psf = length(Count)/Reps
psf

psf.truth

### Exercise.  Write and R program that simulates a system of 
### n components connected in parallel. The probability that
### a component fails is p.  

####################################################################

### This script plots the normal density four times with different 
### parameter values.

par(mfrow=c(2,2)) # makes a grid of plots

# plot 1

mu = 0	# standard normal
sigma = 1

x.min1 = mu - 3*sigma
x.max1 = mu + 3*sigma

x1 = seq(x.min1,x.max1,0.01)
y1 = dnorm(x1,mu,sigma)

plot(x1,y1,type="l", main="Normal(0,1)")

# plot 2

mu = 1	# standard normal
sigma = 1

x.min2 = mu - 3*sigma
x.max2 = mu + 3*sigma

x2 = seq(x.min2,x.max2,0.01)
y2 = dnorm(x2,mu,sigma)

plot(x2,y2,type="l", main="Normal(1,1)")

# plot 3

mu = 1	# standard normal
sigma = 2

x.min3 = mu - 3*sigma
x.max3 = mu + 3*sigma

x3 = seq(x.min3,x.max3,0.01)
y3 = dnorm(x3,mu,sigma)

plot(x3,y3,type="l", main="Normal(1,2)")

# plot 4, plot all densities on the same plot

x.min = min(x.min1,x.min2,x.min3)
x.max = max(x.max2,x.max2,x.max3)

y.max = max(y1,y2,y3)

plot(x1,y1,type="l",lty=1,xlim=c(x.min,x.max),ylim=c(0,y.max),main="Normal Densities",xlab="x",ylab="y")
par(new=T)
plot(x2,y2,type="l",lty=2,xlim=c(x.min,x.max),ylim=c(0,y.max),xlab="x",ylab="y")
par(new=T)
plot(x3,y3,type="l",lty=3,xlim=c(x.min,x.max),ylim=c(0,y.max),xlab="x",ylab="y")
  
