### Illustration of the linear congruential method.

#Develop the algorithm to run the gnerator.

a = 5
c = 7
m = 8

n = 20	# number of random values to be generated

x = numeric(n)

x[1] = 4		# seed or x_0

for (i in 2:n){
	x[i] = a*x[i-1]+c
	x[i] = x[i] - trunc(x[i]/m)*m		# this calculates the remainder, i.e., calculates the mod
}
x

#############################################################################################################

# A function that generates psudo random uniform values using the linear congruential method.

RANU = function(seed,n) {
	# a,c,m are the values need for the linear congruential method.
	# seed is the value of the seed for the random number generator.
	# n is the length of the vector returned.

	a = 65539
	c = 0
	m = 2**31

	x = numeric(n)		# creates the output vecotor of length n
	x[1] = seed
	for (i in 2:n){
		x[i] = a*x[i-1]+c
		x[i] = x[i] - trunc(x[i]/m)*m		# this calculates the remainder, i.e., calculates the mod
	}
	x = x/m						# dividing by m gives values between [0,1)
	return(x)
}

# A function that generates psudo random exponential values using the inverse cdf method.

RANDEXPO = function(lam,seed,n){
	# lam is the scale parameter in the density. f(x) = lam*exp(-lam*x) x >= 0
	# seed is the value of the seed for the random number generator.
	# n is the length of the vector returned.
	
    u = RANU(seed,n)		# creates a vector of random uniform values
    x = -log(u)/lam			# creates the output vecotor of length n
	return(x)
}

### Illustration of the RANU.

seed = 4

n = 10000		# number of random values to be generated

u = RANU(seed,n)

# plot a histogram

br = seq(0,1,0.1)

hist(u,main='histogram of random uniform values',probability=T,breaks=br)

# make a scatter plot of pairs of values.

x = u[1:trunc(n/2)]
y = u[(trunc(n/2)+1):n]

plot(x,y,main='scatter plot of pairs of random uniform values')

### Illustration of RANDEXPO.

lambda = 1

e = RANDEXPO( lambda,seed,n)

hist(e,main='histogram of random exponential values',probability=T)

plot(e)
