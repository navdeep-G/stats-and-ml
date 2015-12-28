### Monte Carlo Integration I(f), f is N(mu,sigma^2)
### from a to b, using Unif(a,b), LLN

B = 1000

n = numeric(B)
I_f.hat = numeric(B)

# Limits of integration

a = 0; b = 1;

# Integrate the N(mu,sigma^2) from (a,b)

mu = 0; sigma = 1

# Monte Carlo Integration

for(i in 1:B){
	n[i] = 1000*i
	x = runif(n[i],min=a,max=b)
	
	I_f.hat[i] = (b-a)*mean(dnorm(x,mean=mu,sd=sigma))
	I_f.hat[i]
}

# Check "true" value of the area under the nromal from (a,b)

I_f = pnorm(b,mean=mu,sd=sigma) - pnorm(a,mean=mu,sd=sigma)

# Or using integrate() function in R

integrate(dnorm,0,1)

# Error

eps = abs(I_f.hat - I_f)

error = (max(I_f.hat)-min(I_f.hat))/2

# Convergence in probability plot

X11()
plot(n,I_f.hat,type="l",ylim=c(I_f-error,I_f+error))
abline(I_f,0)

# Error plot

X11()
plot(n,eps,type="l")


