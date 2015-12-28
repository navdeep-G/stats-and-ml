### Monte Carlo Integration I(f) from a to b, CLT

B = 1000

n = 1000
I_f.hat = numeric(B)

a = 0; b = 1;

mu = 0; sigma = 1

for(i in 1:B){

x = runif(n,min=a,max=b)

I_f.hat[i] = (b-a)*mean(dnorm(x,mean=mu,sd=sigma))
I_f.hat[i]

}

# Check

I_f = pnorm(b,mean=mu,sd=sigma) - pnorm(a,mean=mu,sd=sigma)
I_f

# Convergence in distribution plot

X11()
hist(I_f.hat)

I_f.mean = mean(I_f.hat)
I_f.mean
I_f.se = sd(I_f.hat)
I_f.se

I_f.hat.ci = c(I_f.mean - 1.96*I_f.se, I_f.mean + 1.96*I_f.se) 
I_f.hat.ci

quantile(I_f.hat,probs=c(0.025,0.975))



