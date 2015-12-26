#Particle Counter
#A particle counter is imperfect and independently detects each incoming particle with probability p. If the
#distribution of the number of incoming particles in a unit of time is a Poisson distribution with parameter
#λ, what is the distribution of the number of counted particles?
#Let N = # of incoming particles and X = # counted.
#1. What is P(X = k|N = n) = ?
#2. What is P(N = n)?
#3. Compute P(X = k).
#Is the conditional probability a regression?

###Imperfect particle counter

lam = 10
p = 0.9

#Probability distribution
len = 5
z = matrix(0,1+len,1+len)

for(i in 0:len){
       for(j in 0:i){
               z[1+j,1+i] = dbinom(j,size=i,prob=p)
       }
}
z

z.sum = apply(z,2,sum)
z.sum

x.mean=0
for(i in 1:len){
       x.mean = c(x.mean,i*p)
}
x.mean

for(i in 0:len){
       X11()
       plot(z[,1+i],type="h")
}


# simulation, how to estimate p using linear regression through the
# origin

B = 1000000

n = rpois(B,lam)

x = rbinom(n=B,size=n,prob=p)

plot(n,x,main="Counts detected vs Counts emitted, with E[X|N]")

x.fit = lm(x ~ 0+n)
summary(x.fit)
abline(x.fit)

# Note that the estimated regression slope is very close to the true p
