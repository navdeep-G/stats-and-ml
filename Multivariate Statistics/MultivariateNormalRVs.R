#### Script for Multivariate Normal Random Variables

library(rgl)    # for 3-D plots
library(mvtnorm) # generate MVN distribution

set.seed(200) # for reproducibility

#    X will be a vector of 3 univariate normal variables

A = matrix(runif(9),ncol=3)
S = t(A) %*% A

eigen(S)$values

# one eigenvalue is very close to 0.
# so make them bigger
S = S + diag(c(1,1,1)) # add 1 to diagonal elements
                       # eigenvalues now are increased by 1
print(S)

# For fun, let's have some negative covariance
S[2,3] = -0.6
S[3,2] = -0.6
print(eigen(S))
# S is our covariance matrix

x = rmvnorm(10000,c(1,3,2),S)

# x has 10,000 vectors of size 3. The mean vector is (1,3,2)
# and S is the covariance matrix

dim(x)

x.df = data.frame(x)
dev.new()
plot(x.df)

###   find mean and variance for individual R.V.s
mu.x1  = mean(x[,1])
var.x1 = var(x[,1])
mu.x2  = mean(x[,2])
var.x2 = var(x[,2])
mu.x3  = mean(x[,3])
var.x3 = var(x[,3])
sprintf("means are: %6.4f %6.4f %6.4f",mu.x1,mu.x2,mu.x3)
sprintf("vars  are: %6.4f %6.4f %6.4f",var.x1,var.x2,var.x3)

S.pop    = S     # save S as the population covariance matrix

######### CAUTION ###########################
# if x were 3x10,000 instead of 10,000x3 we should not
# use the cov() or cor() functions. It will attempt to compute a
# 10,000x10,000 matrix. This will be painful for you. Your computer
# will quickly white-out and die.

S.sample = cov(x)
print(S.sample)
print(S.pop)

# sample S is close to population S

# plot x. use mouse to reorient the data
plot3d(x)

##################  Cholesky Transform ##########################
#   transform x to y, where z has 3  standard normal variables

# We use S.sample as our S

# find the cholesky decomposition
V = chol(S.sample)
print(V)           # V is upper triangle

print( t(V) %*%V - S.sample)
#  looks like a good factorization

# remove mean from x
x.0 = x - matrix( rep(c(mu.x1,mu.x2,mu.x3),10000),ncol=3,byrow=T)
mean(x.0[,1])
mean(x.0[,2])
mean(x.0[,3])
# we have verified the mean is (0,0,0)

# 
# V is 3x3, x.0 is 10000x3
# We want y to be 10000x3
V.t = solve(t(V))
y = t(  V.t%*%t(x.0))    #### y is the transformed MVN. It has 3 independent variables
print(dim(y))
cov(y)
y.S = cov(y)
mu.y1  = mean(y[,1])
var.y1 = var(y[,1])
mu.y2  = mean(y[,2])
var.y2 = var(y[,2])
mu.y3  = mean(y[,3])
var.y3 = var(y[,3])
sprintf("means are: %6.4f %6.4f %6.4f",mu.y1,mu.y2,mu.y3)
sprintf("vars  are: %6.4f %6.4f %6.4f",var.y1,var.y2,var.y3)
## means and variances as expected
colnames(y) = c('Y1','Y2','Y3')
y.df = data.frame(y)
dev.new()
plot(y.df)

###  3d plot
plot3d(y)


############### Eigendecompostion Transform #####################

eig.decomp = eigen(S.sample)
print(eig.decomp$values)
print(t(eig.decomp$vectors) %*% eig.decomp$vectors)
# eigenvectors are orthonormal
D.inv.sqrt = diag(1/sqrt(eig.decomp$values))
v.t        =   t(eig.decomp$vectors)

###  Now transform x to z

###   Recall that x.0 is the original x AFTER removing the mean of x
z = t(D.inv.sqrt%*%v.t%*%t(x.0))
print(dim(z))
cov(z)
mu.z1  = mean(z[,1])
var.z1 = var(z[,1])
mu.z2  = mean(z[,2])
var.z2 = var(z[,2])
mu.z3  = mean(z[,3])
var.z3 = var(z[,3])
sprintf("means are: %6.4f %6.4f %6.4f",mu.z1,mu.z2,mu.z3)
sprintf("vars  are: %6.4f %6.4f %6.4f",var.z1,var.z2,var.z3)

colnames(z) = c('Z1','Z2','Z3')
z.df = data.frame(z)
dev.new()
plot(z.df)

plot3d(z)

######   decorrelate x only
######   u1,u2,u3 will be independent, but their variances equal
######   the eigenvalues of S.
######   You can then scale u1,u2,u3 to get any desired set of variances

u = t(v.t%*%t(x.0))
print(dim(u))

print(cov(u))

#### by the way, to do a correlaion
print(cor(u))

mu.u1  = mean(u[,1])
var.u1 = var(u[,1])
mu.u2  = mean(u[,2])
var.u2 = var(u[,2])
mu.u3  = mean(u[,3])
var.u3 = var(u[,3])
sprintf("means are: %6.4f %6.4f %6.4f",mu.u1,mu.u2,mu.u3)
sprintf("vars  are: %6.4f %6.4f %6.4f",var.u1,var.u2,var.u3)

colnames(u) = c('U1','U2','U3')
u.df = data.frame(u)
dev.new()
plot(u.df, xlim=c(-6,6),ylim=c(-6,6) )

plot3d(u, xlim = c(-6,6),ylim = c(-6,6), zlim = c(-6,6))

######### Note: we could generate x with more variables, and
#               everything would work except plot3d. Even there
#               you can select 3 variables at a time for plot3d   



####################################################################################
#
#
####################################################################################

# define an auxiliary function
dprod <- function(u,v)
{ # dotprod
  stopifnot(is.vector(u))
  stopifnot(is.vector(v))
  stopifnot(length(u) == length(v))
  return(sum(u*v))
} # dprod


S = matrix(c(6,3,-1,3,4,2,-1,2,3),ncol=3)
mu = c(1,2,3)
set.seed(50)

#   generate 10,000 multivariate normal random vectors using S as the
#   covariance matrix and mu as the expected value of the random vector.
#   Find an expression for a alpha% confidence interval for the random vector.
#   Use the given set.seed to generate the random vectors.

#   Use 80%, 90%, 95%, and 99% for alpha.

#   How many of the vectors are in the confidence interval?

n = 10000
x = rmvnorm(n,mu,S)
e = eigen(S)
S.inv.1 = (e$vectors)%*%diag(1/e$values)%*%t(e$vectors) # one way
print(S.inv.1)

S.inv = solve(S)  # another way
print(S.inv) 

d = numeric(10000)  # d is mapped from x using the standardizing transform 
for ( i in 1:n)
   {
      v=x[i,]-mu
      d[i] = t(v)%*%S.inv%*%v
   }

d            = sort(d)
v            = c(0.5,0.6,0.7,0.8,0.9,0.95,0.99)
num          = numeric(length(v))
expected.num = numeric(length(v))
for ( i in 1:length(v) )
{
   k   = qchisq(v[i],3)
   num[i] = length(d[d< k]) 
   expected.num[i] = v[i]*n
}

print(num)
print(expected.num) 


in.90 = numeric(10000)
out.90 = numeric(10000)
in.count = 0
out.count = 0
k = qchisq(0.90,3)
for ( i in 1:n)
   {
      v=x[i,]-mu
      d[i] = t(v)%*%S.inv%*%v
      if ( d[i] <= k )
        {
          in.count          = in.count + 1
          in.90[in.count]   = i
        }
      else
        {
          out.count         = out.count + 1
          out.90[out.count] = i
        }
   }
in.90 = in.90[1:in.count]
out.90 = out.90[1:out.count]

###  now in.90 has the indices where x is inside the ellipsoid containg qchisq(0.90,3) of
###  the probability of x. out.90 is similar, but its indices are to points of x outside
###  the ellipsoid

#####  Note the location of the "in" and "out" points for x
plot3d(x[out.90,], col='blue')

##### pause here to view only the out points

points3d(x[in.90,], col = 'red')

###   remove mean from x.in points thst are "inside"
x.in = x[in.90,] -matrix( rep(mu,in.count),ncol=3,byrow = T)

###   use a standardizing transform
z.in = t(e$vectors%*%diag(1/sqrt(e$values))%*%t(e$vectors)%*%t(x.in))
dim(z.in)

mag.z.in = numeric(in.count)
for ( i in 1:in.count )
  {
    mag.z.in[i] = (dprod(z.in[i,],z.in[i,]))
  }
max(mag.z.in) #### largest magnitude of an "inside" point
k             #### the boundary for inside and outside. qchisq(0.90,3)

####
x.out = x[out.90,] -matrix( rep(mu,out.count),ncol=3,byrow = T)
z.out = t(e$vectors%*%diag(1/sqrt(e$values))%*%t(e$vectors)%*%t(x.out))
dim(z.out)

mag.z.out = numeric(out.count)
for ( i in 1:out.count )
  {
    mag.z.out[i] = (dprod(z.out[i,],z.out[i,]))
  }
min(mag.z.out) 
k

plot3d(z.out,col = 'blue')
points3d(z.in,col = 'red')






