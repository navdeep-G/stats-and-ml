library(mvtnorm)
library(rgl)
rho= 0.3;  S = matrix(c(1,rho,rho,1),ncol=2)
eigen(S)


#######   Principle components on 2 dimensional MVN
rmvnorm(2000,c(0,0),S);plot(z)

rho= 0.7;  S = matrix(c(1,rho,rho,1),ncol=2)

mv2.cor <- function(rho)
{
  S = matrix(c(1,rho,rho,1),ncol=2)
  z = rmvnorm(2000,c(0,0),S);plot(z)
  d = eigen(S)
  v1 = d$vectors[,1]
  v2 = d$vectors[,2]
  m1 = min(z)
  m2 = max(z)
  plot(z, xlim=c(m1,m2), ylim = c(m1,m2))
  abline(h = 0); abline(v = 0)
  points(rbind(t(v1),t(v2)), pch=3,col = 'red')
  lines(c(-10*v1[1],10*v1[1]),c(-10*v1[2],10*v1[2]), col = 'red')
  lines(c(-10*v2[1],10*v2[1]),c(-10*v2[2],10*v2[2]), col = 'blue')
  print(d$values)
}

mv2.cor(0.8)

###### Note: eigenvectors of correlation matrices are ALWAYS in the
###### (1,1) and (1,-1) direction (that is 45 and -45 degrees)
###### This remark applies only to 2x2 correlation matrices
 
mv2.cov <- function(s1,rho,s2)
{
  S = matrix(c(s1,rho,rho,s2),ncol=2)
  z = rmvnorm(2000,c(0,0),S);
  d = eigen(S)
  v1 = d$vectors[,1]
  v2 = d$vectors[,2]
  m1 = min(z)
  m2 = max(z)
  plot(z, xlim=c(m1,m2), ylim = c(m1,m2))
  abline(h = 0); abline(v = 0)
  points(rbind(t(v1),t(v2)), pch=3,col = 'red')
  lines(c(-10*v1[1],10*v1[1]),c(-10*v1[2],10*v1[2]), col = 'red')
  lines(c(-10*v2[1],10*v2[1]),c(-10*v2[2],10*v2[2]), col = 'blue')
  print(d$values)
}
mv2.cov(8,2,3)

#### In the 2 variable case, the principal component direction may differ 
#### from 45 and -45 degrees when we use the covariance matrix

#S3 = matrix(c(5,1,-2,1,8,1,-2,1,6),ncol=3)
 
mv3.cov <- function(S3, sample = T)
{
  n  = 12000
  x3 = rmvnorm(n,c(0,0,0),S3)
  mu = apply(x3,2,mean) # find column means
  x3 = x3 - matrix(rep(mu,n),ncol = 3,byrow = T)
  if ( sample == T )
    {
       d = eigen(cov(x3))    # Use sample covariance matrix
       print("Sample covariance matrix")
       print(cov(x3))
    }
  else
    {
       d  = eigen(S3) # Uses population covariance matrix
    }    
  print('eigenvalues = variance of PC')
  print(d$values)
  print('SD of PC')
  print(sqrt(d$values))
  print('sum of eigenvalues')
  print(sum(d$values))
  print('trace of covariance matrix')
  if ( sample == TRUE )
    { print( sum( diag(cov(x3)) ) ) }
  else
    { print(sum(diag(S3))) }

  v1 = d$vectors[,1]
  v2 = d$vectors[,2]
  v3 = d$vectors[,3]
  print("principal components")
  print(d$vectors)
  m1 = min(x3)
  m2 = max(x3)
  colnames(x3) = c('x','y','z')
  plot3d(x3, xlim=c(m1,m2), ylim = c(m1,m2), zlim = c(m1,m2))
  eig.1 = matrix(40*c(-v1[1],-v1[2],-v1[3],v1[1],v1[2],v1[3]),ncol=3,
               byrow=T)
lines3d(eig.1, col = 'red')
eig.2 = matrix(20*c(-v2[1],-v2[2],-v2[3],v2[1],v2[2],v2[3]),ncol=3,
               byrow=T)
lines3d(eig.2, col = 'blue')
eig.3 = matrix(20*c(-v3[1],-v3[2],-v3[3],v3[1],v3[2],v3[3]),ncol=3,
               byrow=T)
lines3d(eig.3, col = 'green')
if (0) {
xy.plane = matrix(c(-m2,0,0,
                     0, m2,0,
                     m2, 0, 0,
                     0,-m2,0,
                    -m2,0,0),
           ncol=3,byrow=T)
lines3d(xy.plane,col='brown3')
lines3d(matrix(c(0,0,-m2,0,0,m2),ncol=3,byrow=T), col = 'brown3') }
return(x3)
}

########## you can repeat this code to see many
########## examples of principal components.
t = matrix(floor(runif(9,-3,8)),ncol=3)
sym = t(t)%*%t
print(sym)
sym=0.6*sym
print(sym)

####### Use the sym matrix to generate a sample
x3 = mv3.cov(sym, sample = T)

eig.cov = eigen(cov(x3))
print(eig.cov)

eig.cor = eigen(cor(x3))
print(eig.cor)

#### notice we have different PCA if we use correlation matrix

####  Note the effect of scaling.
x3.s = cbind(12*x3[,1],x3[,2:3])
eigen(cov(x3.s))


scores = x3%*%eig.cov$vectors
sprintf("range of scores along PC 1: %f to %f",
         min(scores[,1]), max(scores[,1]))
sprintf("range of scores along PC 2: %f to %f",
         min(scores[,2]), max(scores[,2]))
sprintf("range of scores along PC 3: %f to %f",
         min(scores[,3]), max(scores[,3]))

hist(x3[,1])        # histogram for X1 variable
dev.new()
hist(scores[,1])    # histogram for PC 1 variable

####   pause here
hist(x3[,2])
dev.new()
hist(scores[,2])

####  pause here
hist(x3[,3])
dev.new()
hist(scores[,3])

#####   scores have principle components in the natural direction
#####   eigenvectors of covarince matrix is I
print(eigen(cov(scores)))

m2     = max(abs(scores)) 
plot3d(scores, xlim = c(-m2,m2), ylim = c(-m2,m2), zlim = c(-m2,m2) )
x.ax = matrix(c(-m2,0,0,m2,0,0),ncol = 3, byrow = T)
lines3d(x.ax,col = 'red')
y.ax = matrix(c(0,-m2,0,0,m2,0),ncol = 3, byrow = T)
lines3d(y.ax,col = 'blue')
z.ax = matrix(c(0,0,-m2,0,0,m2),ncol = 3, byrow = T)
lines3d(z.ax,col = 'green')


plot(scores[,2:3], xlim = c(-m2,m2), ylim = c(-m2,m2) )


####### Loadings for covariance matrix
loading = eig.cov$vectors%*%sqrt(diag(eig.cov$values))
loading.sq = loading*loading
print(loading)

print(loading.sq)
rowsum = apply(loading.sq,1,sum)
print(rowsum)
print(cumsum(loading.sq[1,])/rowsum[1])
print(cumsum(loading.sq[2,])/rowsum[2])
print(cumsum(loading.sq[3,])/rowsum[3])

colsum = apply(loading.sq,2,sum)
print(colsum)
print(cumsum(colsum)/sum(colsum))

####  compare to sample covariance matrix
print(cov(x3))
print(eigen(cov(x3))$values)

#### plot scores as Y
par(mfrow=c(3,3))
plot.lim = max(abs(cbind(x3,scores)))
plot(x3[,1],scores[,1], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X1 and Y1')
plot(x3[,1],scores[,2], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X1 and Y2')
plot(x3[,1],scores[,3], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X1 and Y3')
plot(x3[,2],scores[,1], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X2 and Y1')
plot(x3[,2],scores[,2], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X2 and Y2')
plot(x3[,2],scores[,3], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X2 and Y3')
plot(x3[,3],scores[,1], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X3 and Y1')
plot(x3[,3],scores[,2], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X3 and Y2')
plot(x3[,3],scores[,3], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X3 and Y3')

print(loading)

#####   Plot X vs. X
dev.new()
par(mfrow=c(3,3))
plot.lim = max(abs(x3))
plot(x3[,1],x3[,1], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X1 and X1')
plot(x3[,1],x3[,2], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X1 and X2')
plot(x3[,1],x3[,3], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X1 and X3')
plot(x3[,2],x3[,1], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X2 and X1')
plot(x3[,2],x3[,2], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X2 and X2')
plot(x3[,2],x3[,3], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X2 and X3')
plot(x3[,3],x3[,1], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X3 and X1')
plot(x3[,3],x3[,2], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X3 and X2')
plot(x3[,3],x3[,3], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'X3 and X3')


#####   Plot Y vs. Y
y3 = scores           # easier name
dev.new()
par(mfrow=c(3,3))
plot.lim = max(abs(y3))
plot(y3[,1],y3[,1], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'Y1 and Y1')
plot(y3[,1],y3[,2], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'Y1 and Y2')
plot(y3[,1],y3[,3], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'Y1 and Y3')
plot(y3[,2],y3[,1], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'Y2 and Y1')
plot(y3[,2],y3[,2], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'Y2 and Y2')
plot(y3[,2],y3[,3], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'Y2 and Y3')
plot(y3[,3],y3[,1], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'Y3 and Y1')
plot(y3[,3],y3[,2], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'Y3 and Y2')
plot(y3[,3],y3[,3], xlim = c(-plot.lim,plot.lim), ylim = c(-plot.lim,plot.lim), main = 'Y3 and Y3')


graphics.off()   # kill the plots

########## Loadings for correlation matrix
std = sqrt(diag(cov(x3)))
loading.std = diag(1/std)%*%eig.cov$vectors%*%sqrt(diag(eig.cov$values))
print(loading.std)

loading.std.sq = loading.std*loading.std
print(loading.std.sq)

#######  Compare the loading.std.sq matrix with the X vs. Y plots given above

rowsum = apply(loading.std.sq,1,sum)
print(cumsum(loading.std.sq[1,])/rowsum[1])
print(cumsum(loading.std.sq[2,])/rowsum[2])
print(cumsum(loading.std.sq[3,])/rowsum[3])

###############################################################################
######### Distribution of eigenvalues
###############################################################################

num        = 500
sample.eig = matrix(rep(0,3*num),ncol = 3)
for ( i in 1:num )
  {
     temp           = rmvnorm(400,c(0,0,0),sym) # 400 values
     sample.eig[i,] = eigen(cov(temp))$values
  }
eigen(sym)$values
eig.mu = apply(sample.eig,2,mean)
print(eig.mu)
apply(sample.eig,2,var)
#Theoretical variance is 2*lambda^2/400
print(2*eigen(sym)$values^2/400)

ci.l = eig.mu/(1+qnorm(0.975)*sqrt(2/400)) 
ci.h = eig.mu/(1-qnorm(0.975)*sqrt(2/400)) 
print(ci.l)
print(ci.h)
sum( length(sample.eig[,1][sample.eig[,1] < ci.l[1]]) +
     length(sample.eig[,1][sample.eig[,1] > ci.h[1]]) )
sum( length(sample.eig[,2][sample.eig[,2] < ci.l[2]]) +
     length(sample.eig[,2][sample.eig[,2] > ci.h[2]]) )
sum( length(sample.eig[,3][sample.eig[,3] < ci.l[3]]) +
     length(sample.eig[,3][sample.eig[,3] > ci.h[3]]) )

############################################################################
############  Detect Outliers
#############################################################################
S3 = matrix(c(7,1,-2,1,13,1,-2,1,3),ncol=3)
set.seed(3020);S3 = matrix(runif(9,-4,12),ncol=3); s = 0.5*t(S3)%*%S3;min(eigen(s)$values)

x = rmvnorm(4000,c(0,0,0),s)
open3d();plot3d(x)
err     = runif(3,-1,1)
x[246,] = x[246,] + err
open3d();plot3d(x)

#points3d(x=x[246,1]+0.1,y= x[246,2],z=x[246,3],col= 'red', pch = 4)
#lines3d(matrix(c(0,0,0,t(U[,3]),ncol=3,byrow=T), col = 'blue')
U = eigen(cov(x))$vectors
print(U)
print(eigen(cov(x))$values)

points3d(x=x[246,1],y= x[246,2],z=x[246,3],col= 'red', pch = 2, size = 4)
u3 = rbind(c(0,0,0),10*t(U[,3]))
lines3d(u3, col = 'blue')


Y = x%*%U
sum(err*u3)
dev.new()
par(mfrow=c(3,1))
plot(Y[,3]);abline(v=246)
plot(Y[,2]);abline(v=246)
plot(Y[,1]);abline(v=246)

##### look at data near the perturbed value
print(x[241:250,])

dev.new()
par(mfrow=c(3,1))
plot(x[,3]);abline(v=246)
plot(x[,2]);abline(v=246)
plot(x[,1]);abline(v=246)

mu  = apply(x,2,mean)
x.c = x - matrix(rep(mu,4000),ncol=3,byrow = T)
Sig.inv    = solve(s)
mahal.dist = numeric(4000)
for ( i in 1:4000 )
  {
    mahal.dist[i] = t(x.c[i,])%*%Sig.inv%*%(x.c[i,])
  }
dev.new();plot(mahal.dist, main = 'Mahal');abline(v = 246)


t           = x.c*x.c
euclid.dist = apply(t,1,sum)
dev.new();plot(euclid.dist, main = 'Euclid');abline(v=246)
##

graphics.off()

############################   Malhanobis Distance ##############################


r = 1
theta = seq(0,2*pi, length = 100)
phi   = seq(0,2*pi,  length = 100)
#phi = 0.4
num    = length(theta)*length(phi)
sphere = matrix(rep(0,3*num),ncol = 3)
rownum = 1
for ( i in 1:length(theta) )
   {   # theta ~= i/2pi
     for ( j in 1:length(phi) )
       { # phi ~= j/2pi

         z               = r*sin(phi[j])
         r.sq.xy         = r*r*abs(cos(phi[j]))
         x               = r.sq.xy*cos(theta[i])
         y               = r.sq.xy*sin(theta[i])
         sphere[rownum,] = c(x,y,z)
         rownum          = rownum + 1       

       } # phi ~= j/2pi
   }   # the
open3d()
origin = matrix(c(0,0,0),ncol = 3)
plot3d(origin, col = 'red', xlim = c(-r,r), ylim = c(-r,r), zlim = c(-r,r) ) 
points3d(sphere)

##########  map spherical distance to Malhalonbis distance
s.inv = solve(s)
Mdist = numeric( nrow(sphere) )
   for ( i in 1:length(Mdist) )
     {
       Mdist[i] = sphere[i,]%*%s.inv%*%as.matrix(sphere[i,])
     }
Msphere = cbind( Mdist*sphere[,1], Mdist*sphere[,2], Mdist*sphere[,3] )
m2 = max(Mdist)
open3d()
origin = matrix(c(0,0,0),ncol = 3)
plot3d(origin, col = 'red', xlim = c(-m2,m2), ylim = c(-m2,m2), zlim = c(-m2,m2) )
points3d(Msphere) 


print(max(Mdist))
print(min(Mdist))
print(eigen(s.inv)$values)

v = eigen(s.inv)$vectors
print(t(as.matrix(v[,1]))%*%s.inv%*%as.matrix(v[,1]))
print(t(as.matrix(v[,2]))%*%s.inv%*%as.matrix(v[,2]))
print(t(as.matrix(v[,3]))%*%s.inv%*%as.matrix(v[,3]))

##################  more outlier detection ##########################################
#######################    7 variables   #############################################
#########################################################################################

t = matrix(floor(runif(49,-3,8)),ncol=7)
sym = t(t)%*%t
print(sym)
sym=0.4*sym
print(sym)
e =eigen(sym)
print(e$values)
U = e$vectors

x = rmvnorm(4000,c(rep(0,7)),sym)
x[246,] = x[246,] + c(rep(3,7))

### cheating
#x[246,] = x[246,] + 4*U[,7]
Y      = x%*%U
par(mfrow=c(3,1))
plot(Y[,7]);abline(v=246)
plot(Y[,6]);abline(v=246)
plot(Y[,5]);abline(v=246)

dev.new()
par(mfrow=c(4,1))
plot(x[,7]);abline(v=246)
plot(x[,6]);abline(v=246)
plot(x[,5]);abline(v=246)
plot(x[,4]);abline(v=246)

dev.new()
par(mfrow=c(3,1))
plot(x[,3]);abline(v=246)
plot(x[,2]);abline(v=246)
plot(x[,1]);abline(v=246)

mahal.dist = numeric(4000)
mu = apply(x,2,mean)
x.c = x - matrix(rep(mu,4000),ncol=7,byrow = T)
Sig.inv    = solve(sym)
for ( i in 1:4000 )
  {
    mahal.dist[i] = t(x[i,] - mu)%*%Sig.inv%*%(x[i,] - mu)
  }
dev.new();plot(mahal.dist, main = 'Mahal');abline(v = 246)

t           = x*x
euclid.dist = apply(t,1,sum)
dev.new();plot(euclid.dist, main = 'Euclid');abline(v=246)
##

rank.euclid = length(euclid.dist[ euclid.dist <euclid.dist[246] ]) + 1
sprintf("mahlanobis dist = %f and euclidean dist = %f",
         mahal.dist[246],euclid.dist[246])



graphics.off()


######   Example from Johnson and Wichern text
######   Note: This example comes from an earlier edition of the text.
######   The CSUEB library has this edition

S = matrix(rep(0,25),ncol=5)

S[,1] = c(4.308, 1.683, 1.803, 2.155, -0.253)
S[,2] = c(1.683, 1.768, 0.588, 0.177, 0.176)
S[,3] = c(1.803, 0.588, 0.801, 1.065, -0.158)
S[,4] = c(2.155,0.177, 1.065, 1.970, -0.357)
S[,5] = c(-0.253, 0.176, -0.158, -0.357, 0.504)
eig.S = eigen(S)
std   =sqrt(diag(S))
corr.loading = diag(1/std)%*%eig.S$vectors%*%sqrt(diag(eig.S$values))
print(corr.loading)

##### We get agreement with the text
   
