# This R program transforms independent BVN to (X1,X2) correlated BVN (W1,W2)

n = 2000

# The goal is to simulate the the 

muw1 = 10
muw2 = 25

sigmaw1 = 2
sigmaw2 = 3

sigmasqw1 = sigmaw1^2
sigmasqw2 = sigmaw2^2

rhow1w2 = -0.4
covw1w2 = rhow1w2*sigmaw1*sigmaw2

# plot the BVN

mu1<-muw1 # setting the expected value of x1
mu2<-muw2 # setting the expected value of x2
s11<-sigmasqw1 # setting the variance of x1
s12<-covw1w2 # setting the covariance between x1 and x2
s22<-sigmasqw2 # setting the variance of x2
rho<-rhow1w2 # setting the correlation coefficient between x1 and x2
x1<-seq(mu1-10,mu1+10,length=41) # generating the vector series x1 
x2<-seq(mu2-10,mu2+10,length=41) 

f<-function(x1,x2){
	term1 <- 1/(2*pi*sqrt(s11*s22*(1-rho^2))) 
	term2 <- -1/(2*(1-rho^2))
	term3 <- (x1-mu1)^2/s11
	term4 <- (x2-mu2)^2/s22
	term5 <- -2*rho*((x1-mu1)*(x2-mu2))/(sqrt(s11)*sqrt(s22)) 
	term1*exp(term2*(term3+term4-term5)) 
} # setting up the function of the multivariate normal density

z<-outer(x1,x2,f) # calculating the density values 

persp(x1, x2, z, 
      main="Two dimensional Normal Distribution",
      sub=expression(italic(f)~(bold(x))==frac(1,2~pi~sqrt(sigma[11]~ 
                     sigma[22]~(1-rho^2)))~phantom(0)~exp~bgroup("{", 
	             list(-frac(1,2(1-rho^2)), 
	             bgroup("[", frac((x[1]~-~mu[1])^2, sigma[11])~-~2~rho~frac(x[1]~-~mu[1],
	             sqrt(sigma[11]))~ frac(x[2]~-~mu[2],sqrt(sigma[22]))~+~ 
	             frac((x[2]~-~mu[2])^2, sigma[22]),"]")),"}")),
      col="lightgreen", 
      theta=30, phi=20,
      r=50,
      d=0.1,
      expand=0.5,
      ltheta=90, lphi=180,
      shade=0.75,
      ticktype="detailed",
      nticks=5) # produces the 3-D plot

# adding a text line to the graph
mtext(expression(list(mu[1]==10,mu[2]==25,sigma[11]==2,sigma[22]==3,sigma[12]==-2.4,rho==-0.4)), side=3) 

# simulate independent random uniforms

u1 = runif(n)
u2 = runif(n)

X11()
par(mfrow=c(2,2))
hist(u1);hist(u2);plot(u1,u2)

umean = c(mean(u1),mean(u2));umean
usd = c(sd(u1),sd(u2));usd
ucor = cor(u1,u2);ucor

# simulate independent random normals - Box-Muller Method

x1 = sqrt(-2*log(u1))*cos(2*pi*u2)
x2 = sqrt(-2*log(u1))*sin(2*pi*u2)

X11()
par(mfrow=c(2,2))
hist(x1);hist(x2);plot(x1,x2)

xmean = c(mean(x1),mean(x2));xmean
xsd = c(sd(x1),sd(x2));xsd
xcor = cor(x1,x2);xcor

# trasnsform to correlated normals BVN(0,0,sigmasqw1,sigmassqw2,rho)

c11 = sigmaw1
c21 = rhow1w2*sigmaw2
c22 = sigmaw2*sqrt(1-rhow1w2^2)

w1 = c11*x1
w2 = c21*x1 + c22*x2

X11()
par(mfrow=c(2,2))
hist(w1);hist(w2);plot(w1,w2)

wmean = c(mean(w1),mean(w2));wmean
wsd = c(sd(w1),sd(w2));wsd
wcor = cor(w1,w2);wcor

# tansform to add means BVN(muw1,muw2,sigmasqw1,sigmassqw2,rho)

y1 = muw1 + w1
y2 = muw2 + w2

X11()
par(mfrow=c(2,2))
hist(y1);hist(y2);plot(y1,y2)

ymean = c(mean(y1),mean(y2));ymean
ysd = c(sd(y1),sd(y2));ysd
ycor = cor(y1,y2);ycor

# rotate by theta to make independent BVN(muw1,muw2,sigmasqw1,sigmassqw2,0)

theta = 0.5*atan((2*rhow1w2*sigmaw1*sigmaw2)/(sigmasqw1-sigmasqw2))
theta

v1 = y1*cos(theta) + y2*sin(theta)
v2 = -y1*sin(theta) + y2*cos(theta)

X11()
par(mfrow=c(2,2))
hist(v1);hist(v2);plot(v1,v2)

vmean = c(mean(v1),mean(v2));vmean
vsd = c(sd(v1),sd(v2));vsd
vcor = cor(v1,v2);vcor

# using the mvrnorm( ) function from the library MASS

library(MASS)

mu = c(muw1,muw2);mu

Sigma = matrix(c(sigmasqw1,covw1w2,covw1w2,sigmasqw2),2,2);Sigma

aa = mvrnorm(n, mu, Sigma)

X11()
par(mfrow=c(2,2))
hist(aa[,1]);hist(aa[,2]);plot(aa)

aamean = c(mean(aa[,1]),mean(aa[,2]));aamean
aasd = c(sd(aa[,1]),sd(aa[,2]));aasd
aacor = cor(aa[,1],aa[,2]);aacor

###################################################################
# scatterplots for large data

#  install.packages("IDPmisc", repos = "http://cran.cnr.Berkeley.edu")

library(IDPmisc)

X11()
ipairs(matrix(c(u1,u2),n,2))
X11()
ipairs(matrix(c(x1,x2),n,2))
X11()
ipairs(matrix(c(w1,w2),n,2))
X11()
ipairs(matrix(c(y1,y2),n,2))
X11()
ipairs(matrix(c(v1,v2),n,2))
X11()
ipairs(aa)

#  install.packages("gplots", repos = "http://cran.cnr.Berkeley.edu")

library(gplots) 

X11()
par(mfrow=c(2,2))
hist2d(u1,u2, nbins=50, col = c("white",heat.colors(16))) 
box()
hist2d(x1,x2, nbins=50, col = c("white",heat.colors(16)))
box()
hist2d(w1,w2, nbins=50, col = c("white",heat.colors(16)))
box()
hist2d(y1,y2, nbins=50, col = c("white",heat.colors(16)))
box()

