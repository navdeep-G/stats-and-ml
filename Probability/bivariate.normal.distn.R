### Example of a joint density.

#Note: The graphics below will require that you have XQuartz installed. 

x = seq(0,5,0.2)
y = x
f = function(x,y){ 2*exp(-x)*exp(-2*y) }
z = outer(x,y,f)

#X11()
#par(mfrow=c(2,2))

#persp(z)
#persp(z,eye=c(5,-1,3))
#persp(z,eye=c(-1,5,3))
#persp(z,eye=c(5,5,3))

### Example of plotting the joint normal distribution. 

x = seq(-2,2,0.1)
y = seq(-2,2,0.1)

X11()
par(mfrow=c(2,2))

#3-D Bivariate Normal with different values of the correlation coefficient.
bvn = function(x,y){
  mu1 = 0
  sigma1 = 1
  mu2 = 0
  sigma2 = 1
  rho = 0
  ((2*pi*sigma1*sigma2*sqrt(1-rho^2))^(-1))*
    exp( -(2*((1-rho^2))^(-1)) * ( (x-mu1)^2/sigma1^2 + (y-mu2)^2/sigma2^2
                                   - ( (2*rho*(x-mu1)*(y-mu2) )/ (sigma1*sigma2) ) ) )
}

z1 = outer(x,y,bvn)
zi <- z1[ -1,-1] + z1[ -1,-41] + z1[-41,-1] + z1[-41,-41]  ## / 4

fcol <- matrix(0, nr = nrow(z1)-1, nc = ncol(z1)-1)
fcol <- topo.colors(20)[cut(zi, stats::quantile(zi, seq(0,1, len = 21)), include.lowest = TRUE)]

persp(x, y, z1, theta = -25, r=8, col = fcol, shade = 0.4, border = NA, main = expression(paste(rho,"=0")))
bvn = function(x,y){
  mu1 = 0
  sigma1 = 1
  mu2 = 0
  sigma2 = 1
  rho = 0.3
  ((2*pi*sigma1*sigma2*sqrt(1-rho^2))^(-1))*
    exp( -(2*((1-rho^2))^(-1)) * ( (x-mu1)^2/sigma1^2 + (y-mu2)^2/sigma2^2
                                   - ( (2*rho*(x-mu1)*(y-mu2) )/ (sigma1*sigma2) ) ) )
}

z2 = outer(x,y,bvn)
persp(x, y, z2, theta = -25, r=8, col = fcol, shade = 0.4, border = NA, main = expression(paste(rho,"=0.3")))

bvn = function(x,y){
  mu1 = 0
  sigma1 = 1
  mu2 = 0
  sigma2 = 1
  rho = 0.6
  ((2*pi*sigma1*sigma2*sqrt(1-rho^2))^(-1))*
    exp( -(2*((1-rho^2))^(-1)) * ( (x-mu1)^2/sigma1^2 + (y-mu2)^2/sigma2^2
                                   - ( (2*rho*(x-mu1)*(y-mu2) )/ (sigma1*sigma2) ) ) )
}

z3 = outer(x,y,bvn)
persp(x, y, z3, theta = -25, r=8, col = fcol, shade = 0.4, border = NA, main = expression(paste(rho,"=0.6")))

bvn = function(x,y){
  mu1 = 0
  sigma1 = 1
  mu2 = 0
  sigma2 = 1
  rho = -0.9
  ((2*pi*sigma1*sigma2*sqrt(1-rho^2))^(-1))*
    exp( -(2*(1-rho^2)^(-1)) * ( (x-mu1)^2/sigma1^2 + (y-mu2)^2/sigma2^2
                                 - ( (2*rho*(x-mu1)*(y-mu2) )/ (sigma1*sigma2) ) ) )
}

z4 = outer(x,y,bvn)
persp(x, y, z4, theta = -25, r=8, d = .1, col = fcol, shade = 0.4, border = NA, main = expression(paste(rho,"=-0.9")))
X11()
par(mfrow=c(2,2))

contour(x,y,z1, main = expression(paste(rho,"=0")))
contour(x,y,z2, main = expression(paste(rho,"=0.3")))
contour(x,y,z3, main = expression(paste(rho,"=0.6")))
contour(x,y,z4, main = expression(paste(rho,"=-0.9")))

X11()
filled.contour(x,y,z1, color = heat.colors, main = expression(paste(rho,"=0")))
X11()
filled.contour(x,y,z2, color = heat.colors, main = expression(paste(rho,"=0.3")))
X11()
filled.contour(x,y,z3, color = heat.colors, main = expression(paste(rho,"=0.6")))
X11()
filled.contour(x,y,z4, color = heat.colors, main = expression(paste(rho,"=-0.9")))



