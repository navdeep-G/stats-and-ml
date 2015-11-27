#Joint p.d.f. example where f(xy) = 3/2x^2(1-|y|),  -1<x<1, -1<y<1
x = seq(-1.2, 1.2, length = 80)
y = seq(-1.2, 1.2, length = 80)
n= length(x)
fxy = matrix(0, nrow = n, ncol = n)

#for (i in 1:length(x)){ for(j in 1:length(y)){ fxy[i,j] = ifelse(x[i] <=1 & y[j] <=1 & x[i] >=0 & y[j]>=0,ifelse(x[i] < y[j], 2,0), 0) } }

for (i in 1:length(x)){ for(j in 1:length(y)){ fxy[i,j] = ifelse(x[i] <=1 & y[j] <=1 & x[i] >=-1 & y[j]>=-1, 3/2*(x[i])^2*(1-abs(y[j])), 0) } }

persp(x,y,fxy, zlim = c(0,1.47), shade = 0.4, border = NA, col='yellow', theta=-30, phi=30, ticktype="detailed")
title(main='fxy = 3/2x^2(1-|y|)')