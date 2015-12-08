##############  iris PCA
library(rgl)

d = iris
head(d)
tail(d)

d = d[,1:4]    # get rid of flower type
d.prep = data.prep(d)
d.0    = d.prep$x.0
d.s    = d.prep$std

print(d.prep$mu)
print(d.prep$std.dev)

cov.d.0 = cov(d.0)
cor.d.0 = cor(d.0)

cov.d.s = cov(d.s)
cor.d.s = cor(d.s)

print(cov.d.0)
print(cor.d.0)
print(cov.d.s)
print(cor.d.s)

eig.cov = eigen(cov.d.0)
cov.lam = eig.cov$values
print(cumsum(cov.lam)/sum(cov.lam))

cov.v   = eig.cov$vectors

eig.cor = eigen(cov.d.s)
cor.lam = eig.cor$values
print(cumsum(cor.lam)/sum(cor.lam))

cor.v   = eig.cor$vectors

print(eig.cov)
print(eig.cor)

###  compute the scores
cov.sc  = as.matrix(d.0)%*%cov.v
cor.sc  = as.matrix(d.s)%*%cor.v

#####  plot first dimension
dev.new()
plot(d[1:50,1], col = 'red', main = 'Sepal Length',
     ylim = c(min(d[,1]),max(d[,1])) )
points(d[51:100,1], col = 'blue')
points(d[101:150,1], col = 'green')


dev.new()
plot(cov.sc[1:50,1], col = 'red', main = 'First P.C.',
     ylim = c(min(cov.sc[,1]),max(cov.sc[,1])) )
points(cov.sc[51:100,1], col = 'blue')
points(cov.sc[101:150,1], col = 'green')

######  plot second dimension
dev.new()
plot(d[1:50,2], col = 'red', main = 'Sepal Width',
     ylim = c(min(d[,2]),max(d[,2])) )
points(d[51:100,2], col = 'blue')
points(d[101:150,2], col = 'green')


dev.new()
plot(cov.sc[1:50,2], col = 'red', main = 'Second P.C.',
     ylim = c(min(cov.sc[,2]),max(cov.sc[,2])) )
points(cov.sc[51:100,2], col = 'blue')
points(cov.sc[101:150,2], col = 'green')



######  plot third dimension
dev.new()
plot(d[1:50,3], col = 'red', main = 'Petal Length',
     ylim = c(min(d[,3]),max(d[,3])) )
points(d[51:100,3], col = 'blue')
points(d[101:150,3], col = 'green')


dev.new()
plot(cov.sc[1:50,3], col = 'red', main = 'Third P.C.',
     ylim = c(min(cov.sc[,3]),max(cov.sc[,3])) )
points(cov.sc[51:100,3], col = 'blue')
points(cov.sc[101:150,3], col = 'green')


######  plot fourth dimension
dev.new()
plot(d[1:50,4], col = 'red', main = 'Petal Width',
     ylim = c(min(d[,4]),max(d[,4])) )
points(d[51:100,4], col = 'blue')
points(d[101:150,4], col = 'green')


dev.new()
plot(cov.sc[1:50,4], col = 'red', main = 'Fourth P.C.',
     ylim = c(min(cov.sc[,4]),max(cov.sc[,4])) )
points(cov.sc[51:100,4], col = 'blue')
points(cov.sc[101:150,4], col = 'green')

######## plot petal length vs. petal width

dev.new()
plot(d[1:50,3],d[1:50,4], col = 'red', main = 'Length vs. Width',
     xlim = c(min(d[,3]), max(d[,3]) ),
     ylim = c(min(d[,4]),max(d[,4])) )
points(d[51:100,3],d[51:100,4], col = 'blue')
points(d[101:150,3],d[101:150,4], col = 'green')

#########  plot first two P.C.s

dev.new()
plot(cov.sc[1:50,1],cov.sc[1:50,2], col = 'red', main = 'PC 1 vs. PC 2',
     xlim = c(min(cov.sc[,1]), max(cov.sc[,1]) ),
     ylim = c(min(cov.sc[,2]),max(cov.sc[,2])) )
points(cov.sc[51:100,1],cov.sc[51:100,2], col = 'blue')
points(cov.sc[101:150,1],cov.sc[101:150,2], col = 'green')


open3d()
m1 = min(cov.sc)
m2 = max(cov.sc)
plot3d(cov.sc[1:50,1:3],xlim = c(m1,m2), ylim = c(m1,m2), zlim = c(m1,m2),
        col = 'red' )
points3d(cov.sc[51:100,1:3], col = 'blue')
points3d(cov.sc[101:150,1:3], col = 'green')

#####  loadings

L = cov.v%*%diag(sqrt(cov.lam))
print(L)

L.sq = L*L
print(L.sq)

row.var = matrix( rep(0,16), ncol = 4)
for (i in 1:4) { row.var[i,] = cumsum(L.sq[i,]) }

print(cov.d.0)

###### use princomp
pr = princomp(d)
head(pr$scores)

head(cov.sc)

cov.v   # eigenvectors of covariance matrix
names(pr)
pr$loadings








