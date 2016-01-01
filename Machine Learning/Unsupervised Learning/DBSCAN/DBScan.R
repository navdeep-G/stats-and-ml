#########################################################################
#          DBSCAN Clustering
##########################################################################

##   this code will create four clusters of data

set.seed(100)
num.c  = 4
n.low  = 12
n.high = 100
n      = runif(num.c,n.low,n.high)
n      = trunc(n)                  # force it to be an interger
print(n)

for ( i in 1:num.c)
{ # begin for
  s= matrix( rep(0,4), ncol = 2)
  s[1,1] = runif(1,1,4)
  s[2,2] = runif(1,1,3)
  s[1,2] = s[2,1] = runif(1,-1,1)
  mu     = runif(2,-30,30)
  x      = rmvnorm(n[i],mu,s)
  rownames(x) = rep(i,n[i])
  if ( i == 1 ) 
  { data = x }
  else
  { data = rbind(data,x) }
} #end for

###   look at a plot of the data
dev.new()
plot(data)


## find approriate vslues for eps and MinPts to find the four clusters.
## Call dbscan with showplot = 1

#Solution:

#The eps is 4 and MinPts is 6

dev.new()
cl.db = dbscan(data, eps = 4, MinPts = 6, method = 'hybrid',showplot = 1)


