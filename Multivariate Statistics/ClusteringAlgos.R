###### Using hclust,plclust,cutree

require(rgl)
require(mvtnorm)
require(fpc)      # for dbscan

xy = swiss[,2:3]  # grab two columns of swiss

##   make a plot with row names placed where data points are
dev.new()
plot(xy, type = 'n')
text(xy, label = rownames(xy))

d.xy   = dist(xy)       # need a dist() object for hclust()

###    cluster with complete
hcl.xy = hclust(d.xy)   # method = 'complete' by default

dev.new()
plot(hcl.xy)   # see the dendogram

### Now use cutree to select num.c clusters
num.c = 3
cl.xy = cutree(hcl.xy, k = num.c) # k = the number of clusters

###   This section shows how to obtain plots that are useful

###       plot names associated with data point at
###       location of data point
dev.new()
plot(xy, type = 'n') # suppress actual plot
text(xy, label = labels(cl.xy) )

###       add unique character to previous plot
###       associated with each cluster  
for ( i in 1:num.c )
  {
     curr.cl = which(cl.xy == i)  
     points(xy[curr.cl,], pch = i, col = 10*i)
  }

##### cluster with single
hcl.xy = hclust(d.xy, method = 'single') 
dev.new()
plot(hcl.xy)   # see the dendogram

##### cluster with average
hcl.xy = hclust(d.xy, method = 'average') 
dev.new()
plot(hcl.xy)   # see the dendogram

##### cluster with ward
hcl.xy = hclust(d.xy, method = 'ward') 
dev.new()
plot(hcl.xy)   # see the dendogram

##### cluster with centroid
hcl.xy = hclust(d.xy, method = 'centroid') 
dev.new()
plot(hcl.xy)   # see the dendogram

##### cluster with median
hcl.xy = hclust(d.xy, method = 'median') 
dev.new()
plot(hcl.xy)   # see the dendogram

names(hcl.xy)   # show what values are returned

hcl.xy$merge

hcl.xy$height

hcl.xy$order

hcl.xy$labels

hcl.xy$method

hcl.xy$call

hcl.xy$dist.method

#### We can cluster based on more than two variables
head(swiss)

data = swiss[,-1]
head(data)

d    = dist(data, method = 'manhattan')
hcl  = hclust(d)
dev.new()
plot(hcl, main = 'Use Manhattan Distance')
cl.3 = cutree(hcl, k = 4) # k = the number of clusters
cl.3



###    Recall that xy is data from just columns 2 and 3
###    of the Swiss data set. Here we plot the clustering
###    given by cl.3. The cluster points are plotted at the
###    x,y position associated with the first two columns
dev.new()
plot(data[,1:2], type = 'n') # suppress actual plot
text(data[,1:2], label = cl.3 )

open3d()
xyz = data[,1:3]
plot3d(xyz, type = 'n')
 
###       plot points from each cluster with a
###       different color
for ( i in 1:4 )
  {
     curr.cl = which(cl.3 == i)  
     points3d(xyz[curr.cl,], pch = i, col = 6*i)
  }


open3d()
xyz = data[,3:5]
plot3d(xyz, type = 'n')
 
###       plot points from each cluster with a
###       different color
for ( i in 1:4 )
  {
     curr.cl = which(cl.3 == i)  
     points3d(xyz[curr.cl,], pch = i, col = 6*i)
  }


##########################################################
#########     experiments                   ##############


#################################################
#   Step 1:  Form the clusters                  #
#################################################


####  parameters to control cluster formation
####  num.c is the number of clusters
####  the number of points in each cluster will be
####  a random number between n.low and n.high
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
     mu     = runif(2,-10,10)
     x      = rmvnorm(n[i],mu,s)
     rownames(x) = rep(i,n[i])
     if ( i == 1 ) 
       { data = x }
     else
       { data = rbind(data,x) }
  } #end for

### Look at the clusters formed.
### If the clusters formed do not seem good, just
### repeat the code above and form a new cluster
### until you get an interesting result
dev.new()
plot(data, type = 'n')
text(data, label = rownames(data))

######################################################
#  Step 2: Perform Hierarchical Clustering           #
#  You may change the method for the calculation of  #
#  dist() and hclust().                              #
#  Use ?dist() and ?hclust() to see what arguments   #
#  are available.                                    #
######################################################


###  Calculate the dist() for hierarchical clustering
d = dist(data)

#### choose cluster method
hc = hclust(d, method = 'complete')

dev.new()
plot(hc)

### choose number of clusters (need not be same as the number
### chosen when you formed the clusters)
num.c = 4
cl= cutree(hc, k = num.c) # k= num.c means num.c clusters

###       plot names associated with data point at
###       location of data point
dev.new()
plot(data, type = 'n', main = 'H CLUSTER') # suppress actual plot
text(data, label = labels(cl) )

###       add unique character to previous plot
###       associated with each cluster  
for ( i in 1:num.c )
  {
     curr.cl = which(cl == i) 
     if ( length(curr.cl) > 1)
       { 
          points(data[curr.cl,], pch = i, col = 6*i+2)
       }
     else
       {
          if ( length(curr.cl) == 1 )
            {points(data[curr.cl,1],data[curr.cl,2], pch = i, col = 6*i+2)}
       }
  }


######################################################
#  Step 3: Perform K-means Clustering                #
#  Use ?kmeans() to see what arguments are available.#                                   #
######################################################

cl.kmeans = kmeans(data,num.c,nstart = 2)
names(cl.kmeans)  ### see what kind of information is returened
###       plot names associated with data point at
###       location of data point
dev.new()
plot(data, type = 'n', main = 'K-MEANS') # suppress actual plot
text(data, label = rownames(data) )

###       add unique character to previous plot
###       associated with each cluster 
num.cl = length(cl.kmeans$size) 
for ( i in 1:num.cl )
  {
     curr.cl = which(cl.kmeans$cluster == i) 
     if ( length(curr.cl) > 1)
       { 
          points(data[curr.cl,], pch = i, col = 6*i+2)
       }
     else
       {
          if ( length(curr.cl) == 1 )
            {points(data[curr.cl,1],data[curr.cl,2], pch = i, col = 6*i+2)}
       }
  }


######################################################
#  Step 3: Perform dbscan Clustering                 #
#  Use ?dbscan() to see what arguments are available.#                                   #
######################################################


###  If data is not too big
{ # open syntax unit
if ( nrow(data) < 500 )
  { 
    d   = dist(data)
    d.m = as.matrix(d)
  }
else
  {
    print("Are you sure? d.m might be too big")
    # if d, d.m already exist, kill them so the user
    # doesn't think the new one has been computed
    if ( exists('d') )   { rm(d)   }
    if ( exists('d.m') ) { rm(d.m) }
  }
} # close syntax unit
############  Use this to help find eps and MinPts

k   = 4   # change value of k
k.dist = numeric(nrow(d.m))
for ( i in 1:nrow(d.m) )
  {
     r.dist = sort(d.m[i,-i])  # delete dist measure from i to i
     k.dist[i] = r.dist[k]
  }
k.dist.sort = sort(k.dist)
dev.new()
plot(k.dist.sort)

##### See what the noise points will be for a particular
##### eps and MinPts
eps = 1
MinPts = k + 1
noise.pts = which(k.dist > eps)
dev.new();
par(mfrow=c(2,1))
plot(data, type = 'n', main = 'Data Clusters') # suppress actual plot
text(data, label = rownames(data) )
plot(data[noise.pts,], main = 'Noise Points')

##########################
#  Now try dbscan with varying parameters
#  note you can change method and showplot
#  type ?dbscan for information

dev.new()
cl.db = dbscan(data, eps = 4, MinPts = 6, method = 'hybrid',showplot = 2)
dev.new()
cl.db = dbscan(data, eps = 0.5, MinPts = 6, method = 'hybrid',showplot = 1)
dev.new()
cl.db = dbscan(data, eps = 1, MinPts = 5, method = 'hybrid',showplot = 1)




######   To Plot Result of DBSCAN ##################

######  Noise points are over-plotted with 'N'
######  Cluster points are overplotted with unique print character (pch)
######  color (col)

dev.new()
plot(data, type = 'n', main = 'DBSCAN') # suppress actual plot
text(data, label = rownames(data) )

###       add unique character to previous plot
###       associated with each cluster 
cl.id = unique(cl.db$cluster)  # get unique cluster numbers 

for ( i in cl.id )
  {
     if (  i == 0 )
       { # noise point
          noise.pts = which(cl.db$cluster == 0 )
          if (length(noise.pts) > 1)
            {
               text(data[noise.pts,], label = c('N'))
            }
          else
            {
               text(data[noise.pts,1],data[curr.cl,2], label = c('N'))
            }
       } # noise point
     else
       { # cluster point
         curr.cl = which(cl.db$cluster == i) 
         if ( length(curr.cl) > 1)
           { 
              points(data[curr.cl,], pch = i, col = 6*i+2)
           }
         else
           {
              if ( length(curr.cl) == 1 )
                {points(data[curr.cl,1],data[curr.cl,2], pch = i, col = 6*i+2)}
           }
       } # cluster point
  } # end for






