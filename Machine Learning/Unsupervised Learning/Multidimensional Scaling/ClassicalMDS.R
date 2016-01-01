#######################################################
#
#   Classical Multidimensional Scaling Computations
#
#######################################################


#  Given a D matrix (containing Euclidean distnces squared
#  there are two ways to form the B matrix.
#
#  Below are two functions that take the D matrix as an
#  input and give the B matrix as the output.


#  This one uses a simple matrix multiplication

makeB <- function(d) # d contains distance squared
{ # makeB

 # d = d*d
  n = nrow(d)
  m = diag(rep(1,n)) - (1/n)*matrix(rep(1,n^2),ncol=n)
  d.hat = -0.5*d
  B = m%*%d.hat%*%m
  return(B)
} # makeB


#  Note this code could be made more efficient by calulating
#  the upper triangular portion of the matrix, and then using the
#  symmetry of B to fill in the lower triangular region

makeB2 <- function(d) # d contains distance squared
{
  n = nrow(d)
  rm = apply(d,1,mean)
  allm = sum(d)/(n^2)
  B    = matrix(rep(0,n^2),ncol = n)
  for ( i in 1:n )
    {
      for ( j in 1:n )
        {
          B[i,j] = -0.5*(d[i,j] -rm[i] - rm[j] + allm)
        }
    }
  return(B)
}


####   Example

D = matrix( c(0,4,5,16,20,
              4,0,5,20,16,
              5,5,0,5,5,
              16,20,5,0,4,
              20,16,5,4,0), ncol = 5)

#  D is a matrix of squared Euclidan distances

B  = makeB(D)
B2 = makeB2(D)

print(B)
print(B2)

# Evidently B2 is a little more accurate than B

eigen(B)
eigen(B2)

#  This function takes in a distance squared matrix, calls makeB,
#  and computes the eigendecomposition of B. It then ignores small
#  eigenvalues to give the MDS map.

euclidMDS <- function(d)
{
  B    = makeB(d)
  eigB = eigen(B)
  eig.val = eigB$values
  eig.val = eig.val[ eig.val > sqrt(.Machine$double.eps)]
  n       = length(eig.val)
  x       = eigB$vectors[,1:n]%*%diag(sqrt(eig.val))
  return(x)
}

x = euclidMDS(D)
print(x)
plot(x)



library(mvtnorm)
S = matrix( c(2,1,1,3), ncol = 2) #covariance matrix

n = 22
data = rmvnorm(n,c(0,0),S) # generate some 2 dimensioanl data

D    = as.matrix(dist(data))
D.sq = D*D
x    = euclidMDS(D.sq)

#  plot original data and reconstructed MDS map
dev.new()
plot(data, main = "Data")
dev.new()
plot(x, main = "MDS")
D.mds = as.matrix(dist(x))
D.mds = D.mds*D.mds
{
  if ( n <= 6 )
    {
      print(D.sq)
      print(D.mds)
    } 
  else
    {
      diff = D.sq - D.mds
      print(max(abs(diff)))
    }
}
B     = makeB2(D.sq)
B.eig = eigen(B)
B.eig$values

#####   Now look at higher dimensional data

t = runif(25)
S = matrix(t,ncol = 5)
S = t(S)%*%S + diag(c(2,2,2,2,2))

n    = 15
data = rmvnorm(n, rep(0,5), S)

D    = as.matrix(dist(data))
D.sq = D*D

B     = makeB2(D.sq)
B.eig = eigen(B)

##   Note we have 5 nonzero eigenvalues.
##   This is the dimensionality of the original data.
##   If the D.sq matrix is not exact Euclidean squared
##   data, we will have negative eigenvalues
B.eig$values

#### Now we perturb D.sq so we do not have exact Euclidean
#### squared distances
D.sq[1,2] = 6
D.sq[2,1] = 6


B     = makeB2(D.sq)
B.eig = eigen(B)
B.eig$values

#####  Note the eigenvalues

##   now try to do the MDS on the new B matrix
  eig.val  = B.eig$values[ B.eig$values > sqrt(.Machine$double.eps)]
  m        = length(eig.val)
  x        = B.eig$vectors[,1:m]%*%diag(sqrt(eig.val))
  d.new    = as.matrix(dist(x))
  d.new.sq = d.new*d.new
  diff     = d.new.sq - D.sq
  diff

##### Let's alter another entry
D.sq[3,4] = 9
D.sq[4,3] = 9

B     = makeB2(D.sq)
B.eig = eigen(B)
B.eig$values




  
