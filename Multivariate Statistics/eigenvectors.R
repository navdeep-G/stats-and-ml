require(mvtnorm) # you need this library
require(fpc)     # and this

##############################################
#   Expressing x as a linear combination
#   of eigenvectors
##############################################

A = matrix( c(8, 2, 3,1,
              2,13,-1,2,
              3,-1, 9,4,
              1, 2, 4,6), ncol = 4, byrow = TRUE)
v = eigen(A)$vectors 

x = c(2,-1,4,3)

####   find a vector c such that
####   c[1]*v[,1] + c[2]*v[,2] + c[3]*v[,3] + c[4]*v[,4]
####   equals x.

#Solution:

#To get c, we need the inverse of v multiplied by x
#So, we need c = V^-1*(x)

#Get inverse of v

inV =solve(v)

# Get c

c = inV%*%x

#Check if c is correct

eqX = c[1]*v[,1] + c[2]*v[,2] + c[3]*v[,3] + c[4]*v[,4]

# eqX = (2 -1 4 3), which is the value of x. So, c is equal to
#the vector (-2.7694405, 4.6572283,-.5679891,.5637488)


