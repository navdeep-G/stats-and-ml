############################################
#   SVD and Least Squares Solution
#
#   We start by showing the action of a matrix A
#   can be described by the SVD.
#
#   Then we solve a least squares problem. We plot
#   the geometry that describes how the SVD obtains
#   its solution
##############################################

############## First define some helpful functions ##########

dotprod <- function(u,v)
{ # dotprod
  stopifnot(is.vector(u))
  stopifnot(is.vector(v))
  stopifnot(length(u) == length(v))
  return(sum(u*v))
} # dotprod

###### To compute the Euclidean Norm of a vector

vnorm <- function(v)
{ # vnorm
  stopifnot(is.vector(v))
  return(sqrt(sum(v*v)))
} # vnorm

###########################################################

library(rgl)


A = matrix(c(1,4,7,2,-1,3),ncol=2)
A.svd = svd(A,nu = 3)
d     = A.svd$d

print(d)


v.1 = A.svd$v[,1]
v.2 = A.svd$v[,2]
v.0 = c(0,0)
y.1   = A%*%v.1
y.2   = A%*%v.2
v     = rbind(v.1,v.2)
l1    = cbind(c(0,v.1[1]),c(0,v.1[2]))
l2    = cbind(c(0,v.2[1]),c(0,v.2[2]))
plot(v,col='red', xlim = c(-1,1), ylim = c(-1,1))
abline( h = 0);abline(v=0)
lines(l1)
lines(l2)

plot3d(cbind(y.1,y.2))
points3d(A.svd$u,col='red')

theta = seq(0,2*pi,length=1000)
x.1   = cos(theta)
x.2   = sin(theta)
x     = rbind(x.1,x.2)
y     = A%*%x

plot3d(t(y), type='l',xlim = c(-8,8),ylim = c(-8,8), zlim = c(-8,8))
d.aug = diag(c(d,0))

######   plot the U vector points of the SVD
points3d(t(A.svd$u%*%d.aug), col = 'red') # red is the U vectors scaled 
                                          # by the singular values
points3d(t(A.svd$u), col ='blue')         # blue are the unit norm U vectors

#####    stop here and look at the 3D plot #################
############################################################

c.3 = rbind(0,0,0)   # c.3 is the origin

######  Plot line from origin to A*v1
l3  = cbind(c.3,y.1)
lines3d(t(l3),col = 'green')

######  Plot A*v2
l4  =cbind(c.3,y.2)
lines3d(t(l4), col = 'green')

######  Plot line from origin to u3
lu3 = cbind(c.3,A.svd$u[,3])
lines3d(t(lu3),col = 'green')


###############   stop here and look at the 3D plot  ################
#####################################################################



############  On the first run through the script use the first b given
############  On the second run, use b.new, which is defined in terms of
############  the U vectors. We should be able to predict the error in the
############  second case.

b     = c(4,1,3)
b.new = u[,1] + 2*u[,2] +u[,3]

###  redo this script, but uncomment the line below
#b     = b.new
 

#######       Plot b
points3d(t(b), col = 'brown')

###############   stop here and look at the 3D plot  ################
#####################################################################


u = A.svd$u    # grab the U and V vectors in the A.svd object
v = A.svd$v

b.u = t(u)%*%b  # the norm of the projection of b onto the U vectors
print(b.u)

l.b.proj.u1 = cbind(c.3,b.u[1]*u[,1])  # line from origin to projection of b onto u[,1]
l.b.proj.u2 = cbind(c.3,b.u[2]*u[,2])  # line from origin to projection of b onto u[,2]

l6 = cbind(b.u[1]*u[,1],b.u[1]*u[,1]+b.u[2]*u[,2])

######   Plot the projection of b on to U1
lines3d(t(l.b.proj.u1))

###############   stop here and look at the 3D plot  ################
#####################################################################

# lines3d(t(l.b.proj.u2), col = 'red')  ### line cannot overwrite previous line
points3d(t(l.b.proj.u2), col = 'black')

###############   stop here and look at the 3D plot  ################
#####################################################################

###  add the two projections

p  = b.u[1]*u[,1] + b.u[2]*u[,2]
l6 = cbind(b.u[1]*u[,1],p)
lines3d(t(l6))

###############   stop here and look at the 3D plot  ################
#####################################################################

#######  Plot the line from b to p where p is the projection
#######  of b onto the plane formed by u[,1] and u[,2]
#######  This is the error vector

l7 = cbind(p,b)
lines3d(t(l7))

###############   stop here and look at the 3D plot  ################
#####################################################################

#### Now solve it algebraically

x.ls = (1/d[1])*b.u[1]*v[,1] + (1/d[2])*b.u[2]*v[,2]
y.ls = A%*%x.ls

print(x.ls)
print(y.ls - p)   # difference between geometric and algebraic approach
error = b - y.ls
print(error)  
b.proj.u3 = b.u[3]*u[,3]
print(b.proj.u3)

##### Note the error is the projection of b onto u[,3]
##### A cannot reach in the direction of u[,3]

###### plot nearest reachable point on U1-U2 plane to b
###### and then line from origin to this point.
points3d(t(y.ls),col = 'green')

l.best = cbind(c.3,y.ls)
lines3d(t(l.best),col='red')

###############   stop here and look at the 3D plot  ################
#####################################################################



######################################################
#
#   Back to SVD action
#
#   This time we look at a 3x2 rank deficient matrix
#
######################################################


A     = matrix( c(1,3,2,4,12,8),ncol = 2)
A.svd = svd(A, nu = 3)
u     = A.svd$u
v     = A.svd$v
d     = A.svd$d
print(d)

#  Note the second singular value is effectively zero.
#  Without roundoff error it would be 0.

# The rank of A is 1


v.1 = A.svd$v[,1]
v.2 = A.svd$v[,2]
v.0 = c(0,0)
y.1   = A%*%v.1
y.2   = A%*%v.2
v     = rbind(v.1,v.2)
l1    = cbind(c(0,v.1[1]),c(0,v.1[2]))
l2    = cbind(c(0,v.2[1]),c(0,v.2[2]))
plot(v,col='red', xlim = c(-1,1), ylim = c(-1,1))
abline( h = 0);abline(v=0)
lines(l1)
lines(l2)

plot3d(cbind(y.1,y.2))
points3d(A.svd$u,col='red')

theta = seq(0,2*pi,length=1000)
x.1   = cos(theta)
x.2   = sin(theta)
x     = rbind(x.1,x.2)
y     = A%*%x

plot3d(t(y), type='l',xlim = c(-8,8),ylim = c(-8,8), zlim = c(-8,8))
d.aug = diag(c(d,0))

######   plot the U vector points of the SVD
points3d(t(A.svd$u%*%d.aug), col = 'red') # red is the U vectors scaled 
                                          # by the singular values
points3d(t(A.svd$u), col ='blue')         # blue are the unit norm U vectors

#####    stop here and look at the 3D plot #################
############################################################

c.3 = rbind(0,0,0)   # c.3 is the origin

######  Plot line from origin to A*v1
l3  = cbind(c.3,y.1)
lines3d(t(l3),col = 'green')

######  Plot A*v2
l4  =cbind(c.3,y.2)
lines3d(t(l4), col = 'green')

######  Plot line from origin to u3
lu3 = cbind(c.3,A.svd$u[,3])
lines3d(t(lu3),col = 'green')


###############   stop here and look at the 3D plot  ################
#####################################################################


#  Notice we can only reach a one-dimensional subspace. This subspace is
#  spanned by u[,1]. We cannot reach any (non-zero) multiple of u[,2] or
#  u[,3]


################################################################## 
#
#     Algebraic solution to rank deficient Least Squares problems
#
##################################################################


b  = c(5,3,-1,7,4,6)

w1 = c(3,-1,5,2,8,-3)
w2 = c(4,7,3,-2,5,11)
w3 = c(7,1,-4,2,9,1)
w4 = 2*w1 -w3

A  = cbind(w1,w2,w3,w4)

##  We want to solve Ax = b

A.svd = svd(A, nu = 6)  # get full SVD
u     = A.svd$u
v     = A.svd$v
d     = A.svd$d

print(d)  ### we know d[5] = d[6] = 0, so they are not included

###  First we use all four singular values
d.inv.vec    = 1/d
d.inv.matrix = diag(d.inv.vec)
x.ls.all     = v%*%d.inv.matrix%*%t(u[,1:4])%*%b
print(x.ls.all)

y.ls.all = A %*% x.ls.all
err.all  = as.vector(b - y.ls.all)
dotprod(as.vector(y.ls.all),err.all)

#  The error is NOT perpendicular to the solution. That is bad.

###  find the norm of the projection of x.ls.all on to the right singular vectors


v1.proj = t(x.ls.all)%*%v[,1]*v[,1]
v2.proj = t(x.ls.all)%*%v[,2]*v[,2]
v3.proj = t(x.ls.all)%*%v[,3]*v[,3]
v4.proj = t(x.ls.all)%*%v[,4]*v[,4]

print( vnorm(v1.proj) ) 
print( vnorm(v2.proj) ) 
print( vnorm(v3.proj) ) 
print( vnorm(v4.proj) ) 

##  Notice that almost all of x.ls.all projects onto v[,4]
##  This is predicatable since the reciprocal of the corresponding
##  singular value is so large.

##  The moral of this story is we should ignore the fourth singular vector,
##  and treat this as a rank 3 problem

##  rank 3 d.inv matrix
d.inv.rank3 = d.inv.matrix[1:3,1:3]

x.ls.rank3 = v[,1:3]%*%d.inv.rank3%*%t(u[,1:3])%*%b
print(x.ls.rank3)

###  project the rank 3 solution on to the V vectors
v1.proj = t(x.ls.rank3)%*%v[,1]*v[,1]
v2.proj = t(x.ls.rank3)%*%v[,2]*v[,2]
v3.proj = t(x.ls.rank3)%*%v[,3]*v[,3]
v4.proj = t(x.ls.rank3)%*%v[,4]*v[,4]

print( vnorm(v1.proj) ) 
print( vnorm(v2.proj) ) 
print( vnorm(v3.proj) ) 
print( vnorm(v4.proj) ) 

###  Now project b onto the U vectors

b.on.u.proj = t(u)%*%b
print(b.on.u.proj)

###   Recall SVD    action

#                  d (the singular value)
#     v      --------------------------------->   u
#
#                    d[1]
#     v[,1]  ---------------------------------->  u[,1]
#
#                    d[2]
#     v[,2]  ---------------------------------->  u[,2]
#
#                    d[3]
#     v[,3]  ---------------------------------->  u[,3]
#
#                    d[4] = 0
#     v[,4]  ---------------------------------->  u[,4]   #cannot reach
#
#                    d[4] = 0
#     v[,4]  ---------------------------------->  u[,4]   #cannot reach
#
#                                                 u[,5]   # cannot reach
#                                              
#                                                 u[,6]   # cannot reach

#    So our solution should have a projection on v[,i] , for i = 1,2,3
#    of     b.on.u.proj[i]/d[i]

print(b.on.u.proj[1:3]/d[1:3])

#   Note that we earlier calculated the norm of the projection of the
#   rank 3 solution on to the V vectors

#
#    Also note that we can add scalar multiples of v[,4] to our solution.
#    We still reach the same point. So our rank 3 solution is the minimum
#    norm least squares solution

y.rank3 = A%*%x.ls.rank3
print(y.rank3)

x.new   = x.ls.rank3 + 3*v[,4]
y.new   = A%*%x.new
print(y.new)

## Note that y.rank3 and y.new are identical to several decimal places
## If we had a computer that used exact arithmetic they would be completely
## identical.

###  Also note

b.rank3 = b.on.u.proj[1]*u[,1] + b.on.u.proj[2]*u[,2] + b.on.u.proj[3]*u[,3]
print(b.rank3)
# equals our y.rank3 (as we expect)

err = as.vector(b - y.rank3)
print(err)
print(b.on.u.proj[4]*u[,4] + b.on.u.proj[5]*u[,5] + b.on.u.proj[6]*u[,6])

##  We see the error projects onto u[,4], u[,5] and u[,6]

###  Finally, the error is perpendicular to the least squares solution

dotprod(err,as.vector(y.rank3))












