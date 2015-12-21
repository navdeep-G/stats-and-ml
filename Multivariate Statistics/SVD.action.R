###########################################
#
#    SVD     Action
#
###########################################

w1 = c(3,6,8,9,3)
w2 = c(-2,4,7,8,1)
w3 = c(3,1,-2,6,2)
w4 =c(10,4,3,6,-1)

#   Create a 5x4 matrix
A = cbind(w1,w2,w3,w4)

x = c(1,2,3,4)

########### have A act on some vector
y = A%*%x
print(y)

###   A sends x to y. Note we go from 4 dimensions to 6 dimensions

###   Now we analyze what is going on

A.svd = svd(A,nu = 5)  # we want a full basis for U
u     = A.svd$u
v     = A.svd$v
d     = A.svd$d

####  find the linear combination of V vectors that equal x

kv = as.vector(t(x)%*%v)

print( kv[1]*v[,1] + kv[2]*v[,2] + kv[3]*v[,3] + kv[4]*v[,4] )

#  Yes, x is a linear combination of V's


#####  find the linear combination of U's for y

ku = as.vector(t(y)%*%u)
print(ku)

##  note last constant will be 0 in exact arithmetic

temp = numeric(5)
for ( i in 1:5)
  {
    temp = temp + ku[i]*u[,i]
  }
print(temp)

###  so now we have our linear combination of U's to make y


u.1.svd = kv[1]*d[1]*u[,1]
u.2.svd = kv[2]*d[2]*u[,2]
u.3.svd = kv[3]*d[3]*u[,3]
u.4.svd = kv[4]*d[4]*u[,4]
u.5.svd = 0

print(y)
print(u.1.svd + u.2.svd + u.3.svd + u.4.svd + u.5.svd)

###  Hence we have verified the SVD action

#              multiply by singular value 
#   v   -------------------------------------->  u

#### Now we make a prediction

x.new = c(3,7,2,5)

kv = as.vector(t(x.new)%*%v)

result = numeric(5)
for ( i in 1:4 )
  {
     result = result + d[i]*kv[i]*u[,i]
  }
print(result)
print(A%*%x.new)

####  Again we verify that the SVD explains the result of
####  matrix multiplication.
