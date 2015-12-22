################################################
#
#    QR decomposition in R
#
#  This script just shows R functions for
#  computing the QR decomposition  of a matrix
################################################

#  We want to see numbers to full accuracy

options(digits = 16)

# First we make a 3x3 matrix
v = rnorm(9)
A.3x3 = matrix(v,nrow = 3)

#  The qr function will 
temp = qr(A.3x3)

#  let us find out what the qr function returns

print(temp)

# for us, the most interesting thing is rank. qr() computes the
# numerical rank of the matrix.

B     = A.3x3
B[,3] = A.3x3[,1] + 2*A.3x3[,2] # now replace thirs column of B

temp.B = qr(B)

print(temp.B)

# Indeed, we do see B has rank 2.

B.perturbed = B
B.perturbed[,3] = B[,3] + c(10^-9,-10^-12,10^-12)

print(qr(B.perturbed))

#  Note that rank(B.perturbed) actually equals 3. The numerical rank
#  differs from mathematical rank.


B.perturbed = B
B.perturbed[,3] = B[,3] + c(10^-6,-10^-12,10^-12)

#  A perturbation of 10^-6 causes the numerical rank to increase
print(qr(B.perturbed))


B.perturbed = B
B.perturbed[,3] = B[,3] + c(10^-9,-10^-12,10^-12)

# You can select the tolerance level for qr().
# Here we set the tolerance to 10^-10, so a perturbation of 10^-9
# is seen as changing the rank of the matrix. The default is tol = 10^-7.

print(qr(B.perturbed,tol=10^-10))

###    We still do not have a QR decomposition. The qr() function
###    returns a "qr object". 

###   Let us return to A.3x3

A.3x3.qr.obj = qr(A.3x3)
Q            = qr.Q(A.3x3.qr.obj)
R            = qr.R(A.3x3.qr.obj)

# The qr.Q and qr.R functions return the Q and R matrices respectively.
# Note that one must use the "qr object" as the input to qr.Q and qr.R

A.3x3 - Q %*% R

# Evidently A.3x3 = QR (within machinbe precision)

#  Here is a wrapper function to make things easier

qrd <- function(A,tol = 10^-7)
{  # begin qrd function
   stopifnot(is.matrix(A))
   stopifnot(is.numeric(A))  # really we should check for NA and NaN too.
   stopifnot(length(tol) == 1)
   stopifnot(is.numeric(tol))
   stopifnot( tol > 0 )

   t    = qr(A,tol)
   rank = t$rank
   Q    = qr.Q(t)
   R    = qr.R(t)
   return(list(Q = Q, R = R, rank = rank))

} # end qrd function


qrd(A.3x3)


#   A non-square matrix also has a QR decompostion
v = rnorm(48)

A.8x6 = matrix(v, ncol = 6)

qrd(A.8x6)


 

