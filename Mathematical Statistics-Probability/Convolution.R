############################
#
#      convolution
#
############################

#  source convolution routine

# Convolution has many uses.
# (1) Finding the pdf of a sum of independent RVs
# (2) Multiplying polynomials
# (3) Filtering, or calculating the response
#     of a linear time invariant system
# (4) Convolution in the time domain is the same
#     as multiplication in the frequency domain
# (5) Convolution n the frequency domain is the same
#     as multiplication in the time domain

# Z = X + Y
# X ~ binomial(n1,p1)
# Y ~ binomial(n2,p2)
#
# If p1 = p2, then Z ~ binomial(n1+n2,p1)
# If p1 != p2 then.....

# Look at an example
n.x  = 8      # X
p.x  = 0.7
x.s  = 0:n.x    # support of x
f.x = dbinom(x.s,n.x,p.x) # pdf values over support
n.y = 5      # Y
p.y = 0.4
y.s = 0:n.y    # support of x
f.y = dbinom(y.s,n.y,p.y) #pdf values over support


dev.new()  # open a plot window
plot(x.s,f.x,col = 'red',
        ylim = c(0,1), main = "Two Binomial PDFs")
points(y.s,f.y, pch = 2, col = 'blue')


#  P(Z = k) = SUM( i = 0 to i = k ) {P(X = i)*P(Y = k- i)}
#  Example: P(Z=2) = P(X=2)*P(Y=0) + P(X=1)*P(Y=1) + P(X=0)*P(Y=2)
#  Note that X+Y = 2 in each product
#  Note that each event summed is mutually exclusive with the
#  other events.
#  We cannot have both {X=2,Y=0} and {X=1,Y=1}, for example
#  Since X and Y are independent, we can mutiply the pdf
#  of X and Y to obtain the probability of the joint event
#  {X = i, Y = k - i}

#  First we flip the pdf of Y about the vertical axis
f.y.r = rev(f.y)
y.s.r = -rev(y.s)

#  Then we shift it to the right as k increases

    x.axis = seq(-n.y,n.x+n.y, by = 1) # set plot limits
    y.zero = numeric(length(x.axis))
for ( k in 0:(n.x + n.y) ) 
  {
    par( ask = T)
    plot(x.axis,y.zero, ylab = 'pdf values',
          ylim = c(0,1), main = "Convolve X and Y")
    points(x.s,f.x, col = 'red')
    points(y.s.r,f.y.r, col = 'blue', pch = 2)
    y.s.r = y.s.r + 1 # move reversed Y to right
  }

# Note: We could have flipped X and kept Y in place also.
#       It does not matter theoretically which one we flip.
#       Of course, sometimes in practice one choice may be
#       much easier to do.

# If the support of one of the RVs is infinite, then we
# must stop the computation at some point where the probability
# becomes negligible. Think of summing a binomial RV and a Poisson RV

# Of course, if we can do the sum (for discrete RVs) or the integral
# (for continuous RVs) analytically, then we don't need the computer
# to do the convolution

#  Continuous case. X and Y are independent RVs

#  X ~ U(0,1)
#  Y ~ U(0,1)
#  Z = X + Y


x.s = seq(0,1, by = 0.001)
f.x = rep(1,1001)
#  Flip Y. 
f.y.r = rep(1,1001)
y.s.r = (seq(-1,0, by = 0.001))
# the axis limits for the plot
x.axis = seq(-1,2, by = 0.001)
y.zero = numeric(length(x.axis))
z.s    = seq(0,2, by = 0.001)    # support of Z
f.z    = numeric(length(z.s))

dev.new()
for ( k in 1:2001 )
  {
    plot(x.axis,y.zero, type = 'l', ylab = 'pdf values',
          ylim = c(0,1),
          main = "Convolve X and Y\n pdf(Z) in Green")
    lines(x.s,f.x, col = 'red')
    lines( c(0,0),c(0,1), col = 'red')
    lines( c(1,1), c(0,1), col = 'red')
    lines(y.s.r,f.y.r, col = 'blue')
    lines( c(y.s.r[1],y.s.r[1]), c(0,1), col = 'blue')
    lines( c(y.s.r[1001],y.s.r[1001]), c(0,1), col = 'blue')
    y.s.r = y.s.r + 0.001 # move reversed Y to right
    f.z[k] = 1.00 - abs(z.s[k]-1)
    lines(z.s[1:k], f.z[1:k], col = 'green')   
  }

#  Note that as flipped Y moves to the right, we have Z pick up
#  area at a uniform rate, until k = 1. Then we have Z lose area
#  at a uniform rate as Y slides out of the support of X.
 

# W ~ U(0,1) 

# U = Z + W = X + Y + W

# Flip the pdf of W  
f.w.r = rep(1,1001)
w.s.r = (seq(-1,0, by = 0.001))
x.axis = seq(-1,3, by = 0.001)
y.zero = numeric(length(x.axis))
u.s    = seq(0,3, by = 0.001)    # support of U
f.u    = numeric(length(u.s))
z.s.extend = seq(0,3, by = 0.001) # just make the coding easier
f.z.extend = c(f.z,rep(0,1000))

win.graph()
for ( k in 1:3001 )
  {
    plot(x.axis,y.zero, type = 'l', ylab = 'pdf values',
          ylim = c(0,1),
          main = "Convolve W and Z\n pdf(W) in Brown")
    lines(z.s,f.z, col = 'green')
    lines(w.s.r,f.w.r, col = 'blue')
    lines( c(w.s.r[1],w.s.r[1]), c(0,1), col = 'blue')
    lines( c(w.s.r[1001],w.s.r[1001]), c(0,1), col = 'blue')
    if ( w.s.r[1] < 0 ) { low = 1 } else { low = k-1000 }
    f.u[k]  = 0.001*sum(f.z.extend[low:k])
    w.s.r = w.s.r + 0.001 # move reversed Y to right

    lines(u.s[1:k], f.u[1:k], col = 'brown')   
  }

###  We could also simulate the results
t.0 = proc.time()
n = 10^4
X = runif(n)
Y = runif(n)
W = runif(n)
Z = X + Y
U = Z + W
proc.time() - t.0
win.graph()
hist(Z,breaks = 200, prob = T,main = "PDF(Z)\n n = 10000")
win.graph()
hist(U, breaks = 300, prob = T,main = "PDF(U)\n n = 10000")

t.0 = proc.time()
n = 10^6
X = runif(n)
Y = runif(n)
W = runif(n)
Z = X + Y
U = Z + W
proc.time() - t.0
win.graph()
hist(Z,breaks = 200,prob = T,main = "PDF(Z)\n n = 1000000")
win.graph()
hist(U, breaks = 300, prob = T,main = "PDF(U)\n n = 1000000")

# Moral of the story: If you rely on simulation and you want to
# put a lot credence in your results use a big number of samples
# How much more time did it take to use 1000000 samples vs. 10000
# samples? Was the increase in accuracy worth the extra wait?

# If your results are actually important (not a home work assignment)
# you may take hours or days to run a simulation

######   Add two exponential RVs
# X ~ Exp(mean=1/6)
# Y ~ Exp(mean=1/6)
# X and Y are iid
# Z = X + Y

mu     = 1/6
lambda = 1/mu;
#  First discretize the pdf
#  We will use that portion of the pdf where only 10^-6 
#  of the area or less is excluded
lim = ceiling(qexp(0.999999,lambda))
print(lim)
print(1- pexp(lim,lambda)) # how much area are we excluding?

x.eval         = seq(0,lim, by = 0.001)
N              = length(x.eval)
x.eval.advance = c(x.eval[2:N],lim + 0.001)
#   find area
f.x            = pexp(x.eval.advance,lambda) -
                       pexp(x.eval,lambda)
#   convert back to density
f.x            = 1000*f.x 
x.s            = x.eval
f.y.r          = rev(f.x)
y.s.r          = -rev(x.s)

x.axis         = seq(-lim,lim,0.001)
y.zero         = numeric(length(x.axis))
z.s            = seq(0,lim, by = 0.001)    # support of Z
f.z            = numeric(length(z.s))

win.graph()
for ( k in 1:3001 )
  {
    plot(x.axis,y.zero, type = 'l', ylab = 'pdf values',
         ylim = c(0,6),
         main = "Convolve X and Y\n pdf(Z) in Green")
    lines(x.s,f.x, col = 'red')
    lines( c(0,0),c(0,f.x[1]), col = 'red')
    lines(y.s.r,f.y.r, col = 'blue')
    lines( c(y.s.r[3001],y.s.r[3001]), c(0,f.y.r[3001]), col = 'blue')
    y.s.r = y.s.r + 0.001 # move reversed Y to right
    f.z[k] = 0.001*sum(f.x[1:k]*f.y.r[(N-k+1):N])
    lines(z.s[1:k], f.z[1:k], col = 'green')      
  }

##   How good are our results?
win.graph()
plot(x.eval,dgamma(x.eval,2,lambda), type = 'l', col = 'blue',
     main = 'BLUE: PDF of Gamma(2,lambda)\n RED: PDF(Z)',
     ylab = 'density',
     xlab = 'X')
lines(x.eval-0.001,f.z, col = 'red')

###  Now let X ~ Exp(mean = 1/6)
#            Y ~ Exp(mean = 1/4)
#
mu.y     = 1/4
lambda.y = 1/mu.y
#  reuse f.x, x.s

f.y            = pexp(x.eval.advance,lambda.y) -
                 pexp(x.eval,lambda.y)
#   convert back to density
f.y            = 1000*f.y 
f.y.r          = rev(f.y)
y.s.r          = -rev(x.s)
x.axis         = seq(-lim,lim,0.001)
y.zero         = numeric(length(x.axis))
z.s            = seq(0,lim, by = 0.001)    # support of Z
f.z            = numeric(length(z.s))

win.graph()
for ( k in 1:3001 )
  {
    plot(x.axis,y.zero, type = 'l', ylab = 'pdf values',
         ylim = c(0,6),
         main = "Convolve X and Y\n pdf(Z) in Green")
    lines(x.s,f.x, col = 'red')
    lines( c(0,0),c(0,f.x[1]), col = 'red')
    lines(y.s.r,f.y.r, col = 'blue')
    lines( c(y.s.r[3001],y.s.r[3001]), c(0,f.y.r[3001]), col = 'blue')
    y.s.r = y.s.r + 0.001 # move reversed Y to right
    f.z[k] = 0.001*sum(f.x[1:k]*f.y.r[(N-k+1):N])
    lines(z.s[1:k], f.z[1:k], col = 'green')      
  }

##    Z is not a gamma RV. 
#     exact pdf for Z is
#     f(z) = ((lamda1*lambda2)/(lambda2-lambda1))*(exp(-lambda1*z) -
                                                   exp(-lambda2*z) )
f.pdf =12*(exp(-4*x.s) - exp(-6*x.s))
win.graph()
plot(x.s,f.pdf, type='l', col = 'blue', 
     main = "pdf from convolution (Blue) and \n exact pdf (Red)" )
lines(x.s,f.z, col='red')


###   Now for discrete convolution
#     X is the outcome of rolling one die
###   Roll Two Dice
f.x   = c(0,rep(1/6,6))   # P(X = x) for x = 0,1,2,3,4,5,6

#  Y = X1 + X2  where X1 and X2 are the results of throwing two die
y = conv(f.x,f.x)   # you will write such a function yourself
print(y)
### for comparison
print(1/36*seq(1,6))

####   Explanation. Consider the vector (0,1,1,1,1,1,1)*(1/6)
####   To ease computations, we ignore the 1/6 factor for now.

####   Convolution works like this:
#           index 0     index 6
#             |           |
#             |           |            
#             v           v
#             0 1 1 1 1 1 1      <- original vector
# 1 1 1 1 1 1 0                  <- flipped vector

# The output is the sum of the overlapped products.
# For mathematical ease I will pretend R allows us to
# use a zero index into a vector (as C does).
# In R code, the actual index for y is one bigger than
# shown here.
# R does not allow zero to be an index to a vector. Remember
# here that P(Y = k) is stored in y[k+1].

#  k = 1 output has only one position with overlap.
#     So we sum 0*0, and y[1] = 0

#  k = 2
#            0 1 1 1 1 1 1      <- original vector
#  1 1 1 1 1 1 0                <- flipped vector moved right 1
#  k = 2 output is 0*1 + 1*0 so y[2] = 0

#  k = 3
#            0 1 1 1 1 1 1      <- original vector
#    1 1 1 1 1 1 0              <- flipped vector moved right 2
#  k = 3 output is 0*1 + 1*1 + 1*0 so y[3] = 1

#  k = 4
#            0 1 1 1 1 1 1      <- original vector
#      1 1 1 1 1 1 0            <- flipped vector moved right 2
#  k = 4 output is 0*1 + 1*1 + 1*1 + 1*0 so y[4] = 2

#  k =5
#            0 1 1 1 1 1 1      <- original vector
#        1 1 1 1 1 1 0          <- flipped vector moved right 2
#  k = 5 output is 0*1 + 1*1 + 1*1 + 1*1 + 1*0 so y[5] = 3

#  k =5
#            0 1 1 1 1 1 1      <- original vector
#        1 1 1 1 1 1 0          <- flipped vector moved right 2
#  k = 5 output is 0*1 + 1*1 + 1*1 + 1*1 + 1*0 so y[6] = 3

#  k =6
#            0 1 1 1 1 1 1      <- original vector
#          1 1 1 1 1 1 0        <- flipped vector moved right 2
#  k = 6 output is  y[6] = 4

#  When k = 8, see that the overlap is maximaum, so y[8] = 6
#  When k = 9, the picture is

#  k =9
#            0 1 1 1 1 1 1      <- original vector
#                1 1 1 1 1 1 0  <- flipped vector moved right 2
#  k = 9 output is y[9] = 5

# Now as k gets bigger the overlap gets smaller.

# in short the output sequence looks like:
#  0 0 1 2 3 4 5 6 5 4 3 2 1
#
# Recall we ignored the (1/6) factor to do this.
# When we reintroduce the factor, we multiply by 1/36,
# since both the original and moving flipped vators had
# a 1/6 factor removed. This completely explains the result
# of throwing two die.


#  for k = 1, we slide the flipped vector right 1     
#  Notice we needed to put P(X=0) = 0 into our vector to get
#  y[2] = 1/36

# roll three die
y.3 = conv(y,f.x)
print(y.3)

###  for comparison
print(1/216*seq(1,36))

# If we do a similar analysis for convolving y, and f.x we have
#
#  start position:
#  k = 1
#                0 0 1 2 3 4 5 6 5 4 3 2 1      <- y
#    1 1 1 1 1 1 0                              <- flipped x

# we need to move right 3 positions befroe we have overlap.

# k = 4
#                0 0 1 2 3 4 5 6 5 4 3 2 1      <- y
#          1 1 1 1 1 1 0                        <- flipped x
#   y.3[k] = 1

# k = 5
#                0 0 1 2 3 4 5 6 5 4 3 2 1      <- y
#            1 1 1 1 1 1 0                      <- flipped x
#   y.3[k] = 3

# k = 6
#                0 0 1 2 3 4 5 6 5 4 3 2 1      <- y
#              1 1 1 1 1 1 0                    <- flipped x
#   y.3[k] = 6

# k = 7
#                0 0 1 2 3 4 5 6 5 4 3 2 1      <- y
#                1 1 1 1 1 1 0                  <- flipped x
#   y.3[k] = 10

#  And so on
#  The actual values of y.3 must then be multipled by (1/6)^3 = 1/216

#  Notice the y.3 values match what we have worked out by hand

###  We can multiply polynomials:
#    Suppose we store the coefficients of polynomials
#    in ascending aorder:
p.1 = c(1,0,2,3) # 1 + 2x^2 + 3x^3
p.2 = c(1,1)     # 1 + x
p.3 = conv(p.1,p.2)
print(p.3)

###  Let's compute (a + b)^n
###
bin.poly = c(1,1)
n = 2
bin.poly.2 = conv(bin.poly,bin.poly)
print(bin.poly.2)
bin.poly.3 = conv(bin.poly.2,bin.poly)
print(bin.poly.3)
bin.poly.4 = conv(bin.poly.3,bin.poly)
print(bin.poly.4)
bin.poly.5 = conv(bin.poly.4,bin.poly)
print(bin.poly.5)

#                  1
#             1    2   1
#           1   3    3    1
#        1    4    6    4    1
#     1    5    10   10   5    1
#
# You should recognize we are generating the
# binomial coefficients or Pascal's Triangle
#
# Convolution explains the identity
#   choose(n,k) = choose(n-1,k-1) + choose(n-1,k)

# Look at 100/81
options( digits = 10)
100/81

#  Convolution explains this
#  100/81 = (10/9)^2 = (1.111111111111.....)^2
#  Convolve this, and the answer is revealed

#  You can also use it to calculate the mean
#  of the geometric distribution.
#  P(X=k) = pq^(k-1),   k = 1,2,3,...

# Here we have to compute (1 + 2q + 3q^2 + 4q^3 ....
# etc. without end. Here q = 1-p

# Using convolution we realize this expression is
# (1 + q + q^2 + q^3 + ....)^2
# The geometric series is just 1/p. So we have
# (1 +2q + 3q^2 + 4q^3 + ....) = (1/p^2).
# now it is easy to find E[X].
# By the way, convolving infinite polynomials is known
# as the Cauchy product.









