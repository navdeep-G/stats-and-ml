#########################################################################
#                    Classification Problem
#########################################################################


# Given below is a covariance matrix common to two populations
S = matrix( c(1,-0.2,0.3,-0.2,2,0.5,0.3,0.5,3), ncol = 3)

# given below is the mean for the first population
mu1 = c(1,4,7)

# given below is the mean for the second population
mu2 = c(-1,0,4)


# classify the following points as coming from the first or second
# population. Assume equal a priori probabilities for population 1
# and population 2. Also assume both misclassification costs are equal.

# note that here we assume knowledge of the population parameters.

x0 = c(0,2,5)
x1 = c(2,-3,6)
x2 = c(3,-0.5,2)
x3 = c(0,3,6)
x4 = c(0.5,2,3.5)

x1 = rmvnorm(5000,mu1,S)
x2 = rmvnorm(5000,mu2,S)

x  = rbind(x1,x2)
plot(x, type = 'n', main = "Equal Cost and a priori Prob")
points(x1,col = 'brown4')
points(x2,col = 'cadetblue')

## Find the dividing line between R1 and R2
## Assume equal prior probabilities and equal costs for misclassification.

# First find inverse of covariance matrix
S.inv = solve(S)

## a is a 1x2 vector that multiplies x (which is 2x1)
a = t(mu1-mu2)%*%S.inv

## b is the RHS of the inequality for determining classification
b = 0.5*t(mu1-mu2)%*%S.inv%*%(mu1+mu2)

## Assume equality and find the straight line that divides R1 and R2

m = -a[1]/a[2]
b.int = b/a[2]
abline(b.int,m) ###### The optimal divide line
text(-3,5,labels='R2', cex = 1.8)
text(-3,8,labels='R1', cex = 1.8)

## print line parameters
sprintf("slope = %f and intercept = %f",m,b.int)

############################
##  Now let's test it.
############################

x0 = c(0,2,5)
x1 = c(2,-3,6)
x2 = c(3,-0.5,2)
x3 = c(0,3,6)
x4 = c(0.5,2,3.5)

points(x0, col = 'black', pch=15) # Seems to be from 1st population
points(x1, col = 'red', pch=16) # Hard to tell where its from
points(x2, col = 'green', pch=17) # Seems to be from first population
points(x3, col = 'blue', pch=19) # Seems to be from first population
points(x4, col = 'darkgoldenrod1', pch=20) # Seems to be from first population
