require(mvtnorm)

S = matrix(c(3,2,2,5), ncol = 2)
mu1 = c(0,-4)
mu2 = c(5,6)

##   These points are generated for illustration of a graph.
##   The purpose is to have you visually comprehend the dividing
##   line between R1 and R2.

##   If a point is in R1 we declare it is from the first population.
##   If a point is in R2 we declare it is from the second population.

x1 = rmvnorm(5000,mu1,S)
x2 = rmvnorm(5000,mu2,S)

##  Plot first population in Red and second in Blue.
x  = rbind(x1,x2)
plot(x, type = 'n', main = "Equal Cost and a priori Prob")
points(x1,col = 'red')
points(x2,col = 'blue')

## Find the dividing line between R1 and R2

## Assume equal prior probabilities and equal costs
## for misclassification.

## Recall: We are in R1 when
## t(mu1-mu2)%*%S.inv%*%x >= 0.5%*%t(mu1-mu2)%*%S.inv%*%(mu1+mu2)

# Fisrt find inverse of covariance matrix
S.inv = solve(S)

## a is a 1x2 vector that multiplies x (which is 2x1)
a = t(mu1-mu2)%*%S.inv

## b is the RHS of the inequality for determining classification
b = 0.5*t(mu1-mu2)%*%S.inv%*%(mu1+mu2)

## Assume equality and find the straight line that divides R1
## and R2

m = -a[1]/a[2]
b.int = b/a[2]
abline(b.int,m)
text(-5,0,labels='R1', cex = 1.8)
text(-5,5,labels='R2', cex = 1.8)

## print line parameters
sprintf("slope = %f and intercept = %f",m,b.int)
##  Now let's test it.

# generate 10000 from first population

num = 10000
x1 = rmvnorm(num,mu1,S)

test       = a%*%t(x1) - rep(b,num)
r2         = which(test < 0 )
err.num    = length(r2)
error.rate.2when1 = err.num/num

# the rate of 1->2 misclassifications
print(error.rate.2when1)
points(x1[r2,], col = 'green')

## repeat for the second population
x2 = rmvnorm(num,mu2,S)

test              = a%*%t(x2) - rep(b,num)
r1                = which(test > 0 )
err.num           = length(r1)
error.rate.1when2 = err.num/num
# the rate of 2->1 misclassifications
print(error.rate.1when2)
points(x2[r1,], col = 'yellow')

## Note that both error rates are about the same


##  The above code was written for efficiency in testing

##  This code is slower, but more understandable

region_x1 = numeric(num)
region_x2 = numeric(num)
for ( i in 1:num)
{
  region_x1[i] = a%*%x1[i,] - b
  region_x2[i] = a%*%x2[i,] - b
}

###  here if region_x1 > 0 we classify correctly
###  if region_x2 < 0 we classify correctly

############################################################
#    Unequal Cost Case
############################################################
###  Now suppose C(2|1) is twice the cost of C(1|2)
cost.ratio = 1/2
###  The cost of misclassification is
cost = (1/cost.ratio)*error.rate.2when1 + error.rate.1when2
 
##   Find new line

##   now we need ln(C(1|2)/C(2|1)
k = log(cost.ratio)

##  a is the same as before, as is b
##  but the new b.int is

b.int = (b+k)/a[2] 


## print line parameters
sprintf("slope = %f and intercept = %f",m,b.int)

## Note that the dividing line between R1 and R2 moves up.
## Now we will have fewer 1 -> 2 misclassifications, but more
## 1 -> 2 misclassifications. But the latter are less costly.

dev.new()
plot(x, type = 'n', main = "Unequal Cost and Equal a priori Prob")
points(x[1:5000,],col = 'red')
points(x[5001:10000,],col = 'blue')
abline(b.int,m)
text(-5,-2,labels='R1', cex = 1.8)
text(-5,8,labels='R2', cex = 1.8)



test       = a%*%t(x1) - rep((b+k),num)
r2         = which(test < 0 )
err.num    = length(r2)
error.rate.2when1 = err.num/num
# the rate of 1 -> 2 misclassifications
print(error.rate.2when1)

test              = a%*%t(x2) - rep((b+k),num)
r1                = which(test > 0 )
err.num           = length(r1)
error.rate.1when2 = err.num/num

# the rate of 2 -> 1 misclassifications
print(error.rate.1when2)


# Note the error rates for 1 -> 2 misclassifications have gone
# down, and the 2 -> 1 misclassification rate is higher
cost = (1/cost.ratio)*error.rate.2when1 + error.rate.1when2
print(cost)

#####################################################
#    Unequal a priori probabilities
#    Equal costs
#####################################################
p1 = 1/4
p2 = 3/4

num1 = round(p1*num,0)
num2 = num - num1
##  Generate x1 and x2 with the a priori probabilities
##  indicated.
x1 = rmvnorm(num1,mu1,S)
x2 = rmvnorm(num2,mu2,S)

# new k

k = log(p2/p1)

b.int = (b + k)/a[2]


## print line parameters
sprintf("slope = %f and intercept = %f",m,b.int)

## Note that the dividing line between R1 and R2 moves down.
## Since so many more of the data points are from population 2
## than from population 1, we must move the dividing line.
## We will have a higher RATE of 1 -> 2 mistakes than 2 -> 1
## mistakes, but the TOTSL 1 -> 2 and 2 -> 1 mistakes should
## be the same.

dev.new()
plot(x, type = 'n', main = "Equal Cost and Unequal a priori Prob")
points(x[1:5000,],col = 'red')
points(x[5001:10000,],col = 'blue')
abline(b.int,m)
text(-5,-4,labels='R1', cex = 1.8)
text(-5,8,labels='R2', cex = 1.8)



test       = a%*%t(x1) - rep((b+k),num1)
r2         = which(test < 0 )
err.num    = length(r2)
error.rate.2when1 = err.num/num1
# the rate of 1 -> 2 misclassifications
print(error.rate.2when1)

test              = a%*%t(x2) - rep((b+k),num2)
r1                = which(test > 0 )
err.num           = length(r1)
error.rate.1when2 = err.num/num2

# the rate of 2 -> 1 misclassifications
print(error.rate.1when2)

###  Error rates differ significantly, but total number
###  of errors is about the same for 1 -> 2 and 2 -> 1
###  misclassifications
print(num1*error.rate.2when1)
print(num2*error.rate.1when2)

## Overall error rate
print( (num1*error.rate.2when1 + num2*error.rate.1when2)/num)



#### Suppose we use the dividing line valid for equal
#### a priori probabilities. We should see the rates of
#### each mistake be the same, but the total number of
#### errors will be larger.


test       = a%*%t(x1) - rep(b,num1)
r2         = which(test < 0 )
err.num    = length(r2)
error.rate.2when1 = err.num/num1
# the rate of 1 -> 2 misclassifications
print(error.rate.2when1)

test              = a%*%t(x2) - rep(b,num2)
r1                = which(test > 0 )
err.num           = length(r1)
error.rate.1when2 = err.num/num2
# the rate of 2 -> 1 misclassifications
print(error.rate.1when2)

print(num1*error.rate.2when1)
print(num2*error.rate.1when2)

## Overall error rate
print( (num1*error.rate.2when1 + num2*error.rate.1when2)/num)



