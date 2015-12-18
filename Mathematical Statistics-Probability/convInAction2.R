#X ~ binom(40, 0.8) and Y ~poisson(12). X and Y are independent. Z = X + Y. 
#Use conv() function to calculate the pdf of Z. 
#Using the pdf of Z you calculated, calculate E[Z] and Var(Z). 
#Plot the pdf of Z. 
#Calculate P( Z <= 40 ). Since Y is Poisson, the support of Z is semi-infinite. 
#When convolving X and Y, it will suffice (actually more than suffice) to truncate the pdf of Y at 
#200. 

source("conv.function.R")

#Define binomial(40,.8) and poisson(12)
n = 40 
p = 0.8
lamda = 12
x = dbinom(0:n,n,p) # pdf of values over support
y = dpois(0:140,12)

#Get convolution for Z = x + y
Z = conv(x,y)

# Below is used to find expectation of the convolution
x = Z
k = length(x)
expectation <- function(x)
{
  for (i in 1:k)
  {
    x[i] = (x[i]*(i-1))
  }
  return(x)
}
expected = expectation(x)
expected.value = sum(expected)
expected.value

#Below is used to find variance of convolution
v = Z
k = length(v)
variation <- function(v)
{
  for (i in 1:k)
  {
    v[i] = (v[i]*(i-1)^2)
  }
  return(v)
}
variant = variation(v)
var.conv = sum(variant)
variance = var.conv - (expected.value)^2
variance

#Plot of pdf,Z
plot(0:180,Z,type="l", ylab="Probability", xlab="Total value of Z = X + Y")

#Before we find probabilities, we need the standard deviation
standard.dev = sqrt(variance)
standard.dev

#Find probabilit Z<=40
prob.z = pnorm(40, mean=expected.value, sd=standard.dev)
prob.z