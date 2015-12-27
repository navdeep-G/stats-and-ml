#Let Y be the sum of 25 throws of a die. 
#Use conv() function to calculate the pdf of Y. 
#Using only the pdf of Y, calculate E[Y] and Var(Y). 
#Plot the pdf of Y. 
#Calculate P( 79 <= Y <= 96) and also P( 70 <= Y <= 105). 

source("conv.function.R")

#Define two vectors for convolution function
rolling = c(0,rep(1/6,6))
dice = c(0, rep(1/6,6))

#Convolve 25 rolls of a die. So, if X1 = one roll of a die, then Y = X1+X2+...+X25
for (i in 1:24)
{
  rolling = conv(rolling, dice)
}
rolling

# Plot of roll
plot(0:150,rolling,type="l", ylab="Probability", xlab="Total value of 25 rolls: 0 to 150")

# Below is used to find expectation of the convolution
x = rolling
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
v = rolling
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

#Before we find probabilities, we need the standard deviation
standard.dev = sqrt(variance)
standard.dev

# P( 79 <= Y <= 96)

prob1 = pnorm(96, mean=expected.value, sd=standard.dev)

prob2 = pnorm(79, mean=expected.value, sd=standard.dev)
actualprob = prob1 - prob2
actualprob

#P( 70 <= Y <= 105)

prob3 = pnorm(105, mean=expected.value, sd=standard.dev)
prob4 = pnorm(70, mean=expected.value, sd=standard.dev)
actualprob2  = prob3 - prob4
actualprob2
