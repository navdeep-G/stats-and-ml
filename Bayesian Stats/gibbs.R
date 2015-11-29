#Function for Gibbs sampling
gibbs=function(sims, beta.start, alpha, gamma, delta, y, t)
{
  lambda.draws = matrix(NA, nrow = sims, ncol = length(y))
  beta.draws = c()
  lambda.update = function(alpha, beta, y, t)
  {
    rgamma(length(y), y + alpha, t+beta)
  }
  beta.update = function(alpha, gamma, delta, lambda, y)
  {
    rgamma(1, length(y)*alpha+gamma, delta + sum(lambda))
  }
  
  beta.curr = beta.start
  
  for( i in 1: sims)
  {
    lambda.curr = lambda.update(alpha, beta.curr, y, t)
    beta.curr = beta.update(alpha, gamma, delta, lambda.curr, y)
    
    lambda.draws[i, ] <- lambda.curr
    beta.draws[i] <- beta.curr
  }
  
  return(list(lambda.draws = lambda.draws, beta.draws = beta.draws))
}
#Run for Gibbs function
y= c( 5, 1, 5, 14, 3, 19, 1, 1, 4, 22)
t= c( 94.32, 15.72, 62.88, 125.76, 5.24, 31.44, 1.05, 1.05, 2.10, 10.48)
run = gibbs(6000, 1, 2, 0.01, 1, y, t)

colMeans(run$lambda.draws)
mean(run$beta.draws)
apply(run$lambda.draws, 2, sd)
sd(run$beta.draws)