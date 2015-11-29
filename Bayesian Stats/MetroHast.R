#Metropolis Hastings Algorithm
a = 2
b = 6
N = 5000
x = a/(a+b)

for (i in 2:N){
  x.cand = runif(1)
  r = min(dbeta(x.cand,a,b)/dbeta(x[i-1],a,b),1)
  u = runif(1)
  
  if(u<r)
    
    {x[i] =x.cand}
    
    else
      {x[i] = x[i-1]}
  }
  
time = 1:N
plot(time,x,type = "l")
hist(x)
mean(x)
acf(x)
