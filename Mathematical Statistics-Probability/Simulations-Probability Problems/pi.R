### Probability simulation used to estimate pi = 3.14

B = 100000000
  
x = runif(B, min=-1, max=1)
y = runif(B, min=-1, max=1)

# check if the points (x,y) are in the unit circle x^2 + y^2 = 1

Count = 0

for(i in 1:B){
  if (x[i]^2+y[i]^2 <= 1) Count = Count + 1
}

# Since the area of a circle is A = (pi/4)*D^2, D = diameter
# Here D = 1, so the area A = pi/4 => pi = 4*A
# To estimate A we can estimate the probability of a point (x,y)
# falling into the unit cirle and then multiplying by 4.

A.hat = Count/B

pi.hat = 4*A.hat
pi.hat

# compare to the true value of pi

pi
