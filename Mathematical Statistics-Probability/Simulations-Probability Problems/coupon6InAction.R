#Go to the Coupon6.R script that is included in this repo.
#It starts with a function that simulates the number of times sample is called on a vector of length 3 until all 
#permutations of (1,2,3) are generated. 

#Change the code to find the number of times sample 
#must be called to generate all the permutations of (1,2,3,4).

############
#
#    coupon6 function
#
#    Specialized to 6 coupons
#
#    This code is not good for a general number of coupons
#    It gives an idea of one kind of strategy that may work
#    very well in other applications
#
#    Simulates the distribution of number of coupons bought
#    to acquire at least one of each coupon type.

#    Another interpretation is: how many permutations from sample()
#    do we compute before we have computed all 6 permuations?
#############

coupon6 <- function(n)   # n is number of simulations
{ # begin coupon 6
  
  t = 0   # number of coupons bought
  v = numeric(313)
  #   We represent the sample as a base 4 number
  #   Each permutation of (1,2,3,4) represents a coupon
  #  (1,2,3,4) maps to 64*1 + 16*2 + 4*3 +4 => v[112]
  #  (1,2,4,3) maps to 64*1 + 16*2 + 4*4 +3 => v[115]
  #  (1,4,2,3) maps to 64*1 + 16*4 + 4*2 +3 => v[139]
  #  (1,4,3,2) maps to 64*1 + 16*4 + 4*3 +2 => v[142]
  #  (1,3,4,2) maps to 64*1 + 16*3 + 4*4 +2 => v[130]
  #  (1,3,2,4) maps to 64*1 + 16*3 + 4*2 +4 => v[124]
  #  (2,3,1,4) maps to 64*2 + 16*3 + 4*1 +4 => v[184]
  #  (2,3,4,1) maps to 64*2 + 16*3 + 4*4 +1 => v[193]
  #  (2,4,3,1) maps to 64*2 + 16*4 + 4*3 +1 => v[205]
  #  (2,4,1,3) maps to 64*2 + 16*4 + 4*1 +3 => v[199]
  #  (2,1,4,3) maps to 64*2 + 16*1 + 4*4 +3 => v[163]
  #  (2,1,3,4) maps to 64*2 + 16*1 + 4*3 +4 => v[160]
  #  (3,1,2,4) maps to 64*3 + 16*1 + 4*2 +4 => v[220]
  #  (3,1,4,2) maps to 64*3 + 16*1 + 4*4 +2 => v[226]
  #  (3,4,1,2) maps to 64*3 + 16*4 + 4*1 +2 => v[262]
  #  (3,4,2,1) maps to 64*3 + 16*4 + 4*2 +1 => v[265]
  #  (3,2,4,1) maps to 64*3 + 16*2 + 4*4 +1 => v[241]
  #  (3,2,1,4) maps to 64*3 + 16*2 + 4*1 +4 => v[232]
  #  (4,1,2,3) maps to 64*4 + 16*1 + 4*2 +3 => v[283]
  #  (4,1,3,2) maps to 64*4 + 16*1 + 4*3 +2 => v[286]
  #  (4,3,1,2) maps to 64*4 + 16*3 + 4*1 +2 => v[310]
  #  (4,3,2,1) maps to 64*4 + 16*3 + 4*2 +1 => v[313]
  #  (4,2,3,1) maps to 64*4 + 16*2 + 4*3 +1 => v[301]
  #  (4,2,1,3) maps to 64*4 + 16*2 + 4*1 +3 => v[295]
  
  
  p = c(4^3,4^2,4,1) #Add 4^3 for base 4
  #Redefine vector s based on above
  s = c(112,115,124,130,139,142,160,163,184,193,199,205,220,226,232,241,262,265,283,286,295,301,310,313) 
  num.bought = numeric(n)
  for ( i in 1:n )
  { # for
    # each iteration of the for loop is one trial 
    t    = 0           # number of coupons bought
    v[s] = c(rep(0,6)) # so far no coupon bought
    while ( prod(v[s]) == 0 ) # true while one coupon not obtained yet
    { # while
      t = t + 1
      x = sample(c(1,2,3,4),4,replace = F)
      v[sum(p*x)] = 1
    } # while
    num.bought[i] = t       
  } # for
  return(num.bought)    
} # end coupon 6

n = 5000
b = coupon6(n)  # n trials
print(mean(b))
dev.new()
plot(table(b)/n, main = "Simulation PDF for\n obtaining 6 coupons") # simulation pdf

