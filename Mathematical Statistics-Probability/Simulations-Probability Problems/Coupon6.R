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
   v = numeric(57)
#   We represent the sample as a base 4 number
#   Each permutation of (1,2,3) represents a coupon
#  (1,2,3) maps to 16*1 + 4*2 + 3 => v[27]
#  (1,3,2) maps to 16*1 + 4*3 + 2 => v[30]
#  (2,1,3) maps to 16*2 + 4*1 + 3 => v[39]
#  (2,3,1) maps to 16*2 + 4*3 + 1 => v[45]
#  (3,1,2) maps to 16*3 + 4*1 + 2 => v[54]
#  (3,2,1) maps to 16*3 + 4*2 + 1 => v[57]
   p = c(4^2,4,1)
   s = c(27,30,39,45,54,57)
   num.bought = numeric(n)
   for ( i in 1:n )
     { # for
                            # each iteration of the for loop is one trial 
         t    = 0           # number of coupons bought
         v[s] = c(rep(0,6)) # so far no coupon bought
         while ( prod(v[s]) == 0 ) # true while one coupon not obtained yet
           { # while
               t = t + 1
               x = sample(c(1,2,3),3,replace = F)
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
###################################
#    Geometric pdf's
###################################

# g1 is the RV associated with acquiring the first coupon
g.1 = c(0,1)           # the certain event

# g2 is the RV associated with acquiring the second coupon
p    = 1/6
q    = 1 - p  # P(success) or P(moving to state 2)
q.v  = rep(q,100)
pow  = 0:99
g.2  = c(0,p*q.v^pow)
print( sum(g.2) )

# g3 is the RV associated with acquiring the third coupon
p    = 2/6
q    = 1 - p  # P(success) or P(moving to state 3)
q.v  = rep(q,100)
pow  = 0:99
g.3  = c(0,p*q.v^pow)

#   etc

p    = 3/6
q    = 1 - p  # P(success) or P(moving to state 4)
q.v  = rep(q,100)
pow  = 0:99
g.4  = c(0,p*q.v^pow)

p    = 4/6
q    = 1 - p  # P(success) or P(moving to state 5)
q.v  = rep(q,100)
pow  = 0:99
g.5  = c(0,p*q.v^pow)
 
p    = 5/6
q    = 1 - p  # P(success) or P(moving to state 6)
q.v  = rep(q,100)
pow  = 0:99
g.6  = c(0,p*q.v^pow)

pdf.2 = conv(g.1,g.2)
pdf.3 = conv(pdf.2,g.3)
pdf.4 = conv(pdf.3,g.4)
pdf.5 = conv(pdf.4,g.5)
pdf.6 = conv(pdf.5,g.6)

#     Let Y be the RV indicating the number of purchases
#     required to obtain all six coupons
#     pdf.6[i] = P(Y = i - 1)
#####  truncate pdf.6

coupon.pdf = pdf.6[1:100]

coupon.mean = sum( (0:99)*coupon.pdf)
print(coupon.mean)
print( (6/6)+(6/5)+(6/4)+(6/3)+(6/2)+(6/1) ) # theoretical mean

#  If we use the entire pdf.6
sum( (0:(length(pdf.6) - 1))*pdf.6)

#  What is the probaility we get all 6 coupons with 14 or fewer purchases?

sum(pdf.6[1:15])

options( digits = 16)
print(pdf.6[7])  # P(we neeeded 6 purchases). 

# This should equal (1)*(5/6)*(4/6)*(3/6)*(2/6)*(1/6)
print( factorial(5)/6^5)

#  It appears the accuracy is adequate. The number has a lovely pattern,
#  hasn't it?

# It is (5/9)*(1/36) or
#  0.5555555555.... times 0.027777777777777...     

# 
x11()
plot(pdf.6[1:60], ylab = 'density',type = 'h', main = "PDF for obtaining 6 Coupons")
## note the effect of type = 'h'   

print(pdf.6[8])  # P(we neeeded 7 purchases). 

#   Ratio of 7 purchases to 6 purchases is explicable from the state
#   diagram
print(pdf.6[8]/pdf.6[7])
    