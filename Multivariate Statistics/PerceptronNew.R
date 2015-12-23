library(mvtnorm)

S1 = matrix(c(2,-1,-1,2),ncol=2)
S2 = matrix(c(3,-0.5,-0.5,3), ncol = 2)

n1 = 106    # number of points to generate for class label 1
n2 = 100    # number of points to generate for class label 1

x1 = rmvnorm(n1,c(9,9),S1)

x2 = rmvnorm(n2,c(3,3),S2)

##  calulate plot limits. Limit will give a border of 1 around the extreme data points
x.min = min(c(x1[,1],x2[,1])) - 1
x.max = max(c(x1[,1],x2[,1])) + 1

y.min = min(c(x1[,2],x2[,2])) - 1
y.max = max(c(x1[,2],x2[,2])) + 1

dev.new()
plot(x1, xlim = c(x.min, x.max), ylim = c(y.min, y.max), col = 'red')
points(x2,col = 'blue')

x.all = c(x1[,1],x2[,1])
y.all = c(x1[,2],x2[,2])

#   initialize weights 
w0 = 0
w = c(0,0)

num.training.loops = 7

x1 = cbind(x1,rep(1,n1))  # append a class label of 1
x2 = cbind(x2,rep(-1,n2)) # append a class label of -1
d  = rbind(x1,x2)         # combine the data (including class labels) into a single matrix

for (k in 1:num.training.loops)
  {   #  train on a permutation of the data
    index = sample(1:(n1+n2),replace=FALSE) # index is a permutation of 1:n
    alpha = 0.01                            # set weight update rate
    #  note: we could have alpha depend on k. For example, use a larger alpha
    #        for the first 1 or two iterations of the k loop and then make it smaller
    err.count = 0                           # counts errors during this run through the data
    for ( i in 1:(2*n) )
      { # train on the permutation of the data given by the for (k ...) loop
        j = index[i]
        if ( ( w0 + w[1]*d[j,1] + w[2]*d[j,2]) > 0 )
          {
            fx = 1   # perceptron computes class label 1
          }
        else
          { 
            fx = -1   # perceptron computes class label -1
          }
        err = d[j,3] - fx    # d[j,3] is the actual class label
        if ( err != 0 ){  err.count = err.count + 1}
        # update perceptron weights
        w0   = w0   + alpha*(err)
        w[1] = w[1] + alpha*(err)*d[j,1]
        w[2] = w[2] + alpha*(err)*d[j,2]
      } # train on the permutation of the data given by the for (k ...) loop
        # view results of latest training cycle
    print(c(w0,w))
    print(err.count)
    m = -w[1]/w[2]
    b = -w0/w[2]
    abline(b,m)
    if ( err.count == 0 ) break;  # quit when you have no errors
  } # train on a permutation of the data

#  plot final class seperation line and perceptron weights
m = -w[1]/w[2]
b = -w0/w[2]
abline(b,m, col = 'green')
print(w)

