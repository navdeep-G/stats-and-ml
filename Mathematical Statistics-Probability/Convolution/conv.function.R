#Convolution function

conv <- function(x,y)
{ # function starts
  
  #Error Checking
  if ( !is.vector(x) )  { stop("parameter must be a vector") }
  if ( !is.numeric(x) ) { stop("parameter must be numeric") }
  if ( !is.vector(y) )  { stop("parameter must be a vector") }
  if ( !is.numeric(y) ) { stop("parameter must be numeric") }
  
  #Get relevant lengths of vectors x & y
  n = length(x)
  m = length(y)
  
  #Make a new sequence a where new.x is a vector of x flipped from n:1 and the rest are 0's
  new.x = c(x[n:1],rep(0,(m-1)))
  
  #Make a new sequence new.y where new.y is a vector of 0's and then y
  new.y = c( rep(0,(n-1)),y )
  
  #Get relevant lengths k (only need to get of a since length(new.x) = length(new.y))
  k = (length(new.x))
  
  #Allocate vector for convolution
  conv.vec = numeric(k)
  
  #Start loop to calculate convolution
  for (i in 1:k)
  { #i
    conv.vec[i]=sum(new.x*new.y)
    
    #Re allocate new.x for next iteration in loop. This is essentially shifting, getting the overlap, multiplying, and adding
    #across the overlap.
    new.x = c(0,new.x)
    new.x = new.x[1:k]
  }#i
  return(conv.vec)
} # function ends
