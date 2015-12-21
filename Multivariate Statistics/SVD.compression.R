###############################################
#
#    SVD as a compression tool
#
###############################################

t = require(pixmap)
if ( t == FALSE )
  {
    install.packages("pixmap")
    require(pixmap)
  }

#  set the wrking directory
setwd(choose.dir())

#  read in the file
A = read.pnm("noyce_landmark_sig.pgm")
dev.new()
plot(A, main = "Original Picture")

data = A@grey   # S4 object uses @ instead of $
data.dim = dim(data)
print(data.dim)

data.svd = svd(data)
u        = data.svd$u   # get left singular vectors
v        = data.svd$v   # get right singular vectors
d        = data.svd$d   # get singular values

dev.new()
plot(d)  # plot the singular values
print(d)

#######################################################
#
#   function that compresses the picture
#
#######################################################
compress.pic <- function(A,u,v,d,r)
{ # compress.pic

  B = A   # copy original

  if ( r == 1 )
    { # handle special case
      D = matrix(d[1], ncol = 1)
    } # handle special case
  else
    { # normal case
      D = diag(d[1:r])
    } # normal case

  data.comp = u[,1:r]%*%D%*%t(v[,1:r])
  err       = data.comp - A@grey
  
  # ensure no elements in data.comp are not in the [0,1] range
  # note: plot will not work otherwise
  for ( i in 1: data.dim[1] )
    for ( j in 1:data.dim[2] )
      { # j loop
        if ( data.comp[i,j] < 0 )
          {
            data.comp[i,j] = 0
          }
        if ( data.comp[i,j] > 1 )
          {
            data.comp[i,j] = 1
          }
      } # j loop 
  B@grey = data.comp
  dev.new()
  plot(B, main = c(paste(r)," vectors"))
  return(err)
} # compress.pic

s = c(1,5,8,10,20,30,40,80,100,120,160,180,200,250,300,400,480)
for ( i in s )
  {
    err = compress.pic(A,u,v,d,i)
    print(i)
    print( c("max absolute error ", paste(max(abs(err)))) )
    total.err.sq = sum(err*err)
    print( c( "total error squared" ,paste(total.err.sq) ) )
    print("average error squared")
    print( total.err.sq/( nrow(err)*ncol(err) ) ) # avg. square error
    print("proportion of singular values")
    print(sum(d[1:i])/sum(d))
   }

