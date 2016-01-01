#Utility functions for KMeans

##                                  ##
#                                    #
#  Calculate the Euclidean distance  #  
#                                    #
##                                  ##
euc.d<- 
function(v1,v2) {
  return(sqrt(sum( (v1 - v2)**2)))
}
  
##                                  ##
#                                    #
#  Calculate the within cluster      #
#  Sum of squares                    #  
#                                    #
##                                  ##
wcss<-
function(df, k, centroids) {
  within_cluster_SS <- 0
  doSum<- 
  function(i) {
    cluster_centroid <- rep.row(as.numeric(centroids[i,]), sum(df$cluster == i)) 
    cluster_members <- df[df$cluster ==i, -1] 
    difference_vectors <- cluster_members - cluster_centroid
    within_cluster_SS <- within_cluster_SS + sum(difference_vectors**2)
  }
  #For each row, compute the difference between the row and the centroid.
  #Each row is the component-wise difference of two points in Euclidean space
  #The square sum of this difference vector is the distance from the point to the
  #centroid.
  sapply(seq_along(1:k), doSum)
  return(within_cluster_SS)    
}

##                                  ##
#  Return the row index that         # 
#  corresponds to the largest        #
#  distance value among each of the  #
#  ids for the given row.            #
#                                    # 
#  The result is a label from 1:k.   #
##                                  ##
assign_membership<-
function(row, cluster_indexes){
  return(sort(row[cluster_indexes], index.return = T)$ix[1])
}

##                                  ##
#  Create a matrix of the same row   #
#  repeated n times                  #
#                                    #
##                                  ##
rep.row<-
function(x,n){
  if (n <= 0) return()
  matrix(rep(x,each=n),nrow=n)
}

##                                  ##
#  A mapping between cluster index   #
#  and the class id                  #
#                                    #
##                                  ##

#A global variable holding the mapping between cluster IDs and class IDs
map <- NULL
clustID_to_classID<-
function(cluster_indexes, response_column) {
  classes <- response_column[cluster_indexes]
  map <<- sapply(cluster_indexes, list)
  names(map) <<- classes
}

