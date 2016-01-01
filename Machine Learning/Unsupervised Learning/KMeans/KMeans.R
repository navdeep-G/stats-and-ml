source("util_kmeans.R")
#kmeans
k.means <- function(data, k, class.col = NULL, eps = 1e-12, metric = "euclidean", pp=2) {
  
  init.centroids <- function(data, k, metric = "euclidean",pp=2) {
    
    #Create a distance/similarity matrix for the rows of data
    D <- as.matrix(dist(data, method = metric, p = pp))
    
    cluster_ids <- sample(nrow(data), 3) #numeric(k)
    
    #sample a row at random
    #cluster_ids[1] <- sample(nrow(data), 1)
    
    #Choose centroid 2 to be furthest from centroid 1.
    #This is done by sorting the distances from centroid 1 to
    #all other points and returning the index in the sorted array
    #corresponding to the greatest number in the array (sort
    #decreasig and grab the first one).
    #cluster_ids[2] <- sort(D[cluster_ids,], T, index.return = T)$ix[1]
   
    #D <- D^2 
    #for(i in 3:k) { 
    #    cluster_ids[i] <- sort(rowSums(D[cluster_ids[1:(i-1)],]), T, index.return = T)$ix[1]
    #  }
    
    #centroids are simply the rows of the data corresponding to the indexes.
    centroids <- data[cluster_ids,]
   
    #calculate the memberships of each of the rows
    initial_membership <- apply(D, 1, assign_membership, cluster_ids)

    #paste the initial memberships onto the front of the data frame
    df <- data.frame(cluster = initial_membership, data)

    #compute the initial within cluster SS
    J.init <- wcss(df, k, centroids)
    if(!is.null(class.col)) {
      clustID_to_classID(cluster_ids, class.col)
    }
    return(list(df, J.init))
  }
  
  update_centroids <- function(d, k) {
    centroids <- data.frame()
    for (i in 1:k) {
      #Compute the number of members in cluster i
      n_i <- sum(d$cluster == i)
      #calculate the new means of the observations in cluster i
      #Each row of centroids is simply the mean of the members in the cluster
      m_i <- (1 / n_i) * colSums(d[d$cluster == i, -1])
      
      #Append the new row to the current matrix of centroids
      centroids <- rbind(centroids, m_i)
      
      #colnames(centroids) <- strsplit
      #colnames(centroids) <- strsplit(paste(1:2, sep = " "), split = " ")
    }
    return(centroids)
  }
  
  update_membership <- function(row, mus) {
    #takes a row and the set of means

    #Creates a single column matrix of the means
    mus <- as.matrix(mus)

    #Compute the euclidean distance between each centroid (aka mu) and the given row
    #Sort the distances by least to greatest
    #Return the first index as the new cluster ID for the given row
    return(sort(apply(mus, 1, euc.d, row), F, index.return = T)$ix[1])
  }
 
  main <- function() { 
    #Initialize centroids
    initial <- init.centroids(data, k, metric, pp)


    #initial[[1]] is the original data with the initial members glued on the front
    data_ <- initial[[1]]

    #initial[[2]] is the initial within cluster sum of squares
    J <- initial[[2]]

    #Jp is the previous J
    Jp <- 0
    
    Js <- rep(Inf, 2)

    #while loop:
    #while current and previous wcss differences above threshold, loop
    while(abs(J - Js[2]) > eps & abs(Jp - Js[1]) > eps){
      Jp <- J
      #update the centroids
      centroids <- update_centroids(data_, k)
      #update the cluster memberships
      memberships <- apply(data, 1, update_membership, centroids)
      data_ <- data.frame(clusters = memberships, data)
      J <- wcss(data_, k, centroids)
      if (J < Js[1]) Js[1] <- J
      if (Jp < Js[2]) Js[2] <- Jp
    }
    clusters <- 1:k
    if(!is.null(map)) {
      #Take a cluster ID and send it it back to the class using the map
      map.to.class <- function(i) {
        names(map[i])
      }
      memberships <- sapply(data_$clusters, map.to.class)
      data_$clusters <- memberships
      clusters <- sapply(clusters, map.to.class)
    }
    return(list(data = data_, clusters = data.frame(clusters = 1:k, centroids), centers = memberships))
  }

  return(main())
}

