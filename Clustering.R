##############################################################################################

clusteringKmeans <- function(data, k = 2) {

    preprocessed <- dataPreprocess_Clustering(data)
    result <- kmeans(as.data.frame(preprocessed[2]), k)
    data <- as.data.frame(preprocessed[1])
    data$Clustered_result <- as.character(result$cluster)

    return(list(data, result))

}

##############################################################################################

clusteringEM <- function(data) {

    preprocessed <- dataPreprocess_Clustering(data)
    result <- Mclust(as.data.frame(preprocessed[2]))
    data <- as.data.frame(preprocessed[1])
    data$Clustered_result <- as.character(result$classification)

    return(list(data, result))

}

##############################################################################################

clusteringDBSCAN <- function(data, eps = 0.3, minPts = 10) {

    preprocessed <- dataPreprocess_Clustering(data)
    result <- dbscan(as.data.frame(preprocessed[2]), eps = eps, minPts = minPts)    
    data <- as.data.frame(preprocessed[1])
    data$Clustered_result <- as.character(result$cluster + 1)

    return(list(data, result))

}

##############################################################################################


clusteringHeirachical <- function(data) {
    preprocessed <- dataPreprocess_Clustering(data)
    result <- dist(as.data.frame(preprocessed[2]), method="euclidean")
    data <- as.data.frame(preprocessed[1])

     return(list(data, result))
  
}

##############################################################################################

clusteringSpectral <- function(data, centers = NULL, nn = 7) {
    
    if(!is.null(centers) && centers == 0) centers <- NULL
    
    preprocessed <- dataPreprocess_Clustering(data)
    result <- specClust(as.data.frame(preprocessed[2]), centers = centers, nn = nn)
    data <- as.data.frame(preprocessed[1])
    data$Clustered_result <- as.character(result$cluster)
    
    return(list(data, result))
    
}
