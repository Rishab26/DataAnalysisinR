

nci.data <- read.table("~/nci.data.txt", col.names = read.table("~/label.txt", header = FALSE)$V1)
col_names = read.table("~/label.txt", header = FALSE)
nci.data<- t(nci.data)
nci.data_scaled = scale(nci.data)
distance_matrix = dist(nci.data_scaled, method = "euclidean")
distance_matrix = as.matrix(distance_matrix)
distancemat_dataframe <- data.frame(distance_matrix)
countofrows <- nrow(distancemat_dataframe)
merge <- matrix(0, countofrows-1, 2)
valueofminpair <- vector(length = countofrows-1)
diag(distancemat_dataframe) <- Inf # avoiding zeros in calculations
colnames(distancemat_dataframe) <- -(1:countofrows)
rownames(distancemat_dataframe) <- -(1:countofrows)
for (i in 1:(countofrows-1)) { 
  cols <- colnames(distancemat_dataframe)
  # Finding the pair with the minimum distance
  min_d <- which(distancemat_dataframe == min(distancemat_dataframe), arr.ind = TRUE)[1,,drop=FALSE]# The which() function returns the row and column position of the pair
  valueofminpair[i] <- min(distancemat_dataframe) # The height is the value of the pair with the minimum distance
  # The row and column position of the minimum pair is stored as sequence m in the merge object
  merge[i,] <- as.numeric(cols[min_d])
  
  # Johnson's algorithm Step 3: The pair with the minimum distance is merged
  
  # The cluster object is used to find previous clusters that the pair belong to (if they exist)
  # Does this by finding any columns above 0 (since all column names are negative, a positive 
  # column value implies it has been clustered)
  cluster <- c(min_d, which(cols %in% cols[min_d[1, cols[min_d] > 0]]))
  
  colnames(distancemat_dataframe)[cluster] <- i # Rename the columns indicated by cluster to the sequence number, m
  
  # Merge the pairs according to Johnson's algorithm and the single linkage method
  nci.data.merged <- apply(distancemat_dataframe[min_d,], 2, min)
  
  # Johnson's algorithm Step 4: Remove column and row corresponding to old clusters and
  # insert a new column and row for newly formed cluster.
  
  # The insertion of the cluster is done by setting the first sequential row and column of the
  # minimum pair in the distance matrix (top to bottom, left to right) as the cluster resulting 
  # from the single linkage step
  distancemat_dataframe[min(min_d),] <- nci.data.merged
  distancemat_dataframe[,min(min_d)] <- nci.data.merged
  
  # Make sure the minimum distance pair is not used again by setting it to Inf
  distancemat_dataframe[min(min_d), min(min_d)] <- Inf
  
  # The removal step is done by setting the second sequential row and column of the minimum pair
  # (farthest right, farthest down) to Inf
  distancemat_dataframe[max(min_d),] <- Inf
  distancemat_dataframe[,max(min_d)] <- Inf
}
distance_matrix = dist(nci.data_scaled, method = "euclidean")
poll.clust <- hclust(distance_matrix, method = 'single')
hclust.obj <- list() # Initialize an empty list
hclust.obj$merge <- merge # Add the merge component obtained earlier
hclust.obj$order <- poll.clust$order # Here the order component from hclust is added to the list
hclust.obj$height <- valueofminpair # The height component determines the lengths of the dendogram nodes
#hclust.obj$labels <- col_names # Add the city names to the labels component
class(hclust.obj) <- 'hclust' # The list is set to class hclust
plot(hclust.obj)


hc.complete=hclust(distance_matrix, method="complete")
hc.average=hclust(distance_matrix, method ="average")
hc.cen=hclust(distance_matrix, method ="centroid")
par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.cen, main="Centroid Linkage", xlab="", sub="", cex=.9)


set.seed(2611)
library(ISLR)
nci_labels = NCI60$labs
nci_data = NCI60$data
nci.data_scaled = scale(nci_data)


for (i in seq(1,50,1)){
  km_out2 <- kmeans(nci.data_scaled, i,nstart = 20)
  print(i)
  km_clusters2 = km_out2$cluster
  print(km_clusters2)
  print(table(km_clusters2,nci_labels))
}

# for k = 4 is giving the best result

km_out = kmeans(nci.data_scaled, 4, nstart = 20) 
km_clusters = km_out$cluster
print(km_clusters)
print(table(km_clusters,nci_labels))


hc_out = hclust(distance_matrix)
hc_clusters = cutree(hc_out,4)
table(km_clusters,hc_clusters)



