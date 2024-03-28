# Read the dataset
data <- read.csv("C:/Users/sriva/OneDrive/Desktop/Mall_Customers.csv")

# Remove the CustomerID column as it's not needed for clustering
data <- data[, -1]

# Extract numeric columns only
numeric_cols <- sapply(data, is.numeric)
data_numeric <- data[, numeric_cols]

# Standardize the numeric data
data_scaled <- scale(data_numeric)

# Calculate distance matrix
dist_matrix <- dist(data_scaled)

# Perform hierarchical clustering
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Cut the dendrogram to get clusters
num_clusters <- 5 # Set the desired number of clusters
clusters <- cutree(hclust_result, k = num_clusters)

# Plot the dendrogram
plot(hclust_result, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "")
rect.hclust(hclust_result, k = num_clusters, border = 2:6) # Highlight clusters with rectangles

# Add legend for clusters
legend("topright", legend = unique(clusters), col = 1:num_clusters, pch = 20, title = "Cluster")

# Plot the clusters in the data space
plot(data_scaled, col = clusters, pch = 20, main = "Hierarchical Clustering", 
     xlab = "Scaled Annual Income (k$)", ylab = "Scaled Spending Score (1-100)")
