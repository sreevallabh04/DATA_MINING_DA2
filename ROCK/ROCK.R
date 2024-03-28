# Function to compute distance between two data points
distance <- function(point1, point2) {
  sqrt(sum((point1 - point2)^2))
}

# Function to find mutual nearest neighbors within a δ-neighborhood
find_mutual_neighbors <- function(data, index, delta) {
  neighbors <- numeric()
  for (i in 1:nrow(data)) {
    if (i != index && distance(data[index, ], data[i, ]) <= delta) {
      if (sum(distance(data[i, ], data) <= delta) > 1) {
        neighbors <- c(neighbors, i)
      }
    }
  }
  return(neighbors)
}

# Function to perform ROCK clustering
ROCK_clustering <- function(data, k, delta) {
  # Initialize cluster assignments
  cluster <- rep(0, nrow(data))
  cluster_index <- 1
  
  # Iterate over data points
  for (i in 1:nrow(data)) {
    if (cluster[i] == 0) {
      # Find mutual nearest neighbors within δ-neighborhood
      neighbors <- find_mutual_neighbors(data, i, delta)
      
      if (length(neighbors) >= k) {
        # Assign current point and its neighbors to new cluster
        cluster[i] <- cluster_index
        cluster[neighbors] <- cluster_index
        cluster_index <- cluster_index + 1
      }
    }
  }
  
  return(cluster)
}

# Example usage:
# Read the dataset
data <- read.csv("C:/Users/sriva/OneDrive/Desktop/Mall_Customers.csv")

# Extract relevant numeric columns for clustering
data_numeric <- data[, c("Annual.Income..k..", "Spending.Score..1.100.")]

# Standardize the data
data_scaled <- scale(data_numeric)

# Set parameters for ROCK clustering
k <- 5     # Number of clusters
delta <- 0.5   # Distance threshold

# Perform ROCK clustering
rock_clusters <- ROCK_clustering(data_scaled, k, delta)

# Plot the clusters
plot(data_scaled, col = rock_clusters, pch = 20, 
     main = "ROCK Clustering", xlab = "Scaled Annual Income (k$)", 
     ylab = "Scaled Spending Score (1-100)")

# Add legend for clusters
legend("topright", legend = unique(rock_clusters), 
       col = 1:length(unique(rock_clusters)), pch = 20, 
       title = "Cluster")
# Check for NaN or infinite values
any(is.nan(data_scaled))
any(is.infinite(data_scaled))
# Check if any points are assigned to clusters
any(rock_clusters == 0)
# Plot the data points without clustering
plot(data_scaled, pch = 20, 
     main = "Data Points", xlab = "Scaled Annual Income (k$)", 
     ylab = "Scaled Spending Score (1-100)")

