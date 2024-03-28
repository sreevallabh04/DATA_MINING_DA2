# Load required library
library(dbscan)

# Read the dataset
data <- read.csv("C:/Users/sriva/OneDrive/Desktop/Mall_Customers.csv")

# Extract relevant numeric columns for clustering
# In this case, we'll use "Annual.Income..k.." and "Spending.Score..1.100."
data_numeric <- data[, c("Annual.Income..k..", "Spending.Score..1.100.")]

# Standardize the data
data_scaled <- scale(data_numeric)

# Set parameters for DBSCAN
epsilon <- 0.5  # Adjust this value according to your dataset
MinPts <- 5     # Adjust this value according to your dataset

# Perform DBSCAN clustering
dbscan_result <- dbscan(data_scaled, eps = epsilon, MinPts = MinPts)
dbscan_result
# Plot the clusters
plot(data_scaled, col = dbscan_result$cluster, pch = 20, 
     main = "DBSCAN Clustering", xlab = "Scaled Annual Income (k$)", 
     ylab = "Scaled Spending Score (1-100)")

# Add legend for clusters
legend("topright", legend = unique(dbscan_result$cluster), 
       col = 1:length(unique(dbscan_result$cluster)), pch = 20, 
       title = "Cluster")
