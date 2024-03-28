# Load libraries
library(cluster)

# Read the data from the CSV file
data <- read.csv("C:\\Users\\sriva\\OneDrive\\Desktop\\Mall_customers.csv")

# Check for non-numeric columns
non_numeric_cols <- which(sapply(data, is.numeric) == FALSE)

# Handle non-numeric columns (replace with your specific logic)
if (length(non_numeric_cols) > 0) {
  # Example: Convert a column with income in string format to numeric
  if ("Income" %in% names(data) & "Income" %in% non_numeric_cols) {
    data$Income <- str_to_float(data$Income, na = rm_NA)  # Handles missing values (NA) using rm_NA
  }
  
  # Example: Create dummy variables for a categorical column (Gender)
  if ("Gender" %in% names(data) & "Gender" %in% non_numeric_cols) {
    data <- cbind(data, model.matrix(~ Gender + ., data = data)[-1])  # Creates dummies, excludes intercept
  }
  
  # Add your logic for handling other non-numeric columns if needed
}

# Select only numeric columns for clustering (optional)
if (length(non_numeric_cols) > 0) {
  data <- data[, sapply(data, is.numeric)]  # Select only numeric columns
}

# Perform hierarchical clustering using Ward's method
distance_matrix <- dist(data)
hclust <- hclust(distance_matrix, method = "ward.D2")

# Visualize the clustering dendrogram
plot(hclust, main = "Hierarchical Clustering Dendrogram", 
     sub = "Mall Customers Dataset")
abline(h = cutree(hclust, k = 3), col = "red",lty = "dashed")

# Cut the dendrogram at k=3 to get 3 clusters
clusters <- cutree(hclust, k = 3)

# Add a new column to the data frame indicating the cluster assignment
data$Cluster <- clusters

# Print the data frame with cluster labels
print(data)
