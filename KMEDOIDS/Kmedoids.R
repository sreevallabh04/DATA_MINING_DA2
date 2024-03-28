# Load libraries
library(cluster)

# Read the data from the CSV file
data <- read.csv("C:\\Users\\sriva\\OneDrive\\Desktop\\Mall_customers.csv")  # Update the path if needed

# Check for non-numeric columns (optional)
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

# Define the number of clusters (k) - Experiment with different values!
k <- 3

# Perform K-Medoids clustering
set.seed(123)  # Set a seed for reproducibility
kmedoids_model <- pam(data, k = k, metric = "euclidean")

# Add cluster labels to the data frame
data$Cluster <- kmedoids_model$cluster

# Print the data frame with cluster labels
data
# Assuming your data frame with cluster labels is named 'data'

# Get all data points in cluster 1
cluster_1_data <- data[data$Cluster == 1,]

# Get all data points in cluster 2
cluster_2_data <- data[data$Cluster == 2,]

# And so on for other clusters...

# Print the data for each cluster (optional)
cluster_1_data
cluster_2_data

