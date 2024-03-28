# Load necessary package
library(cluster)

# Set the working directory to where the data file is located
setwd("C:/Users/sriva/OneDrive/Desktop")

# Read the data from the CSV file
df <- read.csv("Mall_Customers.csv")

# Convert categorical variables to numeric
df$Genre <- as.numeric(factor(df$Genre))

# Perform PAM clustering
pam_result <- pam(df[,2:5], k = 5)  # Change 'k' to the number of clusters you want

# Print the clustering result
print(pam_result)

# Perform PAM clustering
pam_result <- pam(df[, c("Age", "Annual.Income..k..")], k = 5)  # Change 'k' to the number of clusters you want

# Add the cluster assignments to your data frame
df$cluster <- pam_result$clustering

# Plot the data points with different colors for each cluster
plot(df$Age, df$Annual.Income..k.., col = df$cluster, 
     xlab = "Age", ylab = "Annual Income (k$)",
     main = "PAM Clustering Result")

# Add legend
legend("topright", legend = unique(df$cluster), col = unique(df$cluster), pch = 1, title = "Cluster")
