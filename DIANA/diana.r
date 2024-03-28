# Load necessary package
library(ggplot2)
library(cluster)


# Set the working directory to where the data file is located
setwd("C:/Users/sriva/OneDrive/Desktop")

# Read the data from the CSV file
df <- read.csv("Mall_Customers.csv")

# Convert categorical variables to numeric
df$Genre <- as.numeric(factor(df$Genre))

# Perform DIANA clustering
diana_result <- diana(df[,2:5])  # No need to specify a method for DIANA

# Print the clustering result
print(diana_result)

# Cut the dendrogram to obtain cluster assignments
cut_result <- cutree(diana_result, k = 5)  # Adjust 'k' to the number of clusters you want

# Add the cluster assignments to your data frame
df$cluster <- cut_result

# Print the data frame with cluster assignments
print(df)
pltree(diana_result, cex = 0.6, hang = -1, main = "Dendrogram of DIANA Clustering")

# Plot the clusters
ggplot(df, aes(x = Annual.Income..k.., y = Spending.Score..1.100., color = as.factor(cluster))) +
  geom_point(alpha = 0.6, size = 2) +
  labs(color = "Cluster") +
  ggtitle("Scatter Plot of Clusters") +
  theme_minimal()

