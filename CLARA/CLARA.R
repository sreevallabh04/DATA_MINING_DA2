# Load necessary package

library(cluster)

# Set the working directory to where the data file is located
setwd("C:/Users/sriva/OneDrive/Desktop")

# Read the data from the CSV file
df <- read.csv("Mall_Customers.csv")

# Convert categorical variables to numeric
df$Genre <- as.numeric(factor(df$Genre))

# Perform CLARA clustering
clara_result <- clara(df[,2:5], k = 5)  # Change 'k' to the number of clusters you want

# Print the clustering result
print(clara_result)

# Add the cluster assignments to your data frame
df$cluster <- clara_result$clustering

# Print the data frame with cluster assignments
print(df)
# Increase max.print limit
options(max.print = 10000)  # Change 10000 to the desired limit

# Now try printing your data again
print(df)
