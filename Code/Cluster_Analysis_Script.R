# INSTITUTO SUPERIOR TECNICO, UNIVERSITY OF LISBON
# TRANSPORT DEMAND MODELLING COURSE, PROF. FILIPE MOURA

# CLUSTER ANALYSIS

# Example Airports

# Variables: 

# Code
# Airport
# Ordem
# Passengers
# Movements
# Number of airlines
# Main air line flights percentage
# Maximum percentage of traffic per country
# Number of Low Cost Airlines
# Low Cost Airlines percentage
# Destinations
# Average_route_Distance
# Distance to closest Similar Airport
# Airport Regional Relevance
# Distance to city km
# Inhabitants corrected
# Number of visitors corrected
# GDP corrected
# Cargoton


# Import Libraries

# Library used for reading excel files
library(readxl)
# Library used for summary statistics
library(skimr)
# Library used in data science to perform exploratory data analysis
library(tidyverse) 
# Library used for model based clustering
library(mclust)
# Library used for cluster analysis
library(cluster)
# Library used for visualizing distances
library(factoextra)


# Set working directory

setwd("G:/O meu disco/TDM - Lecture R/Cluster Analysis")

# Import dataset
dataset <- read_excel("Data_Aeroports_Clustersv1.xlsX")

# Transform dataset into dataframe

df <- data.frame(dataset)

# Summary statistics

skim(df)

# Now let us plot an example and take a look

plot(Numberofairlines ~ Destinations, df)
with(df, text(Numberofairlines ~ Destinations, label = Airport, pos = 4, cex = 0.6))

  ##Note: You can already guess the number of clusters by visualizing the two variables. 
  ##However, this is not clear and it does not consider the other variables. 


# Treat the data before performing a cluster analysis

  ## In this example we do not have missing values. 
  ## In case you do have in the future, you can take out the missing values with listwise deletion
  df <- na.omit(df)

  ## Leave only continuous variables, and take out "Ordem"

  drop <- c("Code","Airport", "Ordem")
  df_reduced = df[,!(names(df) %in% drop)]

  # Analyze how the scale of the values of the variables are different 
  head(df_reduced)

  # Standardization (Z-score) - (xi - xmean / standard deviation)

  mean <- apply(df_reduced, 2, mean) # The "2" in the function is used to select the columns. MARGIN: c(1,2)

  sd <- apply(df_reduced, 2, sd)

  df_scaled <- scale(df_reduced, mean, sd)


# HIERARCHICAL CLUSTERING

  # Measuring Similarity through Euclidean distances

  distance <- dist(df_scaled, method = "euclidean")
  print(distance, digits = 3)

  # Visualize distances in heatmap
  fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), order = FALSE)

  # Note: There are other forms distance measures that can be used such as: 
  ## Minkowski distance; 
  ## Manhattan distance; 
  ## Mahanalobis distance.

# 1. Complete linkage (Farthest neighbor) clustering algorithm 
  ## Based on the maximum distance between observations in each cluster.

  modelc <- hclust(distance, "complete")
  plot(modelc, labels = df$Airport, xlab = "Distance - Complete linkage", hang = -1)

  # Visualize the cut on the tree 
  rect.hclust(modelc, 4, border = "blue")

# 2. Average linkage between groups 
  ## The distance between clusters is the average of the distances between 
  ## observations in one cluster to all the members in the other cluster. 

  modela <- hclust(distance, "average")
  plot(modela, labels = df$Airport, xlab = "Distance - Average linkage", hang = -1)
  rect.hclust(modelc, 4, border = "red")

# 3. Ward`s method
  ## The measures of similarity are the sum of squares within the cluster summed
  ## over all variables. 

  modelw <- hclust(distance, "ward.D2")
  plot(modelw, labels = df$Airport, xlab = "Distance - Ward method", hang = -1)
  # Visualize where to cut on the tree (choose number of clusters)
  rect.hclust(modelw, 4, border = "orange")

# 4. Centroid method
  ## The similarity between two clusters is the distance between its centroids.
  
  modelcen <- hclust(distance, "centroid")
  plot(modelcen, labels = df$Airport, xlab = "Distance - Centroid method", hang = -1)
  rect.hclust(modelcen, 4, border = "green")

# Now lets evaluate the membership of each observation with the cutree function for each method.

  member_com <- cutree(modelc, 4)
  member_av <- cutree(modela, 4)
  member_ward <- cutree(modelw, 4)
  member_cen <- cutree(modelcen, 4)

# Plot table to compare the how common each method is to each other.
# Lets compare the complete linkage with the average linkage
  table(member_com, member_av)


# Silhouette Plot 
## Analyzes how similiar an observation is to its own cluster compared to other clusters.
## The clustering configuration is appropriate when most objects have high values. 
## Low or negative values indicate that the clustering does not have an appropriate number of clusters.


  plot(silhouette(member_com, distance))
  plot(silhouette(member_av, distance))
  plot(silhouette(member_ward, distance))
  plot(silhouette(member_cen, distance))


# NON-HiERARCHICAL CLUSTERING 

# K-means clustering

  km_clust <- kmeans(df_scaled, 3)

  # Print out the results
  km_clust 

  str(km_clust)


# Choosing K 

# This algorithm will detect how many clusters from 1 to 10 explains more variance
  k <- list()
  for(i in 1:10){
    k[[i]] <- kmeans(df_scaled, i)
  }

#Print the k value and take a look at the ratio (between_SS / total_SS)
  k

# Now lets try to plot (between_SS / total_SS) in to a scree plot

  betSS_totSS <- list()
  for(i in 1:10){
    betSS_totSS[[i]] <- k[[i]]$betweenss/k[[i]]$totss
  }
  
  plot(1:10, betSS_totSS, type = "b", ylab = "Between SS / Total SS", xlab = "Number of clusters")

# Let us try to take out the outliers and see the diference in the k-means clustering
 
## Examine the boxplots
  
  par(mar=c(15,2,1,1)) # Make labels fit in the boxplot
  boxplot(df_scaled, las = 2)

  # Detect outliers
outliers <- boxplot.stats(df_scaled)$out

  # remove rows with outliers 
df_no_outliers <- which(df_scaled %in% outliers,) 

# Execute a k-means clustering with the dataset without the outliers and see the diference. 

km_no_outliers <- kmeans(df_no_outliers, 3)

km_no_outliers
  
  
# Finally, try plotting each variable with each other and analyze if the clusters make sense.
# Let us go back to first example and take a look. 

# K-means with outliers

  plot(Numberofairlines ~ Destinations, df, col = km_clust$cluster)
  with(df, text(Numberofairlines ~ Destinations, label = Airport, pos = 1, cex = 0.6))

# k-means without outliers
  
  plot(Numberofairlines ~ Destinations, df, col = km_no_outliers$cluster)
  with(df, text(Numberofairlines ~ Destinations, label = Airport, pos = 1, cex = 0.6))