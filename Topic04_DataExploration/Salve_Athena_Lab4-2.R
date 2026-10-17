# 4.1 Loading and Exploring the Dataset

# 4.1.1 Load the iris dataset

data (iris)

# Run ?iris to read the documentation

?iris

head(iris)
str(iris)
summary (iris)

# Check the number of observations & variables
dim(iris)

# 4.2 Principal Component Analysis

# 4.2.1 Select the Numerical Variables

iris_data <- iris[, 1:4]

head(iris_data)
str(iris_data)

# 4.2.2 Perform PCA

pca_result <- prcomp(
  iris_data,
  center = TRUE,
  scale. = TRUE
)

# examine the results
names(pca_result)
str(pca_result)

# View PCA scores.
head(pca_result$x)

# Examine the variation
summary(pca_result)

# 4.2.3 Visualize the PCA Results

# Convert PCA scores to a data frame.
pca_scores <- as.data.frame(pca_result$x)

# Add the species information.
pca_scores$Species <- iris$Species

# Check the result.
head(pca_scores)

# Create a PCA score plot.
library(ggplot2)

ggplot(
  pca_scores,
  aes(x = PC1, y = PC2, color = Species)
) +
  geom_point() +
  labs(
    title = "PCA of Iris Measurements",
    x = "PC1",
    y = "PC2",
    color = "Species"
  ) +
  theme_classic()

# 4.2.4 Using ggfortify to Visualize a PCA Biplot

library(ggplot2)
library(ggfortify)

# Run the PCA model.
pca_model <- prcomp(
  iris[, 1:4],
  center = TRUE,
  scale. = TRUE
)

# Create the biplot.
autoplot(
  pca_model,
  data = iris,
  colour = "Species",
  loadings = TRUE,
  loadings.label = TRUE,
  loadings.colour = "black",
  loadings.label.colour = "black"
) +
  theme_minimal() +
  labs(
    title = "Principal Component Analysis (PCA) on Iris Dataset",
    subtitle = "Reducing 4 physical measurements down to 2 dimensions using prcomp()",
    caption = "Data source: Built-in R Iris dataset",
    colour = "Flower Species"
  )

# 4.3 Hierarchical Clustering

# 4.3.1 Standardize the Numerical Variables and Calculate Distances

iris_scaled <- scale(iris_data)

# Calculate Euclidean distance.
dist_matrix <- dist(
  iris_scaled,
  method = "euclidean"
)

# 4.3.2 Perform Hierarchical Clustering

hclust_result <- hclust(
  dist_matrix,
  method = "complete"
)

# 4.3.3 Visualize the Dendrogram

plot(
  hclust_result,
  labels = FALSE,
  main = "Hierarchical Clustering of Iris Flowers",
  xlab = "Observations",
  ylab = "Height"
)

# 4.3.4 Create Three Clusters

groups <- cutree(
  hclust_result,
  k = 3
)

# Create the dendrogram showing three clusters.
plot(
  hclust_result,
  labels = FALSE,
  main = "Hierarchical Clustering of Iris Flowers",
  xlab = "Observations",
  ylab = "Height"
)

rect.hclust(hclust_result, k = 3)

# Add cluster assignments to the dataset.
iris$Cluster <- as.factor(groups)

# View the results.
head(iris)

# Create a contingency table.
table(
  iris$Species,
  iris$Cluster
)

# 4.4 K-means Clustering

# 4.4.1 Perform K-means Clustering

# Set a seed.
set.seed(123)

# Perform K-means clustering.
kmeans_result <- kmeans(
  iris_scaled,
  centers = 3,
  nstart = 20
)

# Inspect the object.
kmeans_result
names(kmeans_result)
head(kmeans_result$cluster)

# Add cluster assignments.
iris$KMeans_Cluster <- as.factor(
  kmeans_result$cluster
)

# Check the results.
head(iris)

table(
  iris$Species,
  iris$KMeans_Cluster
)

# 4.4.2 Visualizing K-means Clustering

ggplot(
iris,
aes(
  x = Petal.Length,
  y = Petal.Width,
  color = KMeans_Cluster
)
) +
  geom_point() +
  labs(
    title = "K-means Clustering of Iris Flowers",
    x = "Petal Length (cm)",
    y = "Petal Width (cm)",
    color = "Cluster"
  ) +
  theme_classic()

# Create a contingency table.
table(
  iris$Species,
  iris$KMeans_Cluster
)

# Practice Exercise

# 1. Load and Inspect the Dataset

data(airquality)

head(airquality)
str(airquality)
summary(airquality)

# 2. Handling Missing Values

# Check for missing values.
summary(airquality)

# Select the four numerical air-quality variables.
air_data <- airquality[, c(
  "Ozone",
  "Solar.R",
  "Wind",
  "Temp"
)]

# Remove rows containing missing values.
air_complete <- na.omit(air_data)

# Check the result.
head(air_complete)
str(air_complete)
summary(air_complete)

# 3. Perform PCA

air_pca <- prcomp(
  air_complete,
  center = TRUE,
  scale. = TRUE
)

# Examine the PCA results.
names(air_pca)
head(air_pca$x)
summary(air_pca)

# 4. Create a PCA Plot

# Extract the PCA scores.
air_pca_scores <- as.data.frame(air_pca$x)

# Create the PCA plot.
ggplot(
  air_pca_scores,
  aes(x = PC1, y = PC2)
) +
  geom_point() +
  labs(
    title = "PCA of Air Quality Measurements",
    x = "PC1",
    y = "PC2"
  ) +
  theme_classic()

# 5. Add Month to the PCA Plot

# # Identify the corresponding observations in the original dataset.
air_complete_data <- airquality[
  complete.cases(
    airquality[, c(
      "Ozone",
      "Solar.R",
      "Wind",
      "Temp"
    )]
  ),
]

# Add Month as a factor.
air_pca_scores$Month <- as.factor(
  air_complete_data$Month
)

# Create the PCA plot colored by Month.
ggplot(
  air_pca_scores,
  aes(x = PC1, y = PC2, color = Month)
) +
  geom_point() +
  labs(
    title = "PCA of Air Quality Measurements",
    x = "PC1",
    y = "PC2",
    color = "Month"
  ) +
  theme_classic()

# 6. Hierarchical Clustering

# Standardize the air-quality variables.
air_scaled <- scale(air_complete)

# Calculate Euclidean distances.
air_dist <- dist(
  air_scaled,
  method = "euclidean"
)

# Perform hierarchical clustering using complete linkage.
air_hclust <- hclust(
  air_dist,
  method = "complete"
)

# Create the dendrogram.
plot(
  air_hclust,
  labels = FALSE,
  main = "Hierarchical Clustering of Air Quality Measurements",
  xlab = "Observations",
  ylab = "Height"
)

# 7. K-means Clustering

# Set a seed.
set.seed(123)

# Perform K-means clustering using three clusters.
air_kmeans <- kmeans(
  air_scaled,
  centers = 3,
  nstart = 20
)

# Add cluster assignments.
air_pca_scores$KMeans_Cluster <- as.factor(
  air_kmeans$cluster
)

# Plot the clusters in PCA space.
ggplot(
  air_pca_scores,
  aes(
    x = PC1,
    y = PC2,
    color = KMeans_Cluster
  )
) +
  geom_point() +
  labs(
    title = "K-means Clustering of Air Quality Measurements",
    x = "PC1",
    y = "PC2",
    color = "Cluster"
  ) +
  theme_classic()

# 8. Relate Clusters to Month

# Create a contingency table showing Month and K-means cluster.
table(
  air_pca_scores$Month,
  air_pca_scores$KMeans_Cluster
)