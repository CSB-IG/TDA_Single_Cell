# Install packages if not already installed
if (!require("TDAmapper")) install.packages("TDAmapper")
if (!require("igraph")) install.packages("igraph")
if (!require("ggplot2")) install.packages("ggplot2")

# Load libraries
library(TDAmapper)
library(igraph)
library(ggplot2)

# 1. Generate noisy circle
set.seed(123)
n_points <- 400
theta <- runif(n_points, 0, 2 * pi)
noise_level <- 0.1
circle <- cbind(cos(theta), sin(theta)) + matrix(rnorm(2 * n_points, sd=noise_level), ncol=2)

# 2. Use angle as filter function (projection on arctangent2)
filter_values <- atan2(circle[,2], circle[,1])  # Filter by angle

# 3. Mapper parameters
num_intervals <- 8      # number of intervals
percent_overlap <- 40   # percent of overlap
num_bins_when_clustering <- 5  # for DBSCAN or k-means

# 4. Run Mapper
mapper_result <- mapper1D(
  distance_matrix = dist(circle),
  filter_values = filter_values,
  num_intervals = num_intervals,
  percent_overlap = percent_overlap,
  num_bins_when_clustering = num_bins_when_clustering
)

# 5. Visualize the Mapper graph
graph <- graph_from_adjacency_matrix(mapper_result$adjacency, mode = "undirected")
plot(graph,
     layout = layout_with_fr(graph),
     vertex.size = 8,
     vertex.label = NA,
     main = "Mapper Graph from Noisy Circle (Angle Filter)")

# 6. Optional: Visualize the original point cloud
df <- as.data.frame(circle)
colnames(df) <- c("x", "y")
ggplot(df, aes(x, y)) +
  geom_point(alpha = 0.6) +
  coord_equal() +
  ggtitle("Noisy Circle Input Data")

# Cluster Memberships per Node
# Assign colors to nodes
membership <- rep(NA, length(circle[,1]))
for (i in seq_along(mapper_result$points_in_vertex)) {
  membership[mapper_result$points_in_vertex[[i]]] <- i
}

df$cluster <- factor(membership)

# Visualize cluster membership in 2D
ggplot(df, aes(x, y, color = cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  coord_equal() +
  ggtitle("Original Data Colored by Mapper Cluster")

library(gplots)
heatmap.2(as.matrix(mapper_result$adjacency),
          trace = "none",
          dendrogram = "none",
          main = "Mapper Adjacency Matrix",
          col = bluered(100),
          key = TRUE)

# New Heatmap

# Assuming 'adj_matrix' is the adjacency matrix from the Mapper graph

# Load required package
library(pheatmap)

# Compute optimal ordering using hierarchical clustering
# (we treat the adjacency matrix as a distance-like structure for clustering)
dist_mat <- dist(1 - adj_matrix)  # '1 - adjacency' treats connected nodes as closer
hc <- hclust(dist_mat, method = "average")

# Reorder adjacency matrix based on clustering
ordered_adj_matrix <- adj_matrix[hc$order, hc$order]

pdf("heatmapper.pdf", height=16, width=16)
# Plot reordered adjacency matrix
pheatmap(ordered_adj_matrix,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         color = c("blue", "red"),
         legend_breaks = c(0,1),
         legend_labels = c("No edge", "Edge"),
         main = "Reordered Adjacency Matrix of Mapper Graph")
dev.off()



