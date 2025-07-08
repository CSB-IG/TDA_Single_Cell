# Load packages
library(TDAstats)
library(igraph)

# 1. Generate circular point cloud + Gaussian noise
set.seed(42)
n <- 100
theta <- runif(n, 0, 2 * pi)
circle <- cbind(cos(theta), sin(theta))

# Add small Gaussian noise (mean = 0, sd = 0.05)
noise_sd <- 0.05
noisy_circle <- circle + matrix(rnorm(2 * n, mean = 0, sd = noise_sd), ncol = 2)

plot(noisy_circle, main = paste("Points sampled from a noisy circle"),
     xlab = "x", ylab = "y", asp = 1, pch = 16, col = "black")

# 2. Compute persistent homology (up to β₁)
ph <- calculate_homology(noisy_circle, dim = 1, threshold = 1.5)

# 3. Plot barcode
par(mfrow = c(1, 1))

# Persistence Barcode from Noisy Circle
plot_barcode(ph)

# 4. Define ε values for VR complex panel
epsilon_values <- c(0.2, 0.4, 0.6, 0.8)

# 5. Plot 2x2 panel of VR complexes
par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))

for (epsilon in epsilon_values) {
  
  # Compute distance and adjacency matrices
  dist_matrix <- as.matrix(dist(noisy_circle))
  adj_matrix <- dist_matrix < epsilon
  diag(adj_matrix) <- 0
  
  # Create graph and convert edge list
  g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
  edges <- get.edgelist(g)
  if (nrow(edges) > 0) {
    edges <- apply(edges, 2, as.numeric)
  }
  
  # Plot noisy points and VR edges
  plot(noisy_circle, main = paste("ε =", epsilon),
       xlab = "", ylab = "", asp = 1, pch = 16, col = "black")
  
  if (nrow(edges) > 0) {
    for (i in 1:nrow(edges)) {
      segments(noisy_circle[edges[i, 1], 1], noisy_circle[edges[i, 1], 2],
               noisy_circle[edges[i, 2], 1], noisy_circle[edges[i, 2], 2], col = "blue")
    }
  }
}

# Plotting a Persistence Diagram

# 1. Generate noisy circle
set.seed(42)
n <- 100
theta <- runif(n, 0, 2 * pi)
circle <- cbind(cos(theta), sin(theta))

# Add Gaussian noise
noise_sd <- 0.05
noisy_circle <- circle + matrix(rnorm(2 * n, mean = 0, sd = noise_sd), ncol = 2)

# 2. Compute persistent homology
threshold <- 1.5
ph <- calculate_homology(noisy_circle, dim = 1, threshold = threshold)

pha <- calculate_homology(circle, dim = 1, threshold = threshold)


# Persistence_Diagram
library(ggplot2)

# Convert matrix to data frame
ph_df <- as.data.frame(ph)

ggplot(ph_df, aes(x = birth, y = death, color = factor(dimension))) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("β0", "β1")) +
  labs(title = "Persistence Diagram (Noisy Circle)",
       x = "Birth Scale", y = "Death Scale", color = "Dimension") +
  theme_minimal()

# and for the circle with no-noise

pha_df <- as.data.frame(pha)

ggplot(pha_df, aes(x = birth, y = death, color = factor(dimension))) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("β0", "β1")) +
  labs(title = "Persistence Diagram (Circle)",
       x = "Birth Scale", y = "Death Scale", color = "Dimension") +
  theme_minimal()

