library(mvtnorm)
library(MASS)

# Define parameters
dimension <- 5  # Dimensionality of the distributions
mean1 <- rep(0, dimension)  # Mean of the first distribution
mean2 <- c(0, 1.5, 0, 1.5, 0)  # Mean of the second distribution
cov_matrix <- diag(dimension)  # Identity matrix of dimension 5 as covariance matrix

# Calculate Mahalanobis distance between the means
mahalanobis_distance <- sqrt(sum((mean1 - mean2)^2))

# Calculate the percentage of overlap using the cumulative distribution function (CDF)
overlap_percentage <- pnorm(mahalanobis_distance )^dimension * 100

overlap_percentage
