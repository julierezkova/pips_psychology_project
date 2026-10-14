## UvA AI Chat prompt
## Write an r function that simulates bivariate clustered data based on user inputs of number of clusters, sample size, inter cluster separation, intra cluster closeness, cluster density, and centroid positions. It should return a data frame with the x y coordiantes of each point as well as their class assignment   

simulate_bivariate_clusters <- function(
    n_clusters,
    n_total,
    separation = NULL,
    closeness = 1,
    density = 1,
    centroids = NULL,
    seed = NULL
) {
  positive_integer <- function(z) {
    is.numeric(z) && length(z) == 1L &&
      is.finite(z) && z >= 1 && z == floor(z) &&
      z <= .Machine$integer.max
  }
  
  if (!positive_integer(n_clusters))
    stop("n_clusters must be a positive integer.")
  if (!positive_integer(n_total))
    stop("n_total must be a positive integer.")
  
  k <- as.integer(n_clusters)
  n <- as.integer(n_total)
  
  expand_parameter <- function(z, name) {
    if (!is.numeric(z) || !(length(z) %in% c(1L, k)) ||
        any(!is.finite(z)) || any(z <= 0)) {
      stop(name, " must be positive and have length 1 or n_clusters.")
    }
    rep(z, length.out = k)
  }
  
  closeness <- expand_parameter(closeness, "closeness")
  density <- expand_parameter(density, "density")
  
  if (!is.null(separation) &&
      (!is.numeric(separation) || length(separation) != 1L ||
       !is.finite(separation) || separation < 0)) {
    stop("separation must be NULL or a finite nonnegative number.")
  }
  
  # Generate centroids on a circle if none are supplied.
  automatic <- is.null(centroids)
  
  if (automatic) {
    if (k == 1L) {
      centers <- matrix(c(0, 0), nrow = 1L)
    } else {
      theta <- 2 * pi * (seq_len(k) - 1) / k
      centers <- cbind(cos(theta), sin(theta))
    }
  } else {
    centers <- as.matrix(centroids)
    if (!is.numeric(centers) ||
        !identical(dim(centers), c(k, 2L)) ||
        any(!is.finite(centers))) {
      stop("centroids must be a numeric n_clusters-by-2 matrix/data frame.")
    }
  }
  
  # Default separation is 5 only for automatically generated centroids.
  target <- if (automatic && is.null(separation)) 5 else separation
  
  if (k > 1L && !is.null(target)) {
    center_mean <- colMeans(centers)
    
    if (target == 0) {
      centers <- matrix(rep(center_mean, each = k), nrow = k)
    } else {
      minimum_distance <- min(stats::dist(centers))
      if (!is.finite(minimum_distance) || minimum_distance <= 0)
        stop("Positive separation requires distinct, finite-distance centroids.")
      
      centers <- sweep(centers, 2, center_mean, "-")
      centers <- centers * (target / minimum_distance)
      centers <- sweep(centers, 2, center_mean, "+")
    }
  }
  
  # Allocate exactly n observations using largest-remainder rounding.
  weights <- density / max(density)
  expected <- n * weights / sum(weights)
  counts <- as.integer(floor(expected))
  remaining <- n - sum(counts)
  
  if (remaining > 0L) {
    idx <- order(expected - counts, decreasing = TRUE)[seq_len(remaining)]
    counts[idx] <- counts[idx] + 1L
  }
  
  # Setting seed changes R's global random-number state.
  if (!is.null(seed)) set.seed(seed)
  
  group <- rep(seq_len(k), times = counts)
  cluster_sd <- 1 / closeness
  
  if (any(!is.finite(centers)) || any(!is.finite(cluster_sd)))
    stop("Inputs produce nonfinite centroids or standard deviations.")
  
  result <- data.frame(
    x = rnorm(n, mean = centers[group, 1], sd = cluster_sd[group]),
    y = rnorm(n, mean = centers[group, 2], sd = cluster_sd[group]),
    class = factor(group, levels = seq_len(k))
  )
  
  # Shuffle rows so observations are not ordered by class.
  result <- result[sample.int(n), , drop = FALSE]
  rownames(result) <- NULL
  
  colnames(centers) <- c("x", "y")
  attr(result, "centroids") <- centers
  attr(result, "cluster_sizes") <- counts
  
  result
}


## Testing
# library(ggplot2)
# 
# data <- simulate_bivariate_clusters(3, 150, closeness = 1, centroids = c(1,2,3))
# 
# ggplot(data, mapping = aes(x=x, y=y, color = class)) +
#   geom_point()

