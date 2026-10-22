#' Simulate clustered two-dimensional data
#'
#' Generates `n_total` observations on two variables (x and y), divided over
#' `n_clusters` groups. Each group is drawn from a bivariate normal
#' distribution around its own centre (centroid). Builds on Claire's initial
#' `simulate_bivariate_clusters()`.
#'
#' How it works: the data are first simulated on a standard scale from -5 to 5
#' (where `separation` and `closeness` are defined), and are then stretched
#' to the user's `x_range` and `y_range`. This way, changing the range only
#' changes the units, not how much the groups overlap.
#'
#' @param n_clusters Number of groups (whole number, at least 1). Required.
#' @param n_total Total number of observations (whole number). Required.
#'   Every group must end up with at least 2 observations.
#' @param group_sizes Relative size of each group: one number per group, or a
#'   single number (= equal sizes). For example `c(2, 1, 1)` makes group 1
#'   twice as big as groups 2 and 3. Default: all groups equally big.
#' @param separation Smallest distance between two group centres on the
#'   standard scale (non-negative). Larger values = groups further apart.
#'   Ignored when `centroids` are supplied. Default 3.
#' @param layout How the group centres are arranged when no `centroids` are
#'   supplied: `"circle"` (default; evenly spaced on a circle), `"line"`
#'   (on a diagonal line, so x and y are positively correlated across groups)
#'   or `"random"` (random positions). In every layout the average distance between any two
#'   centres are exactly `separation` apart.
#' @param closeness How tightly the points sit around their centre: the
#'   within-group standard deviation is `1 / closeness` on the standard scale.
#'   One number for all groups or one per group. Default 1 (SD = 1).
#' @param x_range,y_range Numeric vectors `c(min, max)` giving the units of
#'   the x and y variable, e.g. `c(200, 1000)` for reaction times in ms.
#'   Default `c(-5, 5)`. Most points fall inside the range, but because the
#'   groups are normally distributed, a few points can fall slightly outside.
#' @param centroids Optional (advanced): a matrix or data frame with
#'   `n_clusters` rows and 2 columns (x, y) giving each group's centre in the
#'   units of `x_range` and `y_range`. If `NULL` (default), centres are placed
#'   automatically according to `layout`.
#' @param group_names Optional names for the groups, e.g.
#'   `c("ADHD", "Control")`. Default: "1", "2", "3", ...
#' @param seed Optional number for reproducible results. Note: this sets R's
#'   global random seed.
#'
#' @return A data frame with columns `x`, `y` (numeric) and `class` (factor
#'   with the group names), with rows in random order. Extra information is
#'   stored as attributes:
#'   - `centroids`: matrix of group centres, in the units of the ranges
#'   - `cluster_sizes`: number of observations per group
#'   - `min_cohens_d`: Cohen's d between the two closest groups
#'     (= distance between their centres / within-group SD)
#'   - `cluster_names`: the names of each cluster
#'   - `separation`: the input separation value to simulate clusters. If centroids were specified, returns `NA`
#'   - `closeness`: closeness value for each cluster
#' @examples
#' d <- simulate_clusters(n_clusters = 3, n_total = 150, seed = 1)
#' head(d)
#' attr(d, "centroids")
simulate_clusters <- function(
    n_clusters,
    n_total,
    group_sizes = 1,
    separation = 3,
    layout = c("circle", "line", "random"),
    closeness = 1,
    x_range = c(-5, 5),
    y_range = c(-5, 5),
    centroids = NULL,
    group_names = NULL,
    seed = NULL
) {
  
  # ---- 1. Check the inputs -------------------------------------------------
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("The package 'utils' is needed. ",
         "Install it with install.packages(\"utils\").")
  }
  
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The package 'stats' is needed. ",
         "Install it with install.packages(\"stats\").")
  }
  
  is_whole_number <- function(z) {
    is.numeric(z) && length(z) == 1 && is.finite(z) && z >= 1 && z == round(z)
  }
  if (!is_whole_number(n_clusters)) {
    stop("n_clusters must be a whole number of at least 1.")
  }
  if (!is_whole_number(n_total)) {
    stop("n_total must be a whole number of at least 1.")
  }
  k <- as.integer(n_clusters)
  n <- as.integer(n_total)
  
  # Arguments that can be one value (used for every group) or one per group.
  one_per_group <- function(z, name) {
    if (!is.numeric(z) || !(length(z) %in% c(1, k)) ||
        any(!is.finite(z)) || any(z <= 0)) {
      stop(name, " must be positive number(s): one value, or one per group (",
           k, ").")
    }
    rep(z, length.out = k)
  }
  group_sizes <- one_per_group(group_sizes, "group_sizes")
  closeness   <- one_per_group(closeness, "closeness")
  
  if (!is.numeric(separation) || length(separation) != 1 ||
      !is.finite(separation) || separation < 0) {
    stop("separation must be a single non-negative number.")
  }
  
  # match.arg() picks "circle" when nothing is given, and gives an error
  # listing the allowed options when the user types something else.
  layout <- match.arg(layout)
  
  check_range <- function(r, name) {
    if (!is.numeric(r) || length(r) != 2 || any(!is.finite(r)) ||
        r[1] >= r[2]) {
      stop(name, " must be two numbers c(min, max) with min smaller than max.")
    }
  }
  check_range(x_range, "x_range")
  check_range(y_range, "y_range")
  
  if (is.null(group_names)) {
    group_names <- as.character(seq_len(k))
  }
  if (length(group_names) != k || anyDuplicated(group_names) > 0) {
    stop("group_names must contain ", k, " different names (one per group).")
  }
  
  # ---- 2. Conversion between the standard scale and the user's units -------
  # The standard scale runs from -5 to 5 on both axes. A straight-line
  # conversion keeps the shape of the clusters (only the units change).
  
  to_user_units <- function(v, r) r[1] + (v + 5) / 10 * (r[2] - r[1])
  to_standard   <- function(v, r) (v - r[1]) / (r[2] - r[1]) * 10 - 5
  
  # ---- 3. Group centres (on the standard scale) -----------------------------
  
  # Set the seed here (before the "random" layout draws its centres), so that
  # the whole result, including random centres, is reproducible.
  if (!is.null(seed)) set.seed(seed)
  
  if (is.null(centroids)) {
    if (k == 1) {
      centers <- matrix(c(0, 0), nrow = 1)
      
    } else if (layout == "circle") {
      # Evenly on a circle, so neighbouring centres are exactly `separation`
      # apart. Distance between neighbours on a circle with radius r is
      # 2 * r * sin(pi / k), so r = separation / (2 * sin(pi / k)).
      angle  <- 2 * pi * (seq_len(k) - 1) / k
      radius <- separation / (2 * sin(pi / k))
      centers <- cbind(radius * cos(angle), radius * sin(angle))
      
    } else if (layout == "line") {
      # On the diagonal line y = x, centred on 0. Moving `separation` along
      # the diagonal means moving separation / sqrt(2) in both x and y.
      position <- (seq_len(k) - (k + 1) / 2) * separation / sqrt(2)
      centers  <- cbind(position, position)
      
    } else {
      # "random": draw random positions, centre them on 0, then rescale so
      # that the average distance between any two centres are exactly `separation`.
      centers <- matrix(stats::runif(2 * k, min = -1, max = 1), ncol = 2)
      centers <- sweep(centers, 2, colMeans(centers))
      centers <- centers * separation / mean(stats::dist(centers))
    }
  } else {
    centers <- as.matrix(centroids)
    if (!is.numeric(centers) || !identical(dim(centers), c(k, 2L)) ||
        any(!is.finite(centers))) {
      stop("centroids must be a numeric matrix with ", k,
           " rows (one per group) and 2 columns (x, y).")
    }
    # User gives centres in their own units; convert to the standard scale.
    centers <- cbind(to_standard(centers[, 1], x_range),
                     to_standard(centers[, 2], y_range))
  }
  
  # ---- 4. Number of observations per group -----------------------------------
  # Divide n over the groups in proportion to group_sizes. Rounding down can
  # leave a few observations over; give those to the groups that lost the
  # most by rounding ("largest remainder" method), so the total is exactly n.
  
  expected  <- n * group_sizes / sum(group_sizes)
  counts    <- floor(expected)
  leftover  <- n - sum(counts)
  if (leftover > 0) {
    extra <- order(expected - counts, decreasing = TRUE)[seq_len(leftover)]
    counts[extra] <- counts[extra] + 1
  }
  if (any(counts < 2)) {
    stop("Every group needs at least 2 observations. Increase n_total or ",
         "make the group_sizes more equal.")
  }
  
  # ---- 5. Simulate the points -------------------------------------------------
  
  group <- rep(seq_len(k), times = counts)   # group number of every point
  sds   <- 1 / closeness                     # within-group SD per group
  
  x_std <- rnorm(n, mean = centers[group, 1], sd = sds[group])
  y_std <- rnorm(n, mean = centers[group, 2], sd = sds[group])
  
  result <- data.frame(
    x     = to_user_units(x_std, x_range),
    y     = to_user_units(y_std, y_range),
    class = factor(group_names[group], levels = group_names)
  )
  
  # Shuffle the rows so the data are not sorted by group.
  result <- result[sample.int(n), , drop = FALSE]
  rownames(result) <- NULL
  
  # ---- 6. Store extra information as attributes ------------------------------
  
  centers_user <- cbind(x = to_user_units(centers[, 1], x_range),
                        y = to_user_units(centers[, 2], y_range))
  rownames(centers_user) <- group_names
  
  # Cohen's d between the two closest groups, on the standard scale:
  # distance between centres divided by the (pooled) within-group SD.
  min_d <- NA_real_
  if (k > 1) {
    pairs <- utils::combn(k, 2)
    d_values <- apply(pairs, 2, function(p) {
      distance  <- sqrt(sum((centers[p[1], ] - centers[p[2], ])^2))
      pooled_sd <- sqrt((sds[p[1]]^2 + sds[p[2]]^2) / 2)
      distance / pooled_sd
    })
    min_d <- min(d_values)
  }
  
  attr(result, "cluster_names") <- group_names
  attr(result, "separation") <- if (is.null(centroids)) separation else NA_real_
  attr(result, "closeness") <- closeness
  attr(result, "n_clusters") <- n_clusters
  attr(result, "n_total") <- n_total
  attr(result, "centroids") <- centers_user
  attr(result, "cluster_sizes") <- stats::setNames(as.integer(counts),
                                                   group_names)
  attr(result, "min_cohens_d")  <- min_d
  
  result
}
