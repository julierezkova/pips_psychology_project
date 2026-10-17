# Checks for simulate_clusters(). Run with: source("tests/test_simulate.R")
# Every check that passes stays silent; a failing check stops with an error.

source("src/simulate.R")

# 1. Basic output: right size, right columns, right groups
d <- simulate_clusters(n_clusters = 3, n_total = 150, seed = 1)
stopifnot(
  nrow(d) == 150,
  identical(names(d), c("x", "y", "class")),
  nlevels(d$class) == 3,
  all(attr(d, "cluster_sizes") == 50)
)

# 2. Same seed gives the same data (reproducible)
d2 <- simulate_clusters(n_clusters = 3, n_total = 150, seed = 1)
stopifnot(identical(d, d2))

# 3. Unequal group sizes still add up exactly to n_total
d <- simulate_clusters(n_clusters = 3, n_total = 100, group_sizes = c(2, 1, 1),
                       seed = 1)
stopifnot(
  sum(attr(d, "cluster_sizes")) == 100,
  all(attr(d, "cluster_sizes") == c(50, 25, 25))
)

# 4. Group names end up in the class column
d <- simulate_clusters(2, 60, group_names = c("ADHD", "Control"), seed = 1)
stopifnot(identical(levels(d$class), c("ADHD", "Control")))

# 5. Custom ranges: data are in the user's units
d <- simulate_clusters(3, 300, x_range = c(200, 1000), y_range = c(0, 1),
                       seed = 1)
stopifnot(
  mean(d$x) > 200, mean(d$x) < 1000,
  mean(d$y) > 0,   mean(d$y) < 1
)

# 6. With the default circle, Cohen's d equals separation * closeness
d <- simulate_clusters(4, 200, separation = 2, closeness = 1.5, seed = 1)
stopifnot(abs(attr(d, "min_cohens_d") - 3) < 1e-8)

# 7. Custom centroids are returned unchanged (in the user's units)
my_centres <- matrix(c(300, 0.2,
                       600, 0.5,
                       900, 0.8), ncol = 2, byrow = TRUE)
d <- simulate_clusters(3, 90, centroids = my_centres,
                       x_range = c(200, 1000), y_range = c(0, 1), seed = 1)
stopifnot(all(abs(attr(d, "centroids") - my_centres) < 1e-8))

# 8. Invalid inputs give an error (tryCatch returns TRUE if an error happened)
gives_error <- function(expr) {
  tryCatch({ expr; FALSE }, error = function(e) TRUE)
}
stopifnot(
  gives_error(simulate_clusters(0, 100)),                  # no groups
  gives_error(simulate_clusters(2.5, 100)),                # not whole number
  gives_error(simulate_clusters(5, 6)),                    # too few points
  gives_error(simulate_clusters(3, 90, x_range = c(5, 1))),  # min > max
  gives_error(simulate_clusters(3, 90, separation = -1)),
  gives_error(simulate_clusters(3, 90, group_names = c("a", "b"))),
  gives_error(simulate_clusters(3, 90, centroids = c(1, 2, 3)))
)

cat("All simulation checks passed.\n")
