#' Plot simulated clustered data
#'
#' Makes a scatter plot of data created with `simulate_clusters()`: one
#' colour per group, with the group centres marked as black crosses.
#' Can be used on its own, in `main.Rmd`, and inside the Shiny app.
#'
#' @param data A data frame with columns `x`, `y` and `class`, as returned by
#'   `simulate_clusters()`.
#' @param x_label,y_label Axis labels, e.g. "Reaction time (ms)".
#'   Default "x" and "y".
#' @param title Optional plot title. Default: no title.
#' @param show_centroids Draw the group centres? Only possible when the data
#'   still have the `centroids` attribute from `simulate_clusters()`.
#'   Default `TRUE`.
#'
#' @return A ggplot object. It is drawn when printed, and can be changed
#'   further with `+`, like any ggplot.
#'
#' @examples
#' d <- simulate_clusters(n_clusters = 3, n_total = 150, seed = 1)
#' plot_clusters(d, x_label = "Reaction time (ms)", y_label = "Accuracy")
plot_clusters <- function(
    data,
    x_label = "x",
    y_label = "y",
    title = NULL,
    show_centroids = TRUE
) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The package 'ggplot2' is needed. ",
         "Install it with install.packages(\"ggplot2\").")
  }
  if (!is.data.frame(data) || !all(c("x", "y", "class") %in% names(data))) {
    stop("data must be a data frame with columns x, y and class ",
         "(as returned by simulate_clusters()).")
  }

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, colour = class)) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::labs(x = x_label, y = y_label, colour = "Group", title = title) +
    ggplot2::theme_minimal()

  centroids <- attr(data, "centroids")
  if (show_centroids && !is.null(centroids)) {
    centre_points <- data.frame(x = centroids[, "x"], y = centroids[, "y"])
    plot <- plot +
      ggplot2::geom_point(
        data = centre_points,
        mapping = ggplot2::aes(x = x, y = y),
        inherit.aes = FALSE,   # centres have no class column, so no colour
        shape = 4, size = 4, stroke = 1.5, colour = "black"
      )
  }

  plot
}
