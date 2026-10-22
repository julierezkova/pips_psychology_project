# analysis.R
#
# Main function: run_analysis()
#
# Usage:
#   source("src/analysis.R")
#   results <- run_analysis()
#
# Required packages:
#   install.packages(c("ggplot2", "ggstatsplot"))


run_analysis <- function(seed = 1, n_total = 100) {
  
  # 1. Load packages and simulation function
  
  library(ggplot2)
  library(ggstatsplot)
  
  source("src/simulate.R")
  
  # 2. Simulate the data
  
  group_centres <- rbind(
    c(600, 70),  # ADHD
    c(550, 75)   # Control
  )
  
  d <- simulate_clusters(
    n_clusters = 2,
    n_total = n_total,
    group_names = c("ADHD", "Control"),
    centroids = group_centres,
    x_range = c(200, 1000),
    y_range = c(0, 100),
    seed = seed
  )
  
  # Ensure the group order is ADHD, then Control
  d$class <- factor(d$class, levels = c("ADHD", "Control"))
  
  cat("\n--- Simulated data ---\n")
  print(head(d))
  print(table(d$class))
  
  # 3. Descriptive statistics
  
  descriptives <- aggregate(
    cbind(x, y) ~ class,
    data = d,
    FUN = function(v) c(mean = mean(v), sd = sd(v))
  )
  
  cat("\n--- Descriptive statistics ---\n")
  print(descriptives)
  
  # 4. Helper functions
  
  cohens_d <- function(values, group) {
    lev <- levels(group)
    
    a <- values[group == lev[1]]
    b <- values[group == lev[2]]
    
    pooled_sd <- sqrt(
      ((length(a) - 1) * var(a) +
         (length(b) - 1) * var(b)) /
        (length(a) + length(b) - 2)
    )
    
    abs(mean(a) - mean(b)) / pooled_sd
  }
  
  effect_label <- function(d) {
    if (d < 0.2) {
      "negligible"
    } else if (d < 0.5) {
      "small"
    } else if (d < 0.8) {
      "medium"
    } else {
      "large"
    }
  }
  
  p_text <- function(p) {
    if (p < 0.001) {
      "< .001"
    } else {
      paste("=", sub("^0", "", sprintf("%.3f", p)))
    }
  }
  
  # 5. Welch t-tests
  
  t_rt <- t.test(x ~ class, data = d)
  t_acc <- t.test(y ~ class, data = d)
  
  cat("\n--- Reaction time: Welch t-test ---\n")
  print(t_rt)
  
  cat("\n--- Accuracy: Welch t-test ---\n")
  print(t_acc)
  
  # 6. Compare groups and calculate effect sizes
  
  compare_groups <- function(values, group, label) {
    test <- t.test(values ~ group)
    means <- tapply(values, group, mean)
    
    data.frame(
      variable = label,
      mean_1 = unname(means[1]),
      mean_2 = unname(means[2]),
      t = unname(test$statistic),
      df = unname(test$parameter),
      p = test$p.value,
      d = cohens_d(values, group)
    )
  }
  
  results <- rbind(
    compare_groups(d$x, d$class, "Reaction time (ms)"),
    compare_groups(d$y, d$class, "Accuracy (%)")
  )
  
  rownames(results) <- NULL
  
  # Correct for the two statistical tests
  results$p_bonferroni <- p.adjust(
    results$p,
    method = "bonferroni"
  )
  
  results$effect_size <- vapply(
    results$d,
    effect_label,
    character(1)
  )
  
  # Format a readable results table
  results_print <- results
  
  num_cols <- vapply(results_print, is.numeric, logical(1))
  results_print[num_cols] <- lapply(
    results_print[num_cols],
    round,
    digits = 2
  )
  
  results_print$p <- format.pval(
    results$p, digits = 2, eps = 0.001
  )
  
  results_print$p_bonferroni <- format.pval(
    results$p_bonferroni, digits = 2, eps = 0.001
  )
  
  lev <- levels(d$class)
  
  names(results_print) <- c(
    "Variable",
    paste("Mean", lev[1]),
    paste("Mean", lev[2]),
    "t", "df", "p", "d",
    "p (Bonferroni)", "Effect size"
  )
  
  cat("\n--- Results table ---\n")
  print(results_print, row.names = FALSE)
  
  # 7. Plain-language summary
  
  cat("\n--- Interpretation ---\n")
  
  for (i in seq_len(nrow(results))) {
    direction <- if (
      results$mean_1[i] > results$mean_2[i]
    ) {
      "higher"
    } else if (
      results$mean_1[i] < results$mean_2[i]
    ) {
      "lower"
    } else {
      "the same"
    }
    
    if (direction == "the same") {
      cat(sprintf(
        "%s: %s and %s had the same sample mean ",
        results$variable[i], lev[1], lev[2]
      ))
    } else {
      cat(sprintf(
        "%s: %s had a %s mean than %s ",
        results$variable[i], lev[1], direction, lev[2]
      ))
    }
    
    cat(sprintf(
      "(d = %.2f, %s effect, p %s).\n",
      results$d[i],
      results$effect_size[i],
      p_text(results$p[i])
    ))
  }
  
  cat("\nNote: the p-values in the summaries are unadjusted.\n")
  cat("Use the Bonferroni-adjusted p-values when assessing\n")
  cat("statistical significance across both tests.\n")
  
  # 8. Scatter plot with group means
  
  group_means <- aggregate(
    cbind(x, y) ~ class,
    data = d,
    FUN = mean
  )
  
  p_scatter <- ggplot(
    d,
    aes(x = x, y = y, colour = class)
  ) +
    geom_point(size = 2.5, alpha = 0.5) +
    geom_point(
      data = group_means,
      aes(x = x, y = y),
      shape = 23,
      size = 6,
      fill = "white",
      colour = "black",
      stroke = 1.3,
      inherit.aes = FALSE
    ) +
    scale_colour_brewer(
      palette = "Dark2",
      name = "Group"
    ) +
    labs(
      x = "Reaction time (ms)",
      y = "Accuracy (%)",
      title = "Reaction time and accuracy by group",
      subtitle = "Simulated data; diamonds indicate group means",
      caption = "Each point represents one simulated participant."
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "top"
    )
  
  print(p_scatter)
  
  # 9. Reaction time plot
  
  p_rt <- ggbetweenstats(
    data = d,
    x = class,
    y = x,
    type = "parametric",
    pairwise.comparisons = FALSE,
    xlab = "Group",
    ylab = "Reaction time (ms)",
    title = "Reaction time by group"
  )
  
  print(p_rt)
  
  # 10. Accuracy plot
  
  p_acc <- ggbetweenstats(
    data = d,
    x = class,
    y = y,
    type = "parametric",
    pairwise.comparisons = FALSE,
    xlab = "Group",
    ylab = "Accuracy (%)",
    title = "Accuracy by group"
  )
  
  print(p_acc)
  
  # 11. Return everything for later use
  
  return(list(
    data = d,
    descriptives = descriptives,
    reaction_time_test = t_rt,
    accuracy_test = t_acc,
    results = results,
    results_print = results_print,
    scatter_plot = p_scatter,
    reaction_time_plot = p_rt,
    accuracy_plot = p_acc
  ))
}
