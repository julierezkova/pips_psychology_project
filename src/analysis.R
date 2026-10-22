# analysis.R
#
# Question: Do ADHD and Control participants differ in reaction time and in
# accuracy?
#
# Analysis: Welch t-test + Cohen's d for each variable, and a scatter plot.
#
# How to run: set the working directory to the PROJECT FOLDER (the one that
# contains src/ and tests/), then run:  source("src/analysis.R")
# Package needed: install.packages("ggplot2")


library(ggplot2)

source("src/simulate.R")   # loads simulate_clusters()


# 1. Simulate the data

# x = reaction time in ms, y = accuracy in %.
# We choose the group centers (means) ourselves. With closeness = 1, the 
# spread (SD) inside each group is 80 ms for reaction time and 
# 10 percentage points for accuracy (one tenth of each range).
#
# Rows of `centroids` follow the order of `group_names`:
#                         reaction time (ms), accuracy (%)
group_centres <- rbind(c(600, 70),    # ADHD: slower, a bit less accurate
                       c(550, 75))    # Control

d <- simulate_clusters(
  n_clusters  = 2,
  n_total     = 100,                  # 50 participants per group
  group_names = c("ADHD", "Control"),
  centroids   = group_centres,
  x_range     = c(200, 1000),
  y_range     = c(0, 100),
  seed        = 1
)

head(d)                               # first rows: x, y, class
table(d$class)                        # number of participants per group



# 2. Descriptive statistics: mean and SD per group
# aggregate() splits the data by class (ADHD / Control) and applies the
# function in FUN to each group's values. Here FUN returns the mean and SD.

descriptives <- aggregate(cbind(x, y) ~ class, data = d,
                          FUN = function(v) c(mean = mean(v), sd = sd(v)))
print(descriptives)



# 3. Cohen's d (effect size) = difference in means / pooled standard deviation
#    Rough guide: 0.2 = small, 0.5 = medium, 0.8 = large

cohens_d <- function(values, group) {
  lev <- levels(group)
  a <- values[group == lev[1]]
  b <- values[group == lev[2]]
  pooled_sd <- sqrt(((length(a) - 1) * var(a) + (length(b) - 1) * var(b)) /
                      (length(a) + length(b) - 2))
  abs(mean(a) - mean(b)) / pooled_sd
}

# Verbal label for the size of d, using Cohen's rough guidelines:
# 0.2 = small, 0.5 = medium, 0.8 = large
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

# Format a p-value the way papers do: "< .001" or "= .012"
p_text <- function(p) {
  if (p < 0.001) {
    "< .001"
  } else {
    paste("=", sub("^0", "", sprintf("%.3f", p)))
  }
}

# 4. Welch t-tests (R's t.test() uses Welch's version by default)

# Reaction time
t_rt <- t.test(x ~ class, data = d)
print(t_rt)
cat("Cohen's d (reaction time):", round(cohens_d(d$x, d$class), 2), "\n\n")

# Accuracy
t_acc <- t.test(y ~ class, data = d)
print(t_acc)
cat("Cohen's d (accuracy):", round(cohens_d(d$y, d$class), 2), "\n\n")



# 5. Results table: both tests in one table

# Runs a Welch t-test and Cohen's d for one variable and returns one table row.
compare_groups <- function(values, group, label) {
  test  <- t.test(values ~ group)
  means <- tapply(values, group, mean)
  data.frame(
    variable = label,
    mean_1   = means[[1]],            # mean of the first group (ADHD)
    mean_2   = means[[2]],            # mean of the second group (Control)
    t        = unname(test$statistic),
    df       = unname(test$parameter),
    p        = test$p.value,
    d        = cohens_d(values, group)
  )
}

results <- rbind(
  compare_groups(d$x, d$class, "Reaction time (ms)"),
  compare_groups(d$y, d$class, "Accuracy (%)")
)
rownames(results) <- NULL

# We ran two tests, so also correct the p-values for multiple testing
# (Bonferroni: p is multiplied by the number of tests, here 2).
results$p_bonferroni <- p.adjust(results$p, method = "bonferroni")
results$effect_size  <- sapply(results$d, effect_label)

# A tidy copy for printing: rounded numbers and readable p-values
lev <- levels(d$class)
results_print <- results
num_cols <- sapply(results_print, is.numeric)
results_print[num_cols] <- round(results_print[num_cols], 2)
results_print$p            <- format.pval(results$p, digits = 2, eps = 0.001)
results_print$p_bonferroni <- format.pval(results$p_bonferroni, digits = 2, eps = 0.001)
names(results_print) <- c("Variable", paste("Mean", lev), "t", "df", "p", "d",
                          "p (Bonferroni)", "Effect size")
print(results_print, row.names = FALSE)

# Optional: save the table as a file
# write.csv(results_print, "results_table.csv", row.names = FALSE)

# 6. Plain-language summary of each result

for (i in seq_len(nrow(results))) {
  direction <- if (results$mean_1[i] > results$mean_2[i]) "higher" else "lower"
  cat(sprintf("%s: %s had a %s mean than %s (d = %.2f, %s effect, p %s).\n",
              results$variable[i], lev[1], direction, lev[2],
              results$d[i], results$effect_size[i], p_text(results$p[i])))
}
cat("\n")

# 7. Plot: every participant, with the group means as large diamonds

group_means <- aggregate(cbind(x, y) ~ class, data = d, FUN = mean)

p_scatter <- ggplot(d, aes(x = x, y = y, colour = class)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_point(data = group_means, shape = 18, size = 6) +
  scale_colour_brewer(palette = "Dark2", name = NULL) +
  labs(
    x = "Reaction time (ms)",
    y = "Accuracy (%)",
    title = "Reaction time and accuracy of ADHD and Control participants",
    subtitle = "Simulated data; diamonds = group means"
  ) +
  theme_minimal(base_size = 14)

print(p_scatter)

