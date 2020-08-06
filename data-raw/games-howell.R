games.howell <- function(grp, obs) {
  # Create combinations
  combs <- combn(unique(grp), 2)

  # Statistics that will be used throughout the calculations:
  # n      = sample size of each group
  # groups = number of groups in data
  # Mean   = means of each group sample
  # std    = variance of each group sample
  n <- tapply(obs, grp, length)
  groups <- length(tapply(obs, grp, length))
  Mean <- tapply(obs, grp, mean)
  std <- tapply(obs, grp, var)

  statistics <- lapply(1:ncol(combs), function(x) {
    mean.diff <- Mean[combs[2, x]] - Mean[combs[1, x]]

    # t-values
    t <- abs(Mean[combs[1, x]] - Mean[combs[2, x]]) /
      sqrt((std[combs[1, x]] / n[combs[1, x]]) +
        (std[combs[2, x]] / n[combs[2, x]]))

    # Degrees of Freedom
    df <-
      (std[combs[1, x]] / n[combs[1, x]] + std[combs[2, x]] /
        n[combs[2, x]])^2 / # Numerator Degrees of Freedom
        ((std[combs[1, x]] / n[combs[1, x]])^2 / (n[combs[1, x]] - 1) + # Part 1 of Denominator Degrees of Freedom
          (std[combs[2, x]] / n[combs[2, x]])^2 / (n[combs[2, x]] - 1)) # Part 2 of Denominator Degrees of Freedom

    # p-values
    p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)

    # Sigma standard error
    se <-
      sqrt(0.5 * (std[combs[1, x]] / n[combs[1, x]] + std[combs[2, x]] / n[combs[2, x]]))

    # Upper Confidence Limit
    upper.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff + qtukey(
        p = 0.95,
        nmeans = groups,
        df = df
      ) * se
    })[[1]]

    # Lower Confidence Limit
    lower.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff - qtukey(
        p = 0.95,
        nmeans = groups,
        df = df
      ) * se
    })[[1]]

    # Group Combinations
    grp.comb <- paste(combs[1, x], ":", combs[2, x])

    # Collect all statistics into list
    stats <-
      list(grp.comb, mean.diff, se, t, df, p, upper.conf, lower.conf)
  })

  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {
    unlist(x)
  })

  # Create dataframe from flattened list
  results <-
    data.frame(matrix(
      unlist(stats.unlisted),
      nrow = length(stats.unlisted),
      byrow = TRUE
    ))

  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(2, 3:ncol(results))] <-
    round(as.numeric(as.matrix(results[c(2, 3:ncol(results))])), digits = 3)

  # Rename data frame columns
  colnames(results) <-
    c(
      "groups",
      "Mean Difference",
      "Standard Error",
      "t",
      "df",
      "p",
      "upper limit",
      "lower limit"
    )

  return(results)
}
