pruns <- function(r, n1, n2, alternative = c("two.sided", "less", "greater")) {
  .druns_nom <- function(r, n1, n2) {
    pp <- vector(mode = "numeric", length = length(r))
    for (i in seq_along(r)) {
      if (2 * r[i] %/% 2 == r[i]) {
        k <- r[i] / 2
        pp[i] <- 2 * choose(n1 - 1, k - 1) * choose(n2 - 1, k - 1)
      } else {
        k <- (r[i] - 1) / 2
        pp[i] <- choose(n1 - 1, k - 1) * choose(n2 - 1, k) +
          choose(n1 - 1, k) * choose(n2 - 1, k - 1)
      }
    }
    pp
  }

  alternative <- match.arg(alternative)
  n <- n1 + n2
  if (r <= 1) stop("Number of runs must be > 1")
  if (r > n) stop("Number of runs must be < (n1 + n2")
  if (n1 < 1 | n2 < 1) {
    return(0)
  }

  E <- 1 + 2 * n1 * n2 / n
  denom <- choose(n, n1)
  rmax <- ifelse(n1 == n2, 2 * n1, 2 * min(n1, n2) + 1)
  rv <- 2:rmax
  pp <- .druns_nom(rv, n1, n2)
  pL <- sum(pp[rv <= r]) / denom
  pU <- 1 - sum(pp[rv <= (r - 1)]) / denom
  p2 <- sum(pp[abs(rv - E) >= abs(r - E)]) / denom
  p2min <- 2 * min(c(pL, pU, 0.5))
  return(switch(alternative, less = pL, greater = pU, two.sided = p2))
}

# ==============================================================================

# getS3method("RunsTest", "default")
runs_test <- function(x, y = NULL,
                      alternative = c("two.sided", "less", "greater"),
                      exact = NULL, correct = TRUE, na.rm = FALSE, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(y)) {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    xy <- Sort(cbind(
      c(x, y),
      c(rep(0, length(x)), rep(1, length(y)))
    ))[, 2]

    res <- RunsTest(
      x = xy,
      alternative = alternative,
      exact = exact,
      na.rm = na.rm
    )

    res$data.name <- dname
    res$method <- "Wald-Wolfowitz Runs Test "
    return(res)
  }

  alternative <- match.arg(alternative)
  dname <- deparse(substitute(x))

  if (na.rm) {
    x <- na.omit(x)
  }

  if (is.numeric(x) & (length(unique(x)) > 2)) {
    est <- median(x, na.rm = TRUE)
    names(est) <- "median(x)"
    x <- ((x > est) * 1)
  } else {
    est <- NULL
  }

  # Calculate runs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # x <- factor(x)
  #
  # levs <- levels(y)
  # m <- sum(y == levs[1])
  # n <- sum(y == levs[2])
  #
  # get_runs <- function(x) {
  #     if (nlevels(x) %nin% c(1, 2))
  #         stop("Can only process dichotomous variables")
  #
  #     x <- as.numeric(x) - 1
  #     runs <- sum(diff(x) != 0) + 1
  #     runs
  # }


  # x <- factor(x)
  # if (nlevels(x) %nin% c(1, 2))
  #     stop("Can only process dichotomous variables")
  #
  # x <- as.numeric(x) - 1
  # runs <- sum(diff(x) != 0) + 1
  # m <- sum(x == 0)
  # n <- sum(x == 1)



  levs <- levels(x)
  m <- sum(x == levs[1])
  n <- sum(x == levs[2])



  if (is.null(exact)) {
    exact <- ((m + n) <= 30)
  }

  E <- 1 + 2 * n * m / (n + m)
  s2 <- (2 * n * m * (2 * n * m - n - m)) / ((n + m)^2 * (n + m - 1))

  if (correct) {
    switch(as.character(cut(runs - E,
      breaks = c(-Inf, -0.5, 0.5, Inf),
      labels = c("a", "b", "c")
    )),
    a = statistic <- (runs - E + 0.5) / sqrt(s2),
    b = statistic <- 0,
    c = statistic <- (runs - E - 0.5) / sqrt(s2)
    )
  } else {
    statistic <- (runs - E) / sqrt(s2)
  }

  switch(alternative,
    "less" = {
      p.value <- ifelse(exact,
        pruns(runs, m, n, alternative = "less"),
        pnorm(statistic)
      )

      alternative <- "true number of runs is less than expected"
    },

    "greater" = {
      p.value <- ifelse(exact,
        pruns(runs, m, n, alternative = "greater"),
        1 - pnorm(statistic)
      )
      alternative <- "true number of runs is greater than expected"
    },

    "two.sided" = {
      p.value <- ifelse(exact,
        pruns(runs, m, n, alternative = "two.sided"),
        2 * min(pnorm(statistic), 1 - pnorm(statistic))
      )
      alternative <- "true number of runs is not equal the expected number"
    }
  )

  method <- "Runs Test for Randomness"
  names(statistic) <- "z"
  if (exact) {
    statistic <- NULL
  }

  structure(list(
    statistic = statistic,
    p.value = p.value,
    method = method,
    alternative = alternative,
    data.name = dname,
    estimate = est,
    parameter = c(runs = runs, m = m, n = n)
  ),
  class = "htest"
  )
}
# <bytecode: 0x000000001d51bc60>
# <environment: namespace:DescTools>
