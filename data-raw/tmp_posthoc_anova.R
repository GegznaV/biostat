#' @rdname posthoc_anova
#' @export
posthoc_anova.default_0_tmp <-
  function(y,
           group = NULL,
           method = c("Games-Howell", "Tukey"),
           conf_level = 0.95,
           digits = 3,
           p_adjust = "none",
           format_pvalue = TRUE,
           data = NULL,
           ...) {
    ### Based on http://www.psych.yorku.ca/cribbie/6130/games_howell.R
    method <- first_capital(tolower(method))

    tryCatch(
      method <- match.arg(method),
      error = function(err) {
        stop("The `method` is not valid: ", method)
      }
    )


    res <- list(input = as.list(environment()))

    # Intermediate -------------------------------------------------------

    complete.cases(group, y)

    res$intermediate <- list(
      group = factor(group[]),
      y = y[complete.cases(group, y)]
    )

    res$intermediate$n <- tapply(y, group, length)

    res$intermediate$groups <- length(res$intermediate$n)

    res$intermediate$df <- sum(res$intermediate$n) - res$intermediate$groups

    res$intermediate$means <- tapply(y, group, mean)

    res$intermediate$variances <- tapply(y, group, var)

    res$intermediate$names <- levels(res$intermediate$group)
    res$intermediate$pairNames <-
      combn(res$intermediate$groups, 2, function(ij) {
        paste0(rev(res$intermediate$names[ij]), collapse = "-")
      })

    res$intermediate$descriptives <- cbind(
      res$intermediate$n,
      res$intermediate$means,
      res$intermediate$variances
    )

    rownames(res$intermediate$descriptives) <- levels(res$intermediate$group)

    colnames(res$intermediate$descriptives) <- c("n", "means", "variances")


    ### Start on Tukey
    res$intermediate$errorVariance <-
      sum((res$intermediate$n - 1) * res$intermediate$variances) / res$intermediate$df

    res$intermediate$se <- combn(res$intermediate$groups, 2, function(ij) {
      sqrt(res$intermediate$errorVariance * sum(1 / res$intermediate$n[ij]))
    })

    res$intermediate$dmeans <- combn(res$intermediate$groups, 2, function(ij) {
      diff(res$intermediate$means[ij])
    })
    res$intermediate$t <-
      abs(res$intermediate$dmeans) / res$intermediate$se
    res$intermediate$p.tukey <- ptukey(
      res$intermediate$t * sqrt(2),
      res$intermediate$groups,
      res$intermediate$df,
      lower.tail = FALSE
    )

    res$intermediate$alpha <- (1 - conf_level)

    res$intermediate$qcrit <- qtukey(res$intermediate$alpha,
      res$intermediate$groups,
      res$intermediate$df,
      lower.tail = FALSE
    ) / sqrt(2)

    res$intermediate$tukey.low <- res$intermediate$dmeans - (res$intermediate$qcrit * res$intermediate$se)
    res$intermediate$tukey.high <- res$intermediate$dmeans + (res$intermediate$qcrit * res$intermediate$se)


    # Output -------------------------------------------------------------

    res$output <- list()

    res$output$tukey <- data.frame(
      res$intermediate$dmeans,
      res$intermediate$tukey.low,
      res$intermediate$tukey.high,
      res$intermediate$t,
      res$intermediate$df,
      res$intermediate$p.tukey
    )
    res$output$tukey$p.tukey.adjusted <-
      stats::p.adjust(res$intermediate$p.tukey,
        method = tolower(p_adjust)
      )

    rownames(res$output$tukey) <- res$intermediate$pairNames

    colnames(res$output$tukey) <- c("difference", "ci_lower", "ci_upper", "t", "df", "p", "p.adjusted")


    ### Start on Games-Howell
    res$intermediate$df.corrected <-
      combn(res$intermediate$groups, 2, function(ij) {
        sum(res$intermediate$variances[ij] /
          res$intermediate$n[ij])^2 /
          sum((res$intermediate$variances[ij] /
            res$intermediate$n[ij])^2 /
            (res$intermediate$n[ij] - 1))
      })
    res$intermediate$se.corrected <-
      combn(res$intermediate$groups, 2, function(ij) {
        sqrt(sum(res$intermediate$variances[ij] / res$intermediate$n[ij]))
      })
    res$intermediate$t.corrected <-
      abs(res$intermediate$dmeans) / res$intermediate$se.corrected

    res$intermediate$qcrit.corrected <-
      qtukey(
        res$intermediate$alpha,
        res$intermediate$groups,
        res$intermediate$df.corrected,
        lower.tail = FALSE
      ) / sqrt(2)

    res$intermediate$gh.low <- res$intermediate$dmeans -
      res$intermediate$qcrit.corrected * res$intermediate$se.corrected
    res$intermediate$gh.high <- res$intermediate$dmeans +
      res$intermediate$qcrit.corrected * res$intermediate$se.corrected


    res$intermediate$p.gameshowell <-
      ptukey(
        res$intermediate$t.corrected * sqrt(2),
        res$intermediate$groups,
        res$intermediate$df.corrected,
        lower.tail = FALSE
      )
    res$output$games.howell <- data.frame(
      res$intermediate$dmeans,
      res$intermediate$gh.low,
      res$intermediate$gh.high,
      res$intermediate$t.corrected,
      res$intermediate$df.corrected,
      res$intermediate$p.gameshowell
    )

    res$output$games.howell$p.gameshowell.adjusted <-
      stats::p.adjust(res$intermediate$p.gameshowell,
        method = p_adjust
      )

    rownames(res$output$games.howell) <- res$intermediate$pairNames

    colnames(res$output$games.howell) <-
      c("difference", "ci_lower", "ci_upper", "t", "df", "p", "p.adjusted")


    ### Set class and return object --------------------------------------
    class(res) <- "posthoc_anova"

    return(res)
  }
