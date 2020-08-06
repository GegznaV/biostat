posthoc_anova.default_2_tmp <-
  function(y,
           group = NULL,
           method = c("Games-Howell", "Tukey"),
           conf_level = 0.95,
           digits = 2,
           p_adjust = "holm",
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

    include <- complete.cases(group, y)

    res$intermediate <- list(group = factor(group[include]), y = y[include])
    res$intermediate$n <- tapply(y, group, length)
    res$intermediate$n_groups <- length(res$intermediate$n)
    res$intermediate$df <- sum(res$intermediate$n) - res$intermediate$n_groups
    res$intermediate$means <- tapply(y, group, mean)
    res$intermediate$variances <- tapply(y, group, var)
    res$intermediate$gr_names <- levels(res$intermediate$group)

    # res$intermediate$pair_names <-
    #     combn(res$intermediate$n_groups, 2, function(ij) {
    #         paste0(rev(res$intermediate$gr_names[ij]), collapse = "-")
    #
    #     })

    # res$intermediate$descriptives <- cbind(res$intermediate$n,
    #                                        res$intermediate$means,
    #                                        res$intermediate$variances)

    # rownames(res$intermediate$descriptives) <- levels(res$intermediate$group)
    # colnames(res$intermediate$descriptives) <- c('n', 'means', 'variances')


    ### * Start on Tukey ---------------------------------------------------

    res$intermediate$error_variance <-
      sum((res$intermediate$n - 1) * res$intermediate$variances) / res$intermediate$df

    res$intermediate$se <- combn(res$intermediate$n_groups, 2, function(ij) {
      sqrt(res$intermediate$error_variance * sum(1 / res$intermediate$n[ij]))
    })

    res$intermediate$dmeans <- combn(res$intermediate$n_groups, 2, function(ij) {
      diff(res$intermediate$means[ij])
    })
    res$intermediate$t <-
      abs(res$intermediate$dmeans) / res$intermediate$se
    res$intermediate$p_tukey <- ptukey(
      res$intermediate$t * sqrt(2),
      res$intermediate$n_groups,
      res$intermediate$df,
      lower.tail = FALSE
    )

    res$intermediate$alpha <- (1 - conf_level)

    res$intermediate$qcrit <- qtukey(res$intermediate$alpha,
      res$intermediate$n_groups,
      res$intermediate$df,
      lower.tail = FALSE
    ) / sqrt(2)

    res$intermediate$tukey_low <- res$intermediate$dmeans - (res$intermediate$qcrit * res$intermediate$se)
    res$intermediate$tukey_high <- res$intermediate$dmeans + (res$intermediate$qcrit * res$intermediate$se)

    # Output -------------------------------------------------------------

    res$output <- list()

    res$output$tukey <- data.frame(
      res$intermediate$dmeans,
      res$intermediate$tukey_low,
      res$intermediate$tukey_high,
      res$intermediate$t,
      res$intermediate$df,
      res$intermediate$p_tukey
    )
    res$output$tukey$p_tukey.adjusted <-
      stats::p.adjust(res$intermediate$p_tukey,
        method = tolower(p_adjust)
      )

    rownames(res$output$tukey) <- res$intermediate$pair_names

    colnames(res$output$tukey) <- c("difference", "ci_lower", "ci_upper", "t", "df", "p", "p.adjusted")

    ### * Start on Games-Howell ------------------------------------------
    res$intermediate$df_corrected <-
      combn(res$intermediate$n_groups, 2, function(ij) {
        sum(res$intermediate$variances[ij] /
          res$intermediate$n[ij])^2 /
          sum((res$intermediate$variances[ij] /
            res$intermediate$n[ij])^2 /
            (res$intermediate$n[ij] - 1))
      })
    res$intermediate$se_corrected <-
      combn(res$intermediate$n_groups, 2, function(ij) {
        sqrt(sum(res$intermediate$variances[ij] / res$intermediate$n[ij]))
      })
    res$intermediate$t_corrected <-
      abs(res$intermediate$dmeans) / res$intermediate$se_corrected

    res$intermediate$qcrit_corrected <-
      qtukey(
        res$intermediate$alpha,
        res$intermediate$n_groups,
        res$intermediate$df_corrected,
        lower.tail = FALSE
      ) / sqrt(2)

    res$intermediate$gh_low <- res$intermediate$dmeans -
      res$intermediate$qcrit_corrected * res$intermediate$se_corrected
    res$intermediate$gh_high <- res$intermediate$dmeans +
      res$intermediate$qcrit_corrected * res$intermediate$se_corrected


    res$intermediate$p_games_howell <-
      ptukey(
        res$intermediate$t_corrected * sqrt(2),
        res$intermediate$n_groups,
        res$intermediate$df_corrected,
        lower.tail = FALSE
      )
    res$output$games.howell <- data.frame(
      res$intermediate$dmeans,
      res$intermediate$gh_low,
      res$intermediate$gh_high,
      res$intermediate$t_corrected,
      res$intermediate$df_corrected,
      res$intermediate$p_games_howell
    )

    res$output$games.howell$p_games_howell.adjusted <-
      stats::p.adjust(res$intermediate$p_games_howell,
        method = p_adjust
      )

    rownames(res$output$games.howell) <- res$intermediate$pair_names

    colnames(res$output$games.howell) <-
      c("difference", "ci_lower", "ci_upper", "t", "df", "p", "p.adjusted")



    ### Set class and return object --------------------------------------
    structure(res, class = "posthoc_anova")
  }
