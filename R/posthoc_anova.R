# TODO:
# 1. review is needed.
#

#' [!] Post-hoc tests for ANOVA and Welch ANOVA
#'
#' Games-Howell, Tukey HSD and other post-hoc tests for ANOVA and Welch ANOVA.
#' Either Games-Howell test or Tukey honestly significant difference (HSD)
#' post-hoc tests for one-way analysis of variance (ANOVA).
#'
#' The main part of the  function code and descriptions were
#' imported from \code{posthocTGH()} in package \pkg{userfriendlyscience}.
#'
#' @param y (numeric|formula)\cr Either a numeric vector or formula.
#' @param group (factor)\cr a vector that either is a factor or can be converted into
#' one. If \code{y} is a formula, \code{group} is ignored.
#' @param data (data frame)\cr a data frame with data to be used in combination with formula.
#' @param method (\code{"games-howell"}|\code{"tukey"})\cr Name of post-hoc
#'                tests to conduct. Valid values are "tukey" and "games-howell".
#' @param conf_level (number)\cr Confidence level (equals to \code{1 - alpha}, where alpha is significanve level). Number from 0 to 1. Default is 0.95.
#' @param sep (character)\cr A string with a symbol to separate group names if several grouping variables are used.
#' @param digits (integer)\cr The number of digits to round data related numbers to.
#' @param digits_p (integer)\cr The number of digits to round p values to. Must be 2, 3 or more.
#' @param format_pvalue (does not work yet) \cr Whether to format the p values according to APA
#' standards (i.e. replace all values lower than .001 with '<.001'). This only
#' applies to the printing of the object, not to the way the p values are
#' stored in the object.
#' @param p_adjust Any valid \code{\link[stats]{p.adjust}} method.
#' @param x object to print.
#' @param ... Further arguments to methods.
#'
#' @return A list of three elements:
#' \item{input}{List with input arguments}
#' \item{output}{List with post-hoc test results}.
#'
#'
#' @note
#' Options that carry out Games-Howell and Tukey HSD analyses are based
#' on code of function \code{posthocTGH()} in package \pkg{userfriendlyscience}
#' (version 0.7.0).
#'
#'
#'
#' @seealso
#'
#' \itemize{
#'   \item{\url{http://www.gcf.dkf.unibe.ch/BCB/files/BCB_10Jan12_Alexander.pdf}}
#'   \item{\url{https://rpubs.com/aaronsc32/games-howell-test}}
#'   \item{\url{http://aoki2.si.gunma-u.ac.jp/R/src/tukey.R}}
#'   \item{\url{https://stats.stackexchange.com/questions/83941/games-howell-post-hoc-test-in-r}}
#'
#' }
#'
#' @author
#' Gjalt-Jorn Peters (Open University of the Netherlands) \cr
#' Jeff Bagget (University of Wisconsin - La Crosse) \cr
#' Vilmantas Gegzna
#'
#'
#' @keywords utilities
#' @examples
#' library(biostat)
#'
#' # Compute post-hoc statistics using the Games-Howell method
#' posthoc_anova(weight ~ Diet, data = ChickWeight, method = "Games-Howell")
#'
#' # Compute post-hoc statistics using the Tukey method
#' posthoc_anova(weight ~ Diet, data = ChickWeight, method = "Tukey")
#' @export
#'

posthoc_anova <- function(y,
                          group = NULL,
                          method = c("Games-Howell", "Tukey"),
                          conf_level = 0.95,
                          digits = 3,
                          digits_p = 3,
                          p_adjust = "none",
                          format_pvalue = TRUE,
                          data = NULL,
                          ...) {
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  UseMethod("posthoc_anova")
}

#' @rdname posthoc_anova
#' @export
posthoc_anova.formula <- function(y,
                                  group = NULL,
                                  method = c("Games-Howell", "Tukey"),
                                  conf_level = 0.95,
                                  digits = 3,
                                  digits_p = 3,
                                  p_adjust = "none",
                                  format_pvalue = TRUE,
                                  data = NULL,
                                  ...,
                                  sep = " | ") {
  # [!!!] posthoc_anova.formula method needs revision
  DF <- model.frame(y, data = data)

  if (length(y) > 2) {
    values <- DF[[1]]
    groups <- interaction(DF[-1], sep = sep)
  } else {
    stop(
      "\nThe formula (`", y, "`) is incorrect.",
      "\nIt must contain at least 2 variable names."
    )
  }
  posthoc_anova.default(values,
    groups,
    method = method,
    conf_level = conf_level,
    digits = digits,
    p_adjust = p_adjust,
    format_pvalue = format_pvalue,
    ...
  )
}

#' @rdname posthoc_anova
#' @export
posthoc_anova.default <-
  function(y,
           group = NULL,
           method = c("Games-Howell", "Tukey"),
           conf_level = 0.95,
           digits = 3,
           digits_p = 3,
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

    # Input---------------------------------------------------------------

    res <- list(input = as.list(environment()))

    # Intermediate -------------------------------------------------------

    include <- complete.cases(group, y)

    group <- factor(group[include])
    y <- y[include]

    gr_sizes <- tapply(y, group, length)
    n_groups <- length(gr_sizes)

    means <- tapply(y, group, mean)
    variances <- tapply(y, group, var)
    SD <- tapply(y, group, sd)

    gr_names <- levels(group)

    pair_names <- combn(n_groups, 2, function(ij) {
      paste0(rev(gr_names[ij]), collapse = "-")
    })

    alpha <- (1 - conf_level)
    mean_diffs <- combn(n_groups, 2, function(ij) diff(means[ij]))

    tukey_test <- function(n_groups, gr_sizes, pair_names, variances, mean_diffs, alpha, p_adjust) {
      df <- sum(gr_sizes) - n_groups
      error_variance <- sum((gr_sizes - 1) * variances) / df
      se <- combn(n_groups, 2, function(ij) {
        sqrt(error_variance * sum(1 / gr_sizes[ij]))
      })

      t <- abs(mean_diffs) / se
      p_tukey <- ptukey(t * sqrt(2), n_groups, df, lower.tail = FALSE)
      qcrit <- qtukey(alpha, n_groups, df, lower.tail = FALSE) / sqrt(2)

      tukey_ci_low <- mean_diffs - (qcrit * se)
      tukey_ci_high <- mean_diffs + (qcrit * se)

      # Is `p_adjusted` needed? [!!!]
      # Tukey HSD test is correction itself.
      p_adjusted <- stats::p.adjust(p_tukey, method = tolower(p_adjust))

      # Output -------------------------------------------------------------
      output <- data.frame(
        pair_names,
        mean_diffs,
        tukey_ci_low,
        tukey_ci_high,
        t,
        df,
        p_tukey,
        p_adjusted
      )

      colnames(output) <-
        c("groups", "difference", "ci_lower", "ci_upper", "t", "df", "p", "p_adjusted")

      output # Tukey output
    }

    ### * Start on Games-Howell ------------------------------------------


    games_howell_test <- function(n_groups, gr_sizes, pair_names, variances, mean_diffs, alpha, p_adjust) {
      df_corrected <- combn(n_groups, 2, function(ij) {
        sum(variances[ij] / gr_sizes[ij])^2 / sum((variances[ij] / gr_sizes[ij])^2 / (gr_sizes[ij] - 1))
      })

      se_corrected <- combn(n_groups, 2, function(ij) {
        sqrt(sum(variances[ij] / gr_sizes[ij]))
      })

      t_corrected <- abs(mean_diffs) / se_corrected
      qcrit_corrected <- qtukey(alpha, n_groups, df_corrected, lower.tail = FALSE) / sqrt(2)

      gh_low <- mean_diffs - qcrit_corrected * se_corrected
      gh_high <- mean_diffs + qcrit_corrected * se_corrected

      p <- ptukey(t_corrected * sqrt(2), # p_games_howell
        n_groups,
        df_corrected,
        lower.tail = FALSE
      )

      # Is `p_adjusted` needed? [!!!]
      # Games-Howell test is correction itself.
      p_adjusted <- stats::p.adjust(p, method = tolower(p_adjust))

      # Output: Games-Howell ----------------------------------------------
      output <- data.frame(
        pair_names,
        mean_diffs,
        gh_low,
        gh_high,
        t_corrected,
        df_corrected,
        p,
        p_adjusted
      )

      colnames(output) <-
        c("groups", "difference", "ci_lower", "ci_upper", "t", "df", "p", "p_adjusted")

      output # Games Howell output
    }

    res$output <-
      switch(method,
        "Tukey" = {
          res$output <- list(
            method = method,
            result = tukey_test(
              n_groups,
              gr_sizes,
              pair_names,
              variances,
              mean_diffs,
              alpha,
              p_adjust
            )
          )
        },

        "Games-Howell" = {
          res$output <- list(
            method = method,
            result = games_howell_test(
              n_groups,
              gr_sizes,
              pair_names,
              variances,
              mean_diffs,
              alpha,
              p_adjust
            )
          )
        }
      )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    descriptives <- data.frame(
      group = gr_names,
      n = gr_sizes,
      mean = means,
      variance = variances,
      sd = SD,
      stringsAsFactors = FALSE
    )

    res$output$descriptives <- descriptives

    ### Set class and return object --------------------------------------
    structure(res, class = "posthoc_anova")
  }



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# @param x The object to print.
# @param digits The number of significant digits to print.
#' @rdname posthoc_anova
#' @export
print.posthoc_anova <- function(x,
                                digits = x$input$digits,
                                digits_p = x$input$digits_p,
                                digits_param = 2,
                                ...) {
  Method <- x$output$method
  msg <- switch(Method,
    "Tukey" = "Tukey HSD",
    "Games-Howell" =  "Games-Howell"
  )

  dat <- x$output$result

  a_cld <- make_cld(p_adjusted ~ groups, data = dat, swap_compared_names = TRUE)

  if (tolower(x$input$p_adjust) == "none") {
    dat[, "p_adjusted"] <- NULL
    p_cols <- c("p")
  } else {
    p_cols <- c("p", "p_adjusted")
  }

  dat <- format_numbers(dat, digits = c(
    "difference" = digits,
    "ci_lower" = digits,
    "ci_upper" = digits,
    "t" = digits_param,
    "df" = digits_param
  ))

  # dat <- format_as_p_columns(dat,
  #                            colnames = p_cols,
  #                            digits_p = digits_p)
  dat <- format_p_values(dat,
    cols = p_cols,
    digits_p = digits_p
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  desc_stat <- format_numbers(x$output$descriptives,
    digits = c(NA, NA, digits, digits, digits)
  )

  desc_stat <- merge(desc_stat, a_cld, by.x = "group", by.y = "group", all = TRUE)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Print

  cat(msg, " test results (ANOVA post-hoc) \n", sep = "")

  if (tolower(x$input$p_adjust) != "none") {
    cat(paste("\np value adjustment:", x$input$p_adjust, "\n"))
  }
  cat("\n")
  print(dat)

  cat("\n")
  print(desc_stat)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname posthoc_anova
#' @export
#'
#' @param zero_line_color  (character) Color for line indicating zero differences.
#' @param flip_xy (logical) Flag if x and y axes should be swapped.
#' @param add_p (logical) Flag if p values should be added.
#' @param p_color (character) Color for p values.
#' @param p_pos_adj (numeric) Factor for p value position correction
#' @param p_size (numeric) Font size to p-values-related text.
plot.posthoc_anova <- function(x,
                               ...,
                               zero_line_color = "grey",
                               add_p = TRUE,
                               p_size = 1,
                               p_color = "blue",
                               p_pos_adj = 0.22,
                               flip_xy = TRUE) {
  inp <- x$output$result

  p <- ggplot(inp, aes(groups)) +
    geom_hline(yintercept = 0, lty = 2, color = zero_line_color) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    geom_point(aes(y = difference)) +
    labs(
      title = "Confidence intervals for differences in means",
      subtitle = glue::glue("Pairwise comparisons by {x$output$method} method"),
      x = "Groups",
      y = "Differences"
    )

  if (flip_xy == TRUE) {
    p <- p +
      geom_text(aes(as.numeric(as.factor(groups)) + p_pos_adj,
        y = difference,
        label = format_p_values(p_adjusted, add_p = TRUE)
      ),
      color = p_color,
      size = p_size
      )
  }

  if (flip_xy == TRUE) {
    p <- p + coord_flip()
  }
  # Output
  p
}


# ==============================================================================
# Notes from `userfriendlyscience::posthocTGH()`:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This function is used by the 'oneway' function for one-way analysis of
# variance in case a user requests post-hoc tests using the Tukey or
# Games-Howell methods.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This function is based on a file that was once hosted at http://www.psych.yorku.ca/cribbie/6130/games_howell.R, but has been removed since. It was then adjusted for implementation in the \pkg{userfriendlyscience} package. Jeffrey Baggett needed the confidence intervals, and so emailed them, after which his updated function was used. In the meantime, it appears Aaron Schlegel (\url{https://rpubs.com/aaronsc32}) independently developed a version with confidence intervals and posted it on RPubs at \url{https://rpubs.com/aaronsc32/games-howell-test}.
#
# Also, for some reason, \code{p.adjust} can be used to specify additional correction of \emph{p} values. I'm not sure why I implemented this, but I'm not entirely sure it was a mistake either. Therefore, in \code{userfriendlyscience} version 0.6-2, the default of this setting changed from \code{"holm"} to \code{"none"} (also see https://stats.stackexchange.com/questions/83941/games-howell-post-hoc-test-in-r).
# ============================================================================
