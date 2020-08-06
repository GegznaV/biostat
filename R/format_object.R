# Internal functions
# ============================================================================
format_object <- function(x, ...) {
  UseMethod("format_object")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
format_object.test_normality <- function(x,
                                         digits_p = 3,
                                         digits_stat = 3,
                                         format_stat = "auto",
                                         signif_stars = TRUE,
                                         signif_as_separate_column = TRUE,
                                         signif_stars_col_name = "signif.",
                                         rm_zero = FALSE,
                                         show_col_method = FALSE,
                                         ss = ss,
                                         hide_error_msg = FALSE,
                                         ...) {
  # Should column "method" be removed?
  if (show_col_method == FALSE) {
    x$method <- NULL
  }

  # Add columns with significance stars
  p_adjust_method <- attr(x, "p_adjust_method")

  if (signif_stars == TRUE & signif_as_separate_column == TRUE) {
    # Significance stars are for adjusted p values, if present.
    # And for unadjusted otherwise
    x[[signif_stars_col_name]] <-
      if (!is.null(p_adjust_method)) {
        sprintf("%-3s", get_signif_stars(x$p.adjust, ss = ss))
      } else {
        sprintf("%-3s", get_signif_stars(x$p.value, ss = ss))
      }

    # There is no point to show significance stars two times
    signif_stars <- FALSE
  }

  # Round
  # x <- format_as_p_columns(x,
  #                          digits_p = digits_p,
  #                          rm_zero = rm_zero,
  #                          signif_stars = signif_stars)

  suppressMessages({
    # Suppress message which columns with p values were formatted.
    x <- format_p_values(x,
      digits_p = digits_p,
      rm_zero = rm_zero,
      signif_stars = signif_stars,
      ss = ss,
      ...
    )
  })

  if (is.numeric(x$statistic)) {
    # The rounding of values is chosen according to the maximum value
    # of statistic
    # if format is "auto"

    if (format_stat == "auto") {
      all_stat_values_lt <- function(x_c) {
        all(na.omit(x$statistic < x_c))
      }

      if (all_stat_values_lt(1)) {
        digits_stat <- digits_stat
        format_stat <- "f"
      } else if (all_stat_values_lt(10)) {
        digits_stat <- digits_stat - 1
        format_stat <- "f"
      } else if (all_stat_values_lt(100)) {
        digits_stat <- digits_stat - 2
        format_stat <- "f"
      } else {
        digits_stat <- digits_stat
        format_stat <- "g"
      }
    }
    # "!is.na(...)" is used to fornat "NA" values consistently
    # (as "<NA>" and not "NA") in all columns
    x$statistic[!is.na(x$statistic)] <-
      formatC(x$statistic[!is.na(x$statistic)],
        format = format_stat,
        digits = digits_stat
      )
  }

  if (rm_zero == TRUE) {
    x$statistic %<>% biostat::rm_zero()
  }


  if (is.null(x$error_msg) | hide_error_msg) {
    x$error_msg <- NULL
  }

  if (!all(is.na(x$error_msg))) {
    # Make `error_msg` the last column
    x <- dplyr::select(x, -error_msg, dplyr::everything(), error_msg)
  } else {
    x$error_msg <- NULL
  }

  # Output:
  structure(x, ss = ss)
}
