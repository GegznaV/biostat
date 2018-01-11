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
                                         show_col_method = FALSE
) {
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
                sprintf("%-3s", get_signif_stars(x$p.adjust))
            } else {
                sprintf("%-3s", get_signif_stars(x$p.value))
            }

        # There is no point to show significance stars two times
        signif_stars = FALSE
    }

    # Round
    x <- format_as_p_columns(x,
                             digits_p = digits_p,
                             rm_zero = rm_zero,
                             signif_stars = signif_stars)

    if (is.numeric(x$statistic)) {
        # The rounding of values is chosen according to the maximum value
        # of statistic
        # if format is "auto"

        if (format_stat == "auto") {


            all_stat_values_lt <- function(x_c) {
                all(x$statistic < x_c)
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
        x$statistic %<>%
            formatC(format = format_stat, digits = digits_stat)
    }

    if (rm_zero == TRUE) {
        x$statistic %<>% biostat::rm_zero()
    }
    # Output:
    x
}
