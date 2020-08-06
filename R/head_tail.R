# head_tail -------------------------------------------------------------------

#' **[!!]** Show several first and last rows of a data frame
#'
#' This function selects several top and bottom rows of a data frame for a preview.
#'
#' @param obj A data frame.
#' @param n (integer) Number of top and bottom rows to display.
#' @param top (integer) Number of top rows to display.
#' @param bottom (integer) Number of bottom rows to display.
#' @param tolerance (integer) For small datasets, number of rows to show in addition to `top` and `bottom` before the dataset is truncated.
#' @param sep (character) Separator between displayed top and bottom lines.
#' @param signif_digits (integer, `NULL`)
#'        Number of significant digits used to determine appropriate rounding.
#'        If `NULL`, all digits are used to determine the appropriate
#'        rounding.
#' @param max_decimals (integer, `NULL`)
#'        Maximum number of decimal digits to bpint.
#'        If `NULL`, no restrictions applied.
#' @inheritParams base::formatC
#'
#' @return A truncated data frame (which is intended to be printed) with all
#'         variables converted to strings.
#' @export
#'
#' @keywords utilities
#'
#' @examples
#' library(biostat)
#'
#' data(swiss)
#' head_tail(swiss)
#'
#' data(swiss)
#' head_tail(iris, n = 2)
#' head_tail(iris[1:6, ], n = 2, tolerance = 1)
#' head_tail(iris[1:6, ], n = 2, tolerance = 2)
head_tail <- function(obj,
                      n = 4,
                      format = c("f", "g", "e"),
                      sep = "...",
                      max_decimals = NULL,
                      signif_digits = 3,
                      tolerance = 2,
                      top = n,
                      bottom = n) {
  checkmate::assert_data_frame(obj)
  checkmate::assert_integerish(n)
  checkmate::assert_integerish(top, lower = 0)
  checkmate::assert_integerish(bottom, lower = 0)
  checkmate::assert_integerish(tolerance, lower = 0)
  checkmate::assert_integerish(signif_digits, null.ok = TRUE)
  checkmate::assert_integerish(max_decimals, null.ok = TRUE)
  checkmate::assert_character(format)

  format <- match.arg(format)

  obj <- as.data.frame(obj)

  # If data frame is small
  nrows <- nrow(obj)
  small_df <- nrows <= top + bottom + tolerance
  if (small_df == TRUE) {
    top <- nrows
    bottom <- 0
  }

  obj_h <- head(obj, top)
  obj_t <- tail(obj, bottom)


  if (is.null(signif_digits)) {
    obj_ht <- rbind(obj_h, obj_t)
  } else {
    obj_ht <-
      rbind(
        round_numbers(obj_h, fun = signif, digits = signif_digits),
        round_numbers(obj_t, fun = signif, digits = signif_digits)
      )
  }

  decim <- n_decimals_max(obj_ht)

  if (!is.null(max_decimals)) {
    decim[decim > max_decimals] <- max_decimals
  }

  # Process "head" rows
  obj_h <- format_numbers(obj_h, digits = decim, format = format)
  df_h <- dplyr::mutate_all(obj_h, as.character)
  rownames(df_h) <- rownames(obj_h)

  space <- rep(" ", ncol(obj_h))

  # Output
  if (small_df == FALSE) {
    # For refular (big enough) DFs

    # Process "tail" rows
    obj_t <- format_numbers(obj_t, digits = decim, format = format)
    df_t <- dplyr::mutate_all(obj_t, as.character)
    rownames(df_t) <- rownames(obj_t)

    dots <- rep(sep, ncol(obj_h))

    eval_glue("rbind(df_h, `{sep}` = dots, df_t, `  ` = space)")
  } else {
    # For very small DFs
    eval_glue("rbind(df_h, `  ` = space)")
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Number of decimals without tailing zeros
# x - a vector
n_decimals <- function(x) {
  if (!is.numeric(x)) {
    return(rep(NA, length(x)))
  }

  # `scipen = 999` prevents from convertion to scientific number format
  withr::with_options(list(scipen = 999), {
    x[(x %% 1) == 0] <- ""
    # as.character(x)
    nchar(sub("(^.+\\.)(.*)(0*$)", "\\2", as.character(x)))
  })
}
# ## Not prepared function:
#
# # Number of decimals without tailing zeros
# n_decimals_2 <- function(x) {
#     if (!is.numeric(x))
#         return(rep(NA, length(x)))
#
#     # `scipen = 999` prevents from convertion to scientific number format
#     withr::with_options(list(scipen = 999), {
#         without_decimals <- (x %% 1) == 0
#
#         if (without_decimals) {
#             num <- ""
#         } else {
#             nchar(sub('(^.+\\.)(.*)(0*$)', '\\2', as.character(num)))
#
#
#             nchar(sub('-?(^.+\\.)(.*)(0*$)', '\\2', as.character(num)))
#         }
#
#     })
# }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Max num. of significant decimals in each column
n_decimals_max <- function(obj) {
  sapply(obj, function(x) max(n_decimals(x)))
}

signif_needed <- function(x) {
  n_decimals_max(DF)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Round numeric columns in a data frame
round_numbers <- function(data,
                          digits = 3,
                          fun = round,
                          ...) {

  # Apply the recycling of values
  digits <- if (is_named(digits)) {
    adjust_named_vector(digits, data)
  } else {
    adjust_vector_length(digits, data)
  }

  # Apply the formatting
  for (i in seq_along(data)) {
    if (!is.numeric(data[[i]])) next
    if (is.na(digits[i])) next
    data[[i]] <- fun(data[[i]], digits = digits[i], ...)
  }
  # Output:
  data
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#' Min for numeric vectors or NA othervise
#'
#'
#'
#' @param x a vector
#' @param na.rm logical passed to min
#'
#' @return Return minimum value if `x` is numeric and `NA` oterwise.
#' @export
#' @keywords internal
#'
min_numeric <- function(x, na.rm = TRUE) {
  if (is.numeric(x)) {
    min(x, na.rm = na.rm)
  } else {
    NA
  }
}
