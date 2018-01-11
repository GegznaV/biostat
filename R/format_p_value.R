# ====================================================================
#' Format p-values
#'
#' Fuctions to fromat p values.
#'
#' @return A character vector with formatted p values.
#'
#' @param p A (vector of) p-value(s). Numeric or coercible to numeric.
#' @param digits_p (numeric) Number of significant digits to round a p value to.
#'                           No less than 2. \itemize{
#'         \item if \code{digits_p = 2}: \enumerate{
#'               \item values below 0.001 are printed as \code{"<0.001"};
#'               \item values between 0.001 and 0.01 as \code{"<0.01"};
#'               \item all other values are rounded to two decimal places.
#'               }
#'         \item if \code{digits_p = 3}, only formatting \code{"<0.01"} is skipped.
#'         }
#' @param p_i (numeric) A single p value.
#' @param str A string or convertible to string
#' @param signif_stars (logical) Flag if significance stars should be added to each p value.
#'                           Not less than 2.
#' @param rm_zero (logical) Flag if leading zero before the point should be
#'                removed.
#' @param data A dataset.
#' @param colnames (character) vector with column names to be formatted as p values.
#' @param add_p (logical) Flag if letter "p" should included in the expression.
#' @param rm_spaces (logical) Flag if all spaces should be removed.
#' @param ... Arguments to further methods.
#'
#' @details
#'
#' \itemize{
#'  \item \code{format_p} - formats single p value.
#'  \item \code{format_as_p_columns} - formats indicated numeric columns in a dataframe as p values (columns are converted into strings).
#'  \item \code{get_signif_stars} -  takes numeric p values brings appopriate stars of statistical significance.
#'  \item \code{add_signif_stars} - formats numeric p values by adding significance stars (result is character vector).
#'  \item \code{signif_stars_legend} - generates legend for significance stars (result is a string).
#'  \item \code{rm_zero} - function removes zero at the beginning of a number (returns a  string with the same value but without the leading zero).
#' }
#'
#'
#' @export
#' @examples
#' library(BioStat)
#'
#' # Prettify p-values
#'
#' format_p_values(0.0005)
#'
#' format_p_values(0.005)
#' format_p_values(0.005, signif_stars = FALSE)
#' format_p_values(0.005, rm_zero = TRUE)
#' format_p_values(0.005, digits_p = 2)
#' format_p_values(0.005, digits_p = 2, rm_zero = TRUE, signif_stars = FALSE)
#'
#' format_p_values(0.00022)
#' format_p_values("0.00022")
#' format_p_values("0.052")
#'
#' format_p_values(c(0.005, 0.0005, 0.052147))
#'
#'
#' get_signif_stars(0.005)
#' add_signif_stars(0.005)
#'
#' get_signif_stars(0.0005)
#' add_signif_stars(0.0005)
#'
#' get_signif_stars(0.052147)
#' add_signif_stars(0.052147)
#'
#' signif_stars_legend()
#'
format_p_values <- function(p, digits_p = 3, signif_stars = TRUE, rm_zero = FALSE,
                            add_p = FALSE, rm_spaces = FALSE) {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    p %<>% as.character() %>% as.numeric()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!checkmate::test_numeric(p, lower = 0, upper = 1)) {
        stop("`p` must contain numeric values in range from 0 to 1.\n",
             "Values NA, NULL, -Inf, and Inf are not accepted.")
    }
    if (!checkmate::test_number(digits_p, lower = 2, na.ok = TRUE)) {
        stop("`digits` must be a single numeric value in range from 2 to infinity.\n",
             "Values NA, NULL, -Inf, and Inf are not accepted.")
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sapply(p, format_p, digits_p = digits_p,
                        rm_zero = rm_zero,
                        signif_stars = signif_stars,
                        add_p = add_p
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
#' @examples
#'
#' format_p(.02, digits_p = 2)
#' format_p(.0002)
#' format_p(.0002, signif_stars = FALSE)
#'
format_p <- function(p_i, digits_p = 3, signif_stars = TRUE, rm_zero = FALSE, add_p = FALSE, rm_spaces = FALSE) {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    s_i <- if (signif_stars == TRUE) {
        BioStat::get_signif_stars(p_i)
    } else {
        ""
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.na(digits_p)) {
        p_i <- as.character(p_i)

    } else {
        min_limit <- 10^-(digits_p)

        p_i <- if (digits_p > 3 & p_i < min_limit) {
            paste0("<", formatC(min_limit, digits = digits_p, format = "f"))

        } else if (digits_p <= 3 & p_i < 0.001) {
            "<0.001"

        } else if (digits_p <= 2 & p_i < 0.01) {
            "<0.01"

        } else {
            paste0(" ", formatC(p_i, digits = digits_p, format = "f"))
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        p_i <- if (signif_stars == TRUE) {
            sprintf(glue::glue("%{digits_p + 3}s %-3s"), p_i, s_i)
        } else {
            sprintf(glue::glue("%{digits_p + 3}s"), p_i)
        }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (rm_zero == TRUE) {
        p_i <- BioStat::rm_zero(p_i)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (add_p == TRUE) {
        if (grepl("<", p_i)) {
            p_i <- paste0("p ", sub("<", "< ", p_i))
        } else {
            p_i <- paste0("p =", p_i)
        }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (rm_spaces == TRUE) {
        p_i <- gsub(" ", "", p_i)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Output:
    p_i
}
# ============================================================================
#' @rdname format_p_values
#' @export
format_as_p_columns <- function(data,
                                colnames = c("p.value", "p.adjust"),
                                digits_p = 3,
                                rm_zero = FALSE,
                                signif_stars = FALSE,
                                ...)
{
    data_colnames <- names(data)
    colname <- data_colnames[data_colnames %in% colnames]

    for (colname_i in colname) {
        data[[colname_i]] %<>%
                purrr::map_chr(format_p_values,
                               digits_p = digits_p,
                               rm_zero = rm_zero,
                               signif_stars = signif_stars,
                               ...
                               )
        }
    data
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
add_signif_stars <- function(p) {
    paste(p, format(get_signif_stars(p)))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
get_signif_stars <- function(p) {
    checkmate::assert_numeric(p, lower = 0, upper = 1)

    sapply(p, function(p_i) {
        symnum(
            p_i,
            corr = FALSE,
            na = FALSE,
            cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
            symbols = c("***", "**", "*", ".", " ")
        )
    })
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
signif_stars_legend <- function() {
    tmp <- symnum(1, corr = FALSE, na = FALSE,
                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                symbols = c("***", "**", "*", ".", " "))

    attr(tmp, "legend")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
#' @examples
#' rm_zero(0.02)
#'
rm_zero <- function(str) {
    sub("0\\.", ".", as.character(str))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Deprecated =================================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # @rdname format_p_values
# # @export
# prettify_p_value <- function(p) {
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     .Deprecated(new = "format_p_values")
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#     if (length(p) != 1) {
#         stop("The langth of `p` must be 1.")
#     }
#
#     # p %<>% readr::parse_number(p)
#     p %<>% as.character() %>% as.numeric()
#
#     if (!dplyr::between(p, 0, 1)) {stop("`p` must be between 0 and 1.")}
#
#     if (p < 0.001) {
#         "<0.001"
#     } else if (p < 0.01) {
#         "<0.01 "
#     } else {
#         sprintf(" %.2f ", p)
#     }
# }
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # @rdname format_p_values
# # @export
# # @param rm_zero (logical) If \code{TRUE}, leading zero of a number is removed.
# prettify_p_column <- function(data,
#                               colname  = c("p.value", "p.adjust"),
#                               prettify = TRUE,
#                               rm_zero = FALSE,
#                               ...)
# {
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     .Deprecated("format_as_p_columns")
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#     data_colnames <- names(data)
#     colname <- data_colnames[data_colnames %in% colname]
#
#     for (colname_i in colname) {
#         if (prettify == TRUE) {
#             data[[colname_i]] %<>% purrr::map_chr(prettify_p_value)
#         }
#
#         if (rm_zero == TRUE) {
#             data[[colname_i]] %<>% purrr::map_chr(BioStat::rm_zero)
#         }
#     }
#     data
# }