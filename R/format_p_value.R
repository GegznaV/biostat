# TODO:
# 1. Include alpha paramerer to start marking significance.

# ====================================================================
#' [!!!] Format p-values
#'
#' Functions to fromat p values.
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
#' library(biostat)
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

format_p_values <- function(p,
                            digits_p = 3,
                            cols = NULL,
                            ...,
                            alpha = 0.05,
                            signif_stars = TRUE,
                            rm_zero = FALSE,
                            add_p = FALSE,
                            rm_spaces = FALSE,
                            ss = signif_syms
) {

    UseMethod("format_p_values")
}

#' @rdname format_p_values
#' @export
format_p_values.default <- function(p,
                                    digits_p = 3,
                                    cols = NULL,
                                    ...,
                                    signif_stars = TRUE,
                                    rm_zero = FALSE,
                                    add_p = FALSE,
                                    rm_spaces = FALSE,
                                    ss = signif_syms) {
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
    sapply(p, format_p,
           digits_p = digits_p,
           signif_stars = signif_stars,
           rm_zero = rm_zero,
           add_p = add_p,
           rm_spaces = rm_spaces,
           ss = ss,
           ...

    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
#' @examples
#'
#' format_p(0)
#' format_p(.02, digits_p = 2)
#' format_p(.0002)
#' format_p(.0002, signif_stars = FALSE)
#' format_p(.0002, ss = c("*****" = 0.001))

# TODO [!!!]:
# 1. Add parameter to emable p value correction
#    from p = 1 into, e.g., p > 0.999;
#
# 2. merge parameters `ss` and `signif_stars`
# 3. test: format_p(NaN) -- [OK]
# 4. test: format_p(NA) -- this function fails with NA as input.

format_p <-
    function(p_i,
             digits_p = 3,
             signif_stars = TRUE,
             rm_zero = FALSE,
             add_p = FALSE,
             rm_spaces = FALSE,
             ss = signif_syms    ) {

        # if (is.na(p_i)) {
        #     return(as.character(p_i))
        # }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    s_i <- if (signif_stars == TRUE) {
        biostat::get_signif_stars(p_i, ss = ss)
    } else {
        ""
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.na(digits_p) || is.na(p_i)) {
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
        p_i <- biostat::rm_zero(p_i)
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
format_p_values.data.frame <- function(data,
                                       digits_p = 3,
                                       # colnames = NULL,
                                       cols = NULL,
                                       ...,
                                       ss = signif_syms,
                                       signif_stars = TRUE,
                                       rm_zero = FALSE,
                                       add_p = FALSE,
                                       rm_spaces = FALSE)
{
    data_colnames <- names(data)

    if (is.null(cols)) {
        is_p <- grepl("^p$|^p.?val|^p.?adj", data_colnames, ignore.case = TRUE)

        message("The p-value formatting applied for these columns: ",
                paste(data_colnames[is_p], collapse = ", "))

    } else if (is.character(cols)) {
        is_p <- data_colnames %in% cols

    } else if (is.numeric(cols)) {
        is_p <- cols
    } else if (is.logical(cols)) {
        is_p <- cols
    } else {
        stop("The type of argument `cols` is incorrect.")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    is_num <- sapply(data, is.numeric)
    not_numeric_p <- is_p & (!is_num)
    if (any(not_numeric_p)) {
        message("These columns are not numeric thus p-formatting skipped: ",
                paste(data_colnames[not_numeric_p], collapse = ", "))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    colname <- data_colnames[is_p & is_num]

    for (colname_i in colname) {
        data[[colname_i]] %<>%
                purrr::map_chr(format_p_values,
                               digits_p = digits_p,
                               signif_stars = signif_stars,
                               ss = ss,
                               rm_zero = rm_zero,
                               add_p = add_p,
                               rm_spaces = rm_spaces,
                               ...
                               )
        }
    data
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
add_signif_stars <- function(p, ss = signif_syms) {
    paste(p, format(get_signif_stars(p, ss = ss)))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
get_signif_stars <- function(p, ss = signif_syms) {
    checkmate::assert_numeric(p,  lower = 0, upper = 1)
    checkmate::assert_numeric(ss, lower = 0, upper = 1)

    ss_obj <- signif_parse(ss)

    # sapply(p, function(p_i) {
    #     stats::symnum(
    #         p_i,
    #         corr = FALSE,
    #         na = FALSE,
    #         cutpoints = ss_obj$cutpoints,
    #         symbols   = ss_obj$symbols
    #     )
    # })

    res <- stats::symnum(
        p,
        corr = FALSE,
        na = FALSE,
        cutpoints = ss_obj$cutpoints,
        symbols   = ss_obj$symbols
    )

    unclass(res)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
signif_syms_05s  <- c("*" = 0.05)
#' @rdname format_p_values
#' @export
p05 <- c("*" = 0.05)
#' @rdname format_p_values
#' @export
signif_syms_01s  <- c("*" = 0.01)
#' @rdname format_p_values
#' @export
p01  <- c("*" = 0.01)
#' @rdname format_p_values
#' @export
signif_syms_001s <- c("*" = 0.001)
#' @rdname format_p_values
#' @export
p001 <- c("*" = 0.001)
#' @rdname format_p_values
#' @export
signif_syms_001  <- c("***" = 0.001)
#' @rdname format_p_values
#' @export
signif_syms_01   <- c("***" = 0.001, "**" = 0.01)
#' @rdname format_p_values
#' @export
signif_syms_05   <- c("***" = 0.001, "**" = 0.01, "*" = 0.05)
#' @rdname format_p_values
#' @export
signif_syms      <- c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1)
#' @rdname format_p_values
#' @export
p05_01_001   <- c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1)
#' @rdname format_p_values
#' @export
p05plus   <- c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
signif_parse <- function(ss = NULL) {
    # ss is a named numeric vector, e.g.,
    # ss <- c("***" = 0.001, "**" = 0.01, "*" = 0.05)
    ss <- ss[order(ss)]

    list(symbols   = c(names(ss), " "),
         cutpoints = c(0, as.numeric(ss), 1))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
signif_stars_legend_2 <- function(ss = signif_syms) {
    # ss is a named numeric vector, e.g.,
    # ss <- c("***" = 0.001, "**" = 0.01, "*" = 0.05)

    ss_obj <- signif_parse(ss)

    tmp <- stats::symnum(1, corr = FALSE, na = FALSE,
                         cutpoints = ss_obj$cutpoints,
                         symbols   = ss_obj$symbols)

    attr(tmp, "legend")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
# [!!!] en dash (–) might cause error on CRAN checks.
signif_stars_legend <- function(ss = signif_syms,
                                decreasing = FALSE,
                                collapse = c("  \n", ", ", "; ")) {
    collapse <- match.arg(collapse)
    ss <- ss[order(ss, decreasing = decreasing)]
    xx <- c(names(ss),
            paste0("- p < ", as.numeric(ss))) %>%
        matrix(ncol = 2)

    paste(paste(xx[, 1], xx[, 2]), collapse = collapse)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
#' @examples
#' rm_zero(0.020)
#'
rm_zero <- function(str, dec = ".") {
    sub(paste0("0", dec), dec, as.character(str), fixed = TRUE)
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
#             data[[colname_i]] %<>% purrr::map_chr(biostat::rm_zero)
#         }
#     }
#     data
# }


# Deprecated --------------------------------------------------------------


#' @rdname format_p_values
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
format_as_p_columns <- function(data,
                                colnames = c("p.value", "p.adjust"),
                                digits_p = 3,
                                rm_zero = FALSE,
                                signif_stars = FALSE,
                                ...)
{
    .Deprecated("format_p_values")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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