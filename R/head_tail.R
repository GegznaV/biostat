# head_tail -------------------------------------------------------------------

#' [!] Show several first and last rows of a data frame
#'
#' This function selects several top and bottom rows of a data frame for a preview.
#'
#' @param obj A data frame.
#' @param n (integer) Number of top and bottom rows to display.
#' @param top (integer) Number of top rows to display.
#' @param bottom (integer) Number of bottom rows to display.
#' @param sep (character) Separator between displayed top and bottom lines.
#' @inheritParams base::formatC
#'
#' @return A truncated data frame (which is intended to be printed) with all
#'         variables converted to strings.
#' @export
#'
#' @keywords utilities
#'
#' @examples
#' data(swiss)
#' head_tail(swiss)
head_tail <- function(obj,
                      n = 4,
                      top = n,
                      bottom = n,
                      sep = "...",
                      format = "f") {

    format <- match.arg(format)

    obj <- as.data.frame(obj)

    obj_h <- head(obj, top)
    obj_t <- tail(obj, bottom)

    decim <- n_decimals_max(rbind(obj_h, obj_t))

    obj_h <- format_numbers(obj_h, decim, format = format)
    obj_t <- format_numbers(obj_t, decim, format = format)

    df_h <- dplyr::mutate_all(obj_h, as.character)
    df_t <- dplyr::mutate_all(obj_t, as.character)

    rownames(df_h) <- rownames(obj_h)
    rownames(df_t) <- rownames(obj_t)

    dots  <- rep(sep, ncol(obj_h))
    space <- rep(" ", ncol(obj_h))

    rbind(df_h, `...` = dots, df_t, `  ` = space)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Number of decimals without tailing zeros
n_decimals <- function(x) {
    if (!is.numeric(x))
        return(rep(NA, length(x)))

    # `scipen = 999` prebents from convertion to scientific number format
    withr::with_options(list(scipen = 999), {
        x[(x %% 1) == 0] <- ""
        as.character(x)
        nchar(sub('(^.+\\.)(.*)(0*$)', '\\2', as.character(x)))
    })
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Max num. of significan decimals in each column
n_decimals_max <- function(obj) {
    sapply(obj, function(x) max(n_decimals(x)))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
