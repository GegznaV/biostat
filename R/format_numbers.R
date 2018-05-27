# TODO:
# 1) add parameter `skip` - for columns to skip.
# 2) add parameter `include` - for the only columns to format.
# 3) check and warn if `skip`  and `include` contain at least one common column name.
# 4) add value digits = "auto" to automatically choose number of digits to round to.
# 5) check if negalive values of "digits" are accepted. (patikrinti, ar galima naudoti neigiamus skaičius, ar su jais apvalina teisingai.)
#


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Format Numbers in Each Column of a Dataframe
#
# data - data frame
# digits -
# the desired number of digits after the decimal point (format = "f") or significant digits (format = "g", = "e" or = "fg"). \cr
# Either one integer or a vector of inegers for each column.
#
# format - either one value or a vector of values for each column.
#       Each value Will be passed to `fun` separately.
#
# ... - to be passed to `fun`
# fun - formatting function to be applied. Default is `formatC`
#




#' Format values of numeric columns in a dataframe.
#'
#' Function formats numeric columns only.
#' Other classes are left intact.
#'
#'
#' @param data A data frame.
#'
#' @param digits (numeric or \code{NA}) a desired number of digits after the decimal point (if \code{format = "f"}) or a number of significant digits (\code{format = "g"}, \code{= "e"} or \code{= "fg"}). \cr
#' Either one integer (to set the same formatting for all columns) or a vector of inegers (to set formatting for each column separately).\cr
#' Use \code{NA} to leave column unformatted.
#'
#' @param format (character) \code{"f"}, \code{"g"}, \code{"e"}, \code{"fg"}. Either one value or a vector of values for each column.
#'   Each value will be passed to \code{fun} separately.\cr
#'   \code{"f"} gives numbers in the usual \code{xxx.xxx} format;\cr
#'   \code{"e"} and \code{"E"} give \code{n.ddde+nn} or \code{n.dddE+nn} (scientific format);\cr
#'   \code{"g"} and \code{"G"} put number into scientific format only if it saves space to do so.\cr
#'   \code{"fg"} uses fixed format as \code{"f"}, but digits as the minimum number of significant digits. This can lead to quite long result strings
#'
#' @param fun A function that does the formatting.
#' Default is \code{\link[base]{formatC}}.
#' @param ... Other parameters to be passed to \code{fun}.
#'
#' @export
#'
#' @examples
#' library(biostat)
#' DATA <- head(iris)
#'
#' # The same rounding for each column
#' format_numbers(DATA, 2)
#'
#' # Different rounding for different columns
#' format_numbers(DATA, c(2,2,3,3,NA))
#'
#' # Use `NA` to leave a column unformatted
#' format_numbers(DATA, c(4,NA,3,NA,NA))
format_numbers <- function(data,
                           digits = 3,
                           format = "f",
                           fun = formatC,
                           ...)
{
    # Apply the recycling of values
    digits <- if (is_named(digits)) {
        adjust_named_vector(digits, data)
    } else {
        adjust_vector_length(digits, data)
    }

    format <- if (is_named(format)) {
        adjust_named_vector(format, data)
    } else {
        adjust_vector_length(format, data)
    }


    # Apply the formatting
    for (i in seq_along(data)) {
        if (!is.numeric(data[[i]])) next
        if (is.na(digits[i]))       next
        data[[i]] <-
            fun(data[[i]], digits = digits[i], format = format[i], ...)
    }
    # Output:
    data
}


