#' Compute a slope and an intercept for a reference line in a qq-plot
#'
#' Compute a slope and an intercept for a reference line ("qq-line")
#' in a quantile comparison plot given a vector of data an a theoretical
#' distribution.
#'
#' @details
#' The code is based on code of \code{qqline()} in package \pkg{stats}.
#' But instead of plotting a reference line, it returns its coefficients.
#'
#' @param y (numeric) A numeric vector.
#'
#' @inheritParams stats::qqline
#' @inheritParams stats::quantile
#' @param ... Further arguments for \code{distribution}.
#'
#' @return A vector with a \code{slope} and an \code{intercept} for a qq-line.
#' @export
#' @keywords internal
#' @examples
#'
#' set.seed(254)
#' x <- rnorm(50)
#'
#' qq_line_coeffs(x)
#'
qq_line_coeffs <- function(y,
                           datax = FALSE,
                           distribution = qnorm,
                           probs = c(0.25, 0.75),
                           qtype = 7,
                           na.rm = TRUE,
                           ...) {

    checkmate::assert_numeric(probs, lower = 0, upper = 1, len = 2)
    checkmate::assert_function(distribution)

    y <- stats::quantile(y,
                         probs,
                         names = FALSE,
                         type = qtype,
                         na.rm = na.rm)

    x <- distribution(probs, ...)

    if (datax) {
        slope <- diff(x) / diff(y)
        intercept <- x[1L] - slope * y[1L]

    } else {
        slope <- diff(y) / diff(x)
        intercept <- y[1L] - slope * x[1L]
    }
    # Output
    c(intercept = intercept, slope = slope)
}
# =============================================================================