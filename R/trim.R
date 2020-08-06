#' Trim certain proportion of the most extreme values
#'
#' Trim certain proportion of the most extreme values in a numeric vector.
#'
#' @param y A numeric vector
#' @param trim (number) A proportion of values to be removed.
#'                      A number between 0 and 1. Default is 0.1.
#' @param na.rm  (logical) if \code{TRUE}, any \code{NA} and \code{NaN}'s are
#'               removed from \code{y} before quantiles are computed.
#' @param ... further argument to \code{\link[stats]{quantile}}.
#'
#' @return A trimmed vector.
#' @export
#'
#' @examples
#' 1:10
#' # [1] 1 2 3 4 5 6 7 8 9 10
#'
#' trim(1:10)
#' # [1] 2 3 4 5 6 7 8 9
#'
#' trim(1:10, trim = 0.4)
#' # [1] 3 4 5 6 7 8
trim <- function(y, trim = 0.1, na.rm = FALSE, ...) {
  if (!is.vector(y)) stop("`y` must be a vector.")
  if (!is.numeric(y)) stop("`y` must be a numeric vector.")
  checkmate::assert_number(trim, lower = 0, upper = 1)
  p <- stats::quantile(y, probs = c(trim / 2, 1 - trim / 2), na.rm = na.rm, ...)
  # y[y > p[1] & y < p[2]]
  y[dplyr::between(y, p[1], p[2])]
}
