#' Test which values are/are not unique in a vector
#'
#' @param x A vector
#'
#' @return Vector of logical values indicating \code{TRUE} for values which are unique.
#' @export
#'
#' @keywords utilities
#'
#' @examples
#'
#' is_unique(1:10)
#'
#' is_unique(c(1, 1, 1))
#'
#' x <- c(1, 2, 3, 4, 3, 3, 1)
#' rez <- is_unique(x)
#' rez
#'
#' data.frame(x = x, is_unique = rez)
is_unique <- function(x) {
  !x %in% x[duplicated(x)]
}

#' @rdname is_unique
#' @export
is_not_inique <- function(x) {
  x %in% x[duplicated(x)]
}
