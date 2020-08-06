#' Scale a vector
#'
#' From every element in a vector, subtract \code{center} and
#' divide by \code{scale}.
#'
#' @param y A numeric vector.
#' @param center Either a function that computes center of data
#'              (such as \code{mean}) or a single numeric value.
#' @param scale Either a function that computes variability of data
#'              (such as \code{sd}) or a single numeric value.
#'
#' @return The same object as \code{y} just with every element scaled
#' @export
#'
#' @examples
#'
#' y <- 1:10
#' scale_vector(y)
#'
#' scale_vector(y, center = median, scale = IQR)
#'
#' scale_vector(y, center = 10, scale = 2)
scale_vector <- function(y, center = mean, scale = sd) {
  # Preparation
  cener_ <-
    if (is.function(center)) {
      center(y)
    } else if (length(center) == 1) {
      center
    } else {
      stop("Incorrect value of 'center'")
    }

  scale_ <-
    if (is.function(scale)) {
      scale(y)
    } else if (length(scale) == 1) {
      scale
    } else {
      stop("Incorrect value of 'scale'")
    }

  # Transformation
  (y - cener_) / scale_
}
