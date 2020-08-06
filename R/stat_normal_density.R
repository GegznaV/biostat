# A layer function.

#' A ggplot2 layer that fits and plots normal density function
#'
#' A ggplot2 layer that fits and plots a theoretical normal density function.
#' May be used to visually compare between empirical and theoretical normal
#' distributions.
#'
#' @inheritParams ggplot2::stat_identity
#' @inheritParams ggplot2::geom_line
#' @param trim (numeric) Fraction of points to trim before computing mean and
#'                       standard deviation, which are used to construct
#'                       theoretical normal distribution.
#'                       Number between 0 and 1. Default is \code{0}.
#'
#' @section Computed variables:
#'
#' \describe{
#'    \item{density}{normal density}
#'    \item{ndensity}{normal density scaled to maximum of 1}
#'    \item{count}{normal density * number of points}
#' }
#'
#' @export
#'
#' @keywords ggplot2 plots
#'
#' @examples
#' library(ggplot2)
#' library(biostat)
#'
#' ggplot(mtcars, aes(mpg)) +
#'   stat_normal_density()
#'
#' ggplot(mtcars, aes(mpg)) +
#'   stat_density(alpha = 0.8) +
#'   stat_normal_density(color = "red", size = 1)
stat_normal_density <- function(mapping = NULL, data = NULL, geom = "line",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, trim = 0, ...) {
  ggplot2::layer(
    stat = StatNormalDensity, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, trim = trim, ...)
  )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
StatNormalDensity <-
  ggproto("StatNormal", Stat,
    required_aes = c("x"),
    default_aes = aes(y = ..density..),

    setup_params = function(data, params) {
      d <- na.omit(data$x)
      params$min_x <- min(d)
      params$max_x <- max(d)
      params
    },

    compute_group = function(data, scales, min_x, max_x,
                             trim = 0, n = 50) {
      d <- na.omit(data$x)
      d_trimmed <- biostat::trim(d, trim = trim)
      x <- seq(min_x, max_x, length.out = n)
      y <- dnorm(x, mean(d_trimmed), sd(d_trimmed))

      data.frame(x,
        density = y,
        ndensity = y / max(y),
        count = y * length(y)
      )
    }
  )
