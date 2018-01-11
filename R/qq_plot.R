#' [!] A QQ-plot for multiple groups
#'
#' Make a quantile comparison plot (qq-plot) for each subset of groups separately
#' using \pkg{ggplot2} graphics.
#'
#' @details
#' Function \code{qq_plot} is inspired by \code{qqPlot()} in package \pkg{car}
#' (writen by J. Fox).
#'
#' @param y (formula|numeric|character)
#'          Either a formula, a numeric vector or a name of a vector
#'          in \code{data}.
#'          If \code{y} is a formula (e.g. \code{variable ~ factor}), left-hand
#'          side provides variable to be summarized. Right-hand side and condition
#'          describe subsets. If the left-hand side is empty, right-hand side and
#'          condition are shifted over as a convenience.
#'
#' @param data A data frame that contains the variables mentioned in \code{y}.
#'
#' @param sep (character)
#'            Group name separator if more than one grouping variable is used.
#'            default is \code{"|"}.
#'
#' @inheritParams qq_data
#' @inheritParams plot.qqdata
#' @inheritParams car::qqPlot
#' @inheritParams test_normality
#'
#'
#' @export
#'
#' @keywords ggplot2 plots
#'
#' @return A \code{ggplot2} object
#'
#' @seealso \code{\link[car]{qqPlot}} from \pkg{car} package,
#'          \code{\link[stats]{qqplot}} from \pkg{stats} package.
#' @examples
#' library(biostat)
#' data(iris)
#'
#' # Formula (no groups):
#' qq_plot(~Sepal.Length, data = iris)
#' qq_plot("Sepal.Length", data = iris)
#'
#' # Formula (several groups):
#' qq_plot(Sepal.Length ~ Species, data = iris)
#'
#' qq_plot(Sepal.Length ~ Species, data = iris, envelope = 0.90)
#'
#' # Formula (several groups in colors):
#' qq_plot(Sepal.Length ~ Species, data = iris, use_colors = TRUE)
#'
#' # Vectors (several groups):
#' qq_plot(iris$Sepal.Length, groups = iris$Species)
#'
#' # For one group:
#' qq_plot("Sepal.Length", data = iris)
#'
#' qq_plot(~Sepal.Length, data = iris)
#'
#' qq_plot(iris$Sepal.Length)
#'
#'
#' # Other examples
#' qq_plot(~weight, data = chickwts)
#'
#' qq_plot(weight ~ feed, data = chickwts)
#'
#' qq_plot(uptake ~ Type + Treatment, data = CO2)

qq_plot <- function(
    y,
    data = NULL,
    distribution = "norm",
    ...,
    line = c("quartiles", "robust", "int=0,slope=1", "0,1", "none"),
    envelope = 0.95,
    method = c("mle-normal", "trimmed-normal", "moment-normal", "any"),
    labels = NULL,
    groups = NULL,
    use_colors = FALSE,
    scales = "free",
    sep = " | ")
{

    qqdata <-  qq_data(y = y,
                       distribution = distribution,
                       data = data,
                       ...,
                       envelope = envelope,
                       line = line,
                       labels = labels,
                       groups = groups,
                       method = method,
                       sep = sep)

    plot(qqdata,
         use_colors = use_colors,
         scales = scales,
         envelope = envelope,
         line = line,
         ...)

}
# =============================================================================
