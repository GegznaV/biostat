# To do:
#
# 1. Make output as matrix
# 2. Find out why `res` 2 times in a row. Mistake?
# 3. Make formula interface


# %% ~~function to do ... ~~
# %% ~~ A concise (1-5 lines) description of what the function does. ~~
#

#' Cohen's Effect Size
#'
#' Computes the Cohen's d and Hedges'g effect size statistics.
#'
#' @section Origin of the code:
#'
#' The main part of code and documentation of this function is imported from: \cr
#'     Andri Signorell et al. (2017).
#'     \pkg{DescTools}: Tools for descriptive statistics.
#'     R package version \emph{0.99.22} (2017-09-14).
#'
#' The modifications were made by Vilmantas Gegzna.
#' The logics of statistical computations were not changed.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#'
#' @param pooled (logical) Flag indicating whether compute pooled standard deviation
#' or the whole sample standard deviation. Default is \code{TRUE}.
#'
#' @param correct (logical) Flag indicating whether to apply the Hedges correction.
#' (Default: \code{FALSE})
#'
#' @param conf.level (numeric|\code{NA}) confidence level of the interval. Set this to \code{NA}, if no
#' confidence intervals should be calculated. (This is the default)
#' @param na.rm (logical) Should missing values be removed? Defaults to \code{FALSE}.
#'
#' @return A numeric vector with 3 elements:\itemize{
#'     \item{d}{the effect size d}
#'     \item{lwr.ci}{lower bound of the confidence interval}
#'     \item{upr.ci}{upper bound of the confidence interval}
#' }
#'
#' @author
#' Andri Signorell <andri@@signorell.net>\cr
#' Vilamntas Gegzna <GegznaV@@gmail.com>
#'
#' @seealso \code{\link{mean}}, \code{\link{var}}
#'
#'
#' @references
#'
#' Cohen, J. (1988) \emph{Statistical power analysis for the
#' behavioral sciences (2nd ed.)} Academic Press, New York.
#'
#' Hedges, L. V. & Olkin, I. (1985) \emph{Statistical methods for
#' meta-analysis} Academic Press, Orlando, FL
#'
#' Smithson, M.J. (2003) \emph{Confidence Intervals, Quantitative Applications
#' in the Social Sciences Series}, No. 140. Thousand Oaks, CA: Sage. pp. 39-41
#'
#' Cohen, J. (1992). \emph{A power primer. Psychological Bulletin}, 112, 155-159.
#'
#' @export
#' @keywords effect size
#'
#' @examples
#' data("d.pizza", package = "DescTools")
#' library(biostat)
#'
#' x <- d.pizza$price[d.pizza$driver == "Carter"]
#' y <- d.pizza$price[d.pizza$driver == "Miller"]
#'
#' cohens_d(x, y, conf.level = 0.95, na.rm = TRUE)
#'
#'
cohens_d <- function(x,
                     y = NULL,
                     pooled = TRUE,
                     correct = FALSE,
                     conf.level = NA,
                     na.rm = FALSE,
                     group = NULL,
                     ...) {

    UseMethod("cohens_d")
}
#' @rdname cohens_d
#' @export
cohens_d.default <- function(x,
                     y = NULL,
                     pooled = TRUE,
                     correct = FALSE,
                     conf.level = NA,
                     na.rm = FALSE,
                     group = NULL, ...) {


    # Main part of code and documentation imported from:
    #     Andri Signorell et al. (2017).
    #     DescTools: Tools for descriptive statistics.
    #     R package version 0.99.22.

    if (na.rm) {
        x <- na.omit(x)
        if (!is.null(y)) y <- na.omit(y)
    }

    if (is.null(y)) {   # one sample Cohen d
        d <- mean(x) / sd(x)
        n <- length(x)
        if (!is.na(conf.level)) {
            # reference: Smithson Confidence Intervals pp. 36:
            ci <- .nctCI(d / sqrt(n), df = n - 1, conf = conf.level)
            res <- c(d = d,
                     lwr.ci = ci[1] / sqrt(n),
                     upr.ci = ci[3] / sqrt(n))

        } else {
            res <- d
        }

    } else {

        meanx <- mean(x)
        meany <- mean(y)
        #     ssqx <- sum((x - meanx)^2)
        #     ssqy <- sum((y - meany)^2)
        nx <- length(x)
        ny <- length(y)

        DF <- nx + ny - 2
        d <- (meanx - meany)

        if (pooled) {
            d <- d / sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / DF)

        } else{
            d <- d / sd(c(x, y))
        }

        #  if(unbiased) d <- d * gamma(DF/2)/(sqrt(DF/2) * gamma((DF - 1)/2))

        if (correct) {  # "Hedges's g"
            # Hedges, L. V. & Olkin, I. (1985).
            # Statistical methods for meta-analysis.
            # Orlando, FL: Academic Press.
            d <- d * (1 - 3 / (4 * (nx + ny) - 9))
        }

        if (!is.na(conf.level)) {
            # old:
            # The Handbook of Research Synthesis and Meta-Analysis
            # (Cooper, Hedges, & Valentine, 2009) p. 238
            #
            # ci <- d + c(-1, 1) *
            #           sqrt(((nx+ny) / (nx*ny) + .5 * d^2 / DF) * ((nx + ny)/DF)) *
            #           qt((1 - conf.level) / 2, DF)

            # supposed to be better, Smithson's version:
            ci <- .nctCI(d / sqrt(nx * ny / (nx + ny)),
                         df = DF,
                         conf = conf.level)
            # [!!!] Why `res` is repeated 2 times in a row. First result is overwritten
            res <- matrix(c(
                d = d,
                lwr.ci = ci[1] / sqrt(nx * ny / (nx + ny)),
                upr.ci = ci[3] / sqrt(nx * ny / (nx + ny))
            ))

            res <- c(d = d,
                     lwr.ci = ci[1],
                     upr.ci = ci[2])
        } else {
            res <- d
        }
    }

    ## Cohen, J. (1992). A power primer. Psychological Bulletin, 112, 155-159.
    ## Crow, E. L. (1991).
    attr(res, "magnitude") <- c("negligible","small","medium","large")[findInterval(abs(d), c(0.2, 0.5, 0.8)) + 1]

    return(res)

}
