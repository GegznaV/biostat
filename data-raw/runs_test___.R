#' Wald-Wolfowitz Runs Test
#'
#' Performs the Wald-Wolfowitz runs test of randomness for continuous data.
#'
#' Data is transformed into a dichotomous vector according as each values is
#' above or below a given \code{threshold}. Values equal to the level are
#' removed from the sample.
#'
#' The default \code{threshold} value used in applications is the sample median
#' which give us the special case of this test with \eqn{n_1 = n_2}{n1 = n2}, the
#' runs test above and below the median.
#'
#' The possible \code{alternative} values are "\code{two.sided}",
#' "\code{left.sided}" and "\code{right.sided}" define the alternative
#' hypothesis. By using the alternative "\code{left.sided}" the null of
#' randomness is tested against a trend. By using the alternative
#' "\code{right.sided}" the null hypothesis of randomness is tested against a
#' first order negative serial correlation.
#'
#' @param x a numeric vector containing the observations
#'
#' @param alternative a character string with the alternative hypothesis. Must
#' be one of "\code{two.sided}" (default), "\code{left.sided}" or
#' "\code{right.sided}". You can specify just the initial letter.
#'
#' @param threshold the cut-point to transform the data into a dichotomous
#' vector
#'
#' @param pvalue a character string specifying the method used to compute the
#' p-value. Must be one of normal (default), or exact.
#'
#' @param plot a logic value to select whether a plot should be created. If
#' 'TRUE', then the graph will be plotted.
#'
#' @return A list with class "htest" containing the components:
#' \item{statistic}{the value of the normalized statistic test.}
#' \item{parameter}{a vector with the sample size, and the values of
#' \eqn{n_1}{n1} and \eqn{n_2}{n2}.} \item{p.value}{the p-value of the test.}
#' \item{alternative}{a character string describing the alternative
#' hypothesis.} \item{method}{a character string indicating the test
#' performed.} \item{data.name}{a character string giving the name of the
#' data.} \item{runs}{the total number of runs (not shown on screen).}
#' \item{mu}{the mean value of the statistic test (not shown on screen).}
#' \item{var}{the variance of the statistic test (not shown on screen).}
#' @author Frederico Caeiro
#' @references Brownlee, K. A. (1965). \emph{Statistical Theory and Methodology
#' in Science and Engineering}, 2nd ed. New York: Wiley.
#'
#' Gibbons, J.D. and Chakraborti, S. (2003). \emph{Nonparametric Statistical
#' Inference}, 4th ed. (pp. 78--86). URL:
#' \url{http://books.google.pt/books?id=dPhtioXwI9cC&lpg=PA97&ots=ZGaQCmuEUq}
#'
#' Wald, A. and Wolfowitz, J. (1940). On a test whether two samples are from
#' the same population, \emph{The Annals of Mathematical Statistics} \bold{11},
#' 147--162. doi:10.1214/aoms/1177731909.
#' \url{http://projecteuclid.org/euclid.aoms/1177731909}
#'
#' @keywords randomness test
#'
#' @author Functions and descriptions are imported from package \pkg{randtests}.
#'
#' @export
#'
#' @examples
#'
#' ##
#' ## Example 1
#' ## Data from example in Brownlee (1965), p. 223.
#' ## Results of 23 determinations, ordered in time, of the density of the earth.
#' ##
#' earthden <- c(
#'   5.36, 5.29, 5.58, 5.65, 5.57, 5.53, 5.62, 5.29, 5.44, 5.34, 5.79,
#'   5.10, 5.27, 5.39, 5.42, 5.47, 5.63, 5.34, 5.46, 5.30, 5.75, 5.68, 5.85
#' )
#' runs_test(earthden)
#'
#'
#' ##
#' ## Example 2
#' ## Sweet potato yield per acre, harvested in the United States, between 1868 and 1937.
#' ## Data available in this package.
#' ##
#' data(sweetpotato)
#' runs_test(sweetpotato$yield)
runs_test <- function(x, alternative = "two.sided", threshold = median(x), pvalue = "normal", plot = FALSE) {
  # Performs the Runs Test for Randomness.
  #
  # Args:
  #   x: a numeric vector containing the data.
  #   alternative hypothesis, must be one of "two.sided" (default), "left.sided" or "right.sided"
  #   threshold:
  #
  # Returns:
  #   statistic: the (normalized) value of the statistic test.
  #   n: the sample size, after the remotion of consecutive duplicate values.
  #   p.value: the asymptotic p-value.
  #
  dname <- deparse(substitute(x))
  if (alternative == "t") {
    alternative <- "two.sided"
  }
  if (alternative == "l") {
    alternative <- "left.sided"
  }
  if (alternative == "r") {
    alternative <- "right.sided"
  }
  if (alternative != "two.sided" & alternative != "left.sided" & alternative != "right.sided") {
    stop("must give a valid alternative")
  }
  # Remove NAs
  x <- na.omit(x)
  stopifnot(is.numeric(x))
  # Remove values equal to the level
  x <- x[x != threshold]
  s <- sign(x - threshold)
  n1 <- length(s[s > 0])
  n2 <- length(s[s < 0])
  runs <- rle(s)
  r1 <- length(runs$lengths[runs$values == 1])
  r2 <- length(runs$lengths[runs$values == -1])
  n <- n1 + n2
  mu <- 1 + 2 * n1 * n2 / (n1 + n2)
  vr <- 2 * n1 * n2 * (2 * n1 * n2 - n1 - n2) / (n^2 * (n - 1))
  rr <- r1 + r2
  #
  # Plot the data if requested by the user
  if (plot) {
    plot((1:n)[s > 0], x[s > 0], xlim = c(1, n), ylim = c(min(x), max(x)), xlab = "", ylab = dname)
    points((1:n)[s < 0], x[s < 0], col = "red")
    abline(h = threshold, col = gray(.4))
    for (i in 1:(n - 1)) {
      if (s[i] * s[i + 1] < 0) {
        abline(v = i + 0.5, lty = 2)
      }
    }
  }
  #
  # Computes the p-value
  pv <- 0
  if (pvalue == "exact") {
    if (alternative == "two.sided") {
      pv1 <- sum(druns(1:rr, n1, n2))
      pv2 <- sum(druns(rr:(n1 + n2), n1, n2))
      pv <- 2 * min(pv1, pv2)
    }
    if (alternative == "left.sided") {
      pv <- sum(druns(2:rr, n1, n2))
    }
    if (alternative == "right.sided") {
      pv <- sum(druns(rr:(n1 + n2), n1, n2))
    }
  }
  if (pvalue == "normal") {
    pv0 <- pnorm((rr - mu) / sqrt(vr))
    if (alternative == "two.sided") {
      pv <- 2 * min(pv0, 1 - pv0)
    }
    if (alternative == "left.sided") {
      pv <- pv0
    }
    if (alternative == "right.sided") {
      pv <- 1 - pv0
    }
  }
  if (alternative == "two.sided") {
    alternative <- "nonrandomness"
  }
  if (alternative == "left.sided") {
    alternative <- "trend"
  }
  if (alternative == "right.sided") {
    alternative <- "first-order negative autocorrelation"
  }
  #
  rval <- list(
    statistic = c(statistic = (rr - mu) / sqrt(vr)), p.value = pv, runs = rr, mu = mu, var = vr,
    method = "Runs Test", data.name = dname, parameter = c(runs = rr, n1 = n1, n2 = n2, n = n), alternative = alternative
  )
  class(rval) <- "htest"
  return(rval)
}
