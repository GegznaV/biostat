
# smean.cl.boot
ci_mean_median_boot <-
  function(x,
           conf_level = 0.95,
           repetitions = 2000,
           na.rm = TRUE,
           resamples = FALSE) {
    if (na.rm) {
      x <- x[!is.na(x)]
    }

    n <- length(x)
    mean_bar <- mean(x)
    median_bar <- median(x)

    if (n < 2L) {
      return(c(
        Mean = xbar,
        Lower = NA,
        Upper = NA
      ))
    }

    z_mean <- unlist(lapply(
      seq_len(repetitions),
      FUN = function(i, x, N) {
        sum(x[sample.int(N, N, TRUE, NULL)])
      },
      x = x,
      N = n
    )) / n

    z_median <- unlist(lapply(
      seq_len(repetitions),
      FUN = function(i, x, N) {
        median.default(x[sample.int(N, N, TRUE, NULL)])
      },
      x = x,
      N = n
    ))


    probs <- c((1 - conf_level) / 2, (1 + conf_level) / 2)

    quant_median <- quantile(z_median, probs)
    quant_mean <- quantile(z_mean, probs)

    names(quant_mean) <- NULL
    names(quant_median) <- NULL

    res <- matrix(c(
      mean_bar, quant_mean, conf_level,
      median_bar, quant_median, conf_level
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      c("Mean", "Median"),
      c("estimate", "lower", "upper", "conf_level")
    )
    )

    # Mean = xbar,
    #      Lower = quant[1L],
    #      Upper = quant[2L])



    # if (reps)
    #     attr(res, "reps") <- z

    res
  }



# Hmisc::binconf

#' Confidence Intervals for Binomial Probabilities
#'
#' Produces 1-alpha confidence intervals for binomial probabilities.
#'
#'
#' @param x vector containing the number of "successes" for binomial variates
#' @param n vector containing the numbers of corresponding observations
#' @param alpha probability of a type I error, so confidence coefficient =
#' 1-alpha
#' @param method character string specifing which method to use.  The "all"
#' method only works when x and n are length 1.  The "exact" method uses the F
#' distribution to compute exact (based on the binomial cdf) intervals; the
#' "wilson" interval is score-test-based; and the "asymptotic" is the
#' text-book, asymptotic normal interval.  Following Agresti and Coull, the
#' Wilson interval is to be preferred and so is the default.
#' @param include_x logical flag to indicate whether \code{x} should be
#' included in the returned matrix or data frame
#' @param include_n logical flag to indicate whether \code{n} should be
#' included in the returned matrix or data frame
#' @param return_df logical flag to indicate that a data frame rather than a
#' matrix be returned
#'
#' @return a matrix or data.frame containing the computed intervals and,
#' optionally, \code{x} and \code{n}.
#'
#' @author Rollin Brant, Modified by Frank Harrell and \cr
#' Brad Biggerstaff \cr
#' Centers for Disease Control and Prevention \cr
#' National Center for Infectious Diseases \cr
#' Division of Vector-Borne Infectious Diseases \cr
#' P.O. Box 2087, Fort Collins, CO, 80522-2087, USA \cr
#' \email{bkb5@@cdc.gov}
#'
#' @references A. Agresti and B.A. Coull, Approximate is better than "exact"
#' for interval estimation of binomial proportions, \emph{American
#' Statistician,} \bold{52}:119--126, 1998.
#'
#' R.G. Newcombe, Logit confidence intervals and the inverse sinh
#' transformation, \emph{American Statistician,} \bold{55}:200--202, 2001.
#'
#' L.D. Brown, T.T. Cai and A. DasGupta, Interval estimation for a binomial
#' proportion (with discussion), \emph{Statistical Science,}
#' \bold{16}:101--133, 2001.
#'
#' @keywords category htest
#'
#' @examples
#'
#' ci_prop(0:10, 10, include_x = TRUE, include_n = TRUE)
#' ci_prop(46, 50, method = "all")
ci_prop <- function(x,
                    n,
                    method = c("wilson", "exact", "asymptotic", "all"),
                    conf_level = 0.95,
                    include_x = FALSE,
                    include_n = FALSE,
                    return_df = TRUE) {
  alpha <- 1 - conf_level

  ## ..modifications for printing and the addition of a
  ##   method argument and the asymptotic interval
  ##   and to accept vector arguments were
  ##   made by Brad Biggerstaff on 10 June 1999
  ##
  ##   Modifications mane by Vilmantas Grgzna on 31 October 2017

  method <- match.arg(method)

  bc <- function(x, n, alpha, method) {
    nu1 <- 2 * (n - x + 1)
    nu2 <- 2 * x
    ll <- if (x > 0) {
      x / (x + qf(1 - alpha / 2, nu1, nu2) * (n - x + 1))
    } else {
      0
    }

    nu1p <- nu2 + 2
    nu2p <- nu1 - 2
    pp <- if (x < n) {
      qf(1 - alpha / 2, nu1p, nu2p)
    } else {
      1
    }

    ul <- ((x + 1) * pp) / (n - x + (x + 1) * pp)
    zcrit <- -qnorm(alpha / 2)
    z2 <- zcrit * zcrit
    p <- x / n
    cl <- (p + z2 / 2 / n + c(-1, 1) * zcrit * sqrt((p * (1 - p) + z2 / 4 / n) / n)) / (1 + z2 / n)

    if (x == 1) {
      cl[1] <- -log(1 - alpha) / n
    }

    if (x == (n - 1)) {
      cl[2] <- 1 + log(1 - alpha) / n
    }

    asymp.lcl <- x / n - qnorm(1 - alpha / 2) *
      sqrt(((x / n) * (1 - x / n)) / n)

    asymp.ucl <- x / n + qnorm(1 - alpha / 2) * sqrt(((x / n) * (1 - x / n)) / n)
    res <- rbind(c(ll, ul), cl, c(asymp.lcl, asymp.ucl))
    res <- cbind(rep(x / n, 3), res)

    ## dimnames(res) <- list(c("Exact", "Wilson", "Asymptotic"), c(
    ## "Point Estimate", "Lower", "Upper"))
    switch(method,
      wilson =     res[2, ],
      exact =      res[1, ],
      asymptotic = res[3, ],
      all =        res,
      res
    )
  }

  if ((length(x) != length(n)) & length(x) == 1) {
    x <- rep(x, length(n))
  }

  if ((length(x) != length(n)) & length(n) == 1) {
    n <- rep(n, length(x))
  }

  if ((length(x) > 1 | length(n) > 1) & method == "all") {
    method <- "wilson"
    warning(
      "`method = all` will not work with vectors...",
      "setting method to `wilson`"
    )
  }

  if (method == "all" & length(x) == 1 & length(n) == 1) {
    mat <- bc(x, n, alpha, method)
    dimnames(mat) <- list(
      c("Exact", "Wilson", "Asymptotic"),
      c("point_est", "lower", "upper")
    )
    if (include_n) {
      mat <- cbind(N = n, mat)
    }

    if (include_x) {
      mat <- cbind(X = x, mat)
    }

    if (return.df) {
      mat <- as.data.frame(mat)
    }

    return(mat)
  }

  mat <- matrix(ncol = 3, nrow = length(x))

  for (i in 1:length(x)) {
    mat[i, ] <- bc(x[i], n[i], alpha = alpha, method = method)
  }

  dimnames(mat) <- list(
    rep("", dim(mat)[1]),
    c("point_est", "lower", "upper")
  )
  if (include_n) {
    mat <- cbind(N = n, mat)
  }

  if (include_x) {
    mat <- cbind(X = x, mat)
  }

  if (return_df) {
    mat <- as.data.frame(mat, row.names = NULL)
  }

  mat
}
