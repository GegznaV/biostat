# smean.cl.boot

#' Basic nonparametric bootstrap confidence interval of mean
#'
#' A very fast implementation of the basic nonparametric bootstrap for obtaining confidence limits for the population mean without assuming normality.
#'
#' The function is based on code of function \code{\link[Hmisc]{smean.cl.boot}()} from package \pkg{Hmisc}.
#'
#'
#' @param y (numeric) A numeric vector from which \code{NA}s will be removed automatically.
#' @param conf_level (number) Confidence level. Number from 0 to 1. Default 0.95.
#' @param repetitions (integer) Number of bootstrap resamples.
#' @param na.rm (logical) If \code{TRUE} (default), missing values (\code{NA}'s) are removed automatically.
#' @param resampled_means (logical) If \code{TRUE}, the vector of bootstrapped means will be returned as the \code{resampled_means} attribute of the returned object.
#' @param return_df (logical) If \code{TRUE} (default), result is returned as a data frame. If \code{FALSE} - as a matrix.
#'
#' @return An object (data frame or matrix) with a point estimate and confidence interval of mean.
#'
#' @export
#' @examples
#' set.seed(111555)
#' ci_mean_boot(1:60)
#'
#' set.seed(999555)
#' y <- rnorm(35, 10, 5)
#' ci_mean_boot(y)
ci_mean_boot <- function(y,
                         conf_level = 0.95,
                         repetitions = 2000,
                         na.rm = TRUE,
                         resampled_means = FALSE,
                         return_df = TRUE) {
  checkmate::assert_numeric(y, all.missing = FALSE)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  checkmate::assert_number(repetitions, lower = 1)

  if (na.rm) {
    y <- y[!is.na(y)]
  }

  n <- length(y)
  mean_bar <- mean(y)

  if (n < 2L) {
    # If too few data points are present
    ci_of_mean <- c(lower = NA, upper = NA)
    repetitions <- NA
  } else {
    # If CI can be calculated
    all_means <-
      unlist(lapply(
        seq_len(repetitions),
        FUN = function(i, y, n_) {
          sum(y[sample.int(n_, n_, TRUE, NULL)])
        },
        y = y,
        n_ = n
      )) / n

    probs <- c((1 - conf_level) / 2, (1 + conf_level) / 2)
    ci_of_mean <- quantile(all_means, probs)
    names(ci_of_mean) <- NULL
  }

  # Prepare output
  res <- matrix(c(mean_bar, ci_of_mean, conf_level, repetitions),
    nrow = 1,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c("mean", "lower", "upper", "conf_level", "repetitions")
    )
  )

  if (return_df == TRUE) {
    res <- as.data.frame(res)
  }

  if (resampled_means == TRUE) {
    attr(res, "resampled_means") <- all_means
  }

  # Output
  res
}
