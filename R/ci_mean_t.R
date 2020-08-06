#' t-distribution based confidence interval of mean
#'
#' Student's t-distribution based confidence interval of mean
#'
#' @inheritParams ci_mean_boot
#'
#' @return An object (data frame or matrix) with a point estimate and confidence interval of mean.
#' @export
#'
#' @examples
#' set.seed(999555)
#' y <- rnorm(35, 10, 5)
#' ci_mean_t(y)
#' #
ci_mean_t <- function(y,
                      conf_level = 0.95,
                      na.rm = TRUE,
                      return_df = TRUE) {
  checkmate::assert_numeric(y, all.missing = FALSE)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)

  if (na.rm) {
    y <- y[!is.na(y)]
  }

  n <- length(y)
  SD <- sd(y)
  mean_bar <- mean(y)

  t <- qt((1 - conf_level) / 2, df = (n - 1), lower.tail = FALSE)

  Err <- t * SD / sqrt(n)

  res <- matrix(c(mean_bar, mean_bar - Err, mean_bar + Err, conf_level),
    nrow = 1,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c("mean", "lower", "upper", "conf_level")
    )
  )

  if (return_df == TRUE) {
    res <- as.data.frame(res)
  }

  res
}
