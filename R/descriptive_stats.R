#' **[!!]** Descriptive statistics
#'
#' @param y Variable to summarize
#' @param ... futher arguments to methods
#' @inheritParams stats::quantile
#'
#' @return Summary statistic(s).
#' @export
#' @keywords utilities
#'
#' @aliases summary_funs q1 q3 n_missing n_ok
#'
#' @name summary_funs
q1 <- function(y, na.rm = TRUE,
               type = 7, names = FALSE, ...) {
  quantile(y, probs = 0.25, names = names, type = type, na.rm = na.rm, ...)
}

#' @rdname summary_funs
#' @export
q3 <- function(y, na.rm = TRUE,
               type = 7, names = FALSE, ...) {
  quantile(y, probs = 0.75, names = names, type = type, na.rm = na.rm, ...)
}

#' @rdname summary_funs
#' @export
n_missing <- function(y) {
  sum(is.na(y))
}

#' @rdname summary_funs
#' @export
n_ok <- function(y) {
  sum(!is.na(y))
}

#' @rdname summary_funs
#' @export
n_nonmissing <- function(y) {
  sum(!is.na(y))
}

#' @rdname summary_funs
#' @export
# iqv - index of qualitative variation.
# Ranges from 0 (no variation) to 1 (maximum possible variation).
#
# iqv(chickwts$feed)
# with(chickwts, iqv(feed))
#
# iqv(as.table(c(900,   0,   0)))
# iqv(as.table(c(600, 200, 100)))
# iqv(as.table(c(300, 300, 300)))
#
iqv <- function(y, ...) {
  UseMethod("iqv")
}

#' @rdname summary_funs
#' @export
iqv.default <- function(y, ...) {
  # Assert inputs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  checkmate::assert_vector(y, any.missing = FALSE)

  # Calculate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f <- table(y, useNA = "no")
  iqv.table(f)
}

#' @rdname summary_funs
#' @export
iqv.table <- function(y, ...) {
  # Assert inputs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f <- y
  checkmate::assert_integerish(as.vector(f))

  if (length(dim(f)) != 1) {
    stop("`y` must be frequency table of exactly one variable.")
  }

  # Calculate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # IQV formula from:
  # Cekanavicius ir Murauskas. Statistika ir jos taikymiai I.
  # TEV 2006 Vilnius, p. 42.
  n <- sum(f)
  k <- length(f)

  # Formula of IQV
  (k * (n^2 - sum(f^2))) / (n^2 * (k - 1))
}
