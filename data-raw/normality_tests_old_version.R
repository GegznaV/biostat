# =============================================================================
#' Normality tests with formula interface
#'
#' Performs the Shapiro-Wilk test of normality.
#'
#' @inheritParams stats::shapiro.test
#' @inheritParams mosaic::maggregate
#'
#' @return  ...
#' @export
#' @importFrom stats shapiro.test
#' @examples
#' library(biostat)
#' data(CO2)
#' shapiro.test(uptake ~ Type + Treatment, data = CO2)
#'
shapiro.test <- function(x, ..., data = NULL, groups = NULL) {
    test_(x, ..., data = data, groups = groups, FUN = stats::shapiro.test)
}

# =============================================================================

#' Lilliefors (Kolmogorov-Smirnov) test for normality with formula interface
#'
#'
#'Performs the Lilliefors (Kolmogorov-Smirnov) test for the
#' composite hypothesis of normality, see e.g. Thode
#'  (2002, Sec. 5.1.1).
#'
#' @inheritParams nortest::lillie.test
#' @inheritParams mosaic::maggregate
#'
#' @return  ...
#' @export
#' @importFrom nortest lillie.test
#' @examples
#' library(biostat)
#' data(CO2)
#' lillie.test(uptake ~ Type + Treatment, data = CO2)
#'
lillie.test <- function(x, ..., data = NULL, groups = NULL) {
    test_(x, ..., data = data, groups = groups, FUN = nortest::lillie.test)
}

# =============================================================================
#' Anderson-Darling test for normality with formula interface
#'
#'Performs the Anderson-Darling test for the composite
#' hypothesis of normality, see e.g. Thode (2002, Sec. 5.1.4).
#' More information in: \code{\link[nortest]{ad.test}}.
#'
#' @inheritParams nortest::ad.test
#' @inheritParams mosaic::maggregate
#'
#' @return  ...
#' @export
#' @importFrom nortest ad.test
#' @examples
#' library(biostat)
#' data(CO2)
#' ad.test(uptake ~ Type + Treatment, data = CO2)
#'
ad.test <- function(x, ..., data = NULL, groups = NULL) {
    test_(x, ..., data = data, groups = groups, FUN = nortest::ad.test)
}

# =============================================================================
#' Cramer-von Mises test for normality with formula interface
#'
#'Performs the Cramer-von Mises test for the composite
#'hypothesis of normality, see e.g. Thode (2002, Sec. 5.1.3).
#' More information in: \code{\link[nortest]{cvm.test}}.
#'
#' @inheritParams nortest::cvm.test
#' @inheritParams mosaic::maggregate
#'
#' @return  ...
#' @export
#' @importFrom nortest cvm.test
#' @examples
#' library(biostat)
#' data(CO2)
#' cvm.test(uptake ~ Type + Treatment, data = CO2)
#'
cvm.test <- function(x, ..., data = NULL, groups = NULL) {
    test_(x, ..., data = data, groups = groups, FUN = nortest::cvm.test)
}

# =============================================================================
#' Shapiro-Francia test for normality with formula interface
#'
#' Performs the Shapiro-Francia test for the composite
#' hypothesis of normality, see e.g. Thode (2002, Sec. 2.3.2).
#' More information in: \code{\link[nortest]{sf.test}}.
#'
#' @inheritParams nortest::sf.test
#' @inheritParams mosaic::maggregate
#'
#' @return  ...
#' @export
#' @importFrom nortest sf.test
#' @examples
#' library(biostat)
#' data(CO2)
#' sf.test(uptake ~ Type + Treatment, data = CO2)
#'
sf.test <- function(x, ..., data = NULL, groups = NULL) {
    test_(x, ..., data = data, groups = groups, FUN = nortest::sf.test)
}
# =============================================================================
#' Pearson chi-square test for normality with formula interface
#'
#' Performs the Pearson chi-square test for the composite
#' hypothesis of normality, see e.g. Thode (2002, Sec. 5.2).
#' More information in: \code{\link[nortest]{pearson.test}}.
#'
#' @inheritParams nortest::pearson.test
#' @inheritParams mosaic::maggregate
#'
#' @return  ...
#'
#' @export
#' @importFrom nortest pearson.test
#' @examples
#' library(biostat)
#' data(CO2)
#' pearson.test(uptake ~ Type + Treatment, data = CO2)
#'
pearson.test <- function(x,
                         n.classes = ceiling(2 * (n^(2/5))),
                         adjust = TRUE,
                         data = NULL,
                         groups = NULL) {
    test_(x,
          n.classes = n.classes,
          adjust = adjust,
          data = data,
          groups = groups,
          FUN = nortest::pearson.test)
}
# =============================================================================

