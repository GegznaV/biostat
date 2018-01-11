#' Wald-Wolfiwitz test
#'
#'
#' Part of code is imported from package \pkg{DescTools}.
#' The 2-sample runs test is known as the Wald-Wolfowitz test.
#' The runs test for randomness is used to test the hypothesis that a series of
#' numbers is random.
#'
#'
#'
#' Runs Test for Randomness
#'
#' Performs a test whether the elements of \code{x} are serially independent -
#' say, whether they occur in a random order - by counting how many runs there
#' are above and below a threshold. If \code{y} is supplied a two sample
#' Wald-Wolfowitz-Test testing the equality of two distributions against
#' general alternatives will be computed.
#'
#' The runs test for randomness is used to test the hypothesis that a series of
#' numbers is random. The 2-sample test is known as the Wald-Wolfowitz test.
#' \cr
#'
#' For a categorical variable, the number of runs correspond to the number of
#' times the category changes, that is, where \eqn{x_{i}}{x_i} belongs to one
#' category and \eqn{x_{i+1}}{x_(i+1)} belongs to the other. The number of runs
#' is the number of sign changes plus one.\cr
#'
#' For a numeric variable x containing more than two values, a run is a set of
#' sequential values that are either all above or below a specified cutpoint,
#' typically the median. This is not necessarily the best choice. If another
#' threshold should be used use a code like: \code{RunsTest(x > mean(x))}.
#'
#' The exact distribution of runs and the p-value based on it are described in
#' the manual of SPSS "Exact tests"
#' \url{http://www.sussex.ac.uk/its/pdfs/SPSS_Exact_Tests_21.pdf}.
#'
#' The normal approximation of the runs test is calculated with the expected
#' number of runs under the null \deqn{\mu_r=\frac{2 n_0 n_1}{n_0 + n_1} + 1}
#' and its variance \deqn{\sigma_r^2 = \frac{2 n_0 n_1 (2 n_0 n_1 - n_0 - n_1)
#' }{(n_0 + n_1)^2 \cdot (n_0 + n_1 - 1)}} as \deqn{\hat{z}=\frac{r - \mu_r +
#' c}{\sigma_r}} where \eqn{n_0, n_1} the number of values below/above the
#' threshold and \eqn{r} the number of runs.
#'
#' Setting the continuity correction \code{correct = TRUE} will yield the
#' normal approximation as SAS (and SPSS if n < 50) does it, see
#' \url{http://support.sas.com/kb/33/092.html}. The c is set to \eqn{c = 0.5}
#' if \eqn{r < \frac{2 n_0 n_1}{n_0 + n_1} + 1} and to \eqn{c = -0.5} if \eqn{r
#' > \frac{2 n_0 n_1}{n_0 + n_1} + 1}.
#'
#' @aliases RunsTest RunsTest.formula RunsTest.default
#' @param x a dichotomous vector of data values or a (non-empty) numeric vector
#' of data values.
#' @param y an optional (non-empty) numeric vector of data values.
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} gives
#' the data values and rhs the corresponding groups.
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in the formula
#' \code{formula}.  By default the variables are taken from
#' \code{environment(formula)}.
#' @param subset an optional vector specifying a subset of observations to be
#' used.
#' @param na.action a function which indicates what should happen when the data
#' contain NAs. Defaults to \code{getOption("na.action")}.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"two.sided"} (default), \code{"less"} or
#' \code{"greater"}.
#' @param exact a logical indicating whether an exact p-value should be
#' computed. By default exact values will be calculated for small vectors with
#' a total length <= 30 and the normal approximation for longer ones.
#' @param correct a logical indicating whether to apply continuity correction
#' when computing the test statistic. Default is \code{TRUE}. Ignored if
#' \code{exact} is set to \code{TRUE}. See details.
#' @param na.rm defines if \code{NA}s should be omitted. Default is
#' \code{FALSE}.
#' @param \dots further arguments to be passed to or from methods.
#' @return A list with the following components.  \item{statistic}{z, the value
#' of the standardized runs statistic, if not exact p-values are computed.}
#' \item{parameter}{the number of runs, the total number of zeros (m) and ones
#' (n)} \item{p.value}{the p-value for the test.} \item{data.name}{a character
#' string giving the names of the data.} \item{alternative}{a character string
#' describing the alternative hypothesis.}
#' @author Andri Signorell <andri@@signorell.net>, exact p-values by Detlew
#' Labes <detlewlabes@@gmx.de>
#' @seealso Run Length Encoding \code{\link{rle}}
#' @references Wackerly, D., Mendenhall, W. Scheaffer, R. L. (1986):
#' \emph{Mathematical Statistics with Applications}, 3rd Ed., Duxbury Press,
#' CA.
#'
#' Wald, A. and Wolfowitz, J. (1940): On a test whether two samples are from
#' the same population, \emph{Ann. Math Statist}. 11, 147-162.
#' @keywords htest
#'
#' @export
#'
#' @examples
#'
#' # x will be coerced to a dichotomous variable
#' x <- c("S","S", "T", "S", "T","T","T", "S", "T")
#' RunsTest(x)
#'
#'
#' x <- c(13, 3, 14, 14, 1, 14, 3, 8, 14, 17, 9, 14, 13, 2, 16, 1, 3, 12, 13, 14)
#' RunsTest(x)
#' # this will be treated as
#' RunsTest(x > median(x))
#'
#' plot( (x < median(x)) - 0.5, type="s", ylim=c(-1,1) )
#' abline(h=0)
#'
#' set.seed(123)
#' x <- sample(0:1, size=100, replace=TRUE)
#' RunsTest(x)
#' # As you would expect of values from a random number generator, the test fails to reject
#' # the null hypothesis that the data are random.
#'
#'
#' # SPSS example
#' x <- c(31,23,36,43,51,44,12,26,43,75,2,3,15,18,78,24,13,27,86,61,13,7,6,8)
#' RunsTest(x)
#' RunsTest(x, exact=TRUE)
#'
#' # SPSS example small dataset
#' x <- c(1, 1, 1, 1, 0, 0, 0, 0, 1, 1)
#' RunsTest(x)
#' RunsTest(x, exact=FALSE)
#'
#' # if y is not NULL, the Wald-Wolfowitz-Test will be performed
#' A <- c(35,44,39,50,48,29,60,75,49,66)
#' B <- c(17,23,13,24,33,21,18,16,32)
#'
#' RunsTest(A, B, exact=TRUE)
#' RunsTest(A, B, exact=FALSE)
#'
# ============================================================================
wald_wolfowitz_test <- function(x, ...) {
    UseMethod("wald_wolfowitz_test")
}
# ============================================================================
#' @rdname wald_wolfowitz_test
#' @export
wald_wolfowitz_test.formula <- function(x, data = NULL,
                                        alternative = "two.sided",
                                        exact = NULL, correct = TRUE,
                                        na.rm = FALSE, ...)
{
    if (is.null(data)) {
        data <- rlang::f_env(x)
    }
    data <- model.frame(x, data)

    # dname <- deparse(substitute(x))

    wald_wolfowitz_test(data[[1]], data[[2]],
                        alternative = alternative,
                        exact = exact,
                        correct = correct,
                        na.rm = na.rm,
                        ...,
                        # dname = dname
    )
}
# ============================================================================
#' @rdname wald_wolfowitz_test
#' @export
wald_wolfowitz_test.default <- function(x, group,
                                alternative = "two.sided",
                                exact = NULL, correct = TRUE, na.rm = FALSE, ...)
{
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (na.rm) {
        ind_ok <- complete.cases(x, group)
        x <- x[ind_ok]
        group <- group[ind_ok]
    }

    alternative <- match.arg(alternative)

    # if (is.null(as.list(...)$dname))
        dname <- paste(deparse(substitute(x)), "and", deparse(substitute(group)))


    if (!nlevels(group) %in% c(1, 2))
        stop("Can only process dichotomous variables")

    levs <- levels(group)
    m <- sum(group == levs[1])
    n <- sum(group == levs[2])

    if (is.null(exact)) exact <- ((m + n) <= 30)

    # Calculate runs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # "two.sided"
    alternative <- "true number of runs is not equal to the expected number"

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    split_list <- split(group, x)

    # Rezult is list:
    rez_df <- expand.grid(lapply(split_list, get_permutations))

    # Convert to matrix:
    mat <- apply(rez_df, 1, unlist)

    # Calculte number of runs for each column of matrix.
    # Select only minimum and maximum possible number of runs.
    runs <- unique(min_max(apply(mat, 2, calculate_runs)))


    res <- sapply(runs, two_sided_pval_for_runs)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    method <- paste0("Wald-Wolfowitz Runs Test (",
                     ifelse(exact, "exact", "asymptotic"),
                     ifelse(!exact && correct, " with continuity correnction", ""),
                     ")")

    if (length(runs) == 1) {
        structure(list(statistic = unlist(res["statistic", 1]),
                       p.value =   unlist(res["p.value", 1]),
                       method = method,
                       alternative = alternative,
                       data.name = dname,
                       estimate  = NULL,
                       parameter = c(runs = runs[1], n1 = m, n2 = n)),
                  class = c("wwtest", "htest"))

    } else {
        list(runs_min =
        structure(list(statistic = unlist(res["statistic", 1]),
                       p.value = unlist(res["p.value", 1]),
                       method = paste(method,
                                      " \nNOTE: ties exist, p values for MINIMUM",
                                      "possible number of runs calculated."),
                       alternative = alternative,
                       data.name = dname,
                       estimate  = NULL,
                       parameter = c(runs = runs[1], m = m, n = n)),
                  class = c("wwtest", "htest")),
        runs_max =
            structure(list(statistic = unlist(res["statistic", 2]),
                           p.value = unlist(res["p.value", 2]),
                           method = paste(method,
                                          " \nNOTE: ties exist, p values for MAXIMUM",
                                          "possible number of runs calculated."),
                           alternative = alternative,
                           data.name = dname,
                           estimate  = NULL,
                           parameter = c(runs = runs[2], m = m, n = n)),
                      class = c("wwtest", "htest"))
        )
    }
}
# ============================================================================

get_permutations <- function(x) {
    # Get all unique permutations of input  vector (`x`) values.
    n <- length(x)

    # All possible combinations of elements
    ve <- as.matrix(expand.grid(lapply(1:n, function(i){1:n})))

    # Combinations, in which the same object does not repeat
    ind_mat_ok <- apply(ve, 1, function(i)  all(1:n %in% i))

    # Permutations of indices
    dfi <- as.data.frame(t(ve[ind_mat_ok, ]))

    # Permutations of original inputs (with duplication)
    x_pernutations <- lapply(dfi, function(i) x[i])

    # Remove duplications
    x_pernutations[!duplicated(x_pernutations)]
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pruns <- function(r, n1, n2, alternative = c("two.sided", "less", "greater")) {
    .druns_nom <- function(r, n1, n2) {
        pp <- vector(mode = "numeric", length = length(r))
        for (i in seq_along(r)) {
            if (2 * r[i] %/% 2 == r[i]) {
                k <- r[i]/2
                pp[i] <- 2 * choose(n1 - 1, k - 1) * choose(n2 - 1, k - 1)

            } else {
                k <- (r[i] - 1)/2
                pp[i] <- choose(n1 - 1, k - 1) * choose(n2 - 1, k) +
                         choose(n1 - 1, k)     * choose(n2 - 1, k - 1)
            }
        }
        pp
    }

    alternative <- match.arg(alternative)
    n <- n1 + n2
    if (r <= 1) stop("Number of runs must be > 1")
    if (r > n)  stop("Number of runs must be < (n1 + n2")
    if (n1 < 1 | n2 < 1) return(0)

    E <- 1 + 2 * n1 * n2/n
    denom <- choose(n, n1)
    rmax <- ifelse(n1 == n2, 2 * n1, 2 * min(n1, n2) + 1)
    rv <- 2:rmax
    pp <- .druns_nom(rv, n1, n2)
    pL <- sum(pp[rv <= r])/denom
    pU <- 1 - sum(pp[rv <= (r - 1)])/denom
    p2 <- sum(pp[abs(rv - E) >= abs(r - E)])/denom
    p2min <- 2 * min(c(pL, pU, 0.5))
    return(switch(alternative, less = pL, greater = pU, two.sided = p2))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calculate_runs <- function(g) {
    # Calculate number of runs in the vector g.
    # g - two groups factor
    g <- factor(g)
    g <- as.numeric(g) - 1
    runs <- sum(diff(g) != 0) + 1
    runs
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
two_sided_pval_for_runs <- function(runs) {
# two_sided_pval_for_runs <- function(runs, m, n, exact, correct) {
    #  Calculate p value and related statistics for two-tailed
    #  Wald-Wolfowitz test.
    #
    #  runs - (integer) number of runs.
    #  m, n (integer) sample sizes for both groups
    #  exact (logical) Should eact test be used
    #  correct (logical) Sould continuity correction be used for
    #           asymptotic (non-exact test)

    if (exact == TRUE) {
        # Exact text ------------------------------------------------------
        p.value <- pruns(runs, m, n, alternative = "two.sided")
        statistic <- NULL

    } else {
        # Asymptotic test -------------------------------------------------
        E <- 1 + 2 * n * m/(n + m)
        s2 <- (2 * n * m * (2 * n * m - n - m))/((n + m)^2 * (n + m - 1))

        if (correct) {
            switch(as.character(cut(runs - E,
                                    breaks = c(-Inf, -0.5, 0.5, Inf),
                                    labels = c("a", "b", "c"))),
                   a = statistic <- (runs -  E + 0.5)/sqrt(s2),
                   b = statistic <- 0,
                   c = statistic <- (runs - E - 0.5)/sqrt(s2))

        } else {
            statistic <- (runs - E)/sqrt(s2)
        }

        p.value <- 2 * min(pnorm(statistic), 1 - pnorm(statistic))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Return
    list(p.value = p.value,
         statistic = statistic)
}
# ============================================================================