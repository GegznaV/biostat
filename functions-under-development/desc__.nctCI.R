#' Helper: function for CI calculation
#'
#' Function for finding the upper and lower confidence limits for
#' the noncentrality from noncentral t distributions.
#' Especially helpful when forming confidence intervals around the
#' standardized effect size, Cohen's d.
#'
#'
#' @section Origin of the code:
#'
#' The main part of code and documentation of this function is imported from: \cr
#'     Andri Signorell et al. (2017).
#'     \pkg{DescTools}: Tools for descriptive statistics.
#'     R package version \emph{0.99.22} (2017-09-14).
#'
#' The modifications were made by Vilmantas Gegzna.
#' The logics of statistical computations was not changed.
#'
#' Information from the package \pkg{DescTools}:
#'
#' The following code was adapted from code written by Michael Smithson:
#' Australian National University, sometime around the early part of October, 2001
#' Adapted by Joe Rausch & Ken Kelley: University of Notre Dame, in January 2002.
#' Available at: JRausch@@nd.edu & KKelley@@nd.edu
#'
#'

#'
#'
#'
#' @param tval.1 the observed t value
#' @param df     is the degrees of freedom (group size need not be equal)
#' @param conf   is simply 1 - alpha (confidence level)
#'
#' @return Vector of length 4 with lower limit, probability of lower limit,
#'         upper limit, and probability of upper limit.
#'
#' @keywords internal
#'
.nctCI <- function(tval.1, df, conf) {

    tval <- abs(tval.1)


    # This part Finds the Lower bound for the confidence interval ------------
    ulim <- 1 - (1 - conf) / 2

    # This first part finds a lower value from which to start.
    lc <- c(-tval, tval / 2, tval)
    while (pt(tval, df, lc[1]) < ulim)    {
        lc <- c(lc[1] - tval, lc[1], lc[3])
    }

    # This next part finds the lower limit for the ncp.
    diff <- 1
    while (diff > .00000001)    {
        if (pt(tval, df, lc[2]) < ulim) {
            lc <- c(lc[1], (lc[1] + lc[2]) / 2, lc[2])
        } else {
            lc <- c(lc[2], (lc[2] + lc[3]) / 2, lc[3])
        }
        diff <- abs(pt(tval, df, lc[2]) - ulim)
        ucdf <- pt(tval, df, lc[2])
    }
    res.1 <- ifelse(tval.1 >= 0,lc[2],-lc[2])

    # This part Finds the Upper bound for the confidence interval ------------
    llim <- (1-conf)/2

    # This first part finds an upper value from which to start.
    uc <- c(tval, 1.5 * tval, 2 * tval)
    while (pt(tval, df, uc[3]) > llim)   {
        uc <- c(uc[1], uc[3], uc[3] + tval)
    }

    # This next part finds the upper limit for the ncp.
    diff <- 1
    while (diff > .00000001)         {
        if (pt(tval, df, uc[2]) < llim) {
            uc <-
                c(uc[1], (uc[1] + uc[2]) / 2, uc[2])
        } else {
            uc <- c(uc[2], (uc[2] + uc[3]) / 2, uc[3])
        }
        diff <- abs(pt(tval, df, uc[2]) - llim)
        lcdf <- pt(tval, df, uc[2])
    }
    res <- ifelse(tval.1 >= 0, uc[2], -uc[2])


    # This part Compiles the results into a matrix ---------------------------
    return(c(
        lwr.ci = min(res, res.1),
        lprob = ucdf,
        upr.ci = max(res, res.1),
        uprob = lcdf
    ))

    #        Result[1,1] <- min(res,res.1)
    #         Result[1,2] <- ucdf
    #         Result[1,3] <- max(res,res.1)
    #         Result[1,4] <- lcdf
    # dimnames(Result) <- list("Values", c("Lower.Limit", "Prob.Low.Limit", "Upper.Limit", "Prob.Up.Limit"))
    #         Result
}