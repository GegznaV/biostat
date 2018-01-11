# Confidence Intervals for Binomial Proportions


#' Confidence Intervals for Binomial Proportions
#'
#' Compute confidence intervals for binomial proportions following the most
#' popular methods.\cr (Wald, Wilson, Agresti-Coull, Jeffreys, Clopper-Pearson
#' etc.)
#'
#' All arguments are being recycled.
#'
#' The Wald interval is obtained by inverting the acceptance region of the Wald
#' large-sample normal test.
#'
#' The Wilson interval, which is the default, was introduced by Wilson (1927)
#' and is the inversion of the CLT approximation to the family of equal tail
#' tests of p = p0.  The Wilson interval is recommended by Agresti and Coull
#' (1998) as well as by Brown et al (2001).
#'
#' The Agresti-Coull interval was proposed by Agresti and Coull (1998) and is a
#' slight modification of the Wilson interval. The Agresti-Coull intervals are
#' never shorter than the Wilson intervals; cf. Brown et al (2001).
#'
#' The Jeffreys interval is an implementation of the equal-tailed Jeffreys
#' prior interval as given in Brown et al (2001).
#'
#' The modified Wilson interval is a modification of the Wilson interval for x
#' close to 0 or n as proposed by Brown et al (2001).
#'
#' The modified Jeffreys interval is a modification of the Jeffreys interval
#' for \code{x == 0 | x == 1} and \code{x == n-1 | x == n} as proposed by Brown
#' et al (2001).
#'
#' The Clopper-Pearson interval is based on quantiles of corresponding beta
#' distributions. This is sometimes also called exact interval.
#'
#' The arcsine interval is based on the variance stabilizing distribution for
#' the binomial distribution.
#'
#' The logit interval is obtained by inverting the Wald type interval for the
#' log odds.
#'
#' The Witting interval (cf. Beispiel 2.106 in Witting (1985)) uses
#' randomization to obtain uniformly optimal lower and upper confidence bounds
#' (cf. Satz 2.105 in Witting (1985)) for binomial proportions.
#'
#' For more details we refer to Brown et al (2001) as well as Witting (1985).
#'
#' And so, which interval to choose? The Wald interval often has inadequate
#' coverage, particularly for small n and values of p close to 0 or 1.
#' Conversely, the Clopper-Pearson Exact method is very conservative and tends
#' to produce wider intervals than necessary. Brown et al. recommends the
#' Wilson or Jeffreys methods for small n and Agresti-Coull, Wilson, or
#' Jeffreys, for larger n as providing more reliable coverage than the
#' alternatives. Also note that the point estimate for the Agresti-Coull method
#' is slightly larger than for other methods because of the way this interval
#' is calculated.
#'
#' @param x number of successes.
#' @param n number of trials.
#' @param conf.level confidence level, defaults to 0.95.
#' @param method character string specifing which method to use; this can be
#' one out of: \code{"wald"}, \code{"wilson"}, \code{"agresti-coull"},
#' \code{"jeffreys"}, \code{"modified wilson"}, \code{"modified jeffreys"},
#' \code{"clopper-pearson"}, \code{"arcsine"}, \code{"logit"}, \code{"witting"}
#' or \code{"pratt"}. Defaults to \code{"wilson"}.  Abbreviation of method are
#' accepted. See details.
#' @param rand seed for random number generator; see details.
#' @return A vector with 3 elements for estimate, lower confidence intervall
#' and upper for the upper one.
#' @note This function was previously published as \code{binomCI()} in the
#' \pkg{SLmisc} package and has been integrated here with some adaptations
#' concerning the interface, but without any change in the computation logic.
#' @author Matthias Kohl <Matthias.Kohl@@stamats.de>, Rand R. Wilcox (Pratt's
#' method), Andri Signorell <andri@@signorell.net> (interface issues)
#' @seealso \code{\link[stats]{binom.test}}, \code{\link[Hmisc]{binconf}},
#' \code{\link{MultinomCI}}
#' @references Agresti A. and Coull B.A. (1998) Approximate is better than
#' "exact" for interval estimation of binomial proportions.  \emph{American
#' Statistician}, \bold{52}, pp. 119-126.
#'
#' Brown L.D., Cai T.T. and Dasgupta A. (2001) Interval estimation for a
#' binomial proportion \emph{Statistical Science}, \bold{16}(2), pp. 101-133.
#'
#' Witting H. (1985) \emph{Mathematische Statistik I}. Stuttgart: Teubner.
#'
#' Pratt J. W. (1968) A normal approximation for binomial, F, Beta, and other
#' common, related tail probabilities \emph{Journal of the American Statistical
#' Association}, 63, 1457- 1483.
#'
#' Wilcox, R. R. (2005) \emph{Introduction to robust estimation and hypothesis
#' testing}. Elsevier Academic Press
#' @keywords univar
#' @examples
#'
#' BinomCI(x=37, n=43, method=c("wald", "wilson", "agresti-coull", "jeffreys",
#'   "modified wilson", "modified jeffreys", "clopper-pearson", "arcsine", "logit",
#'   "witting", "pratt")
#' )
#'
#'
#' # the confidence interval computed by binom.test
#' #   corresponds to the Clopper-Pearson interval
#' BinomCI(x=42, n=43, method="clopper-pearson")
#' binom.test(x=42, n=43)$conf.int
#'
#'
#' # all arguments are being recycled:
#' BinomCI(x=c(42, 35, 23, 22), n=43, method="wilson")
#' BinomCI(x=c(42, 35, 23, 22), n=c(50, 60, 70, 80), method="jeffreys")
#'
BinomCI <- function(x,
                    n,
                    conf.level = 0.95,
                    method = c(
                        "wilson",
                        "wald",
                        "agresti-coull",
                        "jeffreys",
                        "modified wilson",
                        "modified jeffreys",
                        "clopper-pearson",
                        "arcsine",
                        "logit",
                        "witting",
                        "pratt"
                    ),
                    rand = 123
) {

    if(missing(method)) method <- "wilson"

    iBinomCI <- function(x, n, conf.level = 0.95, method = c("wilson", "wald", "agresti-coull", "jeffreys", "modified wilson",
                                                             "modified jeffreys", "clopper-pearson", "arcsine", "logit", "witting", "pratt"), rand = 123) {

        if(length(x) != 1) stop("'x' has to be of length 1 (number of successes)")
        if(length(n) != 1) stop("'n' has to be of length 1 (number of trials)")
        if(length(conf.level) != 1)  stop("'conf.level' has to be of length 1 (confidence level)")
        if(conf.level < 0.5 | conf.level > 1)  stop("'conf.level' has to be in [0.5, 1]")

        alpha <- 1 - conf.level
        kappa <- qnorm(1-alpha/2)
        p.hat <- x/n
        q.hat <- 1 - p.hat

        switch( match.arg(arg=method, choices=c("wilson", "wald", "agresti-coull", "jeffreys", "modified wilson",
                                                "modified jeffreys", "clopper-pearson", "arcsine", "logit", "witting","pratt"))
                , "wald" = {
                    est <- p.hat
                    term2 <- kappa*sqrt(p.hat*q.hat)/sqrt(n)
                    CI.lower <- max(0, p.hat - term2)
                    CI.upper <- min(1, p.hat + term2)
                }
                , "wilson" = {
                    est <- p.hat
                    term1 <- (x + kappa^2/2)/(n + kappa^2)
                    term2 <- kappa*sqrt(n)/(n + kappa^2)*sqrt(p.hat*q.hat + kappa^2/(4*n))
                    CI.lower <-  max(0, term1 - term2)
                    CI.upper <- min(1, term1 + term2)
                }
                , "agresti-coull" = {
                    x.tilde <- x + kappa^2/2
                    n.tilde <- n + kappa^2
                    p.tilde <- x.tilde/n.tilde
                    q.tilde <- 1 - p.tilde
                    est <- p.tilde
                    term2 <- kappa*sqrt(p.tilde*q.tilde)/sqrt(n.tilde)
                    CI.lower <- max(0, p.tilde - term2)
                    CI.upper <- min(1, p.tilde + term2)
                }
                , "jeffreys" = {
                    est <- p.hat
                    if(x == 0)
                        CI.lower <- 0
                    else
                        CI.lower <- qbeta(alpha/2, x+0.5, n-x+0.5)
                    if(x == n)
                        CI.upper <- 1
                    else
                        CI.upper <- qbeta(1-alpha/2, x+0.5, n-x+0.5)
                }
                , "modified wilson" = {
                    est <- p.hat
                    term1 <- (x + kappa^2/2)/(n + kappa^2)
                    term2 <- kappa*sqrt(n)/(n + kappa^2)*sqrt(p.hat*q.hat + kappa^2/(4*n))
                    ## comment by Andre Gillibert, 19.6.2017:
                    ## old:
                    ## if((n <= 50 & x %in% c(1, 2)) | (n >= 51 & n <= 100 & x %in% c(1:3)))
                    ## new:
                    if((n <= 50 & x %in% c(1, 2)) | (n >= 51 &            x %in% c(1:3)))
                        CI.lower <- 0.5*qchisq(alpha, 2*x)/n
                    else
                        CI.lower <-  max(0, term1 - term2)

                    ## if((n <= 50 & x %in% c(n-1, n-2)) | (n >= 51 & n <= 100 & x %in% c(n-(1:3))))
                    if((n <= 50 & x %in% c(n-1, n-2)) | (n >= 51 &            x %in% c(n-(1:3))))
                        CI.upper <- 1 - 0.5*qchisq(alpha, 2*(n-x))/n
                    else
                        CI.upper <- min(1, term1 + term2)
                }
                , "modified jeffreys" = {
                    est <- p.hat
                    if(x == n)
                        CI.lower <- (alpha/2)^(1/n)
                    else {
                        if(x <= 1)
                            CI.lower <- 0
                        else
                            CI.lower <- qbeta(alpha/2, x+0.5, n-x+0.5)
                    }
                    if(x == 0)
                        CI.upper <- 1 - (alpha/2)^(1/n)
                    else{
                        if(x >= n-1)
                            CI.upper <- 1
                        else
                            CI.upper <- qbeta(1-alpha/2, x+0.5, n-x+0.5)
                    }
                }
                , "clopper-pearson" = {
                    est <- p.hat
                    CI.lower <- qbeta(alpha/2, x, n-x+1)
                    CI.upper <- qbeta(1-alpha/2, x+1, n-x)
                }
                , "arcsine" = {
                    p.tilde <- (x + 0.375)/(n + 0.75)
                    est <- p.tilde
                    CI.lower <- sin(asin(sqrt(p.tilde)) - 0.5*kappa/sqrt(n))^2
                    CI.upper <- sin(asin(sqrt(p.tilde)) + 0.5*kappa/sqrt(n))^2
                }
                , "logit" = {
                    est <- p.hat
                    lambda.hat <- log(x/(n-x))
                    V.hat <- n/(x*(n-x))
                    lambda.lower <- lambda.hat - kappa*sqrt(V.hat)
                    lambda.upper <- lambda.hat + kappa*sqrt(V.hat)
                    CI.lower <- exp(lambda.lower)/(1 + exp(lambda.lower))
                    CI.upper <- exp(lambda.upper)/(1 + exp(lambda.upper))
                }
                , "witting" = {
                    set.seed(rand)
                    x.tilde <- x + runif(1, min = 0, max = 1)
                    pbinom.abscont <- function(q, size, prob){
                        v <- trunc(q)
                        term1 <- pbinom(v-1, size = size, prob = prob)
                        term2 <- (q - v)*dbinom(v, size = size, prob = prob)
                        return(term1 + term2)
                    }
                    qbinom.abscont <- function(p, size, x){
                        fun <- function(prob, size, x, p){
                            pbinom.abscont(x, size, prob) - p
                        }
                        uniroot(fun, interval = c(0, 1), size = size, x = x, p = p)$root
                    }
                    est <- p.hat
                    CI.lower <- qbinom.abscont(1-alpha, size = n, x = x.tilde)
                    CI.upper <- qbinom.abscont(alpha, size = n, x = x.tilde)
                }

                , "pratt" = {

                    est <- p.hat

                    if(x==0) {
                        CI.lower <- 0
                        CI.upper <- 1-alpha^(1/n)
                    } else if(x==1) {
                        CI.lower <- 1-(1-alpha/2)^(1/n)
                        CI.upper <- 1-(alpha/2)^(1/n)
                    } else if(x==(n-1)) {
                        CI.lower <- (alpha/2)^(1/n)
                        CI.upper <- (1-alpha/2)^(1/n)
                    } else if(x==n) {
                        CI.lower <- alpha^(1/n)
                        CI.upper <- 1
                    } else {
                        z <- qnorm(1 - alpha/2)

                        A <- ((x+1) / (n-x))^2
                        B <- 81*(x+1)*(n-x)-9*n-8
                        C <- (0-3)*z*sqrt(9*(x+1)*(n-x)*(9*n+5-z^2)+n+1)
                        D <- 81*(x+1)^2-9*(x+1)*(2+z^2)+1
                        E <- 1+A*((B+C)/D)^3
                        CI.upper <- 1/E

                        A <- (x / (n-x-1))^2
                        B <- 81*x*(n-x-1)-9*n-8
                        C <- 3*z*sqrt(9*x*(n-x-1)*(9*n+5-z^2)+n+1)
                        D <- 81*x^2-9*x*(2+z^2)+1
                        E <- 1+A*((B+C)/D)^3
                        CI.lower <- 1/E
                    }
                }
        )

        ci <- c( est=est, lwr.ci=CI.lower, upr.ci=CI.upper )
        return(ci)

    }

    # handle vectors
    # which parameter has the highest dimension
    lst <- list(x=x, n=n, conf.level=conf.level, method=method, rand=rand)
    maxdim <- max(unlist(lapply(lst, length)))
    # recycle all params to maxdim
    lgp <- lapply( lst, rep, length.out=maxdim )

    res <- sapply(1:maxdim, function(i) iBinomCI(x=lgp$x[i], n=lgp$n[i], conf.level=lgp$conf.level[i], method=lgp$method[i], rand=lgp$rand[i]))
    rownames(res)[1] <- c("est")

    # colnames(res) <- names(x)
    # colnames(res) <- unlist(lapply(lgp, paste, collapse=" "))

    return(t(res))

}
