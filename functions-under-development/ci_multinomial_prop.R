# https://blogs.sas.com/content/iml/2017/02/15/confidence-intervals-multinomial-proportions.html
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Confidence Intervals for Multinomial Proportions
#'
#' Confidence intervals for multinomial proportions are often approximated by
#' single binomial confidence intervals, which might in practice often yield
#' satisfying results, but is properly speaking not correct. This function
#' calculates simultaneous confidence intervals for multinomial proportions
#' either according to the methods of Sison and Glaz, Goodman, Wald, Wald with
#' continuity correction or Wilson.
#'
#' Given a vector of observations with the number of samples falling in each
#' class of a multinomial distribution, builds the simultaneous confidence
#' intervals for the multinomial probabilities according to the method proposed
#' by the mentioned authors. The R code for Sison and Glaz (1995) has been
#' translated from thes SAS code written by May and Johnson (2000).\cr Some
#' approaches for the confidence intervals can potentially yield negative
#' results or values beyond 1. These would be reset such as not to exceed the
#' range of [0, 1].
#'
#' @param x A vector of positive integers representing the number of
#' occurrences of each class. The total number of samples equals the sum of
#' such elements.
#' @param conf.level confidence level, defaults to 0.95.
#' @param method character string specifing which method to use; can be one out
#' of \code{"sisonglaz"}, \code{"cplus1"}, \code{"goodman"}, \code{"wald"},
#' \code{"waldcc"}, \code{"wilson"}.  Method can be abbreviated. See details.
#' Defaults to \code{"sisonglaz"}.
#' @return A matrix with 3 columns: \item{est}{estimate} \item{lwr.ci}{lower
#' bound of the confidence interval} \item{upr.ci}{upper bound of the
#' confidence interval}
#'
#' The number of rows correspond to the dimension of x.
#' @author Pablo J. Villacorta Iglesias <pjvi@@decsai.ugr.es>\cr Department of
#' Computer Science and Artificial Intelligence, University of Granada (Spain)
#' (Sison-Glaz)
#'
#' Andri Signorell <andri@@signorell.net> (Goodman, Wald, Wilson)
#' @references Sison, C.P and Glaz, J. (1995) Simultaneous confidence intervals
#' and sample size determination for multinomial proportions. \emph{Journal of
#' the American Statistical Association}, 90:366-369.
#'
#' Glaz, J., Sison, C.P. (1999) Simultaneous confidence intervals for
#' multinomial proportions. \emph{Journal of Statistical Planning and
#' Inference} 82:251-262.
#'
#' May, W.L., Johnson, W.D.(2000) Constructing two-sided simultaneous
#' confidence intervals for multinomial proportions for small counts in a large
#' number of cells. \emph{Journal of Statistical Software} 5(6) . Paper and
#' code available at \url{http://www.jstatsoft.org/v05/i06}.
#'
#' Goodman, L. A. (1965) On Simultaneous Confidence Intervals for Multinomial
#' Proportions \emph{Technometrics}, 7, 247-254.
#'
#' Wald, A. Tests of statistical hypotheses concerning several parameters when
#' the number of observations is large, \emph{Trans. Am. Math. Soc.} 54 (1943)
#' 426-482.
#'
#' Wilson, E. B. Probable inference, the law of succession and statistical
#' inference, \emph{J.Am. Stat. Assoc.} 22 (1927) 209-212.
#' @keywords univar
#' @examples
#'
#' # Multinomial distribution with 3 classes, from which a sample of 79 elements
#' # were drawn: 23 of them belong to the first class, 12 to the
#' # second class and 44 to the third class. Punctual estimations
#' # of the probabilities from this sample would be 23/79, 12/79
#' # and 44/79 but we want to build 95% simultaneous confidence intervals
#' # for the true probabilities
#'
#' MultinomCI(c(23, 12, 44), conf.level=0.95)
#'
#'
#' x <- c(35, 74, 22, 69)
#'
#' MultinomCI(x, method="goodman")
#' MultinomCI(x, method="sisonglaz")
#' MultinomCI(x, method="cplus1")
#' MultinomCI(x, method="wald")
#' MultinomCI(x, method="waldcc")
#' MultinomCI(x, method="wilson")
#'
#' # compare to
#' BinomCI(x, n=sum(x))
#'
MultinomCI <- function(x, conf.level = 0.95,
                       method = c("sisonglaz", "cplus1", "goodman", "wald", "waldcc", "wilson")) {

    # Code mainly by:
    # Pablo J. Villacorta Iglesias <pjvi@decsai.ugr.es>\n
    # Department of Computer Science and Artificial Intelligence, University of Granada (Spain)

    .moments <- function(c, lambda){

        a <- lambda + c
        b <- lambda - c
        if(b < 0) b <- 0
        if(b > 0) den <- ppois(a, lambda) - ppois(b-1, lambda)
        if(b == 0) den <- ppois(a,lambda)

        mu <- mat.or.vec(4,1)
        mom <- mat.or.vec(5,1)
        for(r in 1:4){
            poisA <- 0
            poisB <- 0

            if((a-r) >=0){ poisA <- ppois(a,lambda)-ppois(a-r,lambda) }
            if((a-r) < 0){ poisA <- ppois(a,lambda) }
            if((b-r-1) >=0){ poisB <- ppois(b-1,lambda)-ppois(b-r-1,lambda) }
            if((b-r-1) < 0 && (b-1)>=0){ poisB <- ppois(b-1,lambda) }
            if((b-r-1) < 0 && (b-1) < 0){ poisB <- 0 }

            mu[r] <- (lambda^r)*(1-(poisA-poisB)/den)
        }
        mom[1] <- mu[1]
        mom[2] <- mu[2] + mu[1] - mu[1]^2
        mom[3] <- mu[3] + mu[2]*(3-3*mu[1]) + (mu[1]-3*mu[1]^2+2*mu[1]^3)
        mom[4] <- mu[4] + mu[3]*(6-4*mu[1]) + mu[2]*(7-12*mu[1]+6*mu[1]^2)+mu[1]-4*mu[1]^2+6*mu[1]^3-3*mu[1]^4
        mom[5] <- den

        return(mom)

    }

    .truncpoi <- function(c, x, n, k){

        m <- matrix(0, k, 5)

        for(i in 1L:k){
            lambda <- x[i]
            mom <- .moments(c, lambda)
            for(j in 1L:5L){ m[i,j] <- mom[j] }
        }
        for(i in 1L:k){ m[i, 4] <- m[i, 4] - 3 * m[i, 2]^2 }

        s <- colSums(m)
        s1 <- s[1]
        s2 <- s[2]
        s3 <- s[3]
        s4 <- s[4]

        probn <- 1/(ppois(n,n)-ppois(n-1,n))
        z <- (n-s1)/sqrt(s2)
        g1 <- s3/(s2^(3/2))
        g2 <- s4/(s2^2)
        poly <- 1 + g1*(z^3-3*z)/6 + g2*(z^4-6*z^2+3)/24
        + g1^2*(z^6-15*z^4 + 45*z^2-15)/72
        f <- poly*exp(-z^2/2)/(sqrt(2)*gamma(0.5))

        probx <- 1
        for(i in 1L:k){ probx <- probx * m[i,5]  }

        return(probn * probx * f / sqrt(s2))
    }


    n <- sum(x, na.rm=TRUE)
    k <- length(x)
    p <- x/n

    if (missing(method)) method <- "sisonglaz"

    method <- match.arg(arg = method, choices = c("sisonglaz", "cplus1", "goodman", "wald", "waldcc", "wilson"))
    if(method == "goodman") {

        q.chi <- qchisq(conf.level, k - 1)
        lci <- (q.chi + 2*x - sqrt(q.chi*(q.chi + 4*x*(n-x)/n))) / (2*(n+q.chi))
        uci <- (q.chi + 2*x + sqrt(q.chi*(q.chi + 4*x*(n-x)/n))) / (2*(n+q.chi))

        res <- cbind(est=p, lwr.ci=pmax(0, lci), upr.ci=pmin(1, uci))

    } else if(method == "wald") {

        q.chi <- qchisq(conf.level, 1)
        lci <- p - sqrt(q.chi * p * (1 - p)/n)
        uci <- p + sqrt(q.chi * p * (1 - p)/n)

        res <- cbind(est=p, lwr.ci=pmax(0, lci), upr.ci=pmin(1, uci))

    } else if(method == "waldcc") {

        q.chi <- qchisq(conf.level, 1)
        lci <- p - sqrt(q.chi * p * (1 - p)/n) - 1/(2*n)
        uci <- p + sqrt(q.chi * p * (1 - p)/n) + 1/(2*n)

        res <- cbind(est=p, lwr.ci=pmax(0, lci), upr.ci=pmin(1, uci))

    } else if(method == "wilson") {

        q.chi <- qchisq(conf.level, 1)
        lci <- (q.chi + 2*x - sqrt(q.chi^2 + 4*x*q.chi * (1 - p))) / (2*(q.chi + n))
        uci <- (q.chi + 2*x + sqrt(q.chi^2 + 4*x*q.chi * (1 - p))) / (2*(q.chi + n))

        res <- cbind(est=p, lwr.ci=pmax(0, lci), upr.ci=pmin(1, uci))

    } else {  # sisonglaz, cplus1

        const <- 0
        pold <- 0

        for(cc in 1:n){
            poi <- .truncpoi(cc, x, n, k)
            if(poi > conf.level && pold < conf.level) {
                const <- cc
                break
            }
            pold <- poi
        }

        delta <- (conf.level - pold)/(poi - pold)
        const <- const - 1

        if(method == "sisonglaz") {
            res <- cbind(est = p, lwr.ci = pmax(0, p - const/n), upr.ci = pmin(1, p + const/n + 2*delta/n))

        } else if(method == "cplus1") {
            res <- cbind(est = p, lwr.ci = pmax(0, p - const/n - 1/n), upr.ci = pmin(1,p + const/n + 1/n))
        }
    }

    return(res)
}
