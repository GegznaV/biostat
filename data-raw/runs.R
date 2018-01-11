#' Distribution of the Wald Wolfowitz Runs Statistic
#'
#' Probability function, distribution function, quantile function and random
#' generation for the distribution of the Runs statistic obtained from samples
#' with \eqn{n_1}{n1} and \eqn{n_2}{n2} elements of each type.
#'
#' The Runs distribution has probability function \deqn{ }{P(R=r) = 2
#' choose(n1-1,r/2-1)choose(n2-1,r/2-1)/choose(n1+n2,n1), if r is even
#'
#' P(R=r) = }\deqn{P(R=r)= }{P(R=r) = 2
#' choose(n1-1,r/2-1)choose(n2-1,r/2-1)/choose(n1+n2,n1), if r is even
#'
#' P(R=r) = }\deqn{\left\{ }{P(R=r) = 2
#' choose(n1-1,r/2-1)choose(n2-1,r/2-1)/choose(n1+n2,n1), if r is even
#'
#' P(R=r) = }\deqn{\begin{array}{cc} }{P(R=r) = 2
#' choose(n1-1,r/2-1)choose(n2-1,r/2-1)/choose(n1+n2,n1), if r is even
#'
#' P(R=r) = }\deqn{\frac{2{n_1-1 \choose r/2-1}{n_2-1 \choose r/2-1}}{{n_1+n_2
#' \choose n_1}}, & \mbox{if } r \mbox{ is even}\\ }{P(R=r) = 2
#' choose(n1-1,r/2-1)choose(n2-1,r/2-1)/choose(n1+n2,n1), if r is even
#'
#' P(R=r) = }\deqn{\frac{{n_1-1 \choose (r-1)/2}{n_2-1 \choose
#' (r-3)/2}\,+\,{n_1-1 \choose (r-3)/2}{n_2-1 \choose (r-1)/2}}{{n_1+n_2
#' \choose n_1}}, & \mbox{if } r \mbox{ is odd}\\ }{P(R=r) = 2
#' choose(n1-1,r/2-1)choose(n2-1,r/2-1)/choose(n1+n2,n1), if r is even
#'
#' P(R=r) = }\deqn{\end{array} }{P(R=r) = 2
#' choose(n1-1,r/2-1)choose(n2-1,r/2-1)/choose(n1+n2,n1), if r is even
#'
#' P(R=r) = }\deqn{\right. }{P(R=r) = 2
#' choose(n1-1,r/2-1)choose(n2-1,r/2-1)/choose(n1+n2,n1), if r is even
#'
#' P(R=r) = }\deqn{%\qquad r=2,3,\ldots, n_1+n_2. }{P(R=r) = 2
#' choose(n1-1,r/2-1)choose(n2-1,r/2-1)/choose(n1+n2,n1), if r is even
#'
#' P(R=r) = } for \eqn{r=2,3,\ldots, 2\min(n_1+n_2)+c}{r = 2, 3, \ldots, 2
#' min(n1+n2)+c} with \eqn{c=0} if \eqn{n_1=n_2}{n1 = n2} or \eqn{c=1} if
#' \eqn{n_1 \neq n_2}{n_1 =! n_2}.
#'
#' If an element of \code{x} is not integer, the result of \code{druns} is
#' zero.
#'
#' The quantile is defined as the smallest value \eqn{x} such that \eqn{F(x)
#' \ge p}, where \eqn{F} is the distribution function.
#'
#' @aliases druns pruns qruns rruns
#'
#' @param x,q a numeric vector of quantiles.
#' @param p a numeric vector of probabilities.
#' @param n number of observations to return.
#' @param n1,n2 the number of elements of first and second type, respectively.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are P[X
#' \eqn{\le} x] otherwise, P[X > x].
#' @return \code{druns} gives the probability function, \code{pruns} gives the
#' distribution function and \code{qruns} gives the quantile function. %, and
#' rruns generates random deviates.
#'
#' %The length of the result is determined by nn for rwilcox, and is the
#' maximum of the lengths of the numerical parameters for the other functions.
#'
#' %The numerical parameters other than nn are recycled to the length of the
#' result. Only the first elements of the logical parameters are used.
#'
#' @references Swed, F.S. and Eisenhart, C. (1943).  Tables for Testing
#' Randomness of Grouping in a Sequence of Alternatives, \emph{Ann. Math
#' Statist.} \bold{14}(1), 66-87.
#'
#' @keywords distribution
#'
#' @author Functions and descriptions are imported from package \pkg{randtests}.
#'
#' @export
#'
#' @examples
#' ##
#' ## Example: Distribution Function
#' ## Creates Table I in Swed and Eisenhart (1943), p. 70,
#' ## with n1 = 2 and n1 <= n2 <= 20
#' ##
#' m <- NULL
#' for (i in 2:20){
#'   m <- rbind(m, pruns(2:5,2,i))
#' }
#' rownames(m)=2:20
#' colnames(m)=2:5
#' #
#' #              2         3         4 5
#' # 2  0.333333333 0.6666667 1.0000000 1
#' # 3  0.200000000 0.5000000 0.9000000 1
#' # 4  0.133333333 0.4000000 0.8000000 1
#' # 5  0.095238095 0.3333333 0.7142857 1
#' # 6  0.071428571 0.2857143 0.6428571 1
#' # 7  0.055555556 0.2500000 0.5833333 1
#' # 8  0.044444444 0.2222222 0.5333333 1
#' # 9  0.036363636 0.2000000 0.4909091 1
#' # 10 0.030303030 0.1818182 0.4545455 1
#' # 11 0.025641026 0.1666667 0.4230769 1
#' # 12 0.021978022 0.1538462 0.3956044 1
#' # 13 0.019047619 0.1428571 0.3714286 1
#' # 14 0.016666667 0.1333333 0.3500000 1
#' # 15 0.014705882 0.1250000 0.3308824 1
#' # 16 0.013071895 0.1176471 0.3137255 1
#' # 17 0.011695906 0.1111111 0.2982456 1
#' # 18 0.010526316 0.1052632 0.2842105 1
#' # 19 0.009523810 0.1000000 0.2714286 1
#' # 20 0.008658009 0.0952381 0.2597403 1
#' #
#'


##
##  probability function of the runs statistic
##
druns <- function(x, n1, n2, log = FALSE) {
    stopifnot(is.numeric(x))
    x <- ifelse(x == round(x), x, 1)
    r0 <- ifelse(
        x %% 2 == 0,
        2 * choose(n1 - 1, round(x / 2) - 1) * choose(n2 - 1, round(x / 2) - 1),
        choose(n1 - 1, round((x - 1) / 2)) * choose(n2 - 1, round((x - 3) / 2)) +
            choose(n1 - 1, round((x - 3) / 2)) * choose(n2 - 1, round((x - 1) / 2))
    )
    r0 <- r0 / choose(n1 + n2, n1)
    # if TRUE, probabilities p are given as log(p).
    ifelse(log, return(log(r0)), return(r0))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname druns
#' @export
##
##  distribution function of the runs statistic
##
pruns <- function(q, n1, n2, lower.tail = TRUE, log.p = FALSE){
    stopifnot(is.numeric(q) & n1>0 & n2>0)
    q <- ifelse(q >= 1, q, 1)
    q <- ifelse(q <= n1+n2, q, n1+n2)
    q <- round(q)
    tmp <- cumsum(druns(1:max(q),n1,n2,log=log.p))
    r0 <- tmp[q]
    if (lower.tail==FALSE){r0<- 1-r0}
    #  r0 <- NULL
    #  if (lower.tail){
    #    for (i in 1:length(q)){r0 <- c(r0,ifelse(q[i]>=2,sum(druns(x=2:floor(q[i]),n1,n2,log=log)),0))}
    #  }
    #  else {r0 <- 1-pruns(q,n1,n2,lower.tail=T, log=log)}
    return(r0)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname druns
#' @export
##
##  Quantile function of the runs statistic
##
qruns <- function(p, n1, n2, lower.tail = TRUE, log.p = FALSE){
    r0 <- NULL
    q1 <- ifelse (n1==n2, 2*n1, 2*min(n1,n2)+1)
    pr <- c(0, cumsum(druns(2:q1, n1, n2)))
    for (i in 1:length(p)){
        if (p[i]>=0 & p[i]<=1){
            #rq<-which(abs(pr-p)==min(abs(pr-p)))
            qr <- NULL
            for (j in 2:q1){
                if (pr[j-1]<p[i] & p[i]<=pr[j]){qr<-j}
            }
            if (p[i] == pr[1]){qr <- 2}
        }
        else {rq<-NA}
        r0<-c(r0, qr)
    }
    return(r0)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname druns
#' @export
##
##  Generates (pseudo) randon values of the runs statistic
##
rruns <- function(n, n1, n2){
    return(qruns(runif(n), n1, n2))
}
