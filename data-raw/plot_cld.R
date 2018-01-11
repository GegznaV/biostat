# getS3method("plot", "cld")
plot.cld <- function (x, type = c("response", "lp"), ...)
{
    mcletters <- x$mcletters
    msletters <- mcletters$monospacedLetters
    vletters <- sapply(msletters, function(x)
        paste(strsplit(x, "")[[1]], "\n", collapse = ""))

    vletters <- vletters[gsub(" ", "", levels(x$x))]
    msletters <- msletters[gsub(" ", "", levels(x$x))]
    type <- match.arg(type)
    dat <- x[c("x", "y", "lp")]

    if (is.null(x$weights)) {
        dat$weights <- rep(1, NROW(x$y))
    }
    else {
        dat$weights <- x$weights
    }
    dat <- as.data.frame(dat)
    xn <- x$xname
    yn <- x$yname

    if (!is.null(list(...)$xlab))
        xn <- list(...)$xlab

    if (!is.null(list(...)$ylab))
        yn <- list(...)$ylab

    if (x$covar || type == "lp") {
        boxplot(lp ~ x, data = dat, xlab = xn, ylab = "linear predictor", ...)
        axis(3, at = 1:nlevels(dat$x), labels = vletters)
    }
    else {
        if (is.integer(dat$y))
            dat$y <- as.numeric(dat$y)
        switch(class(dat$y), numeric = {
            boxplot(y ~ x, data = dat, xlab = xn, ylab = yn,
                    ...)
            axis(3, at = 1:nlevels(dat$x), labels = vletters)
        }, factor = {
            at <- xtabs(weights ~ x, data = dat)/sum(dat$weights)
            at <- cumsum(at) - at/2
            mosaicplot(xtabs(weights ~ x + y, data = dat), main = NULL,
                       xlab = xn, ylab = yn, ...)
            axis(3, at = at, labels = vletters, tick = FALSE)
        }, Surv = {
            plot(survfit(y ~ x, data = dat), lty = 1:nlevels(dat$x),
                 ...)
            nc <- nchar(levels(dat$x))
            spaces <- unlist(lapply(max(nc) - nc, function(x)
                return(paste(rep(" ", x), collapse = ""))))
            legend("topright",
                   lty = 1:nlevels(dat$x),
                   legend = paste(levels(dat$x),  spaces, ": ", msletters, sep = ""), ...)
        })
    }
}
# <bytecode: 0x0000000030cf1720>
# <environment: namespace:multcomp>