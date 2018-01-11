pairwise_mood_median_test <- function(formula = NULL, data = NULL, x = NULL, g = NULL,
                                 exact = NULL, method = "fdr", ...) {
    if (!is.null(formula)) {
        x = eval(parse(text = paste0("data", "$", all.vars(formula[[2]])[1])))
        g = eval(parse(text = paste0("data", "$", all.vars(formula[[3]])[1])))
    }
    if (!is.factor(g)) {
        g = factor(g)
    }
    n = length(levels(g))
    N = n * (n - 1)/2
    d = data.frame(x = x, g = g)
    Z = data.frame(Comparison = rep("A", N),
                   p.value = rep(NA, N),
                   p.adjust = rep(NA, N),
                   stringsAsFactors = FALSE)
    k = 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            k = k + 1
            Namea = as.character(levels(g)[i])
            Nameb = as.character(levels(g)[j])
            Datax = subset(d, g == levels(g)[i])
            Datay = subset(d, g == levels(g)[j])
            Dataz = rbind(Datax, Datay)
            Dataz$g2 = factor(Dataz$g)
            z = RVAideMemoire::mood.medtest(x = Dataz$x, g = Dataz$g2, exact = exact,  ...)
            P = signif(z$p.value, digits = 4)
            P.adjust = NA
            Z[k, ] = c(paste0(Namea, " - ", Nameb, " = 0"), P, P.adjust)
        }
    }
    Z$p.adjust = signif(p.adjust(Z$p.value, method = method), digits = 4)
    return(Z)
}