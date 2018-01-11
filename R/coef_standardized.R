#' [!] Compute Standardized Regression Coefficients
#'
#' Compute the standardized regression coefficients (beta) from an object of class \code{lm}).
#'
#' @param obj (\code{lm} object) A result of function \code{lm()}.
#'
#' @return
#' Object of class \code{lm_beta} which is a list with 2 named fields:
#' \itemize{
#' \item \code{b} named numeric vector with regression coefficients (not standardized);
#' \item \code{beta} named numeric vector  with standardized coefficients from \code{lm()} model.
#' }
#'
#'
#' @details
#' This function is inspired by function  \pkg{QuantPsyc}\code{::lm.beta()} written by Thomas D. Fletcher. \cr
#' \code{coef_standardized()} provides standardized coefficients even when interaction members are present. This is achieved by computing whole model matrix (with all right-hand side members of formula used in call of \code{lm()}) and calculating standard deviations of each regressor (including interaction members) based on these columns. \cr
#' \code{coef_standardized()} does not fail if intercept is not present.\cr
#'
#' The remaining calculations are the same as in \pkg{QuantPsyc}\code{::lm.beta()}.
#'
#'
#'
#' @author
#' Vilmantas Gegzna (modified function written by Thomas D. Fletcher).
#'
#' @seealso
#' \itemize{
#'     \item \code{\link[stats]{lm}} in package \pkg{stats}.
#'     \item \code{\link[QuantPsyc]{lm.beta}} in package \pkg{QuantPsyc}.
#' }
#' @keywords models
#' @export
#' @examples
#'
#' data(USJudgeRatings)
#' us <- USJudgeRatings
#' names(us)
#'
#' lm1 <- lm(CONT ~ INTG + DMNR + log(DILG), data = us)
#' coef_standardized(lm1)
#'
#' lm2 <- lm(CONT ~ INTG + DMNR*DILG, data = us)
#' coef_standardized(lm2)
#'
#' summary(coef_standardized(lm2))
#'
#'
#' # Do not include intercept
#' lm3 <- lm(CONT ~ 0 + INTG , data = us)
#' coef_standardized(lm3)
#' summary(coef_standardized(lm3))

coef_standardized <- function(obj) {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    checkmate::assert_class(obj, "lm")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    COEFS <- summary(obj)$coef
    ind_a <- which(rownames(COEFS) == "(Intercept)")
    if (length(ind_a) == 0) {
        # If intercept is not present
        a <- 0
        b <- COEFS[ , 1]
        b_names <- rownames(COEFS)

    } else {
        # If intercept is present
        a <- COEFS["(Intercept)", 1]
        b <- COEFS[-1, 1]
        b_names <- rownames(summary(obj)$coef)[-1]
        b_names <- rownames(COEFS)[-1]
    }
    # Extracts all members of right-hand side of formulae:
    data_ <- model.matrix(as.formula(obj$call$formula), data = obj$model)
    # Make correct order of columns:
    data_ <- as.data.frame(data_)[, b_names, drop = FALSE]
    # Do the standardization:
      sx_ <- sapply(data_, sd)
      sy_ <- sapply(obj$model[1], sd)
    beta  <- b * sx_ / sy_
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    structure(list(a = a, b = b, beta = beta),
              class = c("lm_beta","list"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

#' @name deprecated_functions
#' @description Depreceted functions. They exist for compatibility with
#'              previous versions.
#' @title Deprecated functions in `BioStat` package
#' @param obj \code{lm} object.
#' @export
standardized_coef <- function(obj) {
    .Deprecated("coef_standardized")
    coef_standardized(obj)
}

#' @rdname coef_standardized
#'
#' @param x \code{lm_beta} object.
#' @param object \code{lm_beta} object.
#' @param digits (integer) number of decimal places to round the answer to.
#'               Default is 3.
#' @param ... further parameters to \code{print} method.
#'
#' @export

print.lm_beta <- function(x, ..., digits = 3) {
    cat("Standardized Regression Coefficients:\n")

    print(unclass(round(x$beta,  digits = digits)), ...)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname coef_standardized
#' @export
summary.lm_beta <- function(object, ..., digits = 3) {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    b    <- rm_names(object$b)
    beta <- rm_names(object$beta)

    rez <- data.frame(
        regressor = c("(Intercept)", names(object$beta)),
            coeff = c(object$a, b),
        standardized_coeff = c(NA, beta),
        influence_rank = c(NA, rank(-abs(beta)))
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    structure(rez,
              class = c("lm_beta_summary","data.frame"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname coef_standardized
#' @export
print.lm_beta_summary <- function(x, ..., digits = 3) {
    cat("Summary of Standardized Regression Coefficients:\n")
    x$standardized_coeff <- round(x$standardized_coeff,  digits = digits)
    print(data.frame(x))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # # This part is the same as QuantPsyc::lm.beta() ~~~~~~~~~~~~~~~~~~~~~~
# b <- summary(obj)$coef[-1, 1]
#
# sx <- sapply(obj$model[-1], sd)
# sy <- sapply(obj$model[1],  sd)
# beta_0 <- b * sx /  sy
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~