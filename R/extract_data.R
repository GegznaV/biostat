extract_data <- function(formula, data = NULL, ...) {
    if (is.null(data)) {
        data <- rlang::f_env(formula)
    }
    model.frame(formula, data, ...)
}