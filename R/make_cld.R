#' Make a compact letter display (cld) for pair-wise comparison
#'
#' Make a compact letter display for results of pair-wise comparisons
#' (e.g., ANOVA post-hoc tests, Kruskal-Wallis post-hoc tests and other).
#'
#'
#' @param obj Object with pair-wise comparisons (e.g., post-hoc test results).
#'   Currently supported objects: \itemize{
#'   \item \emph{posthocTGH} from package \pkg{userfriendlyscience};
#'   \item \emph{PMCMR} from package \pkg{PMCMR}.
#'   }
#'
#' @param ... Further arguments to methods.
#'
#' @param data A dataset with p values and names of comparisons. This argument
#'            is used if \code{obj} is formula. More details in examples.
#'
#' @param formula An R model \code{\link[stats]{formula}} where left-hand side
#' term indicates variable with p values and right-hand side term defines
#' variable with comparisons, e.g. \code{p.adjust ~ Comparison}. Usually is
#' used in combination with \code{data}.
#'
#' @param alpha (numeric from 0 to 1) Significance level.
#'
#' @return (A data frame with) compact letter display.
#' @export
#'
#' @examples
#' library(biostat)
#'
#' # Example 1: class `pairwise.htest`
#'
#' obj1 <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)
#' make_cld(obj1)
#'
#'
#' # Example 2: class `pairwise.htest`
#'
#' obj2 <- with(OrchardSprays, pairwise.t.test(decrease, treatment))
#' make_cld(obj2)
#'
#'
#' # Example 3: class `pairwise.htest`
#' \donttest{
#' smokers <- c(83, 90, 129, 70)
#' patients <- c(86, 93, 136, 82)
#' obj3 <- pairwise.prop.test(smokers, patients)
#'
#' make_cld(obj3)
#' }
#'
#' # Example 4: class `PMCMR`
#' \donttest{
#' obj4 <- PMCMR::posthoc.kruskal.conover.test(count ~ spray, data = InsectSprays)
#' make_cld(obj4)
#' }
#'
#'
#' # Example 5: class `posthocTGH`
#'
#' obj5 <- posthoc_anova(weight ~ Diet,
#'   data = ChickWeight,
#'   method = "Games-Howell"
#' )
#' make_cld(obj5)
#'
#'
#' # Example 6: class `posthoc_anova`
#'
#' obj6 <- posthoc_anova(weight ~ Diet,
#'   data = ChickWeight,
#'   method = "Games-Howell"
#' )
#' make_cld(obj6)
#'
#'
#' # Example 7: class `formula`
#'
#' my_dataframe <- data.table::fread(
#'   'Comparison     p.value p.adjust
#'     "EE - GB = 0"        1 1.000000
#'     "EE - CY = 0" 0.001093 0.003279
#'     "GB - CY = 0" 0.005477 0.008216'
#' )
#'
#' make_cld(p.adjust ~ Comparison, data = my_dataframe)
#'
#'
#' # Example 8: class `matrix`
#'
#' # (for symetric matrices of p values)
#'
#' # Create matrix
#' m <- c(
#'   1.00, 0.22, 0.05, 0.00,
#'   0.22, 1.00, 0.17, 0.01,
#'   0.05, 0.17, 1.00, 0.22,
#'   0.00, 0.01, 0.22, 1.00
#' )
#' obj8 <- matrix(m, nrow = 4)
#' rownames(obj8) <- colnames(obj8) <- c("P", "O", "I", "U")
#' obj8
#'
#' # Make cld
#' make_cld(obj8)
make_cld <- function(obj, ..., alpha = 0.05) {
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  UseMethod("make_cld")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.pairwise.htest <- function(obj, ..., alpha = 0.05) {
  m1 <- obj$p.value
  df <- pval_matrix_to_df(m1)
  res <- make_cld_df(
    comparison = paste0(df$gr1, " - ", df$gr2),
    p.value = df$p_values,
    threshold = alpha,
    remove.space = TRUE,
    ...
  )
  res
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.posthocTGH <- function(obj, ..., alpha = obj$intermediate$alpha) {
  which_posthoc <-
    switch(tolower(obj$input$method),
      "games-howell" = "games.howell",
      "tukey"        = "tukey",
      stop("Incorrect method selected: ", obj$input$method)
    )

  p_val <- obj$output[[which_posthoc]]$p.adjusted

  if (is.null(p_val)) {
    p_val <- obj$output[[which_posthoc]]$p
  }

  res <- make_cld_df(
    comparison = obj$intermediate$pairNames,
    p.value = p_val,
    threshold = obj$intermediate$alpha,
    ...
  )
  res
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.posthoc_anova <- function(obj, ..., alpha = 1 - obj$input$conf_level) {
  which_posthoc <-
    switch(tolower(obj$input$method),
      "games-howell" = "games.howell",
      "tukey"        = "tukey",
      stop("Incorrect method selected: ", obj$input$method)
    )

  obj2 <- obj$output$result

  res <- make_cld_df(
    comparison = obj2$groups,
    p.value = obj2$p_adjusted,
    threshold = alpha,
    swap_compared_names = TRUE,
    ...
  )
  res
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.PMCMR <- function(obj, ..., alpha = 0.05) {
  make_cld.pairwise.htest(obj, ..., alpha = alpha)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.formula <- function(obj, ..., data = NULL, alpha = 0.05) {
  data <- extract_data(obj, data)
  res <- make_cld_df(obj,
    data = data,
    threshold = alpha,
    ...
  )
  res
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.matrix <- function(obj, ..., alpha = 0.05) {
  checkmate::assert_matrix(obj, mode = "numeric")

  if (is_square_matrix(obj)) {
    if (!all(colnames(obj) == rownames(obj))) {
      stop("Matrix is square but its column and row names does not match.")
    }
  } else {
    stop("This function works with square marices only.")
  }

  obj[upper.tri(obj, diag = TRUE)] <- NA
  df <- pval_matrix_to_df(obj)
  make_cld.pairwise_pval_df(df, ...)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.data.frame <- function(obj, ..., formula = p.adjust ~ Comparison, alpha = 0.05) {
  res <- make_cld_df(
    formula = formula,
    data = obj,
    threshold = alpha,
    ...
  )
  res
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.pairwise_pval_df <- function(obj, ..., alpha = 0.05) {
  res <- make_cld_df(
    comparison = paste0(obj$gr1, " - ", obj$gr2),
    p.value = obj$p_values,
    threshold = alpha,
    ...
  )
  res
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pval_matrix_to_df <- function(x) {
  if (is.null(colnames(x))) colnames(x) <- seq_len(ncol(x))
  if (is.null(rownames(x))) rownames(x) <- seq_len(nrow(x))

  df <- data.frame(
    gr1 = colnames(x)[col(x)],
    gr2 = rownames(x)[row(x)],
    p_values = c(x)
  )

  df <- df[complete.cases(df), ]

  structure(df, class = c("pairwise_pval_df", "data.frame"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
