# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse model formula.
#'
#' Parse model formula, create new variables by evaluating expressions and return
#' parsed names and new data frame. These elements are ment to be used by other
#' functions.
#'
#' @param formula A [stats::formula()] of form: \itemize{
#'      \item `~ y`;
#'      \item `~ y1 + y2`;
#'      \item `y ~ x`;
#'      \item `y1 + y2 ~ x1 + x2`;
#'      \item `~ y|condition`;
#'      \item `y ~ x|condition`;
#'      \item `y1 + y2 ~ x1 + x2|cond1 + cond2`;
#'      \item  or similar.
#' }
#' Variables can be expressed as function, e.g., `log(x)`, `as.factor(x)`, which will be evaluated. Dot (`.`) is not accepted.
#' @param data A data frame.
#' @param keep_all_vars (logical) Flag indicating if all variables
#'                      (even those not in formula) should be included in the
#'                      output data.
#'                      Expression `keep_all_vars = TRUE` has effect
#'                      only if `data` is a data frame (and not `NULL`
#'                      or an environment).
#'
#'
#' @return A list with fields: \enumerate{
#'     \item `formula` - the formula used;
#'     \item `names` - alist of character vectors with names in formula arranged in certain way (see below);
#'     \item `data` - a data frame with variables and evaluated expressions in the formula and, optionally, with the remaining variables from the original data frame.
#' }
#'
#' Contents of the field `names`: \itemize{
#'     \item `all_names` - all names (and expressions) in the formula;
#'     \item `y`, `x` - names for "x" and "y" variables (except condition variables);
#'     \item `lhs`, `rhs` - names in left-hand and right-hand side of formula (except condition variables);
#'     \item `condition` - names of condition variables and expressions;
#'     \item `gr` - if condition exists, `gr` is the same as condition, otherwise it is the same as `x`.
#' }
#'
#' @export
#'
parse_formula <- function(formula, data = NULL, keep_all_vars = FALSE) {
  if (!rlang::is_formula(formula)) stop("Variable `formula` is not a formula!")
  # if (!rlang::is_formulaish(formula)) stop("Variable `formula` is not a formula!")
  checkmate::assert_data_frame(data, null.ok = TRUE)
  checkmate::assert_logical(keep_all_vars)

  # Get environment of formula
  envir <- rlang::f_env(formula)

  if (is.null(data)) {
    data <- envir
  }

  # Variable names
  names_by_part <- formula_part_names(formula, data = data)

  switch(as.character(length(formula)),
    "2" = {
      # For one-sided formula
      y_vars <- names_by_part$rhs
      x_vars <- NULL
    },

    "3" = {
      # For two-sided formula
      y_vars <- names_by_part$lhs
      x_vars <- names_by_part$rhs
    },
    stop("Incorrect formula.")
  )
  cond_vars <- names_by_part$condition

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Evaluate expressions to create a necessary dataframe
  all_names_in_formula <- unique(Reduce(c, names_by_part))
  new_data <- data.frame(
    # `sapply` changes factors to numeric thus must be avoided
    lapply(all_names_in_formula, eval_, envir = data, enclos = envir),
    check.names      = FALSE,
    stringsAsFactors = FALSE
  )
  names(new_data) <- all_names_in_formula

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (keep_all_vars == TRUE && is.data.frame(data)) {
    # If all variables (including those not in formula) should be kept.
    new_data <-
      dplyr::bind_cols(
        new_data,
        data[, setdiff(names(data), all_names_in_formula),
          drop = FALSE
        ]
      )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gr <- if (is.null(cond_vars)) x_vars else cond_vars
  # Output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  list(
    formula = formula,
    names =
      list(
        all_names = all_names_in_formula,
        y = y_vars,
        x = x_vars,
        lhs = names_by_part$lhs,
        rhs = names_by_part$rhs,
        condition = names_by_part$condition,
        gr = gr,
        formula = all_names_in_formula # this line will be removed
      ),
    data = new_data
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# formula_part_names()
# Takes:
#     either a formula and returns list of names in lhs, rhs and condition
#     or part of a formula and returns character vector of names in that part.
formula_part_names <- function(obj, data) {
  if (is.null(obj)) {
    return(NULL)
  }

  if (rlang::is_formula(obj)) {
    return(lapply(formula_parts(obj), formula_part_names, data = data))
  } else {
    fml <- as.formula(paste("~", expr2chr(obj)))
  }

  fml_terms <- terms(fml, data = data, keep.order = TRUE)
  fml_vars <- attr(fml_terms, "variables")
  sapply(fml_vars, expr2chr)[-1L]
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Based on code from package "mosaicCore"
formula_parts <- function(formula) {
  # op <- formula[[1]]
  condition <- NULL
  switch(as.character(length(formula)),
    "2" = {
      lhs <- NULL
      rhs <- formula[[2]]
    },
    "3" = {
      lhs <- formula[[2]]
      rhs <- formula[[3]]
    },
    stop("Invalid type of formula.")
  )

  if (inherits(rhs, "call") && rhs[[1]] == "|") {
    condition <- rhs[[3]] # The order of these two rows
    rhs <- rhs[[2]] # must not be changed.
  }

  # Formula parts as expressions
  as_expressions <-
    list(
      "lhs" = lhs,
      "rhs" = rhs,
      "condition" = condition
    )

  # Output  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  structure(as_expressions, class = "parsed_formula")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helpers
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
extract_expr_as_chr <- function(formula, data) {
  Reduce(c, formula_part_names(formula, data = data))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
expr2chr <- function(obj) {
  if (is.null(obj)) {
    return(NULL)
  }
  paste(deparse(obj, width.cutoff = 500), collapse = " ")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get varnames
# fml_terms <- terms(formula, data = data, keep.order = TRUE)
# fml_vars  <- attr(fml_terms, "variables")
# varnames  <- sapply(fml_vars, extract_var_names)[-1L]

# term_labels <- attr(fml_terms, "term.labels")
# varnames <- union(varnames, term_labels)

# Choose for one-sided and two-sided formula
# switch(as.character(length(formula)),
#        "2" = {
#            y_vars <- varnames
#            x_vars <- NULL
#        },
#        "3" = {
#            resp_ind <- attr(fml_terms, "response")
#            y_vars <- varnames[resp_ind]
#            x_vars <- varnames[-resp_ind]
#        },
#        stop("Incorrect formula.")
# )
# new_data <- data.frame(setNames(eval(fml_vars, data, envir), varnames),
# check.names = FALSE,
# stringsAsFactors = FALSE)
