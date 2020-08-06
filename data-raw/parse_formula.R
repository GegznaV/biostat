# Based on:
# <environment: namespace:mosaicCore>
# [!!!]
formula_parts <- function(formula) {
  # op <- formula[[1]]

  condition <- NULL
  if (length(formula) == 2) {
    lhs <- NULL
    rhs <- formula[[2]]
  } else if (length(formula) == 3) {
    lhs <- formula[[2]]
    rhs <- formula[[3]]
  } else {
    stop("Invalid formula type.")
  }

  if (inherits(rhs, "call") && rhs[[1]] == "|") {
    condition <- rhs[[3]]
    rhs <- rhs[[2]]
  }

  # Output  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  structure(
    list( # op = op,
      lhs = lhs,
      rhs = rhs,
      condition = condition
    ),
    class = "parsedFormula"
  )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# keep_all_vars - (logigal) If all variables (even hose not in formula)
#                 should be included in output data.
formula_parse <- function(formula, data = NULL, keep_all_vars = FALSE) {
  envir <- rlang::f_env(formula)

  if (is.null(data)) {
    data <- envir
  }

  extract_var_names <- function(obj) {
    paste(deparse(obj, width.cutoff = 500), collapse = " ")
  }

  formula_parts(formula)

  # Get varnames
  fml_terms <- terms(formula, data = data, keep.order = TRUE)
  fml_vars <- attr(fml_terms, "variables")
  varnames <- sapply(fml_vars, extract_var_names)[-1L]

  # term_labels <- attr(fml_terms, "term.labels")
  # varnames <- union(varnames, term_labels)

  # Choose for one-sided and two-sided formula
  switch(as.character(length(formula)),
    "2" = {
      y_vars <- varnames
      x_vars <- NULL
    },
    "3" = {
      resp_ind <- attr(fml_terms, "response")
      y_vars <- varnames[resp_ind]
      x_vars <- varnames[-resp_ind]
    },
    stop("Incorrect formula.")
  )

  new_data <- data.frame(setNames(eval(fml_vars, data, envir), varnames),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (keep_all_vars == TRUE) {
    # If all variables (including those not in formula) should be kept.
    new_data <- dplyr::bind_cols(
      new_data,
      data[,
        setdiff(names(data), varnames),
        drop = FALSE
      ]
    )
  }

  # Output
  list(
    formula_vars = varnames,
    y_names = y_vars,
    x_names = x_vars,
    data = new_data
  )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
