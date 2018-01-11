# # Modified version of mosaic::magregate
# maggregate <-  function (formula,
#                           data = parent.frame(),
#                           FUN,
#                           groups = NULL,
#                           subset,
#                           drop = FALSE,
#                           ...,
#                           sep = "|",
#                           .format = c("default", "table", "flat"),
#                           .overall = mosaic.par.get("aggregate.overall"),
#                           .multiple = FALSE,
#                           .name = deparse(substitute(FUN)),
#                           .envir = parent.frame())
# {
#     if (!inherits(data, c("environment", "data.frame"))) {
#         if (inherits(data, c("tbl")))
#             stop("Your tbl is not a data.frame. ",
#                  "Perhaps you need dplyr functions here.",
#                  call. = FALSE)
#         else
#             stop("data must be an environment or data.frame.",
#                  call. = FALSE)
#     }
#     formula <-
#         mosaic_formula_q(formula, groups = groups, envir = .envir)
#     if (length(formula) == 2) {
#         return(FUN(eval(formula[[2]], data, .envir), ...))
#     }
#     dots <- list(...)
#     groupName <- ".group"
#     .format <- match.arg(.format)
#     evalF <- evalFormula(formula, data = data)
#     if (!missing(subset)) {
#         subset <- eval(substitute(subset), data, environment(formula))
#         if (!is.null(evalF$left))
#             evalF$left <- evalF$left[subset,]
#         if (!is.null(evalF$right))
#             evalF$right <- evalF$right[subset,]
#         if (!is.null(evalF$condition))
#             evalF$condition <- evalF$condition[subset,]
#     }
#     if (is.null(evalF$left) || ncol(evalF$left) < 1) {
#         if (ncol(evalF$right) > 1)
#             warning("Too many variables in rhs; ignoring all but first.")
#         if (.format == "table") {
#             if (.multiple)
#                 stop("table view unavailable for this function.")
#             ldata <- evalF$right[, 1, drop = FALSE]
#             gdata <- group_by(data)
#             res <-
#                 as.data.frame(dplyr::do(gdata,
#                                         foo = FUN(as.data.frame(.)[, 1], ...)))
#             names(res)[ncol(res)] <- gsub(".*::", "", .name)
#             return(res)
#             return(evalF$right[, 1, drop = FALSE] %>%
#                        group_by() %>%
#                        dplyr::do(do.call(FUN, list(
#                            evalF$right[, 1],
#                            ...
#                        ))) %>% as.data.frame())
#         }
#         return(do.call(FUN, alist(evalF$right[, 1], ...)))
#     }
#     else {
#         if (ncol(evalF$left) > 1)
#             warning("Too many variables in lhs; ignoring all but first.")
#         if (.format == "table") {
#             if (.multiple)
#                 stop("table view unavailable for this function.")
#             ldata <- joinFrames(evalF$left[, 1, drop = FALSE],
#                                 evalF$right, evalF$condition)
#             ldata$.var <- ldata[, 1]
#             gdata <-
#                 do.call(group_by, c(list(ldata), lapply(
#                     union(names(evalF$right),
#                           names(evalF$condition)), as.name
#                 )))
#             res <-
#                 as.data.frame(dplyr::do(gdata, foo = FUN(as.data.frame(.)[,
#                                                                           1], ...)))
#             names(res)[ncol(res)] <- gsub(".*::", "", .name)
#         }
#         else {
#             res <- lapply(split(
#                 evalF$left[, 1],
#                 joinFrames(evalF$right,
#                            evalF$condition),
#                 drop = drop
#             ), function(x) {
#                 do.call(FUN, alist(x, ...))
#             })
#             groupName <-
#                 paste(c(names(evalF$right),
#                         names(evalF$condition)),
#                       collapse = sep)
#             if (!.multiple)
#                 res <- unlist(res)
#             if (!is.null(evalF$condition)) {
#                 if (ncol(evalF$left) > 1)
#                     message("Too many variables in lhs; ignoring all but first.")
#                 res2 <-
#                     lapply(split(evalF$left[, 1], evalF$condition,
#                                  drop = drop), function(x) {
#                                      do.call(FUN, alist(x, ...))
#                                  })
#                 groupName <-
#                     paste(names(evalF$condition), collapse = sep)
#                 if (!.multiple) {
#                     res <- c(res, unlist(res2))
#                 }
#                 else {
#                     res <- c(res, res2)
#                 }
#             }
#             if (.multiple) {
#                 result <- res
#                 res <- result[[1]]
#                 for (item in result[-1]) {
#                     res <- as.data.frame(rbind(res, item))
#                 }
#                 if (nrow(res) == length(names(result))) {
#                     res[groupName] <- names(result)
#                 }
#                 else {
#                     res[groupName] <-
#                         rep(names(result),
#                             each = nrow(res) / length(names(result)))
#                 }
#                 res <- res[, c(ncol(res), 1:(ncol(res) - 1))]
#             }
#         }
#     }
#     w <- grep("V[[:digit:]]+", names(res))
#     if (length(w) == 1) {
#         names(res)[w] <- gsub(".*:{2,3}", "", .name)
#     }
#     else {
#         names(res)[w] <- paste0(gsub(".*:{2,3}", "", .name),
#                                 1:length(w))
#     }
#     row.names(res) <- NULL
#     return(res)
# }