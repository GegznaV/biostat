# x - ggedit object
#' Extract and tidy code from ggedit object.
#'
#' @param x A `ggedit` object.
#' @param ... Other parameters to \code{\link[styler]{style_text}()}.
#'
#' @return String.
#' @export
tidy_ggedit_code <- function(x, ...) {
    if (!inherits(x, "ggedit")) stop("Not ggedit object")

    field_names <- grep("Calls", names(x), value = TRUE)

    calls <- sapply(field_names, function(y) {
        sapply(x[[y]], function(obj) {
            paste0(obj, collapse = " + \n")
        })
    })

    if (is.null(dim(calls))) {
        nm <- names(calls)
        calls = matrix(calls, nrow = 1)
        rownames(calls) <- unique(gsub("^[^.]*.", "", nm))
    }

    out <- apply(calls, 1, function(obj) {
        paste(obj[!obj %in% c("list()", "")], collapse = " + \n")
    })

    out <- styler::style_text(out, ...)
    out
}
