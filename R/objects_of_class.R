
#' List objects of a certain class.
#'
#' @param class (character) The name of class.
#' @param all.names (logical) If \code{TRUE}, all object names are returned.
#'           If \code{FALSE}, names which begin with a \code{.} are omitted.
#' @param envir The environment to search for objects in.
#'
#' @return Character vector.
#' @export
objects_of_class <- function(class = NULL,
                             all.names = FALSE,
                             envir = parent.frame()) {
    checkmate::assert_character(class, null.ok = TRUE)

    all_variable_names <- objects(envir, all.names = all.names)

    if (length(all_variable_names) == 0 || is.null(class)) {
        return(all_variable_names)
    } else {
        # Object names of class to return
        mget(all_variable_names, envir = envir) %>%
            purrr::keep(~inherits(.x, class)) %>%
            names()
    }
}