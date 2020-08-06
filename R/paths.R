
#' Standardize string with path
#'
#' Converts "~" into absolute path to home directory, and replaces escaped back
#' slashes "\\" with forward slash "/".
#'
#' @param path string with path
#'
#' @return string ...
#' @export
#'
#' @examples
#' standardize_path("D:\\folder")
#' standardize_path("~")
standardize_path <- function(path) {
  path <- path.expand(path)
  path <- gsub(pattern = "\\", replacement = "/", x = path, fixed = TRUE)
  path
}


#' [!!!]
#'
#' @param path string with absolute path.
#' @param wd string of path to absolute path.
#' @param warn (logical) flag to warn if folder does not exist on a computer.
#'
#' @return string
#' @export
#'
#' @examples
#' # No examples yet
make_relative_path <- function(path, wd = getwd(), warn = TRUE) {
  path <- standardize_path(path)
  wd <- standardize_path(wd)

  relative_path <- gsub(
    pattern = wd, replacement = ".",
    x = path, fixed = TRUE
  )

  relative_path <- gsub(
    pattern = "^\\./", replacement = "",
    x = relative_path
  )

  if (isTRUE(warn) && !(dir.exists(relative_path) | file.exists(relative_path))) {
    warning("Path '", relative_path, "' was not found.")
  }

  relative_path
}
