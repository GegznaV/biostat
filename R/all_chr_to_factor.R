#' [!] Convert all character variables to factors
#'
#' A convenience function to convert all character variables in a data frame
#' to factor variables.
#'
#' @param data A data frame.
#'
#' @return A data frame with character variables converted to factors.
#' @export
#'
#' @examples
#' library(biostat)
#' data(PlantGrowth)
#'
#'
#' # Basic syntax
#' PlantGrowth <- all_chr_to_factor(PlantGrowth)
#'
#'
#' # Update the same object using operator from `magrittr` package
#' library(magrittr)
#' PlantGrowth %<>% all_chr_to_factor()
#'
#'
#' # Create a data frame
#' df <- data.frame(
#'   letters = letters[1:5],
#'   letters2 = LETTERS[1:5],
#'   stringsAsFactors = FALSE
#' )
#'
#' mapply(class, df) # show classes of columns
#'
#' # Convert all character variables to strings
#' df2 <- all_chr_to_factor(df)
#'
#' # Check the classes in each column
#' mapply(class, df2)
#'
#' # # Works with tibbles too
#' # tbl  <- tibble::as.tibble(df)
#' # tbl2 <- all_chr_to_factor(tbl)
#' # mapply(class, tbl2)
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # v0.3.1:
all_chr_to_factor <- function(data) {
  dplyr::mutate_if(
    data,
    is.character,
    list(~ forcats::fct_inorder(.))
  )
}

# =============================================================================
# The old versions of this function source code:
#
# =============================================================================
# v0.1: Changes the class:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data %<>%
#     purrr::map_if(is.character, factor) %>%
#     as.data.frame()
# # Output:
# data

# =============================================================================
# v0.2 Does not Change the class:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# col_is_character <- purrr::map_chr(data, ~ inherits(., "character"))
#
# for (i in seq_along(col_is_character)) {
#     if (col_is_character[i]) {
#         data[[i]] %<>% factor()
#     }
# }
# attr(data, "converted_fo_factor") <- col_is_character
# # Output:
# data

# =============================================================================
# v0.3.0:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # @export
# # @rdname all_chr_to_factor
# stat_all_chr_to_factor <- function(data) {
#     dplyr::mutate_if(data,
#                      is.character,
#                      list(~forcats::fct_inorder(.)))
# }
