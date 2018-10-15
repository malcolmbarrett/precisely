#' Calculate with precisely functions across values
#'
#' `map_precisely()` is a wrapper around [tidyr::crossing()] and
#' [purrr::pmap_dfr()] to give a set of values to any of the calculation
#' functions in precisely. All possible combinations of the values are passed to
#' the function, returning a tibble where each row is the result for each
#' combination.
#'
#' @param .f a function in precisely
#' @param ... arguments passed to `.f`. All possible combinations of argument
#'   values are given to the function.
#'
#' @return a tibble
#' @export
#'
#' @examples
#' map_precisely(
#'   n_risk_difference,
#'   precision = seq(from = .02, to = .20, by = .005),
#'   exposed = c(.2, .4),
#'   unexposed = c(.1, .3),
#'   group_ratio = 1
#' )
#'
map_precisely <- function(.f, ...) {
  arguments <- tidyr::crossing(...)
  purrr::pmap_dfr(arguments, .f)
}
