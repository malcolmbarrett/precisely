#' Estimate sample size based on precision of a measure
#'
#' These functions calculate the sample size needed to estimate a measure with a
#' certain precision. For ratio measures, like the risk ratio, rate ratio, and
#' odds ratio, this is the ratio of the upper to lower limit of the confidence
#' interval. For difference measures, like the risk difference or rate
#' difference, this is the absolute width of the confidence interval.
#'
#' @param precision For differences, the width of the CI. For ratios, the ratio
#'   of the upper to lower CI.
#' @param exposed The risk or rate among the exposed cohort.
#' @param unexposed The risk or rate among the unexposed cohort.
#' @param exposed_cases The proportion of exposed cases.
#' @param exposed_controls The proportion of exposed controls.
#' @param group_ratio In cohort studies, the ratio of the unexposed to the
#'   exposed. In case-control studies, the ratio of the controls to the cases.
#' @param ci The confidence interval as a probability or percent. Default is
#'   .95.
#'
#' @return a tibble with sample size, effect measure, and precision
#' @export
#'
#' @references Rothman, K.J. and Greenland, S. 2018.
#'   [Planning Study Size Based on Precision Rather
#'   Than Power](https://www.ncbi.nlm.nih.gov/pubmed/29912015). 29(5):599-603.
#'
#' @examples
#'
#' # From Rothman and Greenland 2018
#'
#' n_risk_difference(
#'   precision = .08,
#'   exposed = .4,
#'   unexposed = .3,
#'   group_ratio = 3,
#'   ci = .90
#' )
#'
#' n_risk_ratio(
#'   precision = 2,
#'   exposed = .4,
#'   unexposed = .3,
#'   group_ratio = 3
#' )
#'
n_risk_difference <- function(precision, exposed, unexposed, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  numerator <- group_ratio * odds(exposed) + odds(unexposed)
  denominator <- group_ratio * precision^2
  n_exposed <- ((4*z^2) * numerator) / denominator

  n_unexposed <- n_exposed * group_ratio
  n_total <- n_exposed + n_unexposed

  risk_difference <- exposed - unexposed

  dplyr::tibble(
    n_exposed,
    n_unexposed,
    n_total,
    risk_difference,
    precision,
    exposed,
    unexposed,
    group_ratio,
    ci
  )
}

#' @rdname n_risk_difference
#' @export
n_risk_ratio <- function(precision, exposed, unexposed, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  numerator <- group_ratio * prob_mult(unexposed, exposed) + prob_mult(exposed, unexposed)
  denominator <- group_ratio * exposed * unexposed * (log(precision))^2
  n_exposed <- ((4*z^2) * numerator) / denominator

  n_unexposed <- n_exposed * group_ratio
  n_total <- n_exposed + n_unexposed

  risk_ratio <- exposed / unexposed

  dplyr::tibble(
    n_exposed,
    n_unexposed,
    n_total,
    risk_ratio,
    precision,
    exposed,
    unexposed,
    group_ratio,
    ci
  )
}

#' @rdname n_risk_difference
#' @export
n_rate_difference <- function(precision, exposed, unexposed, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  numerator <- group_ratio * exposed + unexposed
  denominator <- group_ratio * precision^2
  n_exposed <- ((4*z^2) * numerator) / denominator

  n_unexposed <- n_exposed * group_ratio
  n_total <- n_exposed + n_unexposed

  rate_difference <- exposed - unexposed

  dplyr::tibble(
    n_exposed,
    n_unexposed,
    n_total,
    rate_difference,
    precision,
    exposed,
    unexposed,
    group_ratio,
    ci
  )
}

#' @rdname n_risk_difference
#' @export
n_rate_ratio <- function(precision, exposed, unexposed, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  numerator <- group_ratio * unexposed + exposed
  denominator <- group_ratio * exposed * unexposed * (log(precision))^2
  n_exposed <- ((4*z^2) * numerator) / denominator

  n_unexposed <- n_exposed * group_ratio
  n_total <- n_exposed + n_unexposed

  rate_ratio <- exposed / unexposed

  dplyr::tibble(
    n_exposed,
    n_unexposed,
    n_total,
    rate_ratio,
    precision,
    exposed,
    unexposed,
    group_ratio,
    ci
  )
}

#' @rdname n_risk_difference
#' @export
n_odds_ratio <- function(precision, exposed_cases, exposed_controls, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  numerator <- group_ratio * odds(exposed_controls) + odds(exposed_cases)
  denominator <- (log(precision))^2 * group_ratio * exposed_cases * exposed_controls * (1 - exposed_cases) * (1 - exposed_controls)
  n_cases <- ((4*z^2) * numerator) / denominator

  n_controls <- n_cases * group_ratio
  n_total <- n_cases + n_controls

  odds_ratio <- odds_ratio(exposed_cases, exposed_controls)

  dplyr::tibble(
    n_cases,
    n_controls,
    n_total,
    odds_ratio,
    precision,
    exposed_cases,
    exposed_controls,
    group_ratio,
    ci
  )
}

