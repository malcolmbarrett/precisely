#' Estimate precision of a measure based on sample size
#'
#' These functions calculate the precision of an estimate given a certain sample
#' size. For ratio measures, like the risk ratio, rate ratio, and odds ratio,
#' this is the ratio of the upper to lower limit of the confidence interval. For
#' difference measures, like the risk difference or rate difference, this is the
#' absolute width of the confidence interval.
#'
#' @param n_exposed,n_cases In cohort studies, the number of exposed
#'   participants. In case-control studies, the number of cases.
#' @inheritParams n_risk_difference
#' @inheritParams n_odds_ratio
#'
#' @return a tibble with precision, effect measure, and sample size
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
#' precision_odds_ratio(
#'   n_cases = 500,
#'   exposed_cases = .6,
#'   exposed_controls = .4,
#'   group_ratio = 2
#' )
#'
precision_risk_difference <- function(n_exposed, exposed, unexposed, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  numerator <- sqrt(group_ratio * odds(exposed) + odds(unexposed))
  denominator <- sqrt(n_exposed * group_ratio)
  precision <- ((2*z) * numerator) / denominator

  n_unexposed <- n_exposed * group_ratio
  n_total <- n_exposed + n_unexposed

  risk_difference <- exposed - unexposed

  dplyr::tibble(
    precision,
    risk_difference,
    n_exposed,
    n_unexposed,
    n_total,
    exposed,
    unexposed,
    group_ratio,
    ci
  )
}

#' @rdname precision_risk_difference
#' @export
precision_risk_ratio <- function(n_exposed, exposed, unexposed, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  numerator <- sqrt(group_ratio * prob_mult(unexposed, exposed) + prob_mult(exposed, unexposed))
  denominator <- sqrt(n_exposed * (group_ratio * exposed * unexposed))
  precision <- ((2*z) * numerator) / denominator
  precision <- exp(precision)

  n_unexposed <- n_exposed * group_ratio
  n_total <- n_exposed + n_unexposed

  risk_ratio <- exposed / unexposed

    dplyr::tibble(
    precision,
    risk_ratio,
    n_exposed,
    n_unexposed,
    n_total,
    exposed,
    unexposed,
    group_ratio,
    ci
  )
}

#' @rdname precision_risk_difference
#' @export
precision_rate_difference <- function(n_exposed, exposed, unexposed, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  numerator <- sqrt(group_ratio * exposed + unexposed)
  denominator <- sqrt(n_exposed * group_ratio)
  precision <- ((2*z) * numerator) / denominator

  n_unexposed <- n_exposed * group_ratio
  n_total <- n_exposed + n_unexposed

  rate_difference <- exposed - unexposed

    dplyr::tibble(
    precision,
    rate_difference,
    n_exposed,
    n_unexposed,
    n_total,
    exposed,
    unexposed,
    group_ratio,
    ci
  )
}

#' @rdname precision_risk_difference
#' @export
precision_rate_ratio <- function(n_exposed, exposed, unexposed, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  numerator <- sqrt(group_ratio * unexposed + exposed)
  denominator <- sqrt(n_exposed * (group_ratio * exposed * unexposed))
  precision <- ((2*z) * numerator) / denominator
  precision <- exp(precision)

  n_unexposed <- n_exposed * group_ratio
  n_total <- n_exposed + n_unexposed

  rate_ratio <- exposed / unexposed

  dplyr::tibble(
    precision,
    rate_ratio,
    n_exposed,
    n_unexposed,
    n_total,
    exposed,
    unexposed,
    group_ratio,
    ci
  )
}

#' @rdname precision_risk_difference
#' @export
precision_odds_ratio <- function(n_cases, exposed_cases, exposed_controls, group_ratio, ci = .95) {
  z <- get_z_score(ci)

  numerator <- sqrt(group_ratio * odds(exposed_controls) + odds(exposed_cases))
  denominator <- sqrt(n_cases * (group_ratio * exposed_cases * exposed_controls * (1 - exposed_cases) * (1 - exposed_controls)))
  precision <- ((2*z) * numerator) / denominator
  precision <- exp(precision)

  n_controls <- n_cases * group_ratio
  n_total <- n_cases + n_controls

  odds_ratio <- odds_ratio(exposed_cases, exposed_controls)

  dplyr::tibble(
    precision,
    odds_ratio,
    n_cases,
    n_controls,
    n_total,
    exposed_cases,
    exposed_controls,
    group_ratio,
    ci)
}

