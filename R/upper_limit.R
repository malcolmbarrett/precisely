#' Estimate sample size based on probability that upper limit is below level of
#' concern.
#'
#' These functions calculate sample size based on probability that upper limit
#' is below level of concern. The idea behind this approach is to use precision
#' to provide support for the absence of effect. These functions calculate
#' sample size where, when the true effect is null, the upper limit of the
#' confidence interval of the estimate of interest has a probability of being at
#' or under a specified level of concern.
#'
#' @param upper_limit The upper limit of the confidence interval, a level of
#'   concern.
#' @param prob The probability of the estimated upper limit of the confidence
#'   interval being at or below the level of concern.
#' @inheritParams n_risk_difference
#' @inheritParams n_odds_ratio
#'
#' @return a tibble with sample size, effect measure, upper limit, and
#'   probability
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
#' upper_rate_ratio(
#'   upper_limit = 2,
#'   prob = .90,
#'   exposed = .01,
#'   unexposed = .01,
#'   group_ratio = 1
#' )
#'
upper_risk_difference <- function(upper_limit, prob, exposed, unexposed, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  z_cumulative <- get_z_score(prob, cumulative = TRUE)
  numerator <- group_ratio * odds(exposed) + odds(unexposed)
  denominator <- group_ratio * upper_limit^2
  n_exposed <- ((z + z_cumulative)^2 * numerator) / denominator

  n_unexposed <- n_exposed * group_ratio
  n_total <- n_exposed + n_unexposed

  risk_difference <- exposed - unexposed

  dplyr::tibble(
    n_exposed,
    n_unexposed,
    n_total,
    risk_difference,
    upper_limit,
    prob,
    exposed,
    unexposed,
    group_ratio,
    ci
  )
}

#' @rdname upper_risk_difference
#' @export
upper_risk_ratio <- function(upper_limit, prob, exposed, unexposed, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  z_cumulative <- get_z_score(prob, cumulative = TRUE)
  numerator <- group_ratio * prob_mult(unexposed, exposed) + prob_mult(exposed, unexposed)
  denominator <- group_ratio * exposed * unexposed * (log(upper_limit))^2
  n_exposed <- ((z + z_cumulative)^2 * numerator) / denominator

  n_unexposed <- n_exposed * group_ratio
  n_total <- n_exposed + n_unexposed

  risk_ratio <- exposed / unexposed

  dplyr::tibble(
    n_exposed,
    n_unexposed,
    n_total,
    risk_ratio,
    upper_limit,
    prob,
    exposed,
    unexposed,
    group_ratio,
    ci
  )
}

#' @rdname upper_risk_difference
#' @export
upper_rate_difference <- function(upper_limit, prob, exposed, unexposed, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  z_cumulative <- get_z_score(prob, cumulative = TRUE)
  numerator <- group_ratio * exposed + unexposed
  denominator <- group_ratio * upper_limit^2
  n_exposed <- ((z + z_cumulative)^2 * numerator) / denominator

  n_unexposed <- n_exposed * group_ratio
  n_total <- n_exposed + n_unexposed

  rate_difference <- exposed - unexposed

  dplyr::tibble(
    n_exposed,
    n_unexposed,
    n_total,
    rate_difference,
    upper_limit,
    prob,
    exposed,
    unexposed,
    group_ratio,
    ci
  )
}

#' @rdname upper_risk_difference
#' @export
upper_rate_ratio <- function(upper_limit, prob, exposed, unexposed, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  z_cumulative <- get_z_score(prob, cumulative = TRUE)
  numerator <- group_ratio * unexposed + exposed
  denominator <- group_ratio * exposed * unexposed * (log(upper_limit))^2
  n_exposed <- ((z + z_cumulative)^2 * numerator) / denominator

  n_unexposed <- n_exposed * group_ratio
  n_total <- n_exposed + n_unexposed

  rate_ratio <- exposed / unexposed

  dplyr::tibble(
    n_exposed,
    n_unexposed,
    n_total,
    rate_ratio,
    upper_limit,
    prob,
    exposed,
    unexposed,
    group_ratio,
    ci
  )
}

#' @rdname upper_risk_difference
#' @export
upper_odds_ratio <- function(upper_limit, prob, exposed_cases, exposed_controls, group_ratio, ci = .95) {
  z <- get_z_score(ci)
  z_cumulative <- get_z_score(prob, cumulative = TRUE)
  numerator <- group_ratio * odds(exposed_controls) + odds(exposed_cases)
  denominator <- (log(upper_limit))^2 * group_ratio * exposed_cases * exposed_controls * (1 - exposed_cases) * (1 - exposed_controls)
  n_cases <- ((z + z_cumulative)^2 * numerator) / denominator

  n_controls <- n_cases * group_ratio
  n_total <- n_cases + n_controls

  odds_ratio <- odds_ratio(exposed_cases, exposed_controls)

  dplyr::tibble(
    n_cases,
    n_controls,
    n_total,
    odds_ratio,
    upper_limit,
    prob,
    exposed_cases,
    exposed_controls,
    group_ratio,
    ci
  )
}

