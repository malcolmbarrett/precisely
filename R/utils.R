utils::globalVariables(
  c(
    "n_total",
    "precision",
    "upper_limit"
    )
)


#' @importFrom stats qnorm
get_z_score <- function(x, cumulative = FALSE) {
  if (!dplyr::between(x, 0, 1)) {
    if (dplyr::between(x, 0, 100)) {
      x <- x/100
    } else {
      stop("Confidence Interval must be between 0 and 1 or 0 and 100")
    }
  }

  if (!cumulative) x <- 1 - (1 - x) / 2
  stats::qnorm(x)
}

prob_mult <- function(p1, p2) {
  p1 * (1 - p2)
}

odds <- function(p) {
  prob_mult(p, p)
}

odds_ratio <- function(exposed_cases, exposed_controls) {
  prob_mult(exposed_cases, exposed_controls) / prob_mult(exposed_controls, exposed_cases)
}
