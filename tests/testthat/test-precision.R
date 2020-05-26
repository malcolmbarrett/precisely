test_that("precision OR functions work", {
  or <- precision_odds_ratio(
    n_cases = 500,
    exposed_cases = .6,
    exposed_controls = .4,
    group_ratio = 2
  )

  expected_names <- c(
    "precision", "odds_ratio", "n_cases", "n_controls", "n_total",
    "exposed_cases", "exposed_controls", "group_ratio", "ci"
  )

  returns_well_formed(or, .ncol = 9, .nrow = 1, expected_names = expected_names)
})

test_that("precision Risk Difference functions work", {
  rd <- precision_risk_difference(500, .5, .1, group_ratio = 3)

  expected_names <- c(
    "precision", "risk_difference", "n_exposed", "n_unexposed",
    "n_total", "exposed", "unexposed", "group_ratio", "ci"
  )

  returns_well_formed(rd, .ncol = 9, .nrow = 1, expected_names = expected_names)
})

test_that("precision Rate Difference functions work", {
  rd <- precision_rate_difference(500, .5, .1, group_ratio = 3)

  expected_names <- c(
    "precision", "rate_difference", "n_exposed", "n_unexposed",
    "n_total", "exposed", "unexposed", "group_ratio", "ci"
  )

  returns_well_formed(rd, .ncol = 9, .nrow = 1, expected_names = expected_names)
})


test_that("precision Risk Ratio functions work", {
  rr <- precision_risk_ratio(500, .5, .1, group_ratio = 3)

  expected_names <- c(
    "precision", "risk_ratio", "n_exposed", "n_unexposed",
    "n_total", "exposed", "unexposed", "group_ratio", "ci"
  )

  returns_well_formed(rr, .ncol = 9, .nrow = 1, expected_names = expected_names)
})


test_that("precision Rate Ratio functions work", {
  rr <- precision_rate_ratio(500, .5, .1, group_ratio = 3)

  expected_names <- c(
    "precision", "rate_ratio", "n_exposed", "n_unexposed",
    "n_total", "exposed", "unexposed", "group_ratio", "ci"
  )

  returns_well_formed(rr, .ncol = 9, .nrow = 1, expected_names = expected_names)
})

