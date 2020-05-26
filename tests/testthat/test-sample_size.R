test_that("Sample size OR functions work", {
  or <- n_odds_ratio(
    precision = 2,
    exposed_cases = .6,
    exposed_controls = .4,
    group_ratio = 2
  )

  expected_names <- c(
    "n_cases", "n_controls", "n_total", "odds_ratio", "precision",
    "exposed_cases", "exposed_controls", "group_ratio", "ci"
  )

  returns_well_formed(or, .ncol = 9, .nrow = 1, expected_names = expected_names)
})

test_that("Sample size Risk Difference functions work", {
  rd <- n_risk_difference(
    precision = .08,
    exposed = .4,
    unexposed = .3,
    group_ratio = 3,
    ci = .90
  )

  expected_names <- c(
    "n_exposed", "n_unexposed", "n_total", "risk_difference", "precision",
    "exposed", "unexposed", "group_ratio", "ci"
  )

  returns_well_formed(rd, .ncol = 9, .nrow = 1, expected_names = expected_names)
})

test_that("Sample size Rate Difference functions work", {
  rd <- n_rate_difference(1, .5, .1, group_ratio = 3)

  expected_names <- c(
    "n_exposed", "n_unexposed", "n_total", "rate_difference", "precision",
    "exposed", "unexposed", "group_ratio", "ci"
  )

  returns_well_formed(rd, .ncol = 9, .nrow = 1, expected_names = expected_names)
})


test_that("Sample size Risk Ratio functions work", {
  rr <- n_risk_ratio(
    precision = 2,
    exposed = .4,
    unexposed = .3,
    group_ratio = 3
  )

  expected_names <- c(
    "n_exposed", "n_unexposed", "n_total", "risk_ratio", "precision",
    "exposed", "unexposed", "group_ratio", "ci"
  )

  returns_well_formed(rr, .ncol = 9, .nrow = 1, expected_names = expected_names)
})


test_that("Sample size Rate Ratio functions work", {
  rr <- n_rate_ratio(2, .5, .1, group_ratio = 3)

  expected_names <- c(
    "n_exposed", "n_unexposed", "n_total", "rate_ratio", "precision",
    "exposed", "unexposed", "group_ratio", "ci"
  )

  returns_well_formed(rr, .ncol = 9, .nrow = 1, expected_names = expected_names)
})

