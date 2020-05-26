test_that("Upper limit OR functions work", {
  or <- upper_odds_ratio(
    upper_limit = 3,
    prob = .9,
    exposed_cases = .6,
    exposed_controls = .4,
    group_ratio = 2
  )

  expected_names <- c(
    "n_cases", "n_controls", "n_total", "odds_ratio", "upper_limit",
    "prob", "exposed_cases", "exposed_controls", "group_ratio", "ci"
  )

  returns_well_formed(or, .ncol = 10, .nrow = 1, expected_names = expected_names)
})

test_that("Upper limit Risk Difference functions work", {
  rd <- upper_risk_difference(
    upper_limit = .3,
    prob = .9,
    exposed = .4,
    unexposed = .3,
    group_ratio = 3,
    ci = .90
  )

  expected_names <- c(
    "n_exposed", "n_unexposed", "n_total", "risk_difference", "upper_limit",
    "prob", "exposed", "unexposed", "group_ratio", "ci"
  )

  returns_well_formed(rd, .ncol = 10, .nrow = 1, expected_names = expected_names)
})

test_that("Upper limit Rate Difference functions work", {
  rd <- upper_rate_difference(
    upper_limit = .3,
    prob = .9,
    exposed = .4,
    unexposed = .3,
    group_ratio = 3,
    ci = .90
  )

  expected_names <- c(
    "n_exposed", "n_unexposed", "n_total", "rate_difference", "upper_limit",
    "prob", "exposed", "unexposed", "group_ratio", "ci"
  )

  returns_well_formed(rd, .ncol = 10, .nrow = 1, expected_names = expected_names)
})


test_that("Upper limit Risk Ratio functions work", {
  rr <- upper_risk_ratio(
    upper_limit = 2,
    prob = .90,
    exposed = .4,
    unexposed = .3,
    group_ratio = 3
  )

  expected_names <- c(
    "n_exposed", "n_unexposed", "n_total", "risk_ratio", "upper_limit",
    "prob", "exposed", "unexposed", "group_ratio", "ci"
  )

  returns_well_formed(rr, .ncol = 10, .nrow = 1, expected_names = expected_names)
})


test_that("Upper limit Rate Ratio functions work", {
  rr <- upper_rate_ratio(
    upper_limit = 2,
    prob = .90,
    exposed = .01,
    unexposed = .01,
    group_ratio = 1
  )

  expected_names <- c(
    "n_exposed", "n_unexposed", "n_total", "rate_ratio", "upper_limit",
    "prob", "exposed", "unexposed", "group_ratio", "ci"
  )

  returns_well_formed(rr, .ncol = 10, .nrow = 1, expected_names = expected_names)
})

