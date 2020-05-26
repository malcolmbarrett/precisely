test_that("mapping returns correctly", {
  x <- map_precisely(
    n_risk_difference,
    precision = seq(from = .02, to = .20, by = .005),
    exposed = c(.2, .4),
    unexposed = c(.1, .3),
    group_ratio = 1
  )

  expected_names <- c(
    "n_exposed", "n_unexposed", "n_total", "risk_difference",
    "precision", "exposed", "unexposed", "group_ratio", "ci"
  )

  expect_s3_class(x, "tbl")
  expect_length(x, 9)
  expect_equal(nrow(x), 148)
  expect_named(x, expected_names)
})
