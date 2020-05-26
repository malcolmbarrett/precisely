context("precisely-plots")

test_that("precisely plots work", {
  library(dplyr)
  library(ggplot2)

  p1 <- map_precisely(
    n_risk_difference,
    precision = seq(from = .02, to = .20, by = .005),
    exposed = .4,
    unexposed = .3,
    group_ratio = 1
  ) %>%
    plot_sample_size()

  p2 <- map_precisely(
    precision_odds_ratio,
    n_cases = seq(from = 500, to = 1000, by = 10),
    exposed_cases = .6,
    exposed_controls = .4,
    group_ratio = 1:4
  ) %>%
    group_by("Control/Case Ratio" = factor(group_ratio)) %>%
    plot_precision()

  p3 <- map_precisely(
    upper_rate_ratio,
    upper_limit = seq(1.5, 2.5, by = .1),
    prob = seq(.50, .95, by = .05),
    exposed = .01,
    unexposed = .01,
    group_ratio = 1:4
  ) %>%
    group_by("Probability" = factor(prob)) %>%
    plot_upper_limit(line_size = 1) +
    scale_color_viridis_d() +
    theme_precisely() +
    theme(legend.position = "right",
          strip.text = element_text(margin = margin(b = 5), hjust = 0)) +
    facet_wrap(~ group_ratio,
               labeller = as_labeller(function(x) paste("Unexposed/Exposed:", x)))

  vdiffr::expect_doppelganger("Basic line plot works", p1)
  vdiffr::expect_doppelganger("Grouped line plot works", p2)
  vdiffr::expect_doppelganger("Themed line plot works", p3)
})
