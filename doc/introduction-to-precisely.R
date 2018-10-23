## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>",
  fig.height = 5, 
  fig.width = 6
)

## ---- message=FALSE------------------------------------------------------
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(precisely)

n_risk_difference(
  precision = .08,
  exposed = .4,
  unexposed = .3,
  group_ratio = 3,
  ci = .90
)

## ------------------------------------------------------------------------
n_risk_ratio(
  precision = 2,
  exposed = .4,
  unexposed = .3,
  group_ratio = 3
)

## ------------------------------------------------------------------------
precision_odds_ratio(
  n_cases = 500,
  exposed_cases = .6,
  exposed_controls = .4,
  group_ratio = 2
)

## ------------------------------------------------------------------------
upper_rate_ratio(
  upper_limit = 2,
  prob = .90,
  exposed = .01,
  unexposed = .01,
  group_ratio = 1
)

## ------------------------------------------------------------------------
map_precisely(
  precision_odds_ratio,
  n_cases = seq(from = 500, to = 1000, by = 10),
  exposed_cases = .6,
  exposed_controls = .4,
  group_ratio = 1:4
) %>% 
  group_by("Control/Case Ratio" = factor(group_ratio)) %>% 
  plot_precision()

## ------------------------------------------------------------------------
map_precisely(
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
    scale_x_continuous(breaks = scales::pretty_breaks(3)) + 
    theme_precisely() +
    theme(legend.position = "right") + 
    facet_wrap(~ group_ratio,
               labeller = as_labeller(function(x) paste("Unexposed/Exposed:", x)))

## ---- fig.width=7--------------------------------------------------------
arguments <- crossing(
  precision = 2,
  exposed = seq(from = .01, to = .05, by = .005),
  unexposed = seq(from = .01, to = .05, by = .005),
  group_ratio = 1
) 

arguments %>% 
  pmap_dfr(n_rate_ratio) %>% 
  gather(key, value, exposed, unexposed) %>% 
  ggplot(aes(n_total, rate_ratio, col = factor(value))) +
    geom_line(size = 1.1) + 
    facet_wrap(~key, labeller = as_labeller(function(x) paste("Risk among", x))) +
    labs(
      title = "n Needed for an Upper to Lower CI Ratio of 2",
      x = "Sample Size", 
      y = "Rate Ratio", 
      color = "Risk"
     ) +
    scale_color_viridis_d() +
    theme_minimal(14) +
    theme(
      plot.title = element_text(size = 20, margin = margin(.01, .01, .01, .01)),
      strip.text = element_text(size = 16, color = "grey50", hjust = 0)
    )

## ------------------------------------------------------------------------
lower_risk <- tibble(
  precision = seq(from = .02, to = .20, by = .005),
  exposed = .2,
  unexposed = .1,
  group_ratio = 1
)

higher_risk <- tibble(
  precision = seq(from = .02, to = .20, by = .005),
  exposed = .4,
  unexposed = .3,
  group_ratio = 1
)


risk_difference_data <- bind_rows(lower_risk, higher_risk) %>% 
  pmap_dfr(n_risk_difference)

risk_difference_data

## ------------------------------------------------------------------------
rg_plot <- risk_difference_data %>% 
  ggplot(aes(n_total, precision, col = factor(unexposed))) +
    geom_line(size = 1.1) + 
    xlim(c(0, 9000))

rg_plot

## ------------------------------------------------------------------------
rg_plot +
    ggrepel::geom_text_repel( col = "grey30",
      data = function(x) filter(x, near(n_total, 2500, 100)), 
      aes(label = paste("Risk in Unexposed:", unexposed)), 
      size = 4, segment.size = .5,
      nudge_x = c(-900, 1100),
      nudge_y = c(-.015, .03),
     ) +
    labs(
      x = "Study Size", 
      y = "Confidence Interval Width", 
      color = "Risk in\nUnexposed"
     ) +
    theme_precisely() + 
    theme(legend.position = "none",
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12, color = "grey60")) +
    scale_color_manual(values = c("#E69F00", "#56B4E9"))

