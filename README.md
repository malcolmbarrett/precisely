
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/malcolmbarrett/precisely.svg?branch=master)](https://travis-ci.org/malcolmbarrett/precisely)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/malcolmbarrett/precisely?branch=master&svg=true)](https://ci.appveyor.com/project/malcolmbarrett/precisely)

# precisely: An R package for estimating sample size based on precision

precisely is a study planning tool to calculate sample size based on
precision rather than power. Power calculations are focused on whether
or not an estimate will be statistically significant; calculations of
precision are based on the same prinicples as power calculation but turn
the focus to the width of the confidence interval.

These tools are based on the work of [Rothman and
Greenland](https://www.ncbi.nlm.nih.gov/pubmed/29912015).

## Installation

You can install the development version of precisely with:

``` r
# install.packages("devtools")
devtools::install_github("malcolmbarrett/precisely")
```

## Example

precisely has functions for studies using risk differences, risk ratios,
rate differences, rate ratios, and odds ratios. The heart of these
calculations is the desired precision. For ratio measures, this is the
ratio of the upper to lower limit of the confidence interval. For
difference measures, this is the absolute width of the confidence
interval. To calculate sample size for a given precision, you also need
proportions for the two groups (the risk or rate in the exposed and
unexposed participants in a cohort study or the exposure prevalence in
the cases and controls in case-control studies), the ratios of the two
groups (the unexposed to the exposed in cohort studies or controls to
cases in case-control studies), and the desired confidence interval
probability. The default is 95%.

Let’s say we want to calculate the sample size needed to estimate a 90%
CI for a risk difference of .1 with an absolute width of .08. Here, the
risk among the exposed is .4, the risk among the unexposed is .3, and
there are three times as many unexposed participants.

``` r
library(precisely)

n_risk_difference(
  precision = .08,
  exposed = .4,
  unexposed = .3,
  group_ratio = 3,
  ci = .90
)
#> # A tibble: 1 x 5
#>   n_exposed n_unexposed n_total risk_difference precision
#>       <dbl>       <dbl>   <dbl>           <dbl>     <dbl>
#> 1       525        1573    2098             0.1      0.08
```

We need 525 exposed participants and 1,573 unexposed participants for a
total sample size of 2,098. For the same population, what sample size
would we need to estimate a risk ratio where the upper bound of a 95% CI
has a ratio of 2 with the lower bound?

``` r
n_risk_ratio(
  precision = 2,
  exposed = .4,
  unexposed = .3,
  group_ratio = 3
)
#> # A tibble: 1 x 5
#>   n_exposed n_unexposed n_total risk_ratio precision
#>       <dbl>       <dbl>   <dbl>      <dbl>     <dbl>
#> 1        73         219     292       1.33         2
```

Here we only need 73 exposed participants and 219 unexposed
participants, a total sample size of 292.

precisely also provides functionality to calculate the precision of an
estimate given a sample size. For instance, in a case-control study with
500 cases, 1,000 controls and prevalences among the cases and controls
of .6 and .4, respectively, what is the ratio of the upper bound to the
lower bound?

``` r
precision_odds_ratio(
  n_cases = 500,
  exposed_cases = .6,
  exposed_controls = .4,
  group_ratio = 2
)
#> # A tibble: 1 x 5
#>   precision odds_ratio n_cases n_controls n_total
#>       <dbl>      <dbl>   <dbl>      <dbl>   <dbl>
#> 1      1.55       2.25     500       1000    1500
```

We can expect that, on average, the precision will be 1.55.

precisely also has a set of functions for a different way of using
precision: to calculate sample size based on probability that the upper
limit of the confidence interval is below a level of concern. The idea
here is that, if there is no true effect, we want to know how likely it
is that we have a high estimate. For instance, for a rate ratio where
the effect is null, we can calculate the sample size for which there is
a 90% probability that the upper limit will be at or below 2.

``` r
upper_rate_ratio(
  upper_limit = 2,
  prob = .90,
  exposed = .01,
  unexposed = .01,
  group_ratio = 1
)
#> # A tibble: 1 x 6
#>   n_exposed n_unexposed n_total rate_ratio upper_limit  prob
#>       <dbl>       <dbl>   <dbl>      <dbl>       <dbl> <dbl>
#> 1      4374        4374    8748          1           2   0.9
```
