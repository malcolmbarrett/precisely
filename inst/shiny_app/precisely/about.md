## The precisely R Package

You can install the precisely R package from [GitHub](https://github.com/malcolmbarrett/precisely).

These tools are based on the work of [Rothman and Greenland](https://www.ncbi.nlm.nih.gov/pubmed/29912015).

## Sample Size for Precision

These functions calculate the sample size needed to estimate a measure with a
certain precision. For ratio measures, like the risk ratio, rate ratio, and
odds ratio, this is the ratio of the upper to lower limit of the confidence
interval. For difference measures, like the risk difference or rate
difference, this is the absolute width of the confidence interval.

## Precision for Sample Size

These functions calculate the precision of an estimate given a certain sample
size.

## Probability that upper limit is below level of concern

These functions calculate sample size based on probability that upper limit
is below level of concern. The idea behind this approach is to use precision
to provide support for the absence of effect. These functions calculate
sample size where, when the true effect is null, the upper limit of the
confidence interval of the estimate of interest has a probability of being at
or under a specified level of concern.
