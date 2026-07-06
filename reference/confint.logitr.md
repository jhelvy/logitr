# Extract Model Confidence Interval

Returns confidence intervals from an object of class `logitr`.

## Usage

``` r
# S3 method for class 'logitr'
confint(object, parm, level = 0.95, ...)
```

## Arguments

- object:

  is an object of class `logitr` (a model estimated using the
  'logitr()\` function).

- parm:

  A specification of which parameters are to be given confidence
  intervals, either a vector of numbers or a vector of names. If
  missing, all parameters are considered.

- level:

  The confidence level required.

- ...:

  further arguments.

## Value

A data frame of the confidence intervals of model coefficients.

## Examples

``` r
library(logitr)

# Estimate a preference space model
mnl_pref <- logitr(
  data    = yogurt,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price", "feat", "brand")
)
#> Running model...
#> Done!

# Compute a confidence interval
confint(mnl_pref)
#>                   2.5 %     97.5 %
#> price        -0.4147937 -0.3183578
#> feat          0.2525532  0.7273734
#> brandhiland  -4.0020828 -3.4236696
#> brandweight  -0.7463170 -0.5360524
#> brandyoplait  0.5780011  0.8957437
```
