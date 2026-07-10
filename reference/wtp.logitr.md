# Get WTP estimates a preference space model

Returns the computed WTP from a preference space model.

## Usage

``` r
# S3 method for class 'logitr'
wtp(object, scalePar)
```

## Arguments

- object:

  is an object of class `logitr` (a model estimated using the
  [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
  function).

- scalePar:

  The name of the column that identifies the scale variable, which is
  typically "price" for WTP space models, but could be any continuous
  variable, such as "time".

## Value

A data frame of the WTP estimates.

## Details

Willingness to pay is computed by dividing the estimated parameters of a
utility model in the "preference" space by the scale parameter, which is
should be price to obtain WTP estimates. Uncertainty is handled via
simulation.

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

# Compute the WTP implied from the preference space model
wtp(mnl_pref, scalePar = "price")
#>                Estimate Std. Error  z-value  Pr(>|z|)    
#> scalePar       0.366584   0.024296  15.0883 < 2.2e-16 ***
#> feat           1.340574   0.358935   3.7349 0.0001878 ***
#> brandhiland  -10.135727   0.583291 -17.3768 < 2.2e-16 ***
#> brandweight   -1.749077   0.181767  -9.6227 < 2.2e-16 ***
#> brandyoplait   2.003825   0.143428  13.9709 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
