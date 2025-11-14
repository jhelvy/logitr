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
  'logitr()\` function).

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
#> scalePar       0.366555   0.024294  15.0881 < 2.2e-16 ***
#> feat           1.340699   0.358974   3.7348 0.0001879 ***
#> brandhiland  -10.136219   0.583376 -17.3751 < 2.2e-16 ***
#> brandweight   -1.749094   0.181769  -9.6226 < 2.2e-16 ***
#> brandyoplait   2.003848   0.143431  13.9708 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
