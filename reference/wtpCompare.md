# Compare WTP from preference and WTP space models

Returns a comparison of the WTP between a preference space and WTP space
model.

## Usage

``` r
wtpCompare(model_pref, model_wtp, scalePar)
```

## Arguments

- model_pref:

  The output of a "preference space" model estimated using the
  [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
  function.

- model_wtp:

  The output of a "willingness to pay space" model estimated using the
  [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
  function.

- scalePar:

  The name of the column that identifies the scale variable, which is
  typically "price" for WTP space models, but could be any continuous
  variable, such as "time".

## Value

A data frame comparing the WTP estimates from preference space and WTP
space models.

## Details

Willingness to pay (WTP) is first computed from the preference space
model by dividing the estimated parameters by the scale parameter
(typically "price" to obtain WTP estimates). Then those estimates are
compared against the WTP values directly estimated from the "WTP" space
model. Uncertainty is handled via simulation.

## Examples

``` r
library(logitr)

# Estimate a MNL model in the Preference space
mnl_pref <- logitr(
  data    = yogurt,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price", "feat", "brand")
)
#> Running model...
#> Done!

# Compute the WTP implied from the preference space model
wtp_mnl_pref <- wtp(mnl_pref, scalePar = "price")

# Estimate a MNL model in the WTP Space, using the computed WTP values
# from the preference space model as starting points
mnl_wtp <- logitr(
  data      = yogurt,
  outcome   = "choice",
  obsID     = "obsID",
  pars      = c("feat", "brand"),
  scalePar  = "price",
  startVals = wtp_mnl_pref$Estimate
)
#> Running model...
#> Done!

# Compare the WTP between the two spaces
wtpCompare(mnl_pref, mnl_wtp, scalePar = "price")
#>                       pref           wtp  difference
#> scalePar         0.3665845     0.3665845 -0.00000001
#> feat             1.3405737     1.3405736 -0.00000014
#> brandhiland    -10.1357272   -10.1357273 -0.00000009
#> brandweight     -1.7490765    -1.7490766 -0.00000003
#> brandyoplait     2.0038253     2.0038253  0.00000000
#> logLik       -2656.8878779 -2656.8878779  0.00000000
```
