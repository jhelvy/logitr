# Obtain a confidence interval from coefficient draws

Returns a data frame with the columns 'mean', 'lower', and 'upper'
reflecting the mean and lower and upper bounds of a confidence interval
(quantiles) for every column in a data frame of draws

## Usage

``` r
ci(df, level = 0.95)
```

## Arguments

- df:

  A data frame of draws with all numeric columns.

- level:

  The sensitivity of the computed confidence interval (CI). Defaults to
  `level = 0.95`, reflecting a 95% CI.

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

# Obtain 10,000 draws of parameters from model
coefs <- coef(mnl_pref)
covariance <- vcov(mnl_pref)
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))

# Compute a confidence interval
ci(coef_draws, level = 0.95)
#>                    mean      lower      upper
#> price        -0.3667998 -0.4132753 -0.3201336
#> feat          0.4895380  0.2535630  0.7233272
#> brandhiland  -3.7151127 -3.9958466 -3.4308442
#> brandweight  -0.6414189 -0.7465211 -0.5361054
#> brandyoplait  0.7355918  0.5771616  0.8900335
```
