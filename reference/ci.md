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
#> price        -0.3668163 -0.4133064 -0.3201620
#> feat          0.4895479  0.2535552  0.7233242
#> brandhiland  -3.7151683 -3.9959753 -3.4309621
#> brandweight  -0.6414737 -0.7465685 -0.5361505
#> brandyoplait  0.7355967  0.5772076  0.8900850
```
