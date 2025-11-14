# Glance a `logitr` class object

Glance a `logitr` class object

## Usage

``` r
# S3 method for class 'logitr'
glance(x, ...)
```

## Arguments

- x:

  is an object of class `logitr`.

- ...:

  further arguments.

## Value

A tibble of the model summary statistics.

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

# Extract a tibble of the model summary statistics
glance(mnl_pref)
#> # A tibble: 1 × 7
#>   logLik null.logLik   AIC   BIC r.squared adj.r.squared  nobs
#>    <dbl>       <dbl> <dbl> <dbl>     <dbl>         <dbl> <dbl>
#> 1 -2657.      -3344. 5324. 5353.     0.205         0.204  2412
```
