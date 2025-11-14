# Tidy a `logitr` class object

Tidy a `logitr` class object

## Usage

``` r
# S3 method for class 'logitr'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)
```

## Arguments

- x:

  is an object of class `logitr`.

- conf.int:

  Logical indicating whether or not to include a confidence interval in
  the tidied output. Defaults to FALSE.

- conf.level:

  The confidence level to use for the confidence interval if conf.int =
  TRUE. Must be strictly greater than 0 and less than 1. Defaults to
  0.95, which corresponds to a 95 percent confidence interval.

- ...:

  Unused, included for generic consistency only.

## Value

A tidy
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
summarizing component-level information about the model

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

# Extract a tibble of the model coefficients
tidy(mnl_pref)
#> # A tibble: 5 × 5
#>   term         estimate std.error statistic   p.value
#>   <chr>           <dbl>     <dbl>     <dbl>     <dbl>
#> 1 price          -0.367    0.0244    -15.0  0        
#> 2 feat            0.491    0.120       4.09 0.0000425
#> 3 brandhiland    -3.72     0.145     -25.6  0        
#> 4 brandweight    -0.641    0.0545    -11.8  0        
#> 5 brandyoplait    0.735    0.0806      9.11 0        

# Extract a tibble of the model coefficients with confidence intervals
tidy(mnl_pref, conf.int = TRUE)
#> # A tibble: 5 × 7
#>   term         estimate std.error statistic   p.value conf.low conf.high
#>   <chr>           <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#> 1 brandhiland    -3.72     0.145     -25.6  0           -4.00     -3.43 
#> 2 brandweight    -0.641    0.0545    -11.8  0           -0.749    -0.534
#> 3 brandyoplait    0.735    0.0806      9.11 0            0.578     0.891
#> 4 feat            0.491    0.120       4.09 0.0000425    0.261     0.725
#> 5 price          -0.367    0.0244    -15.0  0           -0.414    -0.319
```
