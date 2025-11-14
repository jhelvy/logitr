# Predict probabilities and / or outcomes

This function is a faster implementation of the "type 7"
[`quantile()`](https://rdrr.io/r/stats/quantile.html) algorithm and is
modified from this gist:
https://gist.github.com/sikli/f1775feb9736073cefee97ec81f6b193 It
returns sample quantiles corresponding to the given probabilities. The
smallest observation corresponds to a probability of 0 and the largest
to a probability of 1. For speed, output quantile names are removed as
are error handling such as checking if x are factors, or if probs lie
outside the `[0,1]` range.

## Usage

``` r
fquantile(x, probs = seq(0, 1, 0.25), na.rm = FALSE)
```

## Arguments

- x:

  numeric vector whose sample quantiles are wanted. `NA` and `NaN`
  values are not allowed in numeric vectors unless `na.rm` is `TRUE`.

- probs:

  numeric vector of probabilities with values in `[0,1]`. (Values up to
  `2e-14` outside that range are accepted and moved to the nearby
  endpoint.)

- na.rm:

  logical; if `TRUE`, any `NA` and `NaN`'s are removed from `x` before
  the quantiles are computed.

## Value

A vector of length `length(probs)` is returned;

## Examples

``` r
library(logitr)
```
