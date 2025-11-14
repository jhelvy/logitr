# Simulate expected shares

This function has been depreciated since logitr version 0.1.4. Use
[`predictProbs()`](https://jhelvy.github.io/logitr/reference/predictProbs.md)
instead.

## Usage

``` r
simulateShares(
  model,
  alts,
  obsIDName = NULL,
  priceName = NULL,
  computeCI = TRUE,
  alpha = 0.025,
  numDraws = 10^4
)
```

## Arguments

- model:

  The output of a model estimated model using the
  [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
  function.

- alts:

  A data frame of a set of alternatives for which to simulate shares.
  Each row is an alternative and each column an attribute corresponding
  to parameter names in the estimated model.

- obsIDName:

  The name of the column that identifies each set of alternatives.
  Required if simulating results for more than one set of alternatives.
  Defaults to `NULL` (for a single set of alternatives).

- priceName:

  The name of the parameter that identifies price. Only required for WTP
  space models. Defaults to `NULL`.

- computeCI:

  Should a confidence interval be computed? Defaults to `TRUE`.

- alpha:

  The sensitivity of the computed confidence interval. Defaults to
  `alpha = 0.025`, reflecting a 95% CI.

- numDraws:

  The number of draws to use in simulating uncertainty for the computed
  confidence interval.

## Value

A data frame with the estimated shares for each alternative in `alts`.
