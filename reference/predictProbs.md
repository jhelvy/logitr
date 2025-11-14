# Predict expected choice probabilities

Returns the expected choice probabilities for a single set or multiple
sets of alternatives based on the results from an estimated model.

## Usage

``` r
predictProbs(
  model,
  alts,
  altID,
  obsID = NULL,
  computeCI = TRUE,
  ci = 0.95,
  numDraws = 10^4,
  alpha
)
```

## Arguments

- model:

  The output of a model estimated model using the
  [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
  function.

- alts:

  A data frame of a set of alternatives for which to predict choice
  probabilities. Each row is an alternative and each column an attribute
  corresponding to parameter names in the estimated model.

- altID:

  The name of the column that identifies each alternative in each set of
  alternatives.

- obsID:

  The name of the column that identifies each set of alternatives.
  Required if predicting results for more than one set of alternatives.
  Defaults to `NULL` (for a single set of alternatives).

- computeCI:

  Should a confidence interval be computed? Defaults to `TRUE`.

- ci:

  The sensitivity of the computed confidence interval (CI). Defaults to
  `ci = 0.95`, reflecting a 95% CI.

- numDraws:

  The number of draws to use in simulating uncertainty for the computed
  confidence interval.

- alpha:

  The sensitivity of the computed confidence interval. No longer used as
  of v0.2.7 - if provided, a warning is shown and `ci` is computed from
  `alpha`.

## Value

A data frame with the estimated choice probabilities for each
alternative in `alts`.
