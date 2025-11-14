# Predict choices

Returns the expected choices for a set of one or more alternatives based
on the results from an estimated model.

## Usage

``` r
predictChoices(model, alts, altID, obsID = NULL)
```

## Arguments

- model:

  The output of a model estimated model using the
  [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
  function. Include if you want to compare true choices from actual
  observations (e.g. hold outs) to the predicted choices.

- alts:

  A data frame of a set of alternatives for which to predict choices.
  Each row is an alternative and each column an attribute corresponding
  to parameter names in the estimated model.

- altID:

  The name of the column that identifies each alternative in each set of
  alternatives.

- obsID:

  The name of the column that identifies each set of alternatives.
  Required if predicting results for more than one set of alternatives.
  Defaults to `NULL` (for a single set of alternatives).

## Value

A data frame with the predicted choices for each alternative in `alts`.
