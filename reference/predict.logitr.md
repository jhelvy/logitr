# Predict probabilities and / or outcomes

This method is used for computing predicted probabilities and / or
outcomes for either the data used for model estimation or a new data set
consisting of a single or multiple sets of alternatives.

## Usage

``` r
# S3 method for class 'logitr'
predict(
  object,
  newdata = NULL,
  obsID = NULL,
  type = "prob",
  returnData = FALSE,
  interval = "none",
  level = 0.95,
  numDrawsCI = 10^4,
  pars = NULL,
  scalePar = NULL,
  randPars = NULL,
  randScale = NULL,
  ci,
  ...
)
```

## Arguments

- object:

  is an object of class `logitr` (a model estimated using the
  'logitr()\` function).

- newdata:

  a `data.frame`. Each row is an alternative and each column an
  attribute corresponding to parameter names in the estimated model.
  Defaults to `NULL`, in which case predictions are made on the original
  data used to estimate the model.

- obsID:

  The name of the column that identifies each set of alternatives in the
  data. Required if newdata != NULL. Defaults to `NULL`, in which case
  the value for `obsID` from the data in `object` is used.

- type:

  A character vector defining what to predict: `prob` for probabilities,
  `outcomes` for outcomes. If you want both outputs, use
  `c("prob", "outcome")`. Outcomes are predicted randomly according to
  the predicted probabilities. Defaults to `"prob"`.

- returnData:

  If `TRUE` the data is also returned, otherwise only the predicted
  values ("prob" and / or "outcome") are returned. Defaults to `FALSE`.

- interval:

  Type of interval calculation: "none" (default) or "confidence". Future
  versions will include "prediction" intervals as well.

- level:

  Tolerance / confidence interval. Defaults to 0.95.

- numDrawsCI:

  The number of draws to use in simulating uncertainty for the
  computed CI. Defaults to 10^4.

- pars:

  The names of the parameters to be estimated in the model. Must be the
  same as the column names in the `data` argument. For WTP space models,
  do not include the `scalePar` variable in `pars`.

- scalePar:

  The name of the column that identifies the scale variable, which is
  typically "price" for WTP space models, but could be any continuous
  variable, such as "time". Defaults to `NULL`.

- randPars:

  A named vector whose names are the random parameters and values the
  distribution: `'n'` for normal, `'ln'` for log-normal, or `'cn'` for
  zero-censored normal. Defaults to `NULL`.

- randScale:

  The random distribution for the scale parameter: `'n'` for normal,
  `'ln'` for log-normal, or `'cn'` for zero-censored normal. Only used
  for WTP space MXL models. Defaults to `NULL`.

- ci:

  No longer used as of v1.1.0 - if provided, this is passed to the
  `level` argument, `interval` is set to `"confidence"`, and a warning
  is displayed.

- ...:

  further arguments.

## Value

A data frame of predicted probabilities and / or outcomes.

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

# Predict probabilities and / or outcomes

# Predict probabilities for each alternative in the model data
probs <- predict(mnl_pref)
head(probs)
#>   obsID predicted_prob
#> 1     1     0.41802407
#> 2     1     0.02118240
#> 3     1     0.23691737
#> 4     1     0.32387615
#> 5     2     0.26643822
#> 6     2     0.02255486

# Create a set of alternatives for which to make predictions.
# Each row is an alternative and each column an attribute.
data <- subset(
    yogurt, obsID %in% c(42, 13),
    select = c('obsID', 'alt', 'price', 'feat', 'brand'))
data
#> # A tibble: 8 × 5
#>   obsID   alt price  feat brand  
#>   <int> <int> <dbl> <dbl> <chr>  
#> 1    13     1  8.1      0 dannon 
#> 2    13     2  5.00     0 hiland 
#> 3    13     3  8.60     0 weight 
#> 4    13     4 10.8      0 yoplait
#> 5    42     1  6.30     0 dannon 
#> 6    42     2  6.10     1 hiland 
#> 7    42     3  7.90     0 weight 
#> 8    42     4 11.5      0 yoplait

# Predict probabilities using the estimated model
predict(mnl_pref, newdata = data, obsID = "obsID")
#>   obsID predicted_prob
#> 1    13     0.43685145
#> 2    13     0.03312986
#> 3    13     0.19155548
#> 4    13     0.33846321
#> 5    42     0.60764778
#> 6    42     0.02602007
#> 7    42     0.17803313
#> 8    42     0.18829902

# Predict probabilities and include a 95% confidence interval
predict(
  mnl_pref,
  newdata = data,
  obsID = "obsID",
  interval = "confidence",
  level = 0.95
)
#>   obsID predicted_prob predicted_prob_lower predicted_prob_upper
#> 1    13     0.43685145           0.41614493           0.45781416
#> 2    13     0.03312986           0.02627197           0.04176048
#> 3    13     0.19155548           0.17623321           0.20826852
#> 4    13     0.33846321           0.31881357           0.35845900
#> 5    42     0.60764778           0.57396714           0.63975396
#> 6    42     0.02602007           0.01828488           0.03671086
#> 7    42     0.17803313           0.16225859           0.19513148
#> 8    42     0.18829902           0.16834918           0.20914257

# Predict outcomes
predict(mnl_pref, newdata = data, obsID = "obsID", type = "outcome")
#>   obsID predicted_outcome
#> 1    13                 1
#> 2    13                 0
#> 3    13                 0
#> 4    13                 0
#> 5    42                 0
#> 6    42                 0
#> 7    42                 0
#> 8    42                 1

# Predict outcomes and probabilities
predict(mnl_pref, newdata = data, obsID = "obsID", type = c("prob", "outcome"))
#>   obsID predicted_prob predicted_outcome
#> 1    13     0.43685145                 0
#> 2    13     0.03312986                 0
#> 3    13     0.19155548                 1
#> 4    13     0.33846321                 0
#> 5    42     0.60764778                 1
#> 6    42     0.02602007                 0
#> 7    42     0.17803313                 0
#> 8    42     0.18829902                 0
```
