# Compute logit fraction for sets of alternatives given coefficient draws

Returns a data frame of the predicted probabilities (with a confidence
interval) for a data frame of alternatives given coefficient draws.
WARNING: Most of the time you probably want to use
[`predict()`](https://rdrr.io/r/stats/predict.html) instead of this
function. Where `logit_probs()` is useful is if you estimate a model
with an interaction parameter to see differences between groups. In
those cases, you can obtain draws of the estimated parameters and then
use the draws to predict probabilities for each group after summing
together the appropriate columns of the draws for each group. Also note
that this function is only useful for multinomial logit models and is
not appropriate for mixed logit models.

## Usage

``` r
logit_probs(object, coef_draws, newdata, obsID = NULL, level = 0.95)
```

## Arguments

- object:

  is an object of class `logitr` (a model estimated using the
  'logitr()\` function).

- coef_draws:

  A data frame of coefficients draws.

- newdata:

  A data frame of sets of alternatives for which to compute logit
  probabilities. Each row is an alternative.

- obsID:

  The name of the column in `newdata` that identifies each set of
  alternatives. Defaults to `NULL`, in which case it assumes the newdata
  are all one choice scenario.

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

# Create a set of alternatives for which to simulate probabilities
# (Columns are attributes, rows are alternatives)
data <- data.frame(
  altID       = c(1, 2, 3, 4),
  obsID       = c(1, 1, 1, 1),
  price       = c(8, 6, 7, 10),
  feat    = c(0, 1, 0, 0),
  brand   = c('dannon', 'hiland', 'weight', 'yoplait')
)

# Obtain 10,000 draws of parameters from model
coefs <- coef(mnl_pref)
covariance <- vcov(mnl_pref)
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))

# Compute the probabilities
sim <- logit_probs(
  mnl_pref,
  coef_draws = coef_draws,
  newdata = data,
  obsID = 'obsID',
  level = 0.95
)
```
