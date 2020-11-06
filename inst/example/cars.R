# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the yogurt data
head(cars_us)

# ============================================================================
# Estimate weighted homogeneous MNL model

# Often times, researchers oversample or undersample specific groups of
# people, resulting in a dataset that is not proportionally consistent with
# a desired sample population. To account for this, you can add weights to the
# model that proportionally increase the "weight" of specific choice
# observations. Weight values are evaluated relative to 1. For example, a
# value of 0.2 would weight the choice observation to be 1/5 of that of other
# observations, and a value of 5 would weight the choice observation to be
# 5 times of that of other observations. Weights spanning between 0.2 and 5
# would result in some choice observations being weighted as much as 10 times
# those of others.

# To enable weighting, your data should have a column variable storing the
# weight value to use for each observation. Note that the same weight value
# should be repeated across rows of alternatives from the same choice
# observation.

# To include these weights in the model estimation, provide the column name
# for the "weightsName" argument to the logitr() function. Here is an example:

# Run a MNL model in the WTP Space:
model_unweighted <- logitr(
  data       = cars_us,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  priceName = 'price',
  modelSpace = 'wtp'
)

# Run a weighted MNL model in the Preference Space:
model_weighted <- logitr(
  data       = cars_us,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  priceName = 'price',
  modelSpace = 'wtp',
  weightsName = 'weights' # This is the key argument for enabling weights
)

# Print a summary of the results:
summary(model_unweighted)
summary(model_weighted)

# Compare the coefficients and log-likelihood from the weighted model to
# those of the unweighted model:
coef_compare <- data.frame(
  Unweighted = coef(model_unweighted),
  Weighted   = coef(model_weighted))
logLik_compare <- c(
  "Unweighted" = model_unweighted$logLik,
  "Weighted" = model_weighted$logLik)
coef_compare
logLik_compare

# Save results
saveRDS(model_unweighted,
        here::here('inst', 'extdata', 'model_unweighted.Rds'))
saveRDS(model_weighted,
        here::here('inst', 'extdata', 'model_weighted.Rds'))
