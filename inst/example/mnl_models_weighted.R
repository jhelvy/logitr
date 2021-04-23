# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# ============================================================================
# Estimate weighted and unweighted homogeneous MNL models

# Estimate an unweighted MNL model in the WTP Space using a multistart
mnl_wtp_unweighted <- logitr(
  data       = cars_us,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  priceName = 'price',
  modelSpace = 'wtp',
  options = list(
    # Since WTP space models are non-convex, run a multistart:
    numMultiStarts = 10)
)

# Print a summary of all multistart runs and a summary of the best model
summary(mnl_wtp_unweighted)

# Estimate a weighted MNL model in the WTP Space using a multistart
mnl_wtp_weighted <- logitr(
  data       = cars_us,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  priceName = 'price',
  modelSpace = 'wtp',
  weightsName = 'weights', # This is the key argument for enabling weights
  options = list(
    # Since WTP space models are non-convex, run a multistart:
    numMultiStarts = 10)
)

# Print a summary of all multistart runs and a summary of the best model:
summary(mnl_wtp_weighted)

# Compare the coefficients between the weighted and unweighted models:
coef_compare <- data.frame(
  Unweighted = coef(mnl_wtp_unweighted),
  Weighted   = coef(mnl_wtp_weighted))
coef_compare

# Compare the log-likelihood between the weighted and unweighted models:
logLik_compare <- c(
  "Unweighted" = mnl_wtp_unweighted$logLik,
  "Weighted" = mnl_wtp_weighted$logLik)
logLik_compare

# Save results
saveRDS(mnl_wtp_unweighted,
        here::here('inst', 'extdata', 'mnl_wtp_unweighted.Rds'))
saveRDS(mnl_wtp_weighted,
        here::here('inst', 'extdata', 'mnl_wtp_weighted.Rds'))
