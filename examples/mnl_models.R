# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the reformatted 'Yogurt' data set from the mlogit package
head(yogurt)

# ============================================================================
# Estimate homogeneous MNL models

# Run a MNL model in the Preference Space:
mnl_pref = logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))

# Print a summary of the results:
summary(mnl_pref)

# Get the coefficients from the model:
coef(mnl_pref)

# Get the WTP implied from the preference space model
wtp_mnl_pref = wtp(mnl_pref, priceName = 'price')
wtp_mnl_pref

# Run a MNL model in the WTP Space using a multistart:
mnl_wtp = logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
  priceName  = 'price',
  modelSpace = 'wtp',
  options = list(
    # Since WTP space models are non-convex, run a multistart:
    numMultiStarts = 10,
    # If you want to view the results from each multistart run,
    # set keepAllRuns=TRUE:
    keepAllRuns = TRUE,
    # Use the computed WTP from the preference space model as the starting
    # values for the first run:
    startVals = wtp_mnl_pref$Estimate,
    # Because the computed WTP from the preference space model has values
    # as large as 8, I increase the boundaries of the random starting values:
    startParBounds = c(-5, 5)))

# Print a summary of all multistart runs and a summary of the best model:
summary(mnl_wtp)

# Print a summary of only the third model run:
summary(mnl_wtp$models[[3]])

# Print a summary of the best model:
summary(mnl_wtp$bestModel)

# Get the coefficients from the model:
coef(mnl_wtp)

# CHECKING FOR LOCAL MINIMA IN WTP SPACE MODELS:
# Comparing the WTP and log-likelihood values between the equivalent models in
# the preference space and WTP space is a helpful check for whether you have
# reached a global solution in WTP space models, which have non-convex
# log-likelihoods functions. This can be done using the wtpCompare function:
wtp_mnl_comparison <- wtpCompare(mnl_pref, mnl_wtp, priceName = 'price')
wtp_mnl_comparison

# Save results
saveRDS(mnl_pref,
        here::here('examples', 'results', 'mnl_pref.Rds'))
saveRDS(mnl_wtp,
        here::here('examples', 'results', 'mnl_wtp.Rds'))
saveRDS(wtp_mnl_comparison,
        here::here('examples', 'results', 'wtp_mnl_comparison.Rds'))
