# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the yogurt data
head(yogurt)

# ============================================================================
# Estimate homogeneous MNL models

# Run a MNL model in the Preference Space:
mnl_pref <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'hiland', 'yoplait', 'dannon')
)

# Print a summary of the results:
summary(mnl_pref)

# Get the coefficients from the model:
coef(mnl_pref)

# Get the WTP implied from the preference space model
wtp_mnl_pref <- wtp(mnl_pref, priceName = 'price')
wtp_mnl_pref

# Run a MNL model in the WTP Space using a multistart:
# Extract the WTP computed from the preference space model
# to use as the initial starting values
startingValues <- wtp_mnl_pref$Estimate
mnl_wtp <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'hiland', 'yoplait', 'dannon'),
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
    startVals = startingValues)
)

# Print a summary of all multistart runs and a summary of the best model:
summary(mnl_wtp)

# Print a summary of only the second model run (not the optimal solution):
summary(mnl_wtp$models[[2]])

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
        here::here('inst', 'extdata', 'mnl_pref.Rds'))
saveRDS(mnl_wtp,
        here::here('inst', 'extdata', 'mnl_wtp.Rds'))
saveRDS(wtp_mnl_pref,
        here::here('inst', 'extdata', 'wtp_mnl_pref.Rds'))
saveRDS(wtp_mnl_comparison,
        here::here('inst', 'extdata', 'wtp_mnl_comparison.Rds'))
