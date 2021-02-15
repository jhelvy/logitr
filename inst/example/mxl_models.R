# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the yogurt data
head(yogurt)

# ============================================================================
# Estimate heterogeneous MXL models

# Multistart MXL model in the Preference Space
mxl_pref <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'hiland', 'yoplait', 'dannon'),
  randPars   = c(feat = 'n', hiland = 'n', yoplait = 'n', dannon = 'n'),
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 5 starts for brevity:
  options    = list(numMultiStarts = 5)
)

# View summary of model
summary(mxl_pref)

# Get the WTP implied from the preference space model
wtp_mxl_pref <- wtp(mxl_pref, priceName = 'price')
wtp_mxl_pref

# Multistart MXL model in the WTP Space
# Extract the WTP computed from the preference space model
# to use as the initial starting values
startingValues <- wtp_mxl_pref$Estimate
mxl_wtp <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'hiland', 'yoplait', 'dannon'),
  priceName  = 'price',
  randPars   = c(feat = 'n', hiland = 'n', yoplait = 'n', dannon = 'n'),
  modelSpace = 'wtp',
  options    = list(
    # You should run a multistart for MXL models since they are non-convex,
    # but it can take a long time. Here I just use 5 starts for brevity:
    numMultiStarts = 5,
    # Use the computed WTP from the preference space model as the starting
    # values for the first run:
    startVals = startingValues)
)

# View summary of model
summary(mxl_wtp)

# Compare WTP from each space
wtp_mxl_comparison <- wtpCompare(mxl_pref, mxl_wtp, priceName = 'price')
wtp_mxl_comparison

# Save results
saveRDS(mxl_pref,
        here::here('inst', 'extdata', 'mxl_pref.Rds'))
saveRDS(mxl_wtp,
        here::here('inst', 'extdata', 'mxl_wtp.Rds'))
saveRDS(wtp_mxl_pref,
        here::here('inst', 'extdata', 'wtp_mxl_pref.Rds'))
saveRDS(wtp_mxl_comparison,
        here::here('inst', 'extdata', 'wtp_mxl_comparison.Rds'))
