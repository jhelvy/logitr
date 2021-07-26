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
  data     = yogurt,
  choice   = 'choice',
  obsID    = 'obsID',
  pars     = c('price', 'feat', 'brand'),
  randPars = c(feat = 'n', brand = 'n'),
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 5 starts for brevity:
  numMultiStarts = 5
)

# View summary of model
summary(mxl_pref)

# Get the WTP implied from the preference space model
wtp_mxl_pref <- wtp(mxl_pref, price = 'price')
wtp_mxl_pref

# Multistart MXL model in the WTP Space
mxl_wtp <- logitr(
  data       = yogurt,
  choice     = 'choice',
  obsID      = 'obsID',
  pars       = c('feat', 'brand'),
  price      = 'price',
  randPars   = c(feat = 'n', brand = 'n'),
  modelSpace = 'wtp',
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 5 starts for brevity:
  numMultiStarts = 5,
  # Use the computed WTP from the preference space model as the starting
  # values for the first run:
  startVals = wtp_mxl_pref$Estimate
)

# View summary of model
summary(mxl_wtp)

# Compare WTP from each space
wtp_mxl_comparison <- wtpCompare(mxl_pref, mxl_wtp, price = 'price')
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
