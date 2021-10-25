# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Models that can take longer to estimate so the vignettes build faster

# Multistart MXL model in the Preference Space
mxl_pref <- logitr(
  data     = yogurt,
  outcome  = 'choice',
  obsID    = 'obsID',
  pars     = c('price', 'feat', 'brand'),
  randPars = c(feat = 'n', brand = 'n'),
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 5 starts for brevity:
  numMultiStarts = 10
)

# Extract the wtp estimates
wtp_mxl_pref <- wtp(mxl_pref, "price")

# Multistart MXL model in the WTP Space
mxl_wtp <- logitr(
  data       = yogurt,
  outcome    = 'choice',
  obsID      = 'obsID',
  pars       = c('feat', 'brand'),
  price      = 'price',
  randPars   = c(feat = 'n', brand = 'n'),
  modelSpace = 'wtp',
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 5 starts for brevity:
  numMultiStarts = 10,
  # Use the computed WTP from the preference space model as the starting
  # values for the first run:
  startVals = wtp_mxl_pref$Estimate
)

# Compare results
wtpCompare(mxl_pref, mxl_wtp, "price")

# Save results
saveRDS(mxl_pref, here::here('inst', 'extdata', 'mxl_pref.Rds'))
saveRDS(mxl_wtp,  here::here('inst', 'extdata', 'mxl_wtp.Rds'))
