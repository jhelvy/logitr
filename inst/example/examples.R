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
  panelID  = 'id',
  pars     = c('price', 'feat', 'brand'),
  randPars = c(feat = 'n', brand = 'n'),
  numMultiStarts = 10
)

# Extract the wtp estimates
wtp_mxl_pref <- wtp(mxl_pref, "price")

# Multistart MXL model in the WTP Space
mxl_wtp <- logitr(
  data       = yogurt,
  outcome    = 'choice',
  obsID      = 'obsID',
  panelID    = 'id',
  pars       = c('feat', 'brand'),
  price      = 'price',
  randPars   = c(feat = 'n', brand = 'n'),
  modelSpace = 'wtp',
  numMultiStarts = 10,
  startVals = wtp_mxl_pref$Estimate
)

# Compare results
wtpCompare(mxl_pref, mxl_wtp, "price")

# Multistart MXL model in the Preference Space with correlated heterogeneity
mxl_pref_cor <- logitr(
  data     = yogurt,
  outcome  = 'choice',
  obsID    = 'obsID',
  panelID  = 'id',
  pars     = c('price', 'feat', 'brand'),
  randPars = c(feat = 'n', brand = 'n'),
  numMultiStarts = 10,
  correlation = TRUE
)

# Save results
saveRDS(mxl_pref, here::here('inst', 'extdata', 'mxl_pref.Rds'))
saveRDS(mxl_wtp,  here::here('inst', 'extdata', 'mxl_wtp.Rds'))
saveRDS(mxl_pref_cor, here::here('inst', 'extdata', 'mxl_pref_cor.Rds'))
