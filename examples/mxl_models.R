# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the reformatted 'Yogurt' data set from the mlogit package
head(yogurt)

# ============================================================================
# Estimate heterogeneous MXL models

# Multistart MXL model in the Preference Space:
mxl_pref = logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
  randPars   = c(feat = 'n'),
  options    = list(
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 1 for brevity:
    numMultiStarts = 1,
    numDraws       = 500))

# View summary of model:
summary(mxl_pref)

# Get the WTP implied from the preference space model
wtp_mxl_pref = wtp(mxl_pref, priceName = 'price')
wtp_mxl_pref

# Multistart MXL model in the WTP Space:
mxl_wtp = logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
  priceName  = 'price',
  randPars   = c(feat = 'n'),
  # randPrice  = 'ln',
  modelSpace = 'wtp',
  options = list(
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 1 for brevity:
    numMultiStarts = 1,
    startVals      = wtp_mxl_pref$Estimate,
    startParBounds = c(-5, 5),
    numDraws       = 500))

# View summary of model:
summary(mxl_wtp)

# Compare WTP from each space:
wtp_mxl_comparison <- wtpCompare(mxl_pref, mxl_wtp, priceName = 'price')
wtp_mxl_comparison

# Note that the WTP will not be the same between preference space and WTP
# space MXL models. This is because the distributional assumptions
# in MXL models imply different distributions on WTP depending on the model
# space. See Train and Weeks (2005) and Sonnier, Ainslie, and Otter (2007) for
# details on this topic:

# Train K, Weeks M (2005). “Discrete Choice Models in Preference Space and
# Willingness- to-Pay Space.” In R Scarpa, A Alberini (eds.), Applications of
# Simulation Methods in Environmental and Resource Economics, volume 6 of The
# Economics of Non-Market Goods and Resources, pp. 1-16. Springer-Verlag.

# Sonnier G, Ainslie A, Otter T (2007). “Heterogeneity Distributions of
# Willingness-to-Pay in Choice Models.” Quantitative Marketing and Economics,
# 5(3), 313–331.

# Save results
saveRDS(mxl_pref,
        here::here('examples', 'results', 'mxl_pref.Rds'))
saveRDS(mxl_wtp,
        here::here('examples', 'results', 'mxl_wtp.Rds'))
saveRDS(wtp_mxl_comparison,
        here::here('examples', 'results', 'wtp_mxl_comparison.Rds'))
