setwd('/Users/jhelvy/Documents/GitHub/logitr/vignettes/examples')

# Load logitr package
library('logitr')

# Load the reformatted 'Yogurt' data set from the mlogit package
data(yogurt)

# ============================================================================
# Estimate Homogeneous MNL models

# Run a MNL model in the Preference Space:
mnl.pref = logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))

# Get the WTP implied from the preference space model
mnl.pref.wtp = wtp(mnl.pref, priceName='price')

# Run a MNL model in the WTP Space using a multistart:
mnl.wtp = logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
  priceName  = 'price',
  modelSpace = 'wtp',
  options = list(
    # You should run a multistart for WTP models since they are non-convex:
    numMultiStarts = 10,
    # You can review the results of each multistart run with keepAllRuns=T:
    keepAllRuns = TRUE,
    # It can be useful to use the WTP from the preference space model as the
    # starting values for the first run:
    startVals = mnl.pref.wtp$Estimate,
    # Because the mnl.pref.wtp has values as large as 8, I increase the
    # boundaries of the random starting values:
    startParBounds = c(-5,5)))

# Save results
saveRDS(mnl.pref,     './mnl.pref.Rds')
saveRDS(mnl.pref.wtp, './mnl.pref.wtp.Rds')
saveRDS(mnl.wtp,      './mnl.wtp.Rds')

# ============================================================================
# Estimate Heterogeneous MXL models

# Multistart MXL model in the Preference Space:
mxl.pref = logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
  randPars   = c(feat='n'),
  options    = list(
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 1 for brevity:
    numMultiStarts = 1,
    numDraws       = 500))

# Get the WTP implied from the preference space model
mxl.pref.wtp = wtp(mxl.pref, priceName='price')

# Multistart MXL model in the WTP Space:
mxl.wtp = logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
  priceName  = 'price',
  randPars   = c(feat='n'),
  # randPrice  = 'ln',
  modelSpace = 'wtp',
  options = list(
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 1 for brevity:
    numMultiStarts = 1,
    startVals      = mxl.pref.wtp$Estimate,
    startParBounds = c(-5,5),
    numDraws       = 500))

# Save results
saveRDS(mxl.pref,     './mxl.pref.Rds')
saveRDS(mxl.pref.wtp, './mxl.pref.wtp.Rds')
saveRDS(mxl.wtp,      './mxl.wtp.Rds')

# ============================================================================
# Run Market Simulation Using Estimated Models

# Create a market to simulate.
market = subset(yogurt, obsID==42,
         select=c('feat', 'price', 'dannon', 'hiland', 'yoplait'))
row.names(market) = c('dannon', 'hiland', 'weight', 'yoplait')
market

# Run the simulation using the preference space MNL model:
mnl.pref.simulation = marketSimulation(mnl.pref, market, alpha=0.025)

# Run the simulation using the WTP space MNL model (note that you must denote the "price" variable):
mnl.wtp.simulation = marketSimulation(mnl.wtp, market, priceName='price')

# Run the simulation using the preference space MXL model:
mxl.pref.simulation = marketSimulation(mxl.pref, market, alpha=0.025)

# Run the simulation using the WTP space MXL model (note that you must denote the "price" variable):
mxl.wtp.simulation = marketSimulation(mxl.wtp, market, priceName='price')

# Save results
saveRDS(mnl.pref.simulation, './mnl.pref.simulation.Rds')
saveRDS(mnl.wtp.simulation,  './mnl.wtp.simulation.Rds')
saveRDS(mxl.pref.simulation, './mxl.pref.simulation.Rds')
saveRDS(mxl.wtp.simulation, './mxl.wtp.simulation.Rds')
