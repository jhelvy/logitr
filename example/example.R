# Install logitr package from github
library('devtools')
install_github('jhelvy/logitr')

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

# Print a summary of the results:
summary(mnl.pref)

# Get the coefficients from the model:
coef(mnl.pref)

# Get the WTP implied from the preference space model
mnl.pref.wtp = wtp.logitr(mnl.pref, priceName='price')
mnl.pref.wtp

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
    # Because the prefSpaceWtp has values as large as 8, I increase the
    # boundaries of the random starting values:
    startParBounds = c(-5,5)))

# Print a summary of all multistart runs and a summary of the best model:
summary(mnl.wtp)

# Print a summary of only the third model run:
summary(mnl.wtp$models[[3]])

# Print a summary of the best model:
summary(mnl.wtp$bestModel)

# NOTE ON LOCAL MINIMA IN WTP SPACE MODELS:
# Comparing the WTP and log-likelihood values between the equivalent models in
# the preference space and WTP space is a helpful check for whether you have
# reached a global solution in WTP space models, which have non-convex
# log-likelihoods functions. This can be done using the wtpCompare function:

wtpCompare(mnl.pref, mnl.wtp, priceName='price')

# ============================================================================
# Estimate Heterogeneous MXL models

# Multistart MXL model in the Preference Space:
mxl.pref = logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
  randPars   = c(price='n', feat='n'),
  options    = list(
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 1 for brevity:
      numMultiStarts = 1,
      keepAllRuns    = TRUE,
      numDraws       = 200))

# View summary of model:
summary(mxl.pref)

# Get the WTP implied from the preference space model
mxl.pref.wtp = wtp.logitr(mxl.pref, priceName='price.mu')
mxl.pref.wtp

# Multistart MXL model in the WTP Space:
mxl.wtp = logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
  priceName  = 'price',
  randPars   = c(feat='n'),
  randPrice  = 'n',
  modelSpace = 'wtp',
  options = list(
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 1 for brevity:
    numMultiStarts = 1,
    keepAllRuns    = TRUE,
    startVals      = mxl.pref.wtp$Estimate,
    startParBounds = c(-5,5),
    numDraws       = 200))

# View summary of model:
summary(mxl.wtp)

# Compare WTP from each space:
wtpCompare(mxl.pref, mxl.wtp, priceName='price.mu')

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

# ============================================================================
# Run market simulation using MNL models

# Create a market to simulate. Each row is an alternative and each column an
# attribute. In this example, I just use the first choice observation from the
# yogurt dataset:
market = subset(yogurt, obsID==1,
         select=c('feat', 'price', 'dannon', 'hiland', 'yoplait'))
market

# Run the simulation using the preference space MNL model:
mnl.pref.simulation = marketSimulation(mnl.pref, market, alpha=0.025)
mnl.pref.simulation
# The results show the expected market shares for each alternative.
# The low and high values show a 95% confidence interval, estimated using
# simulation. You can change the CI level by setting alpha to a different
# value (e.g. a 90% CI is obtained with alpha=0.05).

# Run the simulation using the WTP space MNL model:
mnl.wtp.simulation = marketSimulation(mnl.wtp, market, priceName='price')
mnl.wtp.simulation
# Since these two models are equivalent except in different spaces, the
# simulation results should be the same. Note that 'priceName' is the name
# of the price attribute in the market argument and must be included for
# WTP space models.

# Plot simulation results
library(ggplot2)
mnl.pref.simulation$alt = seq(nrow(mnl.pref.simulation))
ggplot(mnl.pref.simulation, aes(x=alt, y=mean)) +
    geom_bar(stat='identity', width=0.7) +
    geom_errorbar(aes(ymin=low, ymax=high), width=0.2) +
    scale_y_continuous(limits=c(0,1)) +
    labs(x='Market Share', y='Alternative') +
    theme_bw()
