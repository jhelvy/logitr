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
mnl.pref.wtp = wtp(mnl.pref, priceName='price')
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
    # Since WTP space models are non-convex, run a multistart:
    numMultiStarts = 10,
    # If you want to view the results from each multistart run,
    # set keepAllRuns=TRUE:
    keepAllRuns = TRUE,
    # Use the computed WTP from the preference space model as the starting
    # values for the first run:
    startVals = mnl.pref.wtp$Estimate,
    # Because the computed WTP from the preference space model has values
    # as large as 8, I increase the boundaries of the random starting values:
    startParBounds = c(-5,5)))

# Print a summary of all multistart runs and a summary of the best model:
summary(mnl.wtp)

# Print a summary of only the third model run:
summary(mnl.wtp$models[[3]])

# Print a summary of the best model:
summary(mnl.wtp$bestModel)

# Get the coefficients from the model:
coef(mnl.wtp)

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
  randPars   = c(feat='n'),
  options    = list(
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 1 for brevity:
    numMultiStarts = 1,
    numDraws       = 500))

# View summary of model:
summary(mxl.pref)

# Get the WTP implied from the preference space model
mxl.pref.wtp = wtp(mxl.pref, priceName='price')
mxl.pref.wtp

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

# View summary of model:
summary(mxl.wtp)

# Compare WTP from each space:
wtpCompare(mxl.pref, mxl.wtp, priceName='price')

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
# Run Market Simulation Using Estimated Models

# Create a set of alternatives for which to simulate shares. Each row is an
# alternative and each column an attribute. In this example, I just use one of the choice observations from the yogurt dataset:
alts = subset(yogurt, obsID==42,
              select=c('feat', 'price', 'dannon', 'hiland', 'yoplait'))
row.names(alts) = c('dannon', 'hiland', 'weight', 'yoplait')
alts

# Run the simulation using the preference space MNL model:
mnl.pref.simulation = simulateShares(mnl.pref, alts, alpha=0.025)
mnl.pref.simulation
# The results show the expected shares for each alternative.
# The low and high values show a 95% confidence interval, estimated using
# simulation. You can change the CI level by setting alpha to a different
# value (e.g. a 90% CI is obtained with alpha=0.05).

# Run the simulation using the WTP space MNL model:
mnl.wtp.simulation = simulateShares(mnl.wtp, alts, priceName='price')
mnl.wtp.simulation
# Since these two models are equivalent except in different spaces, the
# simulation results should be the same. Note that 'priceName' is the name
# of the price attribute in the alts argument and must be included for
# WTP space models.

# Simulations can also be run using MXL models in either space:
mxl.pref.simulation = simulateShares(mxl.pref, alts)
mxl.pref.simulation
mxl.wtp.simulation = simulateShares(mxl.wtp, alts, priceName='price')
mxl.wtp.simulation

# Plot simulation results from preference space MNL model:
library(ggplot2)
mnl.pref.simulation$alt = row.names(mnl.pref.simulation)
ggplot(mnl.pref.simulation, aes(x=alt, y=mean)) +
    geom_bar(stat='identity', width=0.7) +
    geom_errorbar(aes(ymin=low, ymax=high), width=0.2) +
    scale_y_continuous(limits=c(0,1)) +
    labs(x='Alternative', y='Market Share') +
    theme_bw()


