# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the reformatted 'Yogurt' data set from the mlogit package
head(yogurt)

# ============================================================================
# Estimate Homogeneous MNL models

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
wtpCompare(mnl_pref, mnl_wtp, priceName = 'price')

# Save results
saveRDS(mnl_pref, here::here('examples', 'models', 'mnl_pref.Rds'))
saveRDS(wtp_mnl_pref, here::here('examples', 'models', 'wtp_mnl_pref.Rds'))
saveRDS(mnl_wtp, here::here('examples', 'models', 'mnl_wtp.Rds'))

# ============================================================================
# Estimate Homogeneous MNL model with weights

# Often times, researchers oversample or undersample specific groups of
# people, resulting in a dataset that is not proportionally consistent with
# a desired sample population. To account for this, you can add weights to the
# model that proportionally increase the "weight" of specific choice
# observations. Weight values are evaluated relative to 1. For example, a
# value of 0.2 would weight the choice observation to be 1/5 of that of other
# observations, and a value of 5 would weight the choice observation to be
# 5 times of that of other observations. Weights spanning between 0.2 and 5
# would result in some choice observations being weighted as much as 10 times
# those of others.

# To enable weighting, your data should have a column variable storing the
# weight value to use for each observation. Note that the same weight value
# should be repeated across rows of alternatives from the same choice
# observation.

# To include these weights in the model estimation, provide the column name
# for the "weightsName" argument to the logitr() function. Here is an example:

# First, define the weights. Here is an example with different random weights
# for different ids
w <- data.frame(id = seq(max(yogurt$id)))
w$w <- sample(c(0.25, 0.5, 1, 2, 4), nrow(w), replace = TRUE)
yogurt_w <- merge(yogurt, w, by = "id")

# Run a MNL model in the Preference Space with weights:
mnl_pref_w = logitr(
  data       = yogurt_w,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
  weightsName = 'w') # Including this parameter enables weighting

# Print a summary of the results:
summary(mnl_pref_w)

# Compare the coefficients and log-likelihood from the weighted model to
# those of the unweighted model:
data.frame(
  Unweighted = coef(mnl_pref),
  Weighted = coef(mnl_pref_w))
c("Unweighted" = mnl_pref$logLik, "Weighted" = mnl_pref_w$logLik)

# ============================================================================
# Estimate Heterogeneous MXL models

# Multistart MXL model in the Preference Space:
mxl_pref = logitr(
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
summary(mxl_pref)

# Get the WTP implied from the preference space model
mxl_pref_wtp = wtp(mxl_pref, priceName='price')
mxl_pref_wtp

# Multistart MXL model in the WTP Space:
mxl_wtp = logitr(
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
    startVals      = mxl_pref_wtp$Estimate,
    startParBounds = c(-5,5),
    numDraws       = 500))

# View summary of model:
summary(mxl_wtp)

# Compare WTP from each space:
wtpCompare(mxl_pref, mxl_wtp, priceName='price')

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
# alternative and each column an attribute. In this example, I just use one
# of the choice observations from the yogurt dataset:
market = subset(yogurt, obsID == 42,
              select = c('feat', 'price', 'dannon', 'hiland', 'yoplait'))
row.names(market) = c('dannon', 'hiland', 'weight', 'yoplait')
market

# Run the simulation using the preference space MNL model:
mnl_pref_simulation = simulateShares(mnl_pref, market, alpha = 0.025)
mnl_pref_simulation

# The results show the expected shares for each alternative.
# The low and high values show a 95% confidence interval, estimated using
# simulation. You can change the CI level by setting alpha to a different
# value (e.g. a 90% CI is obtained with alpha = 0.05).

# Run the simulation using the WTP space MNL model:
mnl_wtp_simulation = simulateShares(mnl_wtp, market, priceName = 'price')
mnl_wtp_simulation

# Since these two models are equivalent except in different spaces, the
# simulation results should be the same. Note that 'priceName' is the name
# of the price attribute in the market argument and must be included for
# WTP space models.

# Simulations can also be run using MXL models in either space:
mxl_pref_simulation = simulateShares(mxl_pref, market)
mxl_pref_simulation

mxl_wtp_simulation = simulateShares(mxl_wtp, market, priceName = 'price')
mxl_wtp_simulation

# Plot simulation results from preference space MNL model:
library(ggplot2)
mnl_pref_simulation$alt = row.names(mnl_pref_simulation)
ggplot(mnl_pref_simulation, aes(x = alt, y = share_mean)) +
    geom_bar(stat = 'identity', width = 0.7, fill = "dodgerblue") +
    geom_errorbar(aes(ymin = share_low, ymax = share_high), width = 0.2) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = 'Alternative', y = 'Expected Share') +
    theme_bw()

# ============================================================================
# Assess design of experiment sample size

# If you are designing a conjoint survey, you probably would like to know
# the sample size required to produce an informative model. In the example
# below, assume that the yogurt data was not a completed survey but rather a
# blank design of experiment with no observed choices. The sampleSizer function
# fills out the survey with random choices and estimates a model. The function
# does this multiple times with an increasing number of observations, set by
# the "nbreaks" argument. While the coefficients in those models are
# meaningless, the standard errors on the coefficients are informative. The
# example below estimates 10 separate models and then plots the standard errors
# against the number of observations@.

test <- sampleSizer(
  data       = yogurt,
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
  nbreaks    = 10,
  plot       = TRUE)
