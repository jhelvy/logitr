# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the yogurt data
head(yogurt)

# ============================================================================
# Estimate homogeneous MNL models

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
wtp_mnl_comparison <- wtpCompare(mnl_pref, mnl_wtp, priceName = 'price')
wtp_mnl_comparison

# Save results
saveRDS(mnl_pref,
        here::here('inst', 'extdata', 'mnl_pref.Rds'))
saveRDS(mnl_wtp,
        here::here('inst', 'extdata', 'mnl_wtp.Rds'))
saveRDS(wtp_mnl_comparison,
        here::here('inst', 'extdata', 'wtp_mnl_comparison.Rds'))

# ============================================================================
# Estimate weighted homogeneous MNL model

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
mnl_pref <- readRDS(here::here('examples', 'results', 'mnl_pref_w.Rds'))
coef_compare <- data.frame(
  Unweighted = coef(mnl_pref),
  Weighted   = coef(mnl_pref_w))
logLik_compare <- c("Unweighted" = mnl_pref$logLik, "Weighted" = mnl_pref_w$logLik)
coef_compare
logLik_compare

# Save results
saveRDS(mnl_pref_w, here::here('inst', 'extdata', 'mnl_pref_w.Rds'))

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
        here::here('inst', 'extdata', 'mxl_pref.Rds'))
saveRDS(mxl_wtp,
        here::here('inst', 'extdata', 'mxl_wtp.Rds'))
saveRDS(wtp_mxl_comparison,
        here::here('inst', 'extdata', 'wtp_mxl_comparison.Rds'))

# ============================================================================
# Run Market Simulation Using Estimated Models

# Read in saved estimated models
mnl_pref <- readRDS(here::here('inst', 'extdata', 'mnl_pref.Rds'))
mnl_wtp  <- readRDS(here::here('inst', 'extdata', 'mnl_wtp.Rds'))
mxl_pref <- readRDS(here::here('inst', 'extdata', 'mxl_pref.Rds'))
mxl_wtp  <- readRDS(here::here('inst', 'extdata', 'mxl_wtp.Rds'))

# Create a set of alternatives for which to simulate shares. Each row is an
# alternative and each column an attribute. In this example, I just use one
# of the choice observations from the yogurt dataset:
alts = subset(yogurt, obsID == 42,
              select = c('feat', 'price', 'dannon', 'hiland', 'yoplait'))
row.names(alts) = c('dannon', 'hiland', 'weight', 'yoplait')
alts

# Run the simulation using the preference space MNL model:
sim_mnl_pref = simulateShares(mnl_pref, alts, alpha = 0.025)
sim_mnl_pref

# The results show the expected shares for each alternative.
# The low and high values show a 95% confidence interval, estimated using
# simulation. You can change the CI level by setting alpha to a different
# value (e.g. a 90% CI is obtained with alpha = 0.05).

# Run the simulation using the WTP space MNL model:
sim_mnl_wtp = simulateShares(mnl_wtp, alts, priceName = 'price')
sim_mnl_wtp

# Since these two models are equivalent except in different spaces, the
# simulation results should be the same. Note that 'priceName' is the name
# of the price attribute in the alts argument and must be included for
# WTP space models.

# Simulations can also be run using MXL models in either space:
sim_mxl_pref = simulateShares(mxl_pref, alts)
sim_mxl_pref

sim_mxl_wtp = simulateShares(mxl_wtp, alts, priceName = 'price')
sim_mxl_wtp

# Plot simulation results from preference space MNL model:
library(ggplot2)
mnl_pref_simulation$alt = row.names(mnl_pref_simulation)
ggplot(mnl_pref_simulation, aes(x = alt, y = share_mean)) +
    geom_bar(stat = 'identity', width = 0.7, fill = "dodgerblue") +
    geom_errorbar(aes(ymin = share_low, ymax = share_high), width = 0.2) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = 'Alternative', y = 'Expected Share') +
    theme_bw()

# Save results
saveRDS(sim_mnl_wtp,
        here::here('inst', 'extdata', 'sim_mnl_wtp.Rds'))
saveRDS(sim_mnl_pref,
        here::here('inst', 'extdata', 'sim_mnl_pref.Rds'))
saveRDS(sim_mxl_wtp,
        here::here('inst', 'extdata', 'sim_mxl_wtp.Rds'))
saveRDS(sim_mxl_wtp,
        here::here('inst', 'extdata', 'sim_mxl_wtp.Rds'))
