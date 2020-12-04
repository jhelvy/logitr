# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the yogurt data
head(yogurt)

# ============================================================================
# Estimate homogeneous MNL models

# Run a MNL model in the Preference Space:
mnl_pref <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'hiland', 'weight', 'yoplait'))

# Print a summary of the results:
summary(mnl_pref)

# Get the coefficients from the model:
coef(mnl_pref)

# Get the WTP implied from the preference space model
wtp_mnl_pref <- wtp(mnl_pref, priceName = 'price')
wtp_mnl_pref

# Run a MNL model in the WTP Space using a multistart:
mnl_wtp <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'hiland', 'weight', 'yoplait'),
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

# Print a summary of only the third model run (not the optimal solution):
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
saveRDS(wtp_mnl_pref,
        here::here('inst', 'extdata', 'wtp_mnl_pref.Rds'))
saveRDS(wtp_mnl_comparison,
        here::here('inst', 'extdata', 'wtp_mnl_comparison.Rds'))

# ============================================================================
# Estimate heterogeneous MXL models

# Multistart MXL model in the Preference Space:
mxl_pref <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'hiland', 'weight', 'yoplait'),
  randPars   = c(feat = 'n'),
  options    = list(
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 1 for brevity:
    numMultiStarts = 1,
    numDraws       = 500))

# View summary of model:
summary(mxl_pref)

# Get the WTP implied from the preference space model
wtp_mxl_pref <- wtp(mxl_pref, priceName = 'price')
wtp_mxl_pref

# Multistart MXL model in the WTP Space:
mxl_wtp <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'hiland', 'weight', 'yoplait'),
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
alts <- subset(yogurt, obsID == 42,
               select = c('feat', 'price', 'hiland', 'weight', 'yoplait'))
row.names(alts) <- c('dannon', 'hiland', 'weight', 'yoplait')
alts

# Run the simulation using the preference space MNL model:
sim_mnl_pref <- simulateShares(mnl_pref, alts, alpha = 0.025)
sim_mnl_pref

# The results show the expected shares for each alternative.
# The low and high values show a 95% confidence interval, estimated using
# simulation. You can change the CI level by setting alpha to a different
# value (e.g. a 90% CI is obtained with alpha = 0.05).

# Run the simulation using the WTP space MNL model:
sim_mnl_wtp <- simulateShares(mnl_wtp, alts, priceName = 'price')
sim_mnl_wtp

# Since these two models are equivalent except in different spaces, the
# simulation results should be the same. Note that 'priceName' is the name
# of the price attribute in the alts argument and must be included for
# WTP space models.

# Simulations can also be run using MXL models in either space:
sim_mxl_pref <- simulateShares(mxl_pref, alts)
sim_mxl_pref

sim_mxl_wtp <- simulateShares(mxl_wtp, alts, priceName = 'price')
sim_mxl_wtp

# Plot simulation results from preference space MNL model:
library(ggplot2)
sim_mnl_pref$alt <- row.names(sim_mnl_pref)
ggplot(sim_mnl_pref, aes(x = alt, y = share_mean)) +
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
