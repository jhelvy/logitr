# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# ============================================================================
# Run Market Simulation Using Estimated Models

# Read in saved estimated models
mnl_pref <- readRDS(here::here('inst', 'extdata', 'mnl_pref.Rds'))
mnl_wtp  <- readRDS(here::here('inst', 'extdata', 'mnl_wtp.Rds'))
mxl_pref <- readRDS(here::here('inst', 'extdata', 'mxl_pref.Rds'))
mxl_wtp  <- readRDS(here::here('inst', 'extdata', 'mxl_wtp.Rds'))

# Create a set of alternatives for which to simulate shares. Each row is an
# alternative and each column an attribute. In this example, I just use a
# couple of the choice observations from the yogurt dataset:
alts <- subset(yogurt, obsID %in% c(42, 13),
               select = c('obsID', 'price', 'feat', 'brand'))
alts

# Run the simulation using the preference space MNL model:
sim_mnl_pref <- simulateShares(
  model     = mnl_pref,
  alts      = alts,
  obsIDName = "obsID"
)
sim_mnl_pref

# The results show the expected shares for each alternative.
# The low and high values show a 95% confidence interval, estimated using
# simulation. You can change the CI level by setting alpha to a different
# value (e.g. a 90% CI is obtained with alpha = 0.05).

# Run the simulation using the WTP space MNL model:
sim_mnl_wtp <- simulateShares(
  model     = mnl_wtp,
  alts      = alts,
  obsIDName = "obsID",
  priceName = 'price'
)
sim_mnl_wtp

# Since these two models are equivalent except in different spaces, the
# simulation results should be the same. Note that 'priceName' is the name
# of the price attribute in the alts argument and must be included for
# WTP space models.

# Simulations can also be run using MXL models in either space:
sim_mxl_pref <- simulateShares(
  model     = mxl_pref,
  alts      = alts,
  obsIDName = "obsID"
)
sim_mxl_pref

sim_mxl_wtp <- simulateShares(
  model     = mxl_wtp,
  alts      = alts,
  obsIDName = "obsID",
  priceName = 'price'
)
sim_mxl_wtp

# Save results
saveRDS(sim_mnl_pref,
        here::here('inst', 'extdata', 'sim_mnl_pref.Rds'))
saveRDS(sim_mnl_wtp,
        here::here('inst', 'extdata', 'sim_mnl_wtp.Rds'))
saveRDS(sim_mxl_pref,
        here::here('inst', 'extdata', 'sim_mxl_pref.Rds'))
saveRDS(sim_mxl_wtp,
        here::here('inst', 'extdata', 'sim_mxl_wtp.Rds'))

# Plot simulation results from each model:
library(ggplot2)

sims <- rbind(sim_mnl_pref, sim_mnl_wtp, sim_mxl_pref, sim_mxl_wtp)
sims <- sims[sims$obsID == 1,]
sims$model <- c(rep("mnl_pref", 4), rep("mnl_wtp", 4),
                rep("mxl_pref", 4), rep("mxl_wtp", 4))
sims$alt <- rep(c("dannon", "hiland", "weight", "yoplait"), 4)

ggplot(sims, aes(x = alt, y = share_mean, fill = model)) +
    geom_bar(stat = 'identity', width = 0.7, position = "dodge") +
    geom_errorbar(aes(ymin = share_low, ymax = share_high),
                  width = 0.2, position = position_dodge(width = 0.7)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = 'Alternative', y = 'Expected Share') +
    theme_bw()
