# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# ============================================================================
# Predict Market Shares Using Estimated Models

# Read in saved estimated models
mnl_pref <- readRDS(here::here('inst', 'extdata', 'mnl_pref.Rds'))
mnl_wtp  <- readRDS(here::here('inst', 'extdata', 'mnl_wtp.Rds'))
mxl_pref <- readRDS(here::here('inst', 'extdata', 'mxl_pref.Rds'))
mxl_wtp  <- readRDS(here::here('inst', 'extdata', 'mxl_wtp.Rds'))

# Create a set of alternatives for which to predict shares. Each row is an
# alternative and each column an attribute. In this example, I just use a
# couple of the choice observations from the yogurt dataset:
alts <- subset(yogurt, obsID %in% c(42, 13),
               select = c('obsID', 'price', 'feat', 'brand'))
alts

# Run the simulation using the preference space MNL model:
shares_mnl_pref <- predictShares(
  model     = mnl_pref,
  alts      = alts,
  obsIDName = "obsID"
)

shares_mnl_pref

# The results show the expected shares for each alternative.
# The low and high values show a 95% confidence interval, estimated using
# simulation. You can change the CI level by setting alpha to a different
# value (e.g. a 90% CI is obtained with alpha = 0.05).

# Run the simulation using the WTP space MNL model:
shares_mnl_wtp <- predictShares(
  model     = mnl_wtp,
  alts      = alts,
  obsIDName = "obsID",
  priceName = 'price'
)

shares_mnl_wtp

# Since these two models are equivalent except in different spaces, the
# simulation results should be the same. Note that 'priceName' is the name
# of the price attribute in the alts argument and must be included for
# WTP space models.

# Simulations can also be run using MXL models in either space:
shares_mxl_pref <- predictShares(
  model     = mxl_pref,
  alts      = alts,
  obsIDName = "obsID"
)

shares_mxl_pref

shares_mxl_wtp <- predictShares(
  model     = mxl_wtp,
  alts      = alts,
  obsIDName = "obsID",
  priceName = 'price'
)

shares_mxl_wtp

# Save results
saveRDS(shares_mnl_pref,
        here::here('inst', 'extdata', 'shares_mnl_pref.Rds'))
saveRDS(shares_mnl_wtp,
        here::here('inst', 'extdata', 'shares_mnl_wtp.Rds'))
saveRDS(shares_mxl_pref,
        here::here('inst', 'extdata', 'shares_mxl_pref.Rds'))
saveRDS(shares_mxl_wtp,
        here::here('inst', 'extdata', 'shares_mxl_wtp.Rds'))

# Plot simulation results from each model:
library(ggplot2)
library(dplyr)

shares <- rbind(
  shares_mnl_pref, shares_mnl_wtp, shares_mxl_pref, shares_mxl_wtp) %>%
  mutate(
    model = c(rep("mnl_pref", 8), rep("mnl_wtp", 8),
              rep("mxl_pref", 8), rep("mxl_wtp", 8)),
    alt = rep(c("dannon", "hiland", "weight", "yoplait"), 8),
    obs = paste0("Observation ID: ", obsID)
  )

ggplot(shares, aes(x = alt, y = share_mean, fill = model)) +
    geom_bar(stat = 'identity', width = 0.7, position = "dodge") +
    geom_errorbar(aes(ymin = share_low, ymax = share_high),
                  width = 0.2, position = position_dodge(width = 0.7)) +
    facet_wrap(vars(obs)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = 'Alternative', y = 'Expected Share') +
    theme_bw()
